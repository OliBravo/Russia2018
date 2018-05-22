library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(RODBC)
library(RODBCext)
# library(leaflet)



server <- function(output, input, session){
  
  
  options(DT.options = list(pageLength = 10 , lengthMenu = c(5, 10, 20, 30)))
  
  px <- dataTableProxy('tblNewBetsMatches')
  
  
  cnx <- db_connect('test', 'test123')
  
  MATCHES <- queryDB(
    cnx,
    sql = "select * from matches_logo2 where team1 != 'NA';"
  )
  
  
  
  MATCH <- reactiveValues(id = NULL,
                          origin = NULL)
  
  # updateSelectInput(
  #   session = session,
  #   inputId = "select-new-bet",
  #   choices = paste(
  #     res$match_id,
  #     paste(res$team1, res$team2, sep = " - "),
  #     paste(res$date, format(res$time, "%H:%M"), res$venue, sep = ", "),
  #     sep = " | "
  #   )
  # )
  
  
  user <- reactiveValues(logged = F,
                         login = NULL,
                         pwd = NULL)
  
  
  Matches <- reactiveValues(to_bet = NULL,
                            already_bet = NULL)
  
  
  BetHistory <- reactive({
    
    Matches$to_bet
    
    cnx <- db_connect(
      login = user$login,
      password = user$pwd
    )
    
    res <- queryDB(
      connection = cnx,
      sql = "select * from vw_Bets where login=?",
      params = data.frame(login = user$login)
    )
    
    
    if (nrow(res) == 0)
      return(NULL)
    else
      res
  })
  
  
  ### is user logged-in?----
  observe({
    # which page should we render:
    
    if (!user$logged){
      # user's not logged in
      output[['bet-dynamic-ui']] <- renderUI({
        
        renderLoginPage()
      })
    } else {
      
      ### matches already bet by the user:----
      
      cnx <- db_connect(user$login, user$pwd)
      
      res <- queryDB(
        cnx,
        "select *
        from matches_to_bet_already_bet(?, ?)
        order by date, time;
        ",
        params = list(
          as.character(Sys.time()),
          user$login
        )
      )
      
      
      Matches$already_bet <- res %>%
        mutate(
          # date = format(date, "%Y-%m-%d"),
          time = substr(time, 1, 5)) %>%
        arrange(match_id)
      
      
      
      ### matches yet unbet:----
      
      cnx <- db_connect(user$login, user$pwd)
      
      res <- queryDB(
        cnx,
        "select *
       from matches_to_bet(?, ?)
       order by date, time;
      ",
        params = list(
          as.character(Sys.time()),
          user$login
        )
      )
      
      res <- res %>%
        # select(match_id, team1, team2, date, time, logo1, logo2, group, group_phase) %>%
        # mutate(bet1 = 0, bet2 = 0, winner_after_penalties = NULL) %>%
        mutate(
          # date = format(date, "%Y-%m-%d"),
          time = substr(time, 1, 5)) %>%
        arrange(match_id)
      
      
      Matches$to_bet <- res
      
      # user is authorized to see the content
      output[['bet-dynamic-ui']] <- renderUI({
        
        renderAuthorised()
        
      })
      
      
      
    }
  })
  
  
  
  ### user clicked Login button:----
  observeEvent(input$btnLogin, {
    
    
    cnx <- db_connect(input$txtLogin, input$txtPassword)
    
    
    if(is.null(cnx)){
      
      shinyjs::toggle("login-failed", condition = T)
      
    } else {
      
      user$logged <- TRUE
      
      user$login <- input$txtLogin
      user$pwd = input$txtPassword
      
      odbcClose(cnx)
      
    }
  })
  
  
  ### Log out:----
  observeEvent(input[['sign-out']], {
    
    user$logged <- F
  })
  
  
  
  ### user is previewing her bets:----
  observeEvent(input[["my-bets"]], {
    
    output[['bet-dynamic-ui']] <- renderUI({
      
      renderMyBets()
    })
  })
  
  
  ### user creates new bets:----
  observeEvent(input$btnNewBet, {
    
    output[['bet-dynamic-ui']] <- renderUI({
      
      
      
      # renderNewBet(MATCHES)
      renderNewBet2()
    })
  })
  
  
  
  observeEvent(input$tblNewBetsMatches_rows_selected, {
    
    # validate(
    #   need(input[['select-new-bet']], F)
    # )
    # 
    # 
    # match_id <- as.numeric(strsplit(input[['select-new-bet']], split = " | ", fixed = T)[[1]][1])
    
    cursor <- input$tblNewBetsMatches_rows_selected
    
    
    
    output[['new-bet-single']] <- renderUI({
      
      # print(input[['select-new-bet']])
      # 
      match <- Matches$to_bet[cursor,]
      
      MATCH$id <- match$match_id
      MATCH$origin <- 'yet_unbet'

      renderNewBetSingle(match)
    })
  })
  
  
  observeEvent(input$tblAlreadyBet_rows_selected, {
    
    cursor <- input$tblAlreadyBet_rows_selected
    
    output[['new-bet-single']] <- renderUI({
      
      match <- Matches$already_bet[cursor,]
      
      MATCH$id <- match$match_id
      MATCH$origin <- 'already_bet'
      
      renderNewBetSingle(match)
    })
    
  })
  
  
  ### user saves a single bet:----
  observeEvent(input$btnNewBetSave, {
    
    
    
    match_id <- MATCH$id
    
    # move to 'already bet' table if needed:
    if (MATCH$origin == 'yet_unbet'){
      
      match_insert <- Matches$to_bet %>%
        filter(match_id == MATCH$id) %>%
        mutate(bet1 = input[['bet-team1-score']],
               bet2 = input[['bet-team2-score']])
      
      Matches$to_bet <- Matches$to_bet %>%
        filter(match_id != MATCH$id)
      
      
      Matches$already_bet <- rbind(Matches$already_bet,
                                   match_insert)
    }
    
    if (MATCH$origin == 'already_bet'){
      
      match_insert <- Matches$already_bet %>%
        filter(match_id == MATCH$id) %>%
        mutate(bet1 = input[['bet-team1-score']],
               bet2 = input[['bet-team2-score']])
      
      
      
      Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$bet1 <- input[['bet-team1-score']]
      Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$bet2 <- input[['bet-team2-score']]
    }
    
    
    
    
    if (match_insert$group_phase == 0 &
        match_insert$bet1 == match_insert$bet2){
      
      pen_win <- ifelse(input[['bet-penalties-winner']] == match_insert$team1, 1, 2)
      
      # if (MATCH$origin == 'already_bet'){
      #   Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$winner_after_penalties <- pen_win
      # }
      
    } else {
      pen_win <- NA
      
    }
    
    if (MATCH$origin == 'already_bet'){
      Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$winner_after_penalties <- pen_win
    }
    
    # saving goes here
    cnx <- db_connect(user$login, user$pwd)
    
    
    ins <- try(queryDB(
      cnx,
      sql = sprintf("execute usp_Bets_insert
	                  @login = %s,
                    @match_id = %s,
                    @team1 = %s,
                    @team2 = %s,
                    @penalties_winner = %s
                    ",
                    user$login, match_id, input[['bet-team1-score']], input[['bet-team2-score']],
                    ifelse(is.na(pen_win), "NULL", pen_win))
      ),
      silent = F
      )
    
    
    # if the record already existis in the database, i.e. the bet was edited, run a different query:
    if (is.null(ins)){
      
      try(
        queryDB(
        connection = cnx,
        sql = sprintf(
          "execute usp_Bets_update %s, %s, %s, %s, %s",
          match_insert$bet1,
          match_insert$bet2,
          ifelse(is.na(pen_win), "NULL", pen_win),
          match_id,
          user$login)
      ),
      silent = F
      )
    }
    
    
    
    
    
  })
  
  
  
  ### outputs:----
  output$userBetHistory <- renderDataTable({
    
    
    res <- BetHistory()
    
    res <- res %>%
      mutate(time = format(time, "%H:%M"),
             date = format(date, "%Y-%m-%d"),
             winner_after_penalties = ifelse(is.na(winner_after_penalties), "",
                                                   ifelse(winner_after_penalties == 1, team1,
                                                          team2))) %>%
      select(-one_of("login"))
    
    res
  })
  
  
  output$tblNewBetsMatches <- renderDataTable(
    rownames = F,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '200px', targets = c(1, 2, 3)),
        list(targets = 0, searchable = F),
        list(className = "dt-center", targets = '_all')
      )
    ),
    selection = "single", {
    
    
    
    
    
    Matches$to_bet %>%
      select(match_id, team1, team2, date, time, group) %>%
      arrange(match_id)
    
  })
  
  
  output$tblAlreadyBet <- renderDataTable(
    rownames = F,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '200px', targets = c(1, 2, 5)),
        list(targets = 0, searchable = F),
        list(className = "dt-center", targets = '_all')
      )
    ),
    selection = "single",
    {
    
    
    
    Matches$already_bet %>%
      select(match_id, team1, team2, bet1,  bet2, winner_after_penalties, date, time, group) %>%
        mutate(winner_after_penalties = ifelse(is.na(winner_after_penalties),
                                               NA,
                                               ifelse(winner_after_penalties == 1,
                                                      as.character(team1),
                                                      as.character(team2)))) %>%
      arrange(match_id)
    
  })
  

  
}