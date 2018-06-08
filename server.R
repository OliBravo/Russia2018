library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(RODBC)
library(RODBCext)
library(rhandsontable)
# library(leaflet)



server <- function(output, input, session){
  
  
  options(DT.options = list(pageLength = 5 , lengthMenu = c(5, 10, 20, 30)))
  
  
  # data table proxies------------------------
  
  
  proxy_to_bet <- dataTableProxy('tblNewBetsMatches')
  
  proxy_already_bet <- dataTableProxy('tblAlreadyBet')
  
  # ------------------------------------------
  
  
  cnx <- db_connect('test', 'test123')
  
  MATCHES <- queryDB(
    cnx,
    sql = "select * from matches_logo2 where team1 != 'NA';"
  )
  
  
  result_datetime <- '2018-07-19 23:00'
  
  ENTER_RESULTS <- queryDB(
    cnx,
    sql = "select * from enter_results(?)",
    params = list(result_datetime)
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
  
  admin <- reactiveValues(is_admin = F)
  
  
  Matches <- reactiveValues(to_bet = NULL,
                            already_bet = NULL)
  
  
  
  
  BetHistory <- reactive({
    
    validate(
      need(Matches$already_bet, message = "No matches have been bet yet.")
    )
    
    
    cnx <- db_connect(
      login = user$login,
      password = user$pwd
    )
    
    res <- queryDB(
      connection = cnx,
      sql = "select * from vw_Bets where login = ?",
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
        # mutate_at(
        #   .funs = funs(as.character(.)), .vars = vars(-one_of("match_id", "winner_after_penalties"))) %>%
        # mutate(winner_after_penalties = ifelse(
        #   group_phase == 0,
        #   ifelse(winner_after_penalties == 1, team1,
        #          ifelse(winner_after_penalties == 2, team2, NA)),
        #   NA
        # )) %>% 
        # mutate(winner_after_penalties = as.character(winner_after_penalties)) %>%
        arrange(match_id)
      
      
      
      ### matches yet unbet:----
      
      # cnx <- db_connect(user$login, user$pwd)
      
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
        
        cnx2 <- db_connect(user$login, user$pwd)
        
        res <- queryDB(
          cnx2,
          "select role from users where login = ?",
          params = list(user$login)
        )
        
        odbcClose(cnx2)
        
        if (!is.na(res$role))
          admin$is_admin <- T
        else
          admin$is_admin <- F
        
        odbcClose(cnx)
        
        renderAuthorised(admin$is_admin)
        
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
      
      renderMyBets(admin$is_admin)
    })
  })
  
  
  ### user creates new bets:----
  observeEvent(input$btnNewBet, {
    
    output[['bet-dynamic-ui']] <- renderUI({
      
      
      
      # renderNewBet(MATCHES)
      renderNewBet2(admin$is_admin)
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

      renderNewBetSingle(match, admin$is_admin)
    })
  })
  
  
  observeEvent(input$tblAlreadyBet_rows_selected, {
    
    
    cursor <- input$tblAlreadyBet_rows_selected
    
    output[['new-bet-single']] <- renderUI({
      
      match <- Matches$already_bet[cursor,]
      
      MATCH$id <- match$match_id
      MATCH$origin <- 'already_bet'
      
      renderNewBetSingle(match, admin$is_admin)
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
                                   match_insert) %>%
        arrange(match_id)
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
      
      pen_win <- ifelse(input[['bet-penalties-winner']] == as.character(match_insert$team1), 1,
                        ifelse(input[['bet-penalties-winner']] == as.character(match_insert$team2), 2, NA))
      
      
      # if (MATCH$origin == 'already_bet'){
      #   Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$winner_after_penalties <- pen_win
      # }
      
    } else {
      pen_win <- NA
      
    }
    
    print(sprintf("pen win's set to: %s", pen_win))
    
    # if (MATCH$origin == 'already_bet'){
      Matches$already_bet[Matches$already_bet$match_id == MATCH$id,]$winner_after_penalties <- pen_win
    # }
    
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
    
    
    MATCH$id <- NULL
    MATCH$origin <- NULL
    
  })
  
  
  
  #### Enter results (admin only):----
  observeEvent(input$`lnk-res-admin`, {
    
    output$`bet-dynamic-ui` <- renderUI({
      
      renderAdminPanel(admin$is_admin)
    })
  })
  
  
  observeEvent(input$btnResultsSave, {
    
    
    df <- hot_to_r(input$tblEnterResults) %>% 
      # filter(!is.na(team1_regular), !is.na(team2_regular)) %>%
      select(match_id, team1_regular, team2_regular, winner_after_penalties)
    
    
    cnx <- db_connect(user$login, user$pwd)
    
    
    if (nrow(df) > 0){
      for (i in 1:nrow(df)){
        row <- df[i,]
        try(queryDB(
          cnx,
          sql = sprintf("exec usp_Results_insert_update %s, %s, %s, %s",
            ifelse(is.na(row$match_id),"NULL", row$match_id),
            ifelse(is.na(row$team1_regular), "NULL", row$team1_regular),
            ifelse(is.na(row$team2_regular), "NULL", row$team2_regular),
            ifelse(is.na(row$winner_after_penalties), "NULL", row$winner_after_penalties)
          )
        ), silent = F)
      }
    }
    
    
    odbcClose(cnx)
    
  })
  
  
  
  
  
  
  ### outputs:----
  output$userBetHistory <- renderDataTable(
    rownames = T,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '200px', targets = c(2, 3, 4)),
        list(targets = 1, searchable = F),
        list(className = "dt-center", targets = '_all')
      )
    ),
    selection = "none",
    {
    
    
    validate(
      need(BetHistory(), message = F)
    )
    
    res <- BetHistory()
    
    res %>%
      # select(match_id, team1, team2, date, time, logo1, logo2, group, group_phase) %>%
      # mutate(bet1 = 0, bet2 = 0, winner_after_penalties = NULL) %>%
      mutate(
        # date = format(date, "%Y-%m-%d"),
        time = substr(time, 1, 5)) %>%
      mutate_at(
        .funs = funs(as.character(.)), .vars = vars(-one_of("match_id", "winner_after_penalties"))) %>%
      mutate(winner_after_penalties = ifelse(
        group_phase == 0,
        ifelse(winner_after_penalties == 1, team1,
               ifelse(winner_after_penalties == 2, team2, NA)),
        NA)) %>% 
      mutate(winner_after_penalties = as.character(winner_after_penalties)) %>% 
      select(match_id, team1, team2, bet1, bet2, winner_after_penalties,
             date, time, group) %>%
      arrange(match_id)
    
    
  })
  
  
  output$tblNewBetsMatches <- renderDataTable(
    rownames = T,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '200px', targets = c(2, 3, 4)),
        list(targets = 1, searchable = F),
        list(className = "dt-center", targets = '_all')
      )
    ),
    selection = "single", {
    
    
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
      
    dstatic <- res %>%
      select(match_id, team1, team2, date, time, group) 
    
    
    odbcClose(cnx)
    
    
    dstatic
  })
  
  
  output$tblAlreadyBet <- renderDataTable(
    rownames = T,
    filter = 'top',
    options = list(
      autoWidth = TRUE,
      columnDefs = list(
        list(width = '200px', targets = c(2, 3, 6)),
        list(targets = 1, searchable = F),
        list(className = "dt-center", targets = '_all')
      )
    ),
    selection = "single",
    {
    
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
      
      odbcClose(cnx)
      
      
      res %>%
        # select(match_id, team1, team2, date, time, logo1, logo2, group, group_phase) %>%
        # mutate(bet1 = 0, bet2 = 0, winner_after_penalties = NULL) %>%
        mutate(
          # date = format(date, "%Y-%m-%d"),
          time = substr(time, 1, 5)) %>%
        mutate_at(
          .funs = funs(as.character(.)), .vars = vars(-one_of("match_id", "winner_after_penalties"))) %>%
        mutate(winner_after_penalties = ifelse(
          group_phase == 0,
          ifelse(winner_after_penalties == 1, team1,
                 ifelse(winner_after_penalties == 2, team2, NA)),
          NA)) %>% 
        mutate(winner_after_penalties = as.character(winner_after_penalties)) %>% 
        select(match_id, team1, team2, bet1, bet2, winner_after_penalties,
               date, time, group) %>%
        arrange(match_id)
      
    
  })
  

  observe({
    
    
    validate(
      need(Matches$to_bet, message = F)
    )
    
    
    d <- Matches$to_bet %>%
      select(match_id, team1, team2, date, time, group) %>%
      arrange(match_id)
    
    
    d2 <- Matches$already_bet  %>%
      mutate_at(
        .funs = funs(as.character(.)), .vars = vars(-one_of("match_id", "winner_after_penalties"))) %>%
      mutate(winner_after_penalties = ifelse(
        group_phase == 0,
        ifelse(winner_after_penalties == 1, team1,
               ifelse(winner_after_penalties == 2, team2, NA)),
        NA
      )) %>%  
      mutate(winner_after_penalties = as.character(winner_after_penalties)) %>% 
      select(match_id, team1, team2, bet1, bet2, winner_after_penalties,
             date, time, group) %>% 
      arrange(match_id)
    
    
    
    
    
    proxy_to_bet %>% replaceData(data = d, clearSelection = "none")
    
    proxy_already_bet %>% replaceData(data = d2, clearSelection = "none")
    
  })
  
  
  output$tblEnterResults <- renderRHandsontable({
    
    df <- ENTER_RESULTS %>%
      select(match_id, date, time, group, group_phase, team1, team2, team1_regular, team2_regular, winner_after_penalties)
    
    rhandsontable(data = df, digits = 0) %>%
      hot_col(col = 1:7, readOnly = T) %>% 
      hot_col(col = 10, source = c(1,2), strict = F, type = "dropdown") %>% 
      hot_col(col = 8:9, type = "numeric")
  })
  
}