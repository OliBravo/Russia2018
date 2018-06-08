# functions


renderLoginPage <- function(){
  # fluidPage(
    
    # tags$head(
    #   tags$link(href = "style.css", rel="stylesheet", type="text/css")
    # ),
    
    tags$div(
      
      id = "login-container",
      
      div(
        
        textInput("txtLogin", "Login", "robert"),
        
        passwordInput("txtPassword", "Password", "robert123"),
        
        actionButton("btnLogin", type = "button", "Sign In")
      ),
      
      div(
        
        id = "login-failed",
        
        "Unable to connect to the database."
        
      )
    )
  # )
}



renderAuthorised <- function(admin){
  
  # A user has been succesfully authenticated in the database
  
  tags$div(
    
    # tags$p(
    #   "Login succesfull!"
    # ),
    
    renderNewBet2(admin)
  )
}


renderUserMenu <- function(admin){
  
  
  # class for styling the admin panel:
  if (!admin)
    cls = "admin-hidden"
  else
    cls = "admin-visible"
  
  
  
  tags$div(
    id="profile",
    
    tags$span(
      icon(name="user", class = "fal fa-user"),
      
      "My account"
    ),
    
    tags$div(
      id="profile-details",
      
      tags$ul(
        
        tags$li(
          icon(name = "arrow", class="fal fa-arrow-right"),
          
          actionLink(label = "My bets", inputId = "my-bets", class="user-menu-btn shiny-bound-input")),
        
        tags$li(
          
          class = cls,
          
          icon(name = "footbal", class = "fal fa-futbol"),
          
          actionLink(inputId = "lnk-res-admin", label = "Enter results", class="user-menu-btn shiny-bound-input")
        ),
        
        
        tags$li(
          class="log-out",
          
          icon(name = "signout", class="fal fa-times log-out"),
          
          actionLink(label = "Sign out", inputId = "sign-out"))
      )
    )
  )
}



renderMyBets <- function(admin){
  
  tags$div(
    tags$div(
      
      id="bets-history",
      
      dataTableOutput(
        outputId = "userBetHistory"
      )
    ),
    
    actionButton(
      inputId = "btnNewBet",
      label = "New bet"
    ),
    
    
    renderUserMenu(admin)
  )
}




### Rendering New Bets (obsolete):----
# renderNewBet <- function(res){
#   
#   tags$div(
#     
#     selectInput(
#       inputId = "select-new-bet",
#       label = "Select a match:",
#       choices = paste(
#         res$match_id,
#         paste(res$team1, res$team2, sep = " - "),
#         paste(res$date, format(res$time, "%H:%M"), res$venue, sep = ", "),
#         sep = " | "
#       ),
#       multiple = F
#     ),
#     
#     
#     uiOutput(
#       outputId = "new-bet-single"
#     ),
#     
#     renderUserMenu()
#   )
# }
# 
# 
# 



### Rendering new bets (new):----
renderNewBet2 <- function(admin){
  
  tags$div(
    
    h2("Matches I've already bet:"),
    
    dataTableOutput(
      outputId = "tblAlreadyBet"
    ),
    
    
    h2("Matches I haven't bet so far:"),
    
    dataTableOutput(
      outputId = "tblNewBetsMatches"
    ),
    
    
    uiOutput(outputId = "new-bet-single"),
    
    renderUserMenu(admin)
    
  )
}


renderNewBetSingle <- function(match, admin){
  
  
  match <- match %>%
    mutate(team1 = as.character(team1),
           team2 = as.character(team2))
  
  
  if (is.na(match$winner_after_penalties))
    pen_win <- NULL
  else
    pen_win <- ifelse(match$winner_after_penalties == 1,
                    as.character(match$team1),
                    as.character(match$team2))
  
  
  if ('bet1' %in% colnames(match) & 'bet2' %in% colnames(match)){
    val1 <- match$bet1
    val2 <- match$bet2
    
    
  } else {
    val1 <- 0
    val2 <- 0
    
  }
  

  tags$div(
    tags$div(
      class="container row center-items",
      
      tags$span(
        id='bet-match-id',
        match$match_id
      ),

      tags$img(
        id="bet-team1-logo",
        src=match$logo1,
        class="column6 flag"
      ),

      tags$span(
        id="bet-team1-name",
        match$team1,
        class="column6"
      ),

      numericInput(
        inputId = "bet-team1-score",
        value = val1,
        label = "",
        min = 0
      ),

      numericInput(
        inputId = "bet-team2-score",
        value = val2,
        label = "",
        min = 0
      ),

      tags$span(
        id="bet-team2-name",
        match$team2,
        class="column6"
      ),

      tags$img(
        id="bet-team2-logo",
        src=match$logo2,
        class="column6 flag"
      ),
      
      tags$span(
        id='bet-group-phase',
        match$group_phase
      )
    ),
    
    
      
    conditionalPanel(
      condition = "$('#bet-group-phase').text() == 0 &
                      $('#bet-team1-score').val() == $('#bet-team2-score').val()",
      
      # tags$div(
        id='penalties-container',
      
      
        selectInput(
          inputId = 'bet-penalties-winner',
          label = 'Winner after penalties:',
          choices = c(match$team1, match$team2),
          selected = ifelse(is.null(pen_win), match$team1, pen_win)
        )
    ),
    
    actionButton(inputId = "btnNewBetSave", "Save")#,
    
    
    # renderUserMenu(admin)
  )

}



### Results and standings:----
renderResults <- function(){
  
  tags$div(
    
    "Results go here..."
  )
}


renderStandings <- function(){
  
  tags$div(
    
    "Standings go here..."
  )
}


renderAdminPanel <- function(admin){
  
  if (admin){
    
    tags$div(
      "Entering results here",
      
      rHandsontableOutput("tblEnterResults"),
      
      actionButton("btnResultsSave", "Save"),
      
      renderUserMenu(admin)
    )
  } else {
    
    "You are not allowed to see this page."
  }
}
