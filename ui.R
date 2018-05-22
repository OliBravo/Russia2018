#### Shiny application goes here :)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(RODBC)
library(RODBCext)
library(DT)
# library(leaflet)


source("functions.R")
source("database_functions.R")


ui <- dashboardPage(
  
  dashboardHeader(
    
    title = "FIFA World Cup Russia 2018"
  ),
  
  ### sidebar:----
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        
        tabName = "welcome",
        
        text = "Welcome to Russia 2018"
      ),
      
      menuItem(
        
        tabName = "historicResults",
        
        text = "Results"
      ),
      
      
      menuItem(
        
        tabName = "standings",
        
        text = "Standings"
      ),
    
      # menuItem(
      #   
      #   text = "My account",
      #   
      #   tabName = "account"),
      
      menuItem(
        
        tabName = "bet",
        
        text = "Go bet!"
      )
    )
  ),
  
  
  ### body:----
  dashboardBody(
    
    useShinyjs(),
    
    
    tags$link(
      rel = "stylesheet", href = "style/style.css"
    ),
    
    tags$link(rel="stylesheet",
              href="https://use.fontawesome.com/releases/v5.0.13/css/all.css",
              integrity="sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp",
              crossorigin="anonymous"),
    
    
    
    tabItems(
      
      tabItem(
        tabName = "welcome",
        
        tags$div(
          id = "russia2018-img-container"
        ),
        
        tags$section(
          
          class = "center narrow",
          
          id = "welcome-text",
          
          h2(HTML("Welcome to the<br><strong>FIFA World Cup<br>Russia 2018</strong><br>betting application!")),
          
          p("We're glad that You've joined us!")
        )
      ),
      
      tabItem(
        tabName = "bet",
        
        uiOutput(
          "bet-dynamic-ui"
        )
      ),
      
      tabItem(
        tabName = "standings",
        
        renderStandings()
      ),
      
      tabItem(
        tabName = "historicResults",
        
        renderResults()
      )
    )
  )
)