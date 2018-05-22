library(rvest)
library(dplyr)



url <- "http://www.gol24.pl/ms-2018/terminarz/"

mundi <- read_html(url)


tabela <- html_node(mundi, ".phase-day tbody")


i= 1

el <- html_nodes(tabela, "tr")

DT <- character(0)
GR <- character(0)

mecze.lista <- list()


for (i in 1:length(el)){
  
  team1 <- character(0)
  team2 <- character(0)
  
  el.data <- try(
    (xml_nodes(el[[i]], "th"))
  )
  
  
  grupa <- try(
    html_text(xml_nodes(el.data, ".rozgrywka"))
  )
  
  if (length(grupa) >0)
    GR <<- grupa
  
  biezaca.data <- try(
    xml_nodes(el[[i]], ".dzien")
  )
  
  if (length(biezaca.data) >0) 
    DT <<- html_text(biezaca.data)
  
  
  
  el.mecz <- try(
    xml_nodes(el[[i]], "td")
  )
  
  
  if (length(el.mecz) > 0){
    godz <- html_text(xml_nodes(el[[i]], ".godzina"))
    team1 <- html_text(xml_nodes(el[[i]], ".nazwa")[[1]])
    team2 <- html_text(xml_nodes(el[[i]], ".nazwa")[[2]])
    logo <- html_text(xml_nodes(el[[i]], "img"))
    
    logo1 <- html_attrs(html_nodes(el[[i]], "img")[[1]])[['src']]
    logo2 <- html_attrs(html_nodes(el[[i]], "img")[[2]])[['src']]
      
    dzien <- DT
    grupa <- GR
    
    mecze.lista[[length(mecze.lista) + 1]] <- list(
      godz = godz,
      team1 = team1,
      team2 = team2,
      dzien = dzien,
      grupa = grupa,
      logo1 = logo1,
      logo2 = logo2
    )
  }
  
}



save(mecze.lista, file = 'group_phase.RData')


l <- lapply(mecze.lista, function(i) as.data.frame(i))

matches.df.from.l <- data.frame()


l2 <- lapply(l, function(i){
  matches.df.from.l <<- rbind(matches.df.from.l, i)
})


mecze_logo <- l2[[48]]

mecze_logo$ID = as.integer(row.names(mecze_logo))


druzyny_logo1 <- mecze_logo %>%
  select(team = team1,  logo = logo1)



druzyny_logo2 <- mecze_logo %>%
  select(team = team2, logo = logo2)


druzyny_logo <- rbind(druzyny_logo1, druzyny_logo2) %>% unique()


druzyny_logo$eng <- druzyny_logo$team

fix(druzyny_logo)


rm(druzyny_logo1, druzyny_logo2)


# plik od Roberta:----
matches <- read.csv2(
  file = "data/matches.csv",
  sep = "/t",
  header = T
)



## łączenie z logo:----
matches_logo <- right_join(
  x = druzyny_logo,
  y = matches,
  by = c("eng" = "Country1")
)


matches_logo2 <- left_join(
  y = druzyny_logo,
  x = matches_logo,
  by = c("Country2" = "eng")
)


matches_logo2 <- matches_logo2 %>%
  select(match_id = ID,
         team1 = eng,
         team2 = Country2,
         logo1 = logo.x,
         logo2 = logo.y,
         group = Group,
         date = Date,
         time = Time,
         venue = Stadium
         )


write.csv2(x = matches_logo2, file = "C:/Users/jakub.malecki/Documents/Shiny/MyFirstShinyApplications/Mundial2018/data/matches_logo.csv")
#### Koniec

