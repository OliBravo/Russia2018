library(RODBC)
library(RODBCext)
library(dplyr)


getDF <- function(table.name){
  
  cnx <- try(
    
    RODBC::odbcDriverConnect(
      connection = "Driver={SQL Server};
    Server=DB3;
    Database=Russia2018;
    Port=1433;
    Uid=Russia2018;
    Pwd=ek70G1xI9SSSgvZFJ4hG;
    "
    )
  )
  
  sql <- paste("select * from", table.name)
  
  print(sql)
  
  r <- sqlQuery(cnx, sql)
  
  odbcClose(cnx)
  
  r
}



### tables:----
bets <- getDF(table.name = "Bets")
matches_logo2 <- getDF(table.name = "matches_logo2")
results <- getDF(table.name = "Results")
users <- getDF(table.name = "users")

users_shinyproxy <- read.csv2("sql_sripts/users_shinyproxy.csv",
                              header = T,
                              stringsAsFactors = F)


bets <- bets %>% filter(match_id == 0)
results <- results %>% filter(match_id == 0)
users <- users[-(1:4),c(1,2)]
users$login <- as.character(users$login)
users$password <- character(0)
nms <- names(users)

users <- rbind(users, cbind(1:14, users_shinyproxy$ï..login, users_shinyproxy$pwd))
names(users) <- nms

users <- users %>% 
  mutate_if(is.factor, as.character)



### views:----
vw_bets <- function(){
  
  df <- inner_join(
    x = matches_logo2,
    y = bets,
    by = c('match_id' = 'match_id')
  )
  
  df <- inner_join(
    x = df,
    y = users,
    by = c('user_id' = 'id')
  )
  
  df %>% 
    select(login,
           match_id,
           team1 = team1.x,
           bet1 = team1.y,
           bet2 = team2.y,
           team2 = team2.x,
           winner_after_penalties,
           date,
           time,
           group,
           group_phase
           )
}



### stored procedures:----
## Bets----
usp_bets_insert <- function(plogin, pmatch_id, pteam1, pteam2, ppen_winner = NA){
  

  puser_id <- users %>% 
    filter(login == plogin)
  
  nms <- names(bets)
  
  new <- c(user_id = puser_id$id,
           match_id = pmatch_id,
           team1 = pteam1,
           team2 = pteam2,
           winner_after_penalties = ppen_winner)
  
  new <- as.integer(new)
  
  # check if the record already exists:
  check <- bets %>% 
    filter(user_id == puser_id$id, match_id == pmatch_id)
  
  
  if (nrow(check) > 0){
    # record already exists, update it
    
    print('updating a bet')
    bets[bets$user_id == puser_id$id & bets$match_id == pmatch_id,] <- new
    bets <<- bets
  } else {
    
    print('inserting new bet')
    
    bets <- rbind(bets, new)
    names(bets) <- nms
    bets <<- bets
  }
}



usp_bets_insert(
  plogin = 'test', pmatch_id = 64, pteam1 = 2, pteam2 = 4)



## Results:----
usp_results_insert <- function(pmatch_id, pteam1, pteam2, ppen_winner = NA){
  
  
  nms <- names(results)
  
  new <- c(match_id = pmatch_id,
           team1 = pteam1,
           team2 = pteam2,
           winner_after_penalties = ppen_winner)
  
  new <- as.integer(new)
  
  # check if the record already exists:
  check <- results %>% 
    filter(match_id == pmatch_id)
  
  
  if (nrow(check) > 0){
    # record already exists, update it
    
    print('updating a result')
    results[results$match_id == pmatch_id,] <- new
    results <<- results
  } else {
    
    print('inserting new result')
    results <- rbind(results, new)
    names(results) <- nms
    results <<- results
  }
}


usp_results_insert(6, 2, 5)






### table-valued functions:----
## enter results - list of matches: ----
enter_results <- function(pdatetime){

  
  df <- left_join(
    x = matches_logo2,
    y = results %>% select(match_id, team1_regular, team2_regular, winner_after_penalties)
  )
  
  df %>% 
    mutate(time2 = as.POSIXct(paste(date, time))) %>% 
    filter(time2 <= pdatetime)
}


enter_results(Sys.time())

test_time2 <- as.POSIXct("2018-06-19 16:55")
enter_results(test_time2)






#### login check:----
login_check <- function(plogin, ppass){
  
  df <- users %>% 
    filter(login == plogin, password == ppass)
  
  if (nrow(df) == 0)
    return(NULL)
}










### matches to bet: ----
matches_to_bet <- function(ptime, plogin){
  
  
  user <- users %>% 
    filter(login == plogin)
  
  
  # matches user has already bet:
  exclude <- bets %>% 
    filter(user_id == user$id) %>% 
    select(match_id)
  
  matches_logo2 %>% 
    mutate(time2 = as.POSIXct(paste(date, time)),
           bet1 = 0,
           bet2 = 0,
           winner_after_penalties = NA) %>%
    filter(! match_id %in% exclude) %>% 
    filter(time2 >= ptime)
}


test_time2 <- as.POSIXct("2018-06-19 16:55")
matches_to_bet(ptime = test_time2, plogin = 'test')



### matches already bet:
matches_already_bet <- function(ptime, plogin){
  
  
  user <- users %>% 
    filter(login == plogin)
  
  
  df.bets <- bets %>% 
    filter(user_id == user$id)
  
  
  df <- inner_join(
    x = matches_logo2,
    y = df.bets,
    by = c('match_id' = 'match_id')
  )
  
  df %>% 
    mutate(time2 = as.POSIXct(paste(date, time)),
           bet1 = team1.y,
           bet2 = team2.y) %>% 
    filter(time2 >= ptime)
}


test_time2 <- as.POSIXct("2018-06-19 16:55")
matches_already_bet(ptime = test_time2, plogin = 'test')
