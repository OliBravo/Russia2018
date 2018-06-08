# database functions


db_connect <- function(login, password){
  
  
  # return(TRUE)
  
  
  # opens a connection to Russia2018 SQL Server database
  
  if (login == "" | password == ""){
    
    return(NULL)
  }
  
  ## connection string
  # cnx_string <- "Server = AX212\SQLEXPRESS; Database=Russia2018; Integrated Security=True;"
  
  cnx <- try(
    
    RODBC::odbcDriverConnect(
      connection = "Driver=SQL Server;
      Server=myaws-sqlserver.cftriiurha4p.us-west-2.rds.amazonaws.com;
      Database=Russia2018;
      Port=1433;
      Uid=Russia2018;
      Pwd=ek70G1xI9SSSgvZFJ4hG;
      "
    )
    
    # RODBC::odbcDriverConnect(
    #   connection = "Driver={SQL Server};
    #   Server=DB3;
    #   Database=Russia2018;
    #   Port=1433;
    #   Uid=Russia2018;
    #   Pwd=ek70G1xI9SSSgvZFJ4hG;
    #   "
    # )
  )
  
  
  # connection failed:
  if (class(cnx) == "try-error" | cnx == -1){
    
    showNotification(paste("Connection error", cnx))
    
    return(NULL)
    
  } else {
    
    login_try <- try({
      # sql <- paste0("select * from where user=", login, " and password=pwdencrypt(", password, ");")
      
      
      user <- RODBCext::sqlExecute(
        channel = cnx,
        query = "select * from login_check(?,?);",
        data = data.frame(user=login, pass = password),
        fetch = T
      )
      
      user
    })
  }
  
  if (class(login_try) == "try-error" | nrow(login_try) == 0){
    
    showNotification("You are not allowed to play!",
                     type = "error")
    
    odbcClose(cnx)
    
    return(NULL)
  }
  
  cnx
}


queryDB <- function(connection, sql, params = NULL){
  
  
  # return(data.frame(match_id = -999,
  #                   team1 = "A",
  #                   team2 = "B",
  #                   bet1 = 0,
  #                   bet2 = 0,
  #                   winner_after_penalties = 0,
  #                   time = "17:00:00",
  #                   date = "1999-01-01",
  #                   user_id = 0,
  #                   group = "X",
  #                   group_phase = 0,
  #                   role = "",
  #                   team1_regular = 0,
  #                   team2_regular = 0))
  
  # tries to query the database using RPostgres::dbSendQuery method
  # returns NULL in case of errors
  
  tryCatch({
    r <- RODBCext::sqlExecute(
      connection,
      sql,
      data = params,
      fetch = T
    )
    
    r
    
  },
  error = function(e) {
    print(as.character(e))
    NULL
  })
  
}

