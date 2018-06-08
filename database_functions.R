# database functions


db_connect <- function(login, password){
  
  # opens a connection to Russia2018 SQL Server database
  
  if (login == "" | password == ""){
    
    return(NULL)
  }
  
  ## connection string
  # cnx_string <- "Server = AX212\SQLEXPRESS; Database=Russia2018; Integrated Security=True;"
  
  cnx <- try(
    # RODBC::odbcConnect(
    #   "Russia2018"
    # )
    RODBC::odbcDriverConnect(
      connection = "Driver={SQL Server};Server=DB3;Database=Russia2018;Uid=Russia2018;Pwd=ek70G1xI9SSSgvZFJ4hG;Trusted_Connection=Yes;"
    )
  )
  
  
  # connection failed:
  if (class(cnx) == "try-error"){
    
    showNotification("Connection error")
    
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

