#' @rdname SQLserverconnection
#' @name SQL server connection
#' @title SQL server connection
#' @description  This functions allow to work with DB in differents servers ending connections after every interaction.
#' @param server_name character, name of the server to connect, in sqlServer list.
#' @return \code{sqlGetConn()} set the connection by evaluate the expression on List "sqlServers".
#' @return \code{sqlListTable()} returns a character vector with names of every table in database.
#' @return \code{sqlClose()} disconnect from database.
#' @details  sqlServers is
#' a list built-in sqlGetConn().
#' @examples
#'   sqlGetConn(local) #connect with "local" in the list sqlServers.
#'   sqlListTable(local) # its empty
#'   sqlClose(local) ## end connection


#' @author Daniel Fischer
NULL

#' @rdname SQLserverconnection
#' @export
sqlGetConn = function(server_name){
  safeLibrary(DBI)

  sqlServers = list(local = expression({
    safeLibrary(RSQLite)
    dbConnect(SQLite(), "db.sqlite")
  }),
  analytics = expression({
    safeLibrary(RMySQL)
    dbConnect(
      MySQL(),
      dbname = ,
      user = ,
      password = ,
      host = , #Master
      port =     )
  }))

  connObj = switch (class(server_name),
                    character = eval(sqlServers[[server_name]]),
                    server_name)
  if(version$os == "mingw32"){

  }else{
    # sqlQuery(connObj,"SET ANSI_PADDING ON")
    # sqlQuery(connObj,"SET ANSI_WARNINGS ON")
    # sqlQuery(connObj,"SET ANSI_NULLS ON")
  }
  return(connObj)
}

#' @rdname SQLserverconnection
#' @export
sqlClose = function(server_name){
  dbDisconnect(server_name)
}

#' @rdname SQLserverconnection
#' @export
sqlListTables = function(server_name){
  try({
    sql = sqlGetConn(server_name)
    tablas = dbListTables(sql)
  })
  if(class(server_name) == "character"){sqlClose(sql)}
  return(tablas)
}
