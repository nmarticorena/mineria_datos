#'SQL Write Table
#'
#'Writes, overwrite or appends a data frame to a database table.
#'It calls dbWriteTable().
#'@usage sqlWriteTable(server_name, data, table, ...)
#'@param server_name character, name of the DB server from sqlServers list.
#'@param data a data frame or coercible to data frame
#'@param table character, name of the table to write in DB.
#'@param ... other parameters passed on to methods.
#'@details It ends the connection inmediately after getting the results. sqlServers is
#'a list built-in sqlGetConn().
#'@seealso "sqlServerConn" and "setSqlServers()" documentation in toolkitEntel and "dbWriteTable()"
#'from DBI for more details.
#'@return returns booleans as it is specificated for dbWriteTable().
#'@examples
#'sqlWriteTable("local",head(iris),"iris")
#'sqlWriteTable("local",head(mtcars),"MTCARS")
#'con=sqlGetConn("local")
#'dbReadTable(con,"iris")
#'dbReadTable(con,"MTCARS")
#'
#'@export
sqlWriteTable = function(server_name,data,table,...){
  try({
    sql = sqlGetConn(server_name)
    dbWriteTable(sql, table, data, append = TRUE,row.names = F,...)
  })
  if(class(server_name) == "character"){sqlClose(sql)}
}
