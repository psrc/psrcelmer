#' get_query(db_name, sql)
#' 
#' Retrieve a dataset defined by a SQL string
#'
#' @param db_name The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".
#' @param sql The SQL command to send to <db_name>.
#' @return A data frame.
#'
#' @export
get_query <- function(db_name = 'Elmer', sql) {
  
  tryCatch({
    conn <- get_conn(dbname = db_name)
    df <- DBI::dbGetQuery(conn, DBI::SQL(sql))
    return(df)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_query: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_query: {e}"))
    stop(e)
  })
}

get_query_elmer <- function(sql) {
  
  tryCatch({
    df <- get_query('elmer', sql)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_query_elmer: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_query_elmer: {e}"))
    stop(e)
  })
}