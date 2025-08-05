check_sql_driver <- function(driver_name) {

  tryCatch({
    drivers <- odbc::odbcListDrivers()
    d_names <- drivers$name
    if (!(driver_name %in% d_names)) {
      driver_url = "https://learn.microsoft.com/en-us/sql/connect/odbc/download-odbc-driver-for-sql-server?view=sql-server-ver16"
      err_msg <- glue::glue("You don't seem to have the {driver_name} installed.",
                            "Try downloading it from {driver_url} (or just google it :)).")
      stop(err_msg)
    }
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_conn: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_conn: {e}"))
    stop(e)
  })
}

is.windows <- function() {
  tryCatch({
    sysname = Sys.info()['sysname']
    is.win <- FALSE
    if (sysname == 'Windows') {
      is.win <- TRUE
    }
    return(is.win)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in is.windows: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in is.windows: {e}"))
    stop(e)
  })
}

is.linux <- function() {
  tryCatch({
    sysname = Sys.info()['sysname']
    is.linux <- FALSE
    if (sysname == 'Linux') {
      is.linux <- TRUE
    }
    return(is.linux)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in is.linux: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in is.linux: {e}"))
    stop(e)
  })
}


build_conn <- function(dbname, driver_name) {

  tryCatch({

    server_name <- 'SQLserver'

    if (is.windows()) {
      check_sql_driver(driver_name)
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver = driver_name,
                             server = server_name,
                             database = dbname,
                             trusted_connection = "yes")
    } else if (is.linux())  {
      auth <- get_auth()
      driver_name = 'ODBC Driver 18 for SQL Server'
      check_sql_driver(driver_name)
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver = driver_name,
                             server = server_name,
                             database = dbname,
                             uid = auth$uid,
                             pwd = auth$pwd,
                             Encrypt = "no")
    } else {
      auth <- get_auth()
      check_sql_driver(driver_name)
      conn <- DBI::dbConnect(odbc::odbc(),
                             driver = driver_name,
                             server = server_name,
                             database = dbname,
                             uid = auth$uid,
                             pwd = auth$pwd)
    }
    return(conn)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in build_conn_str: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in build_conn_str: {e}"))
    stop(e)
  })
}

get_conn <- function(dbname='ElmerGeo') {

  tryCatch({
    driver_name = 'ODBC Driver 17 for SQL Server'
    conn <- build_conn(dbname, driver_name)
    return(conn)
  }, warning = function(w) {
    print(glue::glue("A warning popped up in get_conn: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in get_conn: {e}"))
  })
}
