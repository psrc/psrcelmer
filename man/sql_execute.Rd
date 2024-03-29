% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_execute.R
\name{sql_execute}
\alias{sql_execute}
\title{send_query(db_name, sql)}
\usage{
sql_execute(sql, db_name = "Elmer")
}
\arguments{
\item{sql}{String.  The SQL command to send to <db_name>.}

\item{db_name}{String.  The name of the database to run the query against.  Should be "Elmer" or "ElmerGeo".  Default = "Elmer".}
}
\description{
Execute a SQL command to the database
}
\details{
Intended for update, merge or other commands that do not retrieve records. For retrieval, use get_query.
}
