as_sf_stage_source <- function(x, layer_name = NULL) {

  tryCatch({
    if (inherits(x, 'sf')) {
      return(x)
    }

    if (inherits(x, 'sfc')) {
      return(sf::st_sf(geometry = x))
    }

    if (is.character(x) && length(x) == 1 && identical(tolower(tools::file_ext(x)), 'gpkg')) {
      if (!file.exists(x)) {
        stop(glue::glue('GeoPackage file does not exist: {x}'))
      }

      return(suppressWarnings(sf::st_read(x, layer = layer_name, quiet = TRUE)))
    }

    stop('x must be an sf object, an sfc geometry vector, or a path to a .gpkg file')
  }, warning = function(w) {
    print(glue::glue("A warning popped up in as_sf_stage_source: {w}"))
  }, error = function(e) {
    stop(glue::glue('An error happened in as_sf_stage_source: {e}'))
  })
}


validate_staged_sf <- function(x, geom_type, srid = 2285) {

  tryCatch({
    geom_type <- match.arg(geom_type, c('geometry', 'geography'))

    if (nrow(x) == 0) {
      stop('x must contain at least one feature')
    }

    if (length(attr(x, 'sf_column')) != 1) {
      stop('x must contain exactly one active geometry column')
    }

    source_srid <- sf::st_crs(x)$epsg

    if (is.null(source_srid) || is.na(source_srid)) {
      stop('x must have a valid EPSG code so it can be projected into the database SRID')
    }

    srid <- as.integer(srid)

    if (is.na(srid)) {
      stop('srid must be coercible to an integer')
    }

    list(geom_type = geom_type, srid = srid, source_srid = as.integer(source_srid))
  }, warning = function(w) {
    print(glue::glue("A warning popped up in validate_staged_sf: {w}"))
  }, error = function(e) {
    stop(glue::glue('An error happened in validate_staged_sf: {e}'))
  })
}


project_staged_sf <- function(x, geom_type, srid) {

  tryCatch({
    if (sf::st_crs(srid) == sf::NA_crs_) {
      stop(glue::glue('srid must reference a known coordinate system: {srid}'))
    }

    if (!identical(sf::st_crs(x)$epsg, srid)) {
      x <- sf::st_transform(x, srid)
    }

    if (geom_type == 'geography' && !sf::st_is_longlat(x)) {
      stop('geography columns require a geographic CRS; use an SRID such as 4326')
    }

    x
  }, warning = function(w) {
    print(glue::glue("A warning popped up in project_staged_sf: {w}"))
  }, error = function(e) {
    stop(glue::glue('An error happened in project_staged_sf: {e}'))
  })
}


stage_geometry_wkt_name <- function(x, base_name = 'geometry_wkt_tmp') {

  tryCatch({
    wkt_name <- base_name
    counter <- 1

    while (wkt_name %in% names(x)) {
      wkt_name <- glue::glue('{base_name}_{counter}')
      counter <- counter + 1
    }

    wkt_name
  }, warning = function(w) {
    print(glue::glue("A warning popped up in stage_geometry_wkt_name: {w}"))
  }, error = function(e) {
    stop(glue::glue('An error happened in stage_geometry_wkt_name: {e}'))
  })
}


#' st_stage_table(x, table_name)
#'
#' Write a simple-features object as a SQL Server spatial table.
#'
#' This is the spatial analogue to [stage_table()]. It writes the non-spatial
#' attributes with [DBI::dbWriteTable()] and then converts the geometry to a
#' native SQL Server `geometry` or `geography` column.
#'
#' @param x An `sf` object, an `sfc` geometry vector, or a path to a GeoPackage
#'   (`.gpkg`) file.
#' @param table_name String. The name you want given the database table.
#' @param db_name String. The name of the database to stage into. Defaults to
#'   "Elmer".
#' @param schema_name String. The target schema. Defaults to "stg".
#' @param geom_type String. The SQL Server spatial type to create: "geometry"
#'   or "geography". Defaults to "geometry".
#' @param srid Integer. The spatial reference id to assign to the geometry
#'   column. Defaults to `2285`, which is the standard projection for most
#'   SQL Server spatial tables in Elmer and ElmerGeo. Input data are
#'   reprojected to this SRID before writing.
#' @param geometry_column String. The name of the geometry column to create.
#'   Defaults to `"Shape"`, which is the standard geometry column name on
#'   this SQL Server.
#' @param layer_name String. If `x` is a GeoPackage path, optionally select a
#'   specific layer from that file.
#'
#' @return `NULL`, invisibly.
#'
#' @note This creates a native SQL Server spatial table. It does not register
#'   the output as an ESRI geodatabase feature class.
#'
#' @export
st_stage_table <- function(x,
                           table_name = deparse(substitute(x)),
                           db_name = 'Elmer',
                           schema_name = 'stg',
                           geom_type = 'geometry',
                           srid = 2285,
                           geometry_column = 'Shape',
                           layer_name = NULL) {

  tryCatch({
    sf_obj <- as_sf_stage_source(x, layer_name = layer_name)
    sf_details <- validate_staged_sf(sf_obj, geom_type = geom_type, srid = srid)
    sf_obj <- project_staged_sf(sf_obj, geom_type = sf_details$geom_type, srid = sf_details$srid)

    if (is.null(geometry_column) || is.na(geometry_column) || geometry_column == '') {
      geometry_column <- 'Shape'
    }

    wkt_column <- stage_geometry_wkt_name(sf_obj)
    stage_df <- sf::st_drop_geometry(sf_obj)

    if (geometry_column %in% names(stage_df)) {
      stop(glue::glue('geometry_column already exists in the attribute data: {geometry_column}'))
    }

    stage_df[[wkt_column]] <- sf::st_as_text(sf::st_geometry(sf_obj))

    conn <- get_conn(dbname = db_name)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    table_id <- DBI::Id(schema = schema_name, table = table_name)
    quoted_table <- as.character(DBI::dbQuoteIdentifier(conn, table_id))
    quoted_geometry_column <- as.character(DBI::dbQuoteIdentifier(conn, geometry_column))
    quoted_wkt_column <- as.character(DBI::dbQuoteIdentifier(conn, wkt_column))

    DBI::dbWriteTable(conn, table_id, stage_df, overwrite = TRUE)

    DBI::dbExecute(
      conn = conn,
      statement = DBI::SQL(glue::glue(
        'ALTER TABLE {quoted_table} ADD {quoted_geometry_column} {sf_details$geom_type}'
      ))
    )

    DBI::dbExecute(
      conn = conn,
      statement = DBI::SQL(glue::glue(
        'UPDATE {quoted_table} ',
        'SET {quoted_geometry_column} = {sf_details$geom_type}::STGeomFromText({quoted_wkt_column}, {sf_details$srid})'
      ))
    )

    DBI::dbExecute(
      conn = conn,
      statement = DBI::SQL(glue::glue(
        'ALTER TABLE {quoted_table} DROP COLUMN {quoted_wkt_column}'
      ))
    )

    return(invisible(NULL))
  }, warning = function(w) {
    print(glue::glue("A warning popped up in st_stage_table: {w}"))
  }, error = function(e) {
    print(glue::glue("An error happened in st_stage_table: {e}"))
    stop(e)
  })
}