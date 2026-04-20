test_that('st_stage_table() rejects non-spatial inputs before connecting', {
  expect_error(
    psrcelmer::st_stage_table(data.frame(id = 1), 'test_table'),
    'sf object'
  )
})


test_that('st_stage_table() rejects missing GeoPackage paths', {
  expect_error(
    psrcelmer::st_stage_table('missing_file.gpkg', 'test_table'),
    'GeoPackage file does not exist'
  )
})


test_that('st_stage_table() requires an EPSG code when srid is not supplied', {
  geom <- sf::st_sfc(sf::st_point(c(0, 0)))
  sf_obj <- sf::st_sf(id = 1, geometry = geom)

  expect_error(
    psrcelmer::st_stage_table(sf_obj, 'test_table'),
    'valid EPSG code'
  )
})


test_that('st_stage_table() validates geography inputs', {
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 2285)
  sf_obj <- sf::st_sf(id = 1, geometry = geom)

  expect_error(
    psrcelmer::st_stage_table(sf_obj, 'test_table', geom_type = 'geography', srid = 2285),
    'geographic CRS'
  )
})


test_that('project_staged_sf() reprojects to 2285 by default', {
  geom <- sf::st_sfc(sf::st_point(c(-122.335167, 47.608013)), crs = 4326)
  sf_obj <- sf::st_sf(id = 1, geometry = geom)

  staged <- psrcelmer:::project_staged_sf(sf_obj, geom_type = 'geometry', srid = 2285)

  expect_equal(sf::st_crs(staged)$epsg, 2285)
})


test_that('st_stage_table() uses Shape as the default geometry column name', {
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 2285)
  sf_obj <- sf::st_sf(id = 1, Shape = 'value', geometry = geom)

  expect_error(
    psrcelmer::st_stage_table(sf_obj, 'test_table'),
    'geometry_column already exists in the attribute data: Shape'
  )
})


test_that('st_stage_table() rejects geometry column name collisions', {
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  sf_obj <- sf::st_sf(id = 1, geom = 'value', geometry = geom)

  expect_error(
    psrcelmer::st_stage_table(sf_obj, 'test_table', geometry_column = 'geom'),
    'geometry_column already exists'
  )
})


test_that('st_stage_table() writes a Shape geometry column into stg', {
  conn <- try(psrcelmer:::get_conn('Elmer'), silent = TRUE)

  if (inherits(conn, 'try-error')) {
    skip('Elmer database connection is not available in this environment')
  }

  DBI::dbDisconnect(conn)

  table_name <- glue::glue(
    'copilot_st_stage_{Sys.getpid()}_{format(Sys.time(), "%Y%m%d%H%M%S")}'
  )

  cleanup_sql <- glue::glue(
    "IF OBJECT_ID('stg.{table_name}', 'U') IS NOT NULL DROP TABLE stg.[{table_name}]"
  )

  table_created <- FALSE

  on.exit({
    if (table_created) {
      psrcelmer::sql_execute(cleanup_sql)
    }
  }, add = TRUE)

  geom <- sf::st_sfc(sf::st_point(c(-122.335167, 47.608013)), crs = 4326)
  sf_obj <- sf::st_sf(feature_id = 1L, feature_name = 'Seattle', geometry = geom)

  expect_no_error(psrcelmer::st_stage_table(sf_obj, table_name = table_name))
  table_created <- TRUE

  metadata <- psrcelmer::get_query(glue::glue(
    "SELECT TYPE_NAME(c.user_type_id) AS data_type ",
    "FROM sys.columns c ",
    "INNER JOIN sys.tables t ON c.object_id = t.object_id ",
    "INNER JOIN sys.schemas s ON t.schema_id = s.schema_id ",
    "WHERE s.name = 'stg' ",
    "  AND t.name = '{table_name}' ",
    "  AND c.name = 'Shape'"
  ))

  expect_equal(nrow(metadata), 1)
  expect_equal(tolower(metadata$data_type[[1]]), 'geometry')

  staged <- psrcelmer::get_query(glue::glue(
    "SELECT COUNT(*) AS n_rows, ",
    "       MIN(feature_id) AS feature_id, ",
    "       MIN(Shape.STSrid) AS srid, ",
    "       MIN(Shape.STGeometryType()) AS geometry_type ",
    "FROM stg.[{table_name}]"
  ))

  expect_equal(staged$n_rows[[1]], 1)
  expect_equal(staged$feature_id[[1]], 1)
  expect_equal(staged$srid[[1]], 2285)
  expect_equal(tolower(staged$geometry_type[[1]]), 'point')
})