#'----------------------------------------------------------------------
#'
#' The envelope of `CMAQ_raster_template` is a basis for our domain.
#'
#'----------------------------------------------------------------------

CMAQ_envelope <-
  require_data(CMAQ_raster_template) %>%
  st_envelope() %>%
  st_segmentize( # add points on edges (100m spacing)
    dfMaxLength = 100)

#'----------------------------------------------------------------------
#'
#' Not really needed? We can spatially filter using CMAQ domain.
#'
#'----------------------------------------------------------------------

ISRM_CA_cell_lookup <-
  data_path(
    "UW",
    "2022-02-10",
    "ca_isrm_gridcells.csv") %>%
  read_csv(
    col_names = c("ISRM_CA_cell_id", "isrm"),
    col_types = cols(.default = col_integer()),
    skip = 1) %>%
  tidy_InMAP_names() %>%
  ensurer::ensure(
    nrow(.) == ISRM_CA_CELL_COUNT)

#'----------------------------------------------------------------------
#'
#' Create `ISRM_cell_geometries`. This corresponds to the entire domain
#' (n = 52,411 cells) --- not just California or Bay Area.
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geometries <- local({

  msg(
    "constructing `ISRM_full_cell_geometries` from ",
    fs::path_rel(ISRM_FULL_LATLON_CSV_PATH, here::here()))

  full_csv_latlon_data <-
    read_csv(
      ISRM_FULL_LATLON_CSV_PATH,
      col_types = cols(
        isrm = col_integer(),
        .default = col_double())) %>%
    ensurer::ensure(
      nrow(.) == ISRM_FULL_CELL_COUNT) %>%
    tidy_InMAP_names()

  # To create a 2-column XY matrix
  as_coord_matrix <- function (
    lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4
  ) {
    cbind(
      x = c(lon1, lon2, lon3, lon4, lon1), # last element must be equal to first
      y = c(lat1, lat2, lat3, lat4, lat1))
  }

  # Create a `coords` column (list of XY matrices), then a `poly` column
  # (list of corresponding curvilinear "rectangular" cells).
  full_geom_data <-
    full_csv_latlon_data %>%
    mutate(
      coords = pmap(
        list(lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4),
        as_coord_matrix),
      poly = map(
        coords,
        ~ sf::st_polygon(list(.))))

  msg("reprojecting ", format_count(nrow(full_geom_data)), " features to CRS ", ISRM_CRS)

  # Convert to `sf` object using `st_as_sf()`.
  full_geom_data %>%
    mutate(
      geometry = st_sfc(poly, crs = ISRM_CRS)) %>%
    select(
      all_of(ISRM_ID_VAR),
      geometry) %>%
    st_as_sf() %>%
    mutate(
      cell_km2 = drop_units(set_units(st_area(.), "km^2"))) %>%
    tbltools::select_last(
      geometry)

})

#'----------------------------------------------------------------------
#'
#' Filter `ISRM_full_cell_geometries`,
#' using the envelope of `CMAQ_raster_template`,
#' yielding `ISRM_SFAB_cell_geodata` (n = 2,553 cells).
#'
#'----------------------------------------------------------------------

msg("filtering `ISRM_full_cell_geometries` using `CMAQ_envelope`")

ISRM_SFAB_cell_geometries <-
  ISRM_full_cell_geometries %>%
  st_filter(
    st_transform(
      CMAQ_envelope, st_crs(.)))

#'----------------------------------------------------------------------
#'
#' Write objects to disk.
#'
#'----------------------------------------------------------------------

write_data(ISRM_CA_cell_lookup)
write_data(ISRM_SFAB_cell_geometries)
write_data(ISRM_full_cell_geometries)

write_geojson(
  as(ISRM_SFAB_cell_geometries, "Spatial"),
  dsn = build_path("Geodata"),
  layer = "ISRM_SFAB_cell_geometries")
