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
  rename(
    US_ISRM_id = isrm) %>%
  ensurer::ensure(
    nrow(.) == ISRM_US_CA_CELL_COUNT) # n = 9,001 CA cells in the full US ISRM

#'----------------------------------------------------------------------
#'
#' Create `ISRM_US_cell_geometries`. This corresponds to the entire domain
#' (n = 52,411 cells) --- not just California or Bay Area.
#'
#'----------------------------------------------------------------------

ISRM_US_cell_geometries <- local({

  msg(
    "constructing `ISRM_US_cell_geometries` from ",
    fs::path_rel(ISRM_US_LATLON_CSV_PATH, here::here()))

  full_csv_latlon_data <-
    read_csv(
      ISRM_US_LATLON_CSV_PATH,
      col_types = cols(
        isrm = col_integer(),
        .default = col_double())) %>%
    ensurer::ensure(
      nrow(.) == ISRM_US_CELL_COUNT) %>%
    tidy_InMAP_names() %>%
    rename(
      US_ISRM_id = isrm)

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

  msg("reprojecting ", format_count(nrow(full_geom_data)),
      " features to EPSG 4326")

  # Convert to `sf` object using `st_as_sf()`.
  full_geom_data %>%
    mutate(
      geometry = st_sfc(poly, crs = 4326)) %>%
    select(
      any_of(ISRM_ID_VARS),
      geometry) %>%
    st_as_sf() %>%
    mutate(
      cell_km2 = drop_units(set_units(st_area(.), "km^2"))) %>%
    tbltools::select_last(
      geometry)

})

comment(ISRM_US_cell_geometries) <-
  fs::path_rel(ISRM_US_LATLON_CSV_PATH, here::here())

#'----------------------------------------------------------------------
#'
#' Filter `ISRM_US_cell_geometries`,
#' using `CMAQ_LCC_envelope`,
#' yielding `ISRM_US_SFAB_cell_geodata` (n = 2,553 cells).
#'
#'----------------------------------------------------------------------

msg("filtering `ISRM_US_cell_geometries` using `CMAQ_LCC_envelope`")

ISRM_US_SFAB_cell_geometries <-
  ISRM_US_cell_geometries %>%
  st_filter(
    st_transform(
      CMAQ_LCC_envelope, st_crs(.))) %>%
  ensurer::ensure(
    nrow(.) == 2547)

#'----------------------------------------------------------------------
#'
#' Extract deltas for the "SFAB" domain, from the **full** ISRM dataset,
#'   into an in-memory array `ISRM_US_SFAB_array`.
#'
#' - Per variable, this step consumes ~50 MB and takes ~2 seconds.
#' - See `R/extract_ISRM_array.R` for more details.
#'
#'----------------------------------------------------------------------

ISRM_US_SFAB_cell_ids <-
  ISRM_US_SFAB_cell_geometries %>%
  select(any_of(ISRM_ID_VARS)) %>%
  unlist()

ISRM_US_SFAB_array <- local({

  ISRM_full_ncdf4_obj <-
    ncdf4::nc_open(
      ISRM_US_NC_PATH)

  varids <- c(
    "PrimaryPM25",
    "SOA",
    "pNH4",
    "pNO3",
    "pSO4")

  array_list <-
    map(varids,
        ~ extract_ISRM_array(
          ISRM_full_ncdf4_obj,
          ISRM_US_SFAB_cell_ids,
          varid = .,
          layer = 1))

  ncdf4::nc_close(
    ISRM_full_ncdf4_obj)

  bind_arrays(
    array_list,
    along = "varid")

})

#'----------------------------------------------------------------------
#'
#' A `tbl_cube` might be friendlier than an array, and it has about the
#' same memory footprint. But, it can't "just do" array multiplication.
#' Let's build it anyway (this is cheap, for the time being at least).
#'
#'----------------------------------------------------------------------

ISRM_US_SFAB_cube <-
  tbl_cube(
    dimensions = dimnames(ISRM_US_SFAB_array),
    measures = list(value = ISRM_US_SFAB_array))

#'----------------------------------------------------------------------
#'
#' Extract baseline data and join it to cell geometries, yielding:
#'
#' - `ISRM_full_cell_geodata`; and
#' - `ISRM_CA_cell_geodata`
#'     - A subset of `ISRM_full_cell_geodata`
#'         - Where `ISRM_id` is in `ISRM_US_SFAB_cell_ids`
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geodata <- local({

  ISRM_full_tidync_obj <-
    tidync::tidync(
      ISRM_US_NC_PATH)

  ISRM_full_baseline_data <-
    ISRM_full_tidync_obj %>%
    tidync::activate("D3") %>%
    hyper_tibble() %>%
    filter(
      Layer == 0) %>%
    ensurer::ensure(
      nrow(.) == ISRM_US_CELL_COUNT,
      all(.$allcells == 1:nrow(.))) %>%
    mutate(
      US_ISRM_id := allcells - 1L) # **NOTE**: `isrm` starts at 0; `allcells` starts at 1

  rm(ISRM_full_tidync_obj)

  powerjoin::power_left_join(
    ISRM_US_cell_geometries,
    ISRM_full_baseline_data,
    check = powerjoin::check_specs(
      column_conflict = "abort",
      unmatched_keys_left = "abort",
      unmatched_keys_right = "abort",
      na_keys = "abort"),
    by = any_of(ISRM_ID_VARS))

})

ISRM_US_SFAB_cell_geodata <-
  ISRM_US_cell_geodata %>%
  filter(across(
    any_of(ISRM_ID_VARS),
    ~ . %in% ISRM_US_SFAB_cell_ids))

#'----------------------------------------------------------------------
#'
#' Write objects to disk.
#'
#'----------------------------------------------------------------------

write_data(ISRM_CA_cell_lookup)
write_data(ISRM_US_cell_geometries)
write_data(ISRM_US_SFAB_cell_geometries)
write_data(ISRM_US_SFAB_cell_geodata)

write_geojson(
  ISRM_US_SFAB_cell_geometries,
  build_path("Geodata", "ISRM_US_SFAB_cell_geometries.geojson"))

write_geojson(
  CMAQ_LCC_envelope,
  build_path("Geodata", "BAAQMD_CMAQ_LCC_envelope.geojson"))
