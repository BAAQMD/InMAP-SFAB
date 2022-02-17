#'----------------------------------------------------------------------
#'
#' The envelope of `CMAQ_raster_template` is a basis for our domain.
#'
#'----------------------------------------------------------------------

CMAQ_raster_template <- local({

  grd_path <-
    "~/GitHub/BAAQMD/NGC-appliances-impact/Build/RData/CMAQ_raster_template.grd"

  warning(
    "Importing `CMAQ_raster_template` from ",
    fs::path_rel(grd_path, here::here()))

  terra::rast(grd_path)

})

CMAQ_envelope <-
  CMAQ_raster_template %>%
  st_envelope() %>%
  st_segmentize( # add points on edges (100m spacing)
    dfMaxLength = 100)

#'----------------------------------------------------------------------
#'
#' Not really needed? We can spatially filter using CMAQ domain.
#'
#'----------------------------------------------------------------------

ISRM_CA_cell_lookup <-
  here::here("Data", "ca_isrm_gridcells.csv") %>%
  read_csv(
    col_names = c("ISRM_CA_cell_id", "isrm"),
    col_types = cols(.default = col_integer()),
    skip = 1) %>%
  ensurer::ensure(
    nrow(.) == ISRM_CA_CELL_COUNT)

#'----------------------------------------------------------------------
#'
#' Create `ISRM_cell_geometries`. This corresponds to the entire domain
#' (n = 52,411 cells) --- not just California or Bay Area.
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geometries <- local({

  # First, read from `Data/isrm_boundaries_latlons.csv`.
  full_csv_latlon_data <-
    read_csv(
      ISRM_FULL_LATLON_CSV_PATH,
      col_types = cols(
        isrm = col_integer(),
        .default = col_double())) %>%
    ensurer::ensure(
      nrow(.) == ISRM_FULL_CELL_COUNT)

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

  # Convert to `sf` object using `st_as_sf()`.
  full_geom_data %>%
    transmute(
      isrm,
      geometry = st_sfc(poly, crs = ISRM_CRS)) %>%
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

ISRM_SFAB_cell_geometries <-
  ISRM_full_cell_geometries %>%
  st_filter(
    st_transform(
      CMAQ_envelope, st_crs(.)))

#'----------------------------------------------------------------------
#'
#' Extract deltas for the "SFAB" domain into an in-memory array
#' `ISRM_SFAB_array`. See `R/extract_ISRM_array.R` for more details.
#'
#' Per variable, this step consumes ~50 MB and takes ~2 seconds.
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cell_ids <-
  ISRM_SFAB_cell_geometries %>%
  pull(isrm)

ISRM_SFAB_array <- local({

  ISRM_full_ncdf4_obj <-
    ncdf4::nc_open(
      ISRM_FULL_NC_PATH)

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
          ISRM_SFAB_cell_ids,
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

ISRM_SFAB_cube <-
  tbl_cube(
    dimensions = dimnames(ISRM_SFAB_array),
    measures = list(value = ISRM_SFAB_array))

#'----------------------------------------------------------------------
#'
#' Extract baseline data and join it to cell geometries, yielding:
#'
#' - `ISRM_full_cell_geodata`; and
#' - `ISRM_CA_cell_geodata`
#'     - A subset of `ISRM_full_cell_geodata`
#'         - Where `isrm` is in `ISRM_SFAB_cell_ids`
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geodata <- local({

  ISRM_full_tidync_obj <-
    tidync::tidync(
      ISRM_FULL_NC_PATH)

  ISRM_full_baseline_data <-
    ISRM_full_tidync_obj %>%
    tidync::activate("D3") %>%
    hyper_tibble() %>%
    filter(
      Layer == 0) %>%
    ensurer::ensure(
      nrow(.) == ISRM_FULL_CELL_COUNT,
      all(.$allcells == 1:nrow(.))) %>%
    mutate(
      isrm = allcells - 1L) # **NOTE**: `isrm` starts at 0; `allcells` starts at 1

  rm(ISRM_full_tidync_obj)

  powerjoin::power_left_join(
    ISRM_full_cell_geometries,
    ISRM_full_baseline_data,
    check = powerjoin::check_specs(
      column_conflict = "abort",
      unmatched_keys_left = "abort",
      unmatched_keys_right = "abort",
      na_keys = "abort"),
    by = "isrm")

})

ISRM_SFAB_cell_geodata <-
  ISRM_full_cell_geodata %>%
  filter(
    isrm %in% ISRM_SFAB_cell_ids)
