#'----------------------------------------------------------------------
#'
#' Connect to the supplied NetCDF file.
#'
#' We use the following packages as interfaces:
#'
#' - [ncmeta], for access to metadata
#' - [ncdf4], for fact access via our in-house `extract_ISRM_array()`; and
#' - [tidync], for attempting to extract and join with `D3` data.
#'
#'----------------------------------------------------------------------

ISRM_CA_tidync_obj <-
  ISRM_CA_nc_path %>%
  tidync::tidync()

ISRM_CA_ncdf4_obj <-
  ISRM_CA_nc_path %>%
  ncdf4::nc_open()

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
  st_transform(
    ISRM_CRS) %>%
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
    here::here("Data", "isrm_boundaries_latlons.csv") %>%
    read_csv(
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
      cell_km2 = drop_units(set_units(st_area(.), "km^2")))

})

#'----------------------------------------------------------------------
#'
#' Filter `ISRM_full_cell_geometries` using the envelope of `CMAQ_raster_template`,
#' yielding `ISRM_SFAB_cell_geodata` (n = 2,553 cells).
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cell_geometries <-
  ISRM_full_cell_geometries %>%
  st_filter(
    CMAQ_envelope) %>%
  mutate(
    cell_km2 = drop_units(set_units(st_area(.), "km^2"))) %>%
  # inner_join(
  #   ISRM_CA_cell_lookup, # note: 5 cells at southwest corner are NA
  #   by = "isrm") %>%
  select_last(
    geometry)

#'----------------------------------------------------------------------
#'
#' Extract selected cells and variables, using the `ncdf4` package.
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cell_ids <-
  ISRM_SFAB_cell_geometries %>%
  pull(isrm) # FIXME: verify this is correct

# Consumes about 2 seconds and 50 MB per variable
ISRM_SFAB_array <-
  c("PrimaryPM25", "SOA", "pNH4", "pNO3", "pSO4") %>%
  map(
    ~ extract_ISRM_array(
      ISRM_full_ncdf4_obj,
      ISRM_SFAB_cell_ids,
      varid = .,
      layer = 1)) %>%
  bind_arrays(
    along = "varid")

#'----------------------------------------------------------------------
#'
#' A `tbl_cube` might be friendlier than an array, and it has about the
#' same memory footprint. But, it can't "just do" array multiplication.
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cube <-
  tbl_cube(
    dimensions = dimnames(ISRM_SFAB_array),
    measures = list(value = ISRM_SFAB_array))

#'----------------------------------------------------------------------
#'
#' Extract baseline data and join it to cell geometries, yielding
#' `ISRM_SFAB_cell_geodata`.
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geodata <- local({

  ISRM_full_data <-
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

  ISRM_full_cell_geometries %>%
    powerjoin::power_left_join(
      ISRM_full_data,
      check = powerjoin::check_specs(
        column_conflict = "abort",
        unmatched_keys_left = "abort",
        unmatched_keys_right = "abort",
        na_keys = "abort"),
      by = "isrm") %>%
    rename_with(
      str_remove, everything(), "^Baseline") %>%
    select_last(
      geometry)

})

ISRM_SFAB_cell_geodata <-
  ISRM_full_cell_geodata %>%
  filter(
    isrm %in% ISRM_SFAB_cell_ids)

#'----------------------------------------------------------------------
#'
#' Quick map, shapefile, and GeoJSON for debugging with UW team.
#' Looks like cell IDs may not be lining up correctly.
#'
#' Ref: email 2022-02-14 "[InMAP] handoff test: population density".
#'
#'----------------------------------------------------------------------

#
# Not true --- why? Is `other` (incl. multi) omitted?
#
# with(
#   ISRM_SFAB_cell_geodata,
#   testthat::expect_equal(
#     sum(TotalPop),
#     sum(Asian + Black + Latino + Native + WhiteNoLat)))
#

ISRM_SFAB_cell_geodata %>%
  mutate(
    TotalPop_km2 = TotalPop / cell_km2) %>%
  select(
    isrm, TotalPop, cell_km2, TotalPop_km2) %>%
  mapview::mapview(
    zcol = "TotalPop_km2")

ISRM_SFAB_cell_geodata %>%
  write_shp(
    here::here("Build", "Geodata", str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}")),
    str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}"))

ISRM_SFAB_cell_geodata %>%
  as("Spatial") %>%
  write_geojson(
    here::here("Build", "Geodata"),
    str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}"))

