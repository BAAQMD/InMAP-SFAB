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
#     sum(all),
#     sum(asian + black + latino + native + white)))

#'----------------------------------------------------------------------
#'
#' Mapview: show the five cells that overlap the CMAQ boundary but were
#' not included in the "California" extraction (isrm_ca.ncf).
#'
#'----------------------------------------------------------------------

leaflet_map_SFBA() %>%
  addPolygons(
    data = ISRM_SFAB_cell_geometries,
    popup = leafpop::popupTable(ISRM_SFAB_cell_geometries, zcol = c("isrm", "TotalPop", "cell_km2")),
    color = "black",
    weight = 0.5, fillColor = "white", fillOpacity = 0.1) %>%
  addPolygons(
    data = CMAQ_envelope,
    stroke = "blue", weight = 1, fillOpacity = 0)

#'----------------------------------------------------------------------
#'
#' Show metadata for the supplied `ca_isrm.ncf` file.
#'
#'----------------------------------------------------------------------

ISRM_full_nc_path %>%
  ncmeta::nc_atts() %>%
  mutate(value = unlist(value)) %>%
  select(variable, name, value) %>%
  spread(name, value) %>%
  knitr::kable()

#'----------------------------------------------------------------------
#'
#' Test some expectations for ISRM deltas.
#'
#'----------------------------------------------------------------------

local({

  #
  # **FIXME**: result in Python notebook is actually:
  #
  #    NH3_irsm0[1201][1202] * 2 ≈ 1.932e-7
  #
  # Python indexing starts at 0, hence I'm expecting:
  #
  #   (Python) 1201,1202 <=> (R/NetCDF) 1202,1203
  #

  i <- 1201; j <- 1202; k <- 0; v <- "pNH4"
  s <- paste0("S", i+1); r <- paste0("R", j+1); l <- paste0("L", k+1)

  expected <- 1.624e-7; tol <- 1e-11

  testthat::expect_equal(
    ISRM_full_ncdf4_obj %>%
      ncdf4::ncvar_get(
        v, start = c(i+1, j+1, k+1), count = c(1,1,1)) %>%
      { as.numeric(.) * 2 },
    expected,
    tol = tol)

  testthat::expect_equal(
    ISRM_full_ncdf4_obj %>%
      extract_ISRM_array(
        i+1, j+1, k+1, varid = v) %>%
      { as.numeric(.) * 2},
    expected,
    tol = tol)

  testthat::expect_equal(
    ISRM_SFAB_array[s, r, l, v] %>%
      { as.numeric(.) * 2 },
    expected,
    tol = tol)

  testthat::expect_equal(
    ISRM_SFAB_cube %>%
      filter(
        source   == s,
        receptor == r,
        layer    == l,
        varid    == v) %>%
      as_tibble() %>%
      pull(value) %>%
      { as.numeric(.) * 2 },
    expected,
    tol = tol)

})


#'----------------------------------------------------------------------
#'
#' Attempt to map population density.
#'
#' As of 2022-02-15, thins doesn't seem to be working correctly.
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cell_geodata %>%
  mutate(
    all_km2 = all / cell_km2) %>%
  select(
    isrm, all, cell_km2, all_km2) %>%
  mapview::mapview(
    zcol = "all_km2")

ISRM_SFAB_cell_geodata %>%
  write_shp(
    here::here("Build", "Geodata", str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}")),
    str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}"))

ISRM_SFAB_cell_geodata %>%
  as("Spatial") %>%
  write_geojson(
    here::here("Build", "Geodata"),
    str_glue("ISRM_SFAB_cell_geodata-{str_datestamp()}"))
