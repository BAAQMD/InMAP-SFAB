#'----------------------------------------------------------------------
#'
#' Quick map, shapefile, and GeoJSON for debugging with UW team.
#' Looks like cell IDs may not be lining up correctly.
#'
#' Ref: email 2022-02-14 "[InMAP] handoff test: population density".
#'
#'----------------------------------------------------------------------

#
# Not true --- why? Is "Other/Multi" omitted?
#
with(
  ISRM_SFAB_cell_geodata,
  testthat::expect_equal(
    sum(TotalPop),
    sum(Asian + Black + Latino + Native + WhiteNoLat)))

#'----------------------------------------------------------------------
#'
#' Mapview: show the five cells that overlap the CMAQ boundary but were
#' not included in the "California" extraction (isrm_ca.ncf).
#'
#'----------------------------------------------------------------------

leaflet_map_SFBA() %>%
  addPolygons(
    data = ISRM_SFAB_cell_geodata,
    popup = leafpop::popupTable(ISRM_SFAB_cell_geodata, zcol = c("isrm", "cell_km2")),
    color = "black",
    weight = 0.5, fillColor = "white", fillOpacity = 0.1) %>%
  addPolygons(
    data = st_transform(CMAQ_envelope, 4326), # WGS84 GPS
    stroke = "blue", weight = 1, fillOpacity = 0)

#'----------------------------------------------------------------------
#'
#' Show metadata for `isrm_v1.2.1.ncf`.
#'
#'----------------------------------------------------------------------

ISRM_FULL_NC_PATH %>%
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

  ISRM_full_ncdf4_obj <-
    ncdf4::nc_open(
      ISRM_FULL_NC_PATH)

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

  ncdf4::nc_close(
    ISRM_full_ncdf4_obj)

})


#'----------------------------------------------------------------------
#'
#' Attempt to map population density.
#'
#' As of 2022-02-15, thins doesn't seem to be working correctly.
#'
#'----------------------------------------------------------------------

color_for_pop_km2 <- function (x, palette = "viridis") {
  pal <- colorNumeric(palette = palette, domain = c(0, 30e3))
  return(pal(x))
}

lltools::leaflet_map_SFBA() %>%
  addGlPolygonOverlay(
    ISRM_SFAB_cell_geodata,
    stroke = "black",
    smoothFactor = 0,
    weight = 0.1,
    fillOpacity = 0.7,
    fillColor = color_for_pop_km2(
      with(ISRM_SFAB_cell_geodata, TotalPop / cell_km2))) %>%
  addPolylines(
    data = CMAQ_envelope %>% st_transform(4326),
    weight = 2,
    color = "black") %>%
  addLegend(
    title = "TotalPop/km<sup>2</sup>",
    opacity = 1.0,
    colors = color_for_pop_km2(seq(0, 30e3, by = 5e3)),
    labels = format_SI(seq(0, 30e3, by = 5e3)))

#'----------------------------------------------------------------------
#'
#' Export copies of the following to `Build/Geodata/`:
#'
#' - `ISRM_SFAB_cell_geodata` (as GeoJSON)
#' - `CMAQ_raster_template` (as GeoTIFF)
#' - `CMAQ_envelope` (as GeoJSON)
#'
#'----------------------------------------------------------------------

# geotools::write_geojson(
#   as(ISRM_SFAB_cell_geodata, "Spatial"),
#   dsn = here::here("Build", "Geodata"),
#   layer = "ISRM_SFAB_cell_geodata")

geotools::write_geojson(
  as(st_as_sf(CMAQ_envelope) %>% mutate(FID = 1), "Spatial"),
  dsn = here::here("Build", "Geodata"),
  layer = "CMAQ_envelope")

terra::writeRaster(
  CMAQ_raster_template,
  here::here("Build", "Geodata", "CMAQ_raster_template.tif"))

