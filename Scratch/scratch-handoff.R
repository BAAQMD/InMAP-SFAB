#'----------------------------------------------------------------------
#'
#' Quick map, shapefile, and GeoJSON for debugging with UW team.
#' Looks like cell IDs may not be lining up correctly.
#'
#' Ref: email 2022-02-14 "[InMAP] handoff test: population density".
#'
#'----------------------------------------------------------------------

require_data(US_ISRM_SFAB_cell_geodata)

#
# Not true --- why? Is "Other/Multi" omitted?
#
with(
  US_ISRM_SFAB_cell_geodata,
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
    data = US_ISRM_SFAB_cell_geodata,
    popup = leafpop::popupTable(
      US_ISRM_SFAB_cell_geodata,
      zcol = c("US_ISRM_id", "cell_km2")),
    color = "black",
    weight = 0.5, fillColor = "white", fillOpacity = 0.1) %>%
  addPolygons(
    data = st_transform(CMAQ_LCC_envelope, 4326), # WGS84 GPS
    stroke = "blue", weight = 1, fillOpacity = 0)

#'----------------------------------------------------------------------
#'
#' Show metadata for `isrm_v1.2.1.ncf`.
#'
#'----------------------------------------------------------------------

US_ISRM_NC_PATH %>%
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
      US_ISRM_NC_PATH)

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
    US_ISRM_SFAB_array[s, r, l, v] %>%
      { as.numeric(.) * 2 },
    expected,
    tol = tol)

  testthat::expect_equal(
    US_ISRM_SFAB_cube %>%
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
#' Map population density, using leaflet.
#' This seems to vary at the 1 km^2 scale.
#'
#'----------------------------------------------------------------------

color_for_pop_km2 <- function (x, palette = "viridis") {
  pal <- colorNumeric(palette = palette, domain = c(0, 30e3))
  return(pal(x))
}

lltools::leaflet_map_SFBA() %>%
  addGlPolygonOverlay(
    US_ISRM_SFAB_cell_geodata,
    stroke = "black",
    smoothFactor = 0,
    weight = 0.1,
    fillOpacity = 0.7,
    fillColor = color_for_pop_km2(
      with(US_ISRM_SFAB_cell_geodata, TotalPop / cell_km2))) %>%
  addPolylines(
    data = CMAQ_LCC_envelope %>% st_transform(4326),
    weight = 2,
    color = "black") %>%
  addLegend(
    title = "TotalPop/km<sup>2</sup>",
    opacity = 1.0,
    colors = color_for_pop_km2(seq(0, 30e3, by = 5e3)),
    labels = format_SI(seq(0, 30e3, by = 5e3)))

#'----------------------------------------------------------------------
#'
#' Map baseline PM2.5.
#' This seems to vary on a 12x12 km^2 scale.
#'
#'----------------------------------------------------------------------

ggplot() +
  aes(
    fill = `Baseline TotalPM25`) +
  ggtools::scale_fill_sepia(
    latex2exp::TeX(str_ugm3("TeX")),
    oob = squish,
    breaks = seq(0, 15, by = 3),
    limits = c(0, 15)) +
  geom_sf(
    color = alpha("white", 0.5), size = 0.1,
    data = US_ISRM_SFAB_cell_geodata) +
  geom_sf(
    color = "white", fill = NA,
    data = CMAQ_LCC_envelope) +
  geom_sf(
    color = alpha("white", 0.8), fill = NA, size = 0.3,
    data = st_intersection(
      SFBA::SFBA_OSM_coast,
      SFAB_WGS84_boundary)) +
  labs(
    title = "Baseline TotalPM25",
    subtitle = str_glue(
      "Basis: {basename(US_ISRM_NC_PATH)} ",
      "and {basename(US_ISRM_LATLON_CSV_PATH)}"),
    caption = str_glue("DRAFT {str_date()}"))

#'----------------------------------------------------------------------
#'
#' Export copies of the following to `Build/Geodata/`:
#'
#' - `US_ISRM_SFAB_cell_geodata` (as GeoJSON)
#' - `CMAQ_raster_template` (as GeoTIFF)
#' - `CMAQ_LCC_envelope` (as GeoJSON)
#'
#'----------------------------------------------------------------------

# geotools::write_geojson(
#   as(US_ISRM_SFAB_cell_geodata, "Spatial"),
#   dsn = build_path("Geodata"),
#   layer = "US_ISRM_SFAB_cell_geodata")

geotools::write_geojson(
  as(st_as_sf(CMAQ_LCC_envelope) %>% mutate(FID = 1), "Spatial"),
  dsn = build_path("Geodata"),
  layer = "CMAQ_LCC_envelope")

terra::writeRaster(
  CMAQ_raster_template,
  build_path("Geodata", "CMAQ_raster_template.tif"))

