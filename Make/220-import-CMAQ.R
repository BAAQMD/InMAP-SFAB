#'----------------------------------------------------------------------
#'
#' Import BAAQMD "Lambert Conformal Conic" (LCC) 1km grid definition
#' from `BAAQMD_1km_toxic_164X224.zip`.
#'
#' Then, re-export to `Build/Geodata/` in GeoJSON and TopoJSON formats.
#'
#'----------------------------------------------------------------------

CMAQ_LCC_1km_grid_shp <-
  data_path(
    "BAAQMD",
    "CMAQ-LCC-1km-grid",
    "BAAQMD_1km_toxic_164X224.zip") %>%
  shptools::read_shp()

CMAQ_LCC_1km_grid_geodata <-
  CMAQ_LCC_1km_grid_shp %>%
  transmute(
    CMAQ_col = as.integer(I),
    CMAQ_row = as.integer(J))

#'----------------------------------------------------------------------
#'
#' The envelope of `CMAQ_raster_template` is a basis for our domain.
#'
#'----------------------------------------------------------------------

CMAQ_LCC_envelope <-
  CMAQ_LCC_1km_grid_geodata %>%
  st_envelope() %>%
  st_segmentize( # add points on edges (100m spacing)
    dfMaxLength = 100)

#'----------------------------------------------------------------------
#'
#' Write objects to disk.
#'
#'----------------------------------------------------------------------

write_data(CMAQ_LCC_envelope)
write_data(CMAQ_LCC_1km_grid_geodata)

local({

  geojson_path <- build_path("Geodata", "CMAQ_LCC_envelope.geojson")
  topojson_path <- fs::path_ext_set(geojson_path, "topojson")

  write_geojson(
    CMAQ_LCC_envelope,
    geojson_path)

  write_topojson(
    CMAQ_LCC_envelope,
    topojson_path)

})


local({

  geojson_path <- build_path("Geodata", "CMAQ_LCC_1km_grid.geojson")
  topojson_path <- fs::path_ext_set(geojson_path, "topojson")

  write_geojson(
    CMAQ_LCC_1km_grid_geodata,
    geojson_path)

  write_topojson(
    CMAQ_LCC_1km_grid_geodata,
    topojson_path)

})

local({

  xfun::write_utf8(
    as.character(st_crs(CMAQ_LCC_1km_grid_shp))[2],
    build_path("Geodata", "CMAQ_LCC.prj"))

})


