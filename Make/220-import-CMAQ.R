
CMAQ_LCC_1km_shp <-
  data_path(
    "BAAQMD",
    "CMAQ-LCC-1km-grid",
    "BAAQMD_1km_toxic_164X224.zip") %>%
  shptools::read_shp()

CMAQ_LCC_1km_geodata <-
  CMAQ_LCC_1km_shp %>%
  select(
    CMAQ_col = I,
    CMAQ_row = J)

local({

  geojson_path <- build_path("Geodata", "CMAQ_LCC_1km_grid.geojson")
  topojson_path <- fs::path_ext_set(geojson_path, "topojson")

  write_geojson(
    CMAQ_LCC_1km_geodata,
    geojson_path)

  write_topojson(
    CMAQ_LCC_1km_geodata,
    topojson_path)

})
