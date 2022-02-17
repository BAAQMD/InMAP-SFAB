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
    popup = leafpop::popupTable(ISRM_SFAB_cell_geometries, zcol = c("isrm", "ISRM_CA_cell_id")),
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

ISRM_CA_nc_path <-
  here::here("Data", "ca_isrm.ncf")

ISRM_CA_nc_path %>%
  ncmeta::nc_atts() %>%
  mutate(value = unlist(value)) %>%
  select(variable, name, value) %>%
  spread(name, value) %>%
  knitr::kable()


pryr::object_size(ISRM_SFAB_array)

# QA: `PrimaryPM25` for cell #1236, layer 1
ISRM_SFAB_array["S1236","R1236","L1","PrimaryPM25",drop=FALSE]

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
