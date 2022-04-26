source(here::here("Make", "100-setup.R"))

require_data(US_ISRM_CA_cell_geometries)
require_data(CA_ISRM_cell_geometries)
require_data(CA_ISRM_SFAB_cell_geometries)

data(us_states, package = "spData")

CA_boundary <-
  SFBA::CA_state_TIGER_2017_geometry

CA_lookup_csv_path <-
  data_path(
    "UW",
    "2022-02-10",
    "ca_isrm_gridcells.csv") %>%
  fs::path_rel(here::here())

fig_description <- labs(
  title = "ISRM Cell Geometries",
  subtitle = str_glue(
    "Polygons imported from {comment(US_ISRM_cell_geometries)}. ",
    "Showing subset that match {CA_lookup_csv_path} (n = {format_count(nrow(US_ISRM_CA_cell_geometries))}).",
    .sep = "\n"),
  caption = str_draft())

fig_theme <-
  theme_simple() +
  theme_remove(
    "axis", c("line", "ticks", "title", "text"), c("x", "y"))

fig0_object <-
  ggplot() +
  fig_theme +
  fig_description

cell_layer <-
  geom_sf(
    color = gray(0.5),
    fill = NA,
    size = I(0.1),
    alpha = I(0.5),
    data = US_ISRM_CA_cell_geometries)

fig1_object <-
  fig0_object +
  geom_sf(
    fill = "white",
    data = CA_boundary) +
  cell_layer +
  coord_sf(
    crs = ISRM_CRS)

fig2_envelope <-
  st_envelope(c(xmin=-122, xmax=-120, ymin=36, ymax=39), crs = 4326) %>%
  st_transform(ISRM_CRS)

SFAB_land_layer <-
  geom_sf(
    fill = "white",
    data = SFAB_boundary %>% st_as_sf() %>% SFBA::clip_to_coastline() %>% st_simplify(dTolerance = 0.1))

fig2_object <-
  fig0_object +
  SFAB_land_layer +
  cell_layer +
  coord_sf(
    crs = ISRM_CRS,
    xlim = -2220e3 + c(0, 150e3),
    ylim = 15e3 + c(0, 100e3))
