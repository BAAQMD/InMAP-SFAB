require_data(SFAB_ISRM_demo_ems_data)
require_data(US_ISRM_SFAB_cell_geometries)

#'----------------------------------------------------------------------
#'
#' Map of `PM25` flux (emission rate per km^2).
#'
#' -
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_ems_data %>%
  filter(
    pol_abbr == "PM25") %>%
  drop_units() %>%
  sum_emissions_by(
    US_ISRM_id, pol_abbr) %>%
  inner_join(
    US_ISRM_SFAB_cell_geometries, .,
    by = "US_ISRM_id") %>%
  mutate(
    ems_km2 = ems_qty / st_km2(.)) %>%
  mapview::mapview(
    zcol = "ems_km2")
