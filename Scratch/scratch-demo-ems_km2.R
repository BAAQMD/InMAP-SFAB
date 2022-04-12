require_data(SFAB_ISRM_demo_ems_data)

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
    ISRM_id, pol_abbr) %>%
  inner_join(
    ISRM_SFAB_cell_geometries, .,
    by = ISRM_ID_VAR) %>%
  mutate(
    ems_km2 = ems_qty / st_km2(.)) %>%
  mapview::mapview(
    zcol = "ems_km2")
