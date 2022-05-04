#'----------------------------------------------------------------------
#'
#' Map of `PM25` flux (emission rate per km^2).
#'
#' -
#'
#'----------------------------------------------------------------------

mapview_ISRM_emissions <- function (
  ISRM_data,
  pollutant,
  digits = 2,
  cell_id_var = "US_ISRM_id",
  unit = "ton/yr",
  zcol = "ems_qty"
) {

  map_geodata <-
    ISRM_data %>%
    filter(
      pol_abbr == pollutant) %>%
    drop_units() %>%
    sum_emissions_by(
      pol_abbr,
      all_of(cell_id_var)) %>%
    left_join(
      require_data(US_ISRM_SFAB_cell_geometries),
      .,
      by = cell_id_var) %>%
    mutate(
      zcol_per_km2 = .[[zcol]] / st_km2(.))

  cell_ids <-
    unlist(select(st_drop_geometry(map_geodata), all_of(cell_id_var)))

  cell_values <-
    map_geodata[[zcol]]

  stopifnot(length(cell_ids) == length(cell_values))

  cell_labels <-
    cell_values %>%
    format_digits(digits) %>%
    str_suffix(str_glue(" {unit}")) %>%
    replace(is.na(cell_values), "NA") %>%
    str_c("#", cell_ids, ": ", ., " ", pollutant)

  html_table <- function (.data) {
    table_data <-
      pivot_wider(
        drop_units(.data),
        names_from = "pol_abbr",
        values_from = zcol)
    knitr::kable(table_data, format = "html")
  }

  popup_html <-
    ISRM_data %>%
    filter(
      pol_abbr == pollutant) %>%
    drop_units() %>%
    sum_emissions_by(
      pol_abbr,
      src_h1,
      all_of(cell_id_var)) %>%
    nest(
      popup_data = c(src_h1, pol_abbr, all_of(zcol))) %>%
    mutate(
      popup_html = map_chr(popup_data, html_table)) %>%
    left_join(
      map_geodata,
      .,
      by = cell_id_var) %>%
    pull(
      popup_html)

  map_object <-
    mapview::mapview(
      map_geodata,
      label = cell_labels,
      layer.name = str_glue("{pollutant} ({unit})"),
      popup = popup_html,
      zcol = "zcol_per_km2")

  return(map_object)

}

SFAB_ISRM_demo_ems_data %>%
  # filter(
  #   src_h1 %in% c("afdust")) %>%
  mapview_ISRM_emissions(
    pollutant = "PM25")
