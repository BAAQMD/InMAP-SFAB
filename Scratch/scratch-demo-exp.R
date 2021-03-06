mapview_ISRM_concentrations <- function (
  ISRM_data,
  pollutant,
  digits = 2,
  unit = NULL,
  zcol = "conc_qty"
) {

  if (is.null(unit)) {
    unit <- unit_for_concentration(pollutant, format = "character")
  }

  ISRM_id_var <-
    intersect(ISRM_ID_VARS, names(ISRM_data)) %>%
    ensurer::ensure(length(.) == 1)

  map_geodata <-
    ISRM_data %>%
    filter(
      pol_abbr == pollutant) %>%
    drop_units() %>%
    sum_concentration_by(
      all_of(ISRM_id_var)) %>%
    left_join(
      require_data(US_ISRM_SFAB_cell_geometries),
      .,
      by = ISRM_id_var)

  cell_ids <-
    map_geodata[[ISRM_id_var]]

  cell_values <-
    map_geodata[[zcol]]

  cell_labels <-
    cell_values %>%
    format_digits(digits) %>%
    str_suffix(str_glue(" {unit}")) %>%
    replace(is.na(cell_values), "NA") %>%
    str_c("#", cell_ids, ": ", ., " ", pollutant)

  html_table <- function (.data) {
    table_data <- spread(drop_units(.data), pol_abbr, conc_qty)
    knitr::kable(table_data, format = "html")
  }

  popup_html <-
    ISRM_data %>%
    nest(
      popup_data = c(src_h1, pol_abbr, conc_qty)) %>%
    mutate(
      popup_html = map_chr(popup_data, html_table)) %>%
    left_join(
      map_geodata,
      .,
      by = ISRM_id_var) %>%
    pull(
      popup_html)

  map_object <-
    mapview::mapview(
      map_geodata,
      label = cell_labels,
      layer.name = str_glue("{pollutant} ({unit})"),
      popup = popup_html,
      zcol = zcol)

  return(map_object)

}

#'----------------------------------------------------------------------
#'
#' Quick map of `PrimaryPM25`.
#'
#'----------------------------------------------------------------------

demo_map_PrimaryPM25 <-
  require_data(
    SFAB_ISRM_demo_conc_data) %>%
  mapview_ISRM_concentrations(
    pollutant = "PrimaryPM25")

show(demo_map_PrimaryPM25)

write_leaflet(
  demo_map_PrimaryPM25,
  build_path(
    "Demo",
    "SFAB_ISRM_demo_conc_data-PrimaryPM25.html"))

#'----------------------------------------------------------------------
#'
#' Demographics. This is 2020 Census, not supplied by UW.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_pop_2020_data %>%
  sum_population_by(pop_h1) %>%
  mutate(pop_share = percent_of(pop_qty))

#'----------------------------------------------------------------------
#'
#' Quick **area-weighted** averages of all pollutants.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_conc_data %>%
  drop_units() %>%
  sum_concentration_by(
    pol_abbr, any_of(ISRM_ID_VARS), cell_km2) %>%
  group_by(
    pol_abbr) %>%
  filter(
    is.finite(conc_qty), is.finite(cell_km2)) %>%
  summarise(
    conc_avg = weighted.mean(conc_qty, cell_km2, na.rm = TRUE))

#'----------------------------------------------------------------------
#'
#' Now for **population-weighted** averages.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_pop_2020_data %>%
  summarise_exposure_to(
    SFAB_ISRM_demo_conc_data,
    by = pol_abbr)

SFAB_ISRM_pop_2020_data %>%
  summarise_exposure_to(
    SFAB_ISRM_demo_conc_data,
    by = c(pol_abbr, pop_h1))

SFAB_ISRM_pop_2020_data %>%
  summarise_exposure_to(
    SFAB_ISRM_demo_conc_data,
    by = c(pol_abbr, src_h1))

#'----------------------------------------------------------------------
#'
#' Pivot table, with visual highlighting.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_pop_2020_data %>%
  summarise_exposure_to(
    SFAB_ISRM_demo_conc_data,
    by = c(pol_abbr, src_h1, pop_h1)) %>%
  pivot_table(
    rows = "src_h1",
    columns = c("pol_abbr", "pop_h1"),
    values = "exp/pop",
    inclusions = list(
      pol_abbr = list("TotalPM25")))

#'----------------------------------------------------------------------
#'
#' Flextable, suitable for MS Word.
#'
#'----------------------------------------------------------------------

local({

  table_data <-
    SFAB_ISRM_pop_2020_data %>%
    summarise_exposure_to(
      SFAB_ISRM_demo_conc_data,
      by = c(pol_abbr, src_h1, pop_h1)) %>%
    group_by(
      pol_abbr) %>%
    summarise_and_spread(
      .fun = summarise_exposure_by,
      values_from = "exp/pop",
      rows_from = "src_h1",
      cols_from = "pop_h1")

  table_data %>%
    drop_units() %>%
    group_by(
      pol_abbr) %>%
    render_flextable_exposure(
      caption = str_glue(
        "Exposures calculated from: (a) Census 2020 tract-level residential counts; and ",
        "(b) concentration estimates, on the SF air basin subset of the full ISRM grid. ",
        "The latter were supplied by the UW team in March 2022, and appear to be pulling ",
        "emissions from the 2014 NEI."),
      digits = 3)

})


