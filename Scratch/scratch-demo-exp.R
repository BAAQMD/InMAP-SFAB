#'----------------------------------------------------------------------
#'
#' Quick map of `PM25_TOT`.
#'
#'----------------------------------------------------------------------

map_geodata <-
  SFAB_ISRM_demo_conc_data %>%
  filter(
    pol_abbr == "PM25_TOT") %>%
  drop_units() %>%
  sum_concentration_by(
    all_of(ISRM_ID_VAR)) %>%
  left_join(
    require_data(ISRM_SFAB_cell_geometries),
    .,
    by = all_of(ISRM_ID_VAR))

mapview::mapview(
  map_geodata,
  zcol = "conc_qty")

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
    pol_abbr, all_of(ISRM_ID_VAR), cell_km2) %>%
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

SFAB_ISRM_demo_2020_exp_data %>%
  summarise_exposure_by(
    pol_abbr) %>%
  select(
    pol_abbr, `exp/pop`)

#'----------------------------------------------------------------------
#'
#' Pivot table, with visual highlighting.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_2020_exp_data %>%
  drop_units() %>%
  summarise_exposure_by(
    pol_abbr, src_h1, pop_h1) %>%
  pivot_table(
    rows = "src_h1",
    columns = c("pol_abbr", "pop_h1"),
    values = "exp/pop",
    inclusions = list(
      pol_abbr = list("PM25_TOT")))

#'----------------------------------------------------------------------
#'
#' Flextable, suitable for MS Word.
#'
#'----------------------------------------------------------------------

local({

  table_data <-
    SFAB_ISRM_demo_2020_exp_data %>%
    # filter(
    #   pol_abbr %in% c("PM25_TOT", "PM25_PRI")) %>%
    drop_units() %>%
    # summarise_exposure_by(
    #   pol_abbr, src_h1, pop_h1) %>%
    # mutate(
    #   src_h1 = factor(src_h1)) %>%
    group_by(
      pol_abbr) %>%
    summarise_and_spread(
      .fun = summarise_exposure_by,
      values_from = "exp/pop",
      rows_from = "src_h1",
      cols_from = "pop_h1",
      label = "(all)",
      .groups = "keep")

  table_data %>%
    group_by(
      pol_abbr) %>%
    render_flextable_exposure(
      caption = "Exposures calculated from demo data supplied by UW team, March 2022.",
      digits = 2)

})


