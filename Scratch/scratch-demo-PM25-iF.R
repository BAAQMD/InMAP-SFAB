require_data(SFAB_tract_2020_raceeth_data)
require_data(SFAB_ISRM_demo_ems_data)
require_data(SFAB_ISRM_demo_conc_data)
require_data(SFAB_ISRM_pop_2020_data)
require_data(ISRM_SFAB_cell_geometries)
require_data(ISRM_ID_VAR)

#'
#' In the support work for the NG appliances rulemaking, the regional average
#' population-weighted concentration of primary PM2.5 (`PM25_PRI`) was
#' found to be 4.40 ug/m^3.
#'
#' That from a total SFAB emissions of 12,396 ton/yr PM2.5, obtained there via:
#'
#'   CMAQ_ems_stobj[SFAB_boundary] %>%
#'     filter(scenario == "Baseline") %>%
#'     pull_total(PM25_PRI)
#'
#' Assuming a breathing rate of 14.5 m^3/d, this works out to
#' an intake fraction (iF) of **2.07 ppt**.
#'

NGC_PM5_PRI_conc_qty <-
  set_units(4.40, "ug/m^3")

NGC_PM25_ems_qty <-
  set_units(12396, "ton/yr") %>%
  set_units("t/d")

breathing_rate <-
  set_units(14.5, "m^3/d/person")

NGC_pop_qty <-
  pull_total(SFAB_tract_2020_raceeth_data, pop_qty) %>%
  set_units("Mperson")

NGC_PM25_iF <-
  intake_fraction(
    pop_qty = NGC_pop_qty,
    conc_qty = NGC_PM5_PRI_conc_qty,
    ems_qty = NGC_PM25_ems_qty,
    breathing_rate = breathing_rate)

testthat::expect_equal(
  NGC_PM25_iF, set_units(15.7, "ppm"), tol = 1e-2)

#' For comparison, in this demo, the numbers for `PM25_PRI` are:
#'
#' - Emissions: 17,885 ton/yr
#' - Exposure:   1.428 ug/m3
#'
#' Assuming a constant breathing rate of 14.5 m3 d-1, the former works out to
#' an intake fraction (iF) of 2.07 ppt, and the latter to 0.446 ppt.
#'
demo_PM25_ems_qty <-
  SFAB_ISRM_demo_ems_data %>%
  filter(
    pol_abbr == "PM25") %>%
  semi_join(
    ISRM_SFAB_cell_geometries,
    by = ISRM_ID_VAR) %>%
  mutate(
    ems_qty = set_units(ems_qty, unique(ems_unit), mode = "character")) %>%
  pull(ems_qty) %>%
  sum()

testthat::expect_equal(
  demo_PM25_ems_qty, 17885, tol = 1e-4)

demo_PM25_ems_qty <-
  set_units(
    demo_PM25_ems_qty,
    "t/d")

demo_PM25_PRI_conc_qty <-
  SFAB_ISRM_pop_2020_data %>%
  summarise_exposure_to(
    filter(
      SFAB_ISRM_demo_conc_data,
      pol_abbr == "PM25_PRI"),
    by = pol_abbr) %>%
  pull(`exp/pop`)

testthat::expect_equal(
  demo_PM25_PRI_conc_qty, 1.428, tol = 1e-3)

demo_pop_qty <-
  pull_total(SFAB_ISRM_pop_2020_data, pop_qty) %>%
  set_units("Mperson")

demo_PM25_iF <-
  intake_fraction(
    pop_qty = demo_pop_qty,
    conc_qty = demo_PM25_PRI_conc_qty,
    ems_qty = demo_PM25_ems_qty,
    breathing_rate = breathing_rate)

testthat::expect_equal(
  demo_PM25_iF, set_units(3.55, "ppm"), tol = 1e-2)

#'
#' That's a ratio of more than 4:1.
#'
show(NGC_PM25_iF / demo_PM25_iF)

#'----------------------------------------------------------------------
#'
#' Put numbers together in a nice table.
#'
#'----------------------------------------------------------------------

table_data <- tribble(
  ~ variable, ~ basis, ~ value, ~ unit,
  "P",  "CMAQ",  drop_units(NGC_pop_qty), deparse_unit(NGC_pop_qty),
  "P",  "InMAP", drop_units(demo_pop_qty), deparse_unit(demo_pop_qty),
  "C",  "CMAQ",  drop_units(NGC_PM5_PRI_conc_qty), deparse_unit(NGC_PM5_PRI_conc_qty),
  "C",  "InMAP", drop_units(demo_PM25_PRI_conc_qty), deparse_unit(demo_PM25_PRI_conc_qty),
  "Q",  "CMAQ",  drop_units(breathing_rate), deparse_unit(breathing_rate),
  "Q",  "InMAP", drop_units(breathing_rate), deparse_unit(breathing_rate),
  "E",  "CMAQ",  drop_units(NGC_PM25_ems_qty), deparse_unit(NGC_PM25_ems_qty),
  "E",  "InMAP", drop_units(demo_PM25_ems_qty), deparse_unit(demo_PM25_ems_qty),
  "iF", "CMAQ",  drop_units(NGC_PM25_iF), deparse_unit(NGC_PM25_iF),
  "iF", "InMAP", drop_units(demo_PM25_iF), deparse_unit(demo_PM25_iF))

table_data %>%
  mutate(
    variable = fct_inorder(variable)) %>%
  mutate(
    value = signif(value, 4)) %>%
  spread(
    basis, value) %>%
  select_last(
    unit) %>%
  knitr::kable(
    format = "markdown")
