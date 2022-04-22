source(here::here("Make", "100-setup.R"))

MV_CSV_PATH <-
  data_path(
    "Zenodo",
    "marginal_values",
    "marginal_values.csv")

mv_data <-
  MV_CSV_PATH %>%
  read_csv()

#'
#' Extract values for cell 1481, the cell that appears as an example
#' on p. 20 of [References/Literature/PDF/apte2019method.pdf].
#'
mv_cell_vars <- c(
  "cell_ID", "Population", "Mortality rate",
  str_c("Location_", c("N", "S", "E", "W")),
  "Centroid_lat", "Centroid_long")

QA_CELL_ID <- 1481

mv_data %>%
  filter(
    cell_ID == QA_CELL_ID) %>%
  select(
    any_of(mv_cell_vars)) %>%
  glimpse()

MV_DESCRIPTION_CODEC <- c(
  "MD" = "Marginal damages",
  "MM" = "Marginal change in mortality",
  "ConPop" = "Change in population exposure to PM2.5")

MV_UNIT_CODEC <- c(
  "MD" = "$/t (2011 USD)",
  "MM" = "death/t",
  "ConPop" = "person*ug/m^3/t")

mv_QA_table_data <-
  mv_data %>%
  filter(
    cell_ID == QA_CELL_ID) %>%
  select(
    -any_of(mv_cell_vars)) %>%
  rename_with(
    ~ str_replace(., "_pop", "Pop")) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Pollutant", "Layer"),
    names_sep = "_",
    values_to = "Value")  %>%
  mutate(
    Unit = dplyr::recode(Variable, !!!MV_UNIT_CODEC),
    Variable = dplyr::recode(Variable, !!!MV_DESCRIPTION_CODEC))

mv_QA_table_data %>%
  filter(
    Layer == "low") %>%
  select(
    -Layer) %>%
  pivot_wider(
    names_from = "Pollutant",
    values_from = "Value") %>%
  select_last(
    Unit) %>%
  knitr::kable(
    format = "markdown",
    caption = str_glue(
      "Low-stack values for cell #{QA_CELL_ID} ",
      "from {fs::path_rel(MV_CSV_PATH)}."))

#'----------------------------------------------------------------------
#'
#'
#'
#'----------------------------------------------------------------------

breathing_rate <-
  set_units(14.5, "m^3/d/person")

Con_pop_VOC_low <-
  set_units(0.166, "person*ug/m^3/t")

Con_pop_PrimaryPM25_low <-
  set_units(3.529, "person*ug/m^3/t")

US_pop_total <-
  set_units(311, "Mperson")

# Exposure factor for US (pop-wtd ug/m^3 per tonne)
Con_pop_PrimaryPM25_low / US_pop_total
