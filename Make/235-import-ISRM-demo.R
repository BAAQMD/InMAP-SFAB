require_data(US_ISRM_SFAB_cell_geometries)
require_data(CA_ISRM_SFAB_cell_geometries)
require_data(SFAB_tract_2020_geodata)

#'----------------------------------------------------------------------
#'
#' Import demo dataset (NEI2014),
#'
#'----------------------------------------------------------------------

ISRM_demo_data <- local({

  csv_path <- data_path(
    "UW",
    "2022-05-04",
    "ISRM",
    "bay_results_aggregate_isrm.csv")

  csv_data <-
    csv_path %>%
    read_csv(
      verbose = TRUE) %>%
    select(
      -any_of("...1"))

  csv_cell_km2 <-
    csv_data %>%
    st_as_sf(wkt = "geometry", crs = WGS84_GPS) %>%
    st_km2()

  msg("renaming fields and setting units in `ISRM_demo_data`")
  msg("calculating Area = TotalPM25 / TotalPM_area")

  tidied_data <-
    csv_data %>%
    tidy_InMAP_names() %>%
    rename(
      US_ISRM_id = isrm)

  POP_PCT_VARS <- c(
    "White_per", "Black_per", "Asian_per", "Hispanic_per")

  unit_aware_data <-
    tidied_data %>%
    mutate(across(
      any_of(ISRM_CONC_VARS),
      set_units, "ug/m^3")) %>%
    mutate(across(
      any_of(ISRM_POP_VARS),
      set_units, "person")) %>%
    mutate(
      cell_km2 = csv_cell_km2) %>%
    mutate(across(
      c(Pop_density),
      set_units, "person/km^2")) %>%
    mutate(across(
      c(TotalPM_area),
      set_units, "ug/m^3/km^2")) %>%
    mutate(across(
      c(DeathsK),
      set_units, "death")) %>%
    mutate(across(
      starts_with("DeathsK_"),
      set_units, "death/km^2")) %>%
    mutate(across(
      any_of(POP_PCT_VARS),
      ~ set_units(. * 100, "%"))) %>%
    mutate(
      US_ISRM_id = as.integer(US_ISRM_id),
      Area = TotalPM25 / TotalPM_area)

  comment(unit_aware_data) <-
    fs::path_rel(csv_path, here::here())

  ISRM_demo_data <-
    unit_aware_data

})

#'----------------------------------------------------------------------
#'
#' Assemble `SFAB_ISRM_demo_conc_geodata` (wide format) and
#' `SFAB_ISRM_demo_conc_data` (long format).
#'
#' The WKT `geometry` in the CSV file `bay_inmap_result.csv` seems to
#' result in identical cell polygons, compared to the previously supplied
#' cell geometries, **for the cells that exist in both**. See the
#' chunk further down that uses `st_as_sf()` to parse WKT polygon
#' representations directly from the CSV's `geometry` column.
#'
#' **Missing cells.** See the warnings issued below by
#' `powerjoin::power_left_join()`:
#'
#' - There's one cell missing from the LHS (isrm #843). That is due to our
#'   own restriction(s) on what counts as an "SFAB" cell, which could be
#'   relaxed.
#'
#' - The 400+ cells present in the LHS (`US_ISRM_SFAB_cell_geometries`), but
#'   not in the RHS (demo CSV data), should probably be supplied by UW.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_conc_geodata <-
  ISRM_demo_data %>%
  select(
    any_of(ISRM_ID_VARS),
    any_of(ISRM_SRC_VARS),
    any_of(ISRM_CONC_VARS)) %>%
  powerjoin::power_right_join(
    US_ISRM_SFAB_cell_geometries,
    .,
    by = "US_ISRM_id",
    check = powerjoin::check_specs(
      duplicate_keys_left = "abort",
      unmatched_keys_left = "warn",
      unmatched_keys_right = "warn")) %>%
  select_last(
    geometry)

comment(SFAB_ISRM_demo_conc_geodata) <-
  comment(ISRM_demo_data)

SFAB_ISRM_demo_conc_data <-
  SFAB_ISRM_demo_conc_geodata %>%
  st_drop_geometry() %>%
  pivot_longer(
    any_of(ISRM_CONC_VARS),
    names_to = "pol_abbr",
    values_to = "conc_qty") %>%
  mutate(
    pol_abbr = factor(pol_abbr, levels = ISRM_CONC_VARS))

comment(SFAB_ISRM_demo_conc_data) <-
  comment(SFAB_ISRM_demo_conc_geodata)

#'----------------------------------------------------------------------
#'
#' Assemble `SFAB_ISRM_pop_2020_geodata` (wide format) and
#' `SFAB_ISRM_pop_2020_data` (long format).
#'
#' This is an area-weighted interpolation of Census 2020 data onto
#' ISRM cell geometries.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_pop_2020_geodata <-
  US_ISRM_SFAB_cell_geometries %>%
  select(
    any_of(ISRM_ID_VARS)) %>%
  with_interpolated_population(
    from = SFAB_tract_2020_geodata,
    id_vars = intersect(ISRM_ID_VARS, names(.)),
    na = 0,
    verbose = TRUE)

SFAB_ISRM_pop_2020_data <-
  SFAB_ISRM_pop_2020_geodata %>%
  st_drop_geometry() %>%
  pivot_longer(
    -any_of(ISRM_ID_VARS),
    names_to = "pop_h3",
    values_to = "pop_qty") %>%
  mutate(
    pop_h2 = raceeth_Census_h2(pop_h3),
    pop_h1 = raceeth_Census_h1(pop_h2)) %>%
  select_last(
    num_range("pop_h", 1:3),
    pop_qty)

#'----------------------------------------------------------------------
#'
#' NEI emission estimates.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_data <- local({

  csv_data <-
    data_path(
      "UW",
      "2022-03-07",
      "ISRM",
      "ca_nei.csv.gz") %>%
    read_csv(
      verbose = TRUE)

  tidied_data <-
    csv_data %>%
    tidy_InMAP_names() %>%
    select(-any_of(c("...1"))) %>%
    rename(US_ISRM_id = isrm)

  filtered_data <-
    tidied_data %>%
    semi_join(
      US_ISRM_SFAB_cell_geometries,
      by = "US_ISRM_id")

  unit_aware_data <-
    filtered_data %>%
    # mutate(across(
    #   c(NH3),
    #   set_units, "ton/yr")) %>%
    mutate(across(
      c(PM25, NOx, VOC, SOx, NH3),
      ~ set_units(set_units(., "ug/s"), "ton/yr"))) %>%
    mutate(across(
      c(Height, Diam),
      set_units, "m")) %>%
    mutate(across(
      c(Temp),
      set_units, "degK")) %>%
    mutate(across(
      c(Velocity),
      set_units, "m/s"))

  unit_aware_data %>%
    select(
      any_of(ISRM_ID_VARS),
      any_of(ISRM_SRC_VARS),
      any_of(ISRM_EMS_VARS)) %>%
    pivot_longer(
      any_of(ISRM_EMS_VARS),
      names_to = "pol_abbr",
      values_to = "ems_qty") %>%
    mutate(
      pol_abbr = factor(pol_abbr, levels = ISRM_EMS_VARS)) %>%
    filter(
      drop_units(ems_qty) > 0) %>%
    ensurer::ensure(
      units::deparse_unit(.$ems_qty) == "ton yr-1") %>%
    mutate(
      #ems_qty = drop_units(ems_qty),
      ems_unit = "ton/yr")

})

#'----------------------------------------------------------------------
#'
#' Assemble `SFAB_ISRM_demo_nested_geodata`, which has nested list-columns
#' `conc_data` and `pop_data`.
#'
#' Working with this seems slow, due to a lag in unnesting either or
#' both of those columns. So, best to regard as a "reference" dataset?
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_nested_geodata <- local({

  nest_vars <-
    union(ISRM_SRC_VARS, ISRM_CONC_VARS)

  nested_conc_data <-
    SFAB_ISRM_demo_conc_geodata %>%
    st_drop_geometry() %>%
    select(
      any_of(ISRM_ID_VARS),
      any_of(nest_vars)) %>%
    nest(
      conc_data = any_of(nest_vars))

  nested_conc_geodata <-
    SFAB_ISRM_demo_conc_geodata %>%
    select(
      -any_of(nest_vars)) %>%
    left_join(
      nested_conc_data,
      by = intersect(names(.), ISRM_ID_VARS))

  nested_pop_data <-
    SFAB_ISRM_pop_2020_data %>%
    nest(
      pop_data = starts_with("pop")) %>%
    select(
      any_of(ISRM_ID_VARS),
      pop_data)

  nested_conc_geodata %>%
    powerjoin::power_left_join(
      nested_pop_data,
      by = intersect(names(.), ISRM_ID_VARS),
      check = powerjoin::check_specs(
        duplicate_keys_right = "abort",
        unmatched_keys_left = "warn")) %>%
    select_last(
      geometry)

})

#'----------------------------------------------------------------------
#'
#' Exposure estimates (ISRM concentrations, weighted by 2020 Census
#' residential population).
#'
#'----------------------------------------------------------------------

# SFAB_ISRM_demo_2020_exp_data <-
#   SFAB_ISRM_pop_2020_data %>%
#   sum_population_by(
#     any_of(ISRM_ID_VARS),
#     pop_h1) %>%
#   filter(
#     drop_units(pop_qty) > 0) %>%
#   left_join(
#     SFAB_ISRM_demo_conc_data,
#     by = any_of(ISRM_ID_VARS)) %>%
#   filter(
#     drop_units(conc_qty) > 0) %>%
#   mutate(
#     exp_qty = conc_qty * pop_qty)

#'----------------------------------------------------------------------
#'
#' Write objects to disk.
#'
#'----------------------------------------------------------------------

write_data(SFAB_ISRM_demo_conc_geodata)
write_data(SFAB_ISRM_demo_conc_data)
write_data(SFAB_ISRM_pop_2020_geodata)
write_data(SFAB_ISRM_pop_2020_data)
write_data(SFAB_ISRM_demo_ems_data)

#'----------------------------------------------------------------------
#'
#' Write summaries of emissions, as CSV, to disk.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_ems_data %>%
  sum_emissions_by(
    any_of(ISRM_ID_VARS),
    pol_abbr,
    signif = 6) %>%
  write_csv(
    build_path("Demo", "SFAB_ISRM_demo_ems_data-pol.csv"))

SFAB_ISRM_demo_ems_data %>%
  sum_emissions_by(
    any_of(ISRM_ID_VARS),
    src_h1,
    pol_abbr,
    signif = 6) %>%
  write_csv(
    build_path("Demo", "SFAB_ISRM_demo_ems_data-pol_x_src.csv"))
