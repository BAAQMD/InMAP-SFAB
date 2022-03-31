require_data(CMAQ_CRS)
require_data(SFAB_boundary)
require_data(SFBA_OSM_coast)

SFAB_tract_2020_geodata <- local({

  shp_path <-
    data_path(
      "Socrata",
      "Population by Race_Ethnicity - US Census 2020 Redistricting Tracts - California")

  message(str_glue(
    "[220-import-Socrata.R] creating SFBA_tract_2020_geodata"))

  message(str_glue(
    "[220-import-Socrata.R] - importing {fs::path_rel(shp_path, here::here())}"))

  # All of California
  shp_geodata <-
    read_shp(shp_path)

  # Tidier column names
  tidied_geodata <-
    shp_geodata %>%
    rename_with(
      str_to_upper, -geometry) %>%
    rename(
      tract_id = GEOID,
      tract_land = ALAND,
      tract_water = AWATER)

  message(str_glue(
    "[220-import-Socrata.R] - filtering counties: {str_and(SFBA::SFBA_COUNTY_FIPS_CODES)}"))

  # Just the 9 counties
  filtered_geodata <-
    tidied_geodata %>%
    filter(
      str_sub(tract_id, 3, 5) %in% SFBA::SFBA_COUNTY_FIPS_CODES)

  message(str_glue(
    "[220-import-Socrata.R] - trimming via SFBA::clip_to_coastline()"))

  # Coastal portions of tracts trimmed out
  trimmed_geodata <-
    filtered_geodata %>%
    st_transform(
      CMAQ_CRS) %>%
    SFBA::clip_to_coastline()

  is_valid <-
    st_is_valid(trimmed_geodata)

  # Are there any invalid features worth keeping?
  # May be some "slivers" along the inland boundary,
  # due to slight misalignment of the CoAbDis boundary and the "coastline" polygon.
  # (Maybe the coastline should be CA-wide, not SFBA-wide.)
  significant <-
    trimmed_geodata[!is_valid,] %>%
    smoothr::drop_crumbs(
      threshold = 1)

  stopifnot(nrow(significant) == 0)

  message(str_glue(
    "[220-import-Socrata.R] - calculating SFAB_frac ..."))

  annotated_geodata <-
    trimmed_geodata[is_valid,] %>%
    mutate(
      SFAB_frac = st_coverage(
        .,
        require_data(SFAB_boundary),
        id_var = "tract_id"))

  message(str_glue(
    "[220-import-Socrata.R] - clipping to SFAB_boundary"))

  pop_vars <-
    names(annotated_geodata) %>%
    tidyselect::vars_select(starts_with("P00"))

  agr_values <-
    replace(pop_vars, values = "aggregate") %>%
    c(tract_id = "identity", cnty_name = "identity") %>%
    c(tract_land = "aggregate", tract_water = "aggregate")

  annotated_geodata %>%
    with_tract_county() %>%
    mutate(across(
      all_of(pop_vars),
      ~ set_units(parse_integer(.), "person"))) %>%
    filter(
      SFAB_frac > 0) %>%
    st_intersection(
      require_data(SFAB_boundary)) %>%
    st_cast(
      "MULTIPOLYGON") %>%
    st_set_agr(
      agr_values) %>%
    mutate(across(
      c(tract_land, tract_water),
      ~ set_units(set_units(., "m^2"), "km^2")))

})

testthat::expect_true(
  all(st_is_valid(SFAB_tract_2020_geodata)))

#'----------------------------------------------------------------------

SFAB_tract_2020_raceeth_data <- local({

  message(str_glue(
    "[220-import-Socrata.R] creating SFAB_tract_2020_raceeth_data"))

  message(str_glue(
    "[220-import-Socrata.R] - dropping geometry from SFAB_tract_2020_geodata"))

  SFAB_tract_2020_data <-
    SFAB_tract_2020_geodata %>%
    st_drop_geometry()

  message(str_glue(
    "[220-import-Socrata.R] - pivoting to long format"))

  tidied_data <-
    SFAB_tract_2020_data %>%
    pivot_longer(
      cols = matches("^P00[0-9]{5}"),
      names_to = "variable",
      values_to = "pop_qty")

  message(str_glue(
    "[220-import-Socrata.R] - deriving pop_h1, pop_h2, and pop_h3 ",
    "from Census variables (P00*)"))

  tidied_data %>%
    mutate(
      pop_qty = pop_qty * SFAB_frac) %>%
    select(
      -SFAB_frac) %>%
    mutate(
      pop_h3 = variable,
      pop_h2 = raceeth_Census_h2(variable),
      pop_h1 = raceeth_Census_h1(variable)) %>%
    select(
      starts_with("tract_"),
      pop_h1, pop_h2, pop_h3, pop_qty)

})

SFAB_tract_2020_raceeth_data %>%
  pull_total(
    pop_qty) %>%
  testthat::expect_equal(
    7570000, tolerance = 0.001)

SFAB_tract_2020_raceeth_data %>%
  pull(pop_h1) %>%
  levels() %>%
  testthat::expect_equal(
    c("White", "HspLt", "AsnPI", "AABlk", "Other"))

#'----------------------------------------------------------------------

message(str_glue(
  "[220-import-Socrata.R] creating SFAB_tract_2020_polygons"))

SFAB_tract_2020_polygons <-
  SFAB_tract_2020_geodata %>%
  select(
    tract_id, SFAB_frac)

#'----------------------------------------------------------------------

write_data(SFAB_tract_2020_geodata)
write_data(SFAB_tract_2020_raceeth_data)
write_data(SFAB_tract_2020_polygons)

