with_interpolated_population <- function (
  geodata,
  from,
  id_vars,
  extensive = TRUE,
  na.rm = FALSE,
  na = 0,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[with_interpolated_population] ", ...)

  target_geodata <-
    select(geodata, all_of(id_vars))

  # Like `c("P0020002", "P0020005", ...)`
  pop_vars <-
    st_drop_geometry(from) %>% # so we don't get `geometry` too
    select(where(~ has_units(., "person"))) %>%
    names() %>%
    set_names()

  if (length(pop_vars) < 1) {
    err_msg <- "Input data must have at least one column with units of \"person\"."
    stop(str_c("[interpolate_population_geodata] ", err_msg))
  }

  pop_geodata <-
    select(from, all_of(pop_vars)) %>%
    drop_units()

  filtered_target_geodata <-
    target_geodata %>%
    st_transform(
      st_crs(pop_geodata)) %>%
    st_filter(
      pop_geodata)

  # Like `c(P0020002 = "aggregate", ...)`
  pop_agr_values <-
    pop_vars %>%
    replace(values = "aggregate")

  filtered_target_agr_values <-
    names(filtered_target_geodata) %>%
    replace(values = "identity")

  # To make `st_interpolate_aw()` aware of semantics.
  st_agr(pop_geodata) <- pop_agr_values
  st_agr(filtered_target_geodata) <- filtered_target_agr_values

  msg(
    "interpolating ", length(pop_vars), " attributes ",
    "from ", nrow(pop_geodata), " source features ",
    "onto ", nrow(filtered_target_geodata), " ",
    "of ", nrow(geodata), " target features ")

  interpolated_pop_geodata <-
    st_interpolate_aw(
      pop_geodata,
      to = filtered_target_geodata,
      extensive = extensive,
      keep_NA = !na.rm,
      ...) %>%
    as_tibble() %>%
    st_as_sf()

  interpolated_pop_data <-
    bind_cols(
      st_drop_geometry(filtered_target_geodata),
      st_drop_geometry(interpolated_pop_geodata))

  pop_na_values <-
    rep_along(names(pop_vars), na) %>%
    set_names(pop_vars)

  annotated_geodata <-
    geodata %>%
    left_join(
      interpolated_pop_data,
      by = id_vars) %>%
    replace_na(
      as.list(pop_na_values)) %>%
    mutate(across(
      all_of(names(pop_vars)),
      set_units, "person")) %>%
    select_last(
      geometry)

  return(annotated_geodata)

}
