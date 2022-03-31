summarise_exposure_by <- function (
  input_data,
  ...,
  na.rm = TRUE,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[summarise_exposure_by] ", ...)

  input_vars <- names(input_data)
  by_vars <- unname(tidyselect::vars_select(input_vars, ...))
  msg("by_vars is: ", str_csv(by_vars))

  summed_pop_data <-
    input_data %>%
    summarise_population_by(
      any_of(by_vars))

  summed_exp_data <-
    input_data %>%
    group_by(
      across(c(pol_abbr, any_of(by_vars)))) %>%
    summarise(
      exp_qty = sum(exp_qty, na.rm = na.rm), .groups = "drop")

  join_vars <-
    intersect(
      names(summed_pop_data),
      names(summed_exp_data))

  joined_data <-
    left_join(
      summed_pop_data,
      summed_exp_data,
      by = join_vars) %>%
    mutate(
      `exp/pop` = exp_qty / pop_qty)

  tidied_data <-
    joined_data %>%
    mutate(
      exp_unit = unit_for_exposure(pol_abbr, format = "character")) %>%
    select_first(
      pol_abbr) %>%
    select_last(
      `exp/pop`,
      exp_qty,
      exp_unit)

  return(tidied_data)

}
