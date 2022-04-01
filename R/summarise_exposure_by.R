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

  pop_summary_data <-
    input_data %>%
    sum_distinct_population_by(
      any_of(by_vars))

  exp_summary_data <-
    input_data %>%
    group_by(
      across(c(any_of(by_vars)))) %>%
    summarise(
      exp_qty = sum(pop_qty * `exp/pop`, na.rm = na.rm),
      .groups = "drop")

  join_vars <-
    intersect(
      names(pop_summary_data),
      names(exp_summary_data))

  joined_data <-
    left_join(
      pop_summary_data,
      exp_summary_data,
      by = join_vars) %>%
    mutate(
      `exp/pop` = exp_qty / pop_qty)

  tidied_data <-
    joined_data %>%
    # mutate(
    #   exp_unit = unit_for_exposure(pol_abbr, format = "character")) %>%
    select_last(
      `exp/pop`)

  return(tidied_data)

}
