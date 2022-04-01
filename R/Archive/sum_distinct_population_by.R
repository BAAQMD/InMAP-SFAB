sum_distinct_population_by <- function (
  input_data,
  ...,
  id_vars = NULL,
  na.rm = TRUE,
  .groups = "drop",
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[summarise_population_by] ", ...)

  input_vars <-
    names(input_data)

  by_vars <-
    tidyselect::vars_select(
      input_vars,
      ...)

  msg("by_vars is: ", str_csv(by_vars))

  if (is.null(id_vars)) {
    id_vars <- tidyselect::vars_select(
      input_vars,
      any_of(c(
        "cell_id",
        "tract_id",
        "blkgrp_id",
        "block_id")))
    if (length(id_vars) == 0) {
      try(id_vars <- vartools::find_id_var(input_data), silent = TRUE)
    }
    msg("auto-detected id_vars: ", str_csv(id_vars))
  }

  pop_qty_vars <-
    find_pop_qty_vars(
      input_data)

  distinct_data <-
    input_data %>%
    select(
      all_of(id_vars),
      any_of(by_vars),
      all_of(pop_qty_vars)) %>%
    distinct()

  summed_data <-
    sum_population_by(
      distinct_data,
      any_of(by_vars),
      na.rm = na.rm,
      .groups = .groups)

  return(summed_data)

}
