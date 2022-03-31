summarise_population_by <- function (
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
      starts_with("cell_"),
      starts_with("tract_"),
      starts_with("blkgrp_"),
      starts_with("block_")
    )
    msg("auto-detected id_vars: ", str_csv(id_vars))
  }

  distinct_data <-
    input_data %>%
    select(
      any_of(by_vars),
      all_of(id_vars),
      all_of(qty_vars)) %>%
    distinct()

  summed_data <-
    sum_population_by(
      distinct_data,
      any_of(by_vars),
      na.rm = na.rm,
      .groups = .groups)

  return(summed_data)

}
