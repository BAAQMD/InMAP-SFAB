sum_population <- function (
  input_data,
  na.rm = TRUE,
  .groups = "drop",
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[sum_population] ", ...)

  input_vars <-
    names(ungroup(input_data))

  pop_name_vars <-
    tidyselect::vars_select(
      input_vars,
      starts_with("pop_qty"))

  pop_unit_vars <-
    names(select(ungroup(input_data), where(~ has_units(., "person"))))

  qty_vars <-
    union(pop_name_vars, pop_unit_vars)

  if (length(qty_vars) == 0) {
    err_msg <- "no variable(s) with units of \"person\" or named \"pop_qty\" found in your data"
    stop(err_msg)
  }

  f <- function (x) sum(x, na.rm = na.rm)

  summed <-
    summarise(
      drop_units(input_data),
      across(all_of(qty_vars), f),
      .groups = .groups)

  restored <-
    unittools::restore_units(
      summed,
      from = input_data)

  return(restored)

}
