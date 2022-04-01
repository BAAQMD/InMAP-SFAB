sum_population <- function (
  input_data,
  na.rm = TRUE,
  .groups = "drop",
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[sum_population] ", ...)

  pop_qty_vars <-
    find_pop_qty_vars(
      input_data)

  f <- function (x) sum(x, na.rm = na.rm)

  summed <-
    summarise(
      drop_units(input_data),
      across(all_of(pop_qty_vars), f),
      .groups = .groups)

  restored <-
    unittools::restore_units(
      summed,
      from = input_data)

  return(restored)

}
