sum_concentration_by <- function (
  input_data,
  ...,
  .groups = "drop",
  na.rm = TRUE,
  cache = TRUE,
  verbose = getOption("verbose", default = TRUE)
) {

  # debugging 2021-09-13

  msg <- function (...) if(isTRUE(verbose)) message("[sum_concentration_by] ", ...)

  input_vars <- names(input_data)
  by_vars <- names(select(input_data, ...))
  msg("by_vars is: ", str_csv(by_vars))

  grouped_data <-
    group_by(
      drop_units(input_data),
      across(all_of(by_vars)))

  if (all(c("conc_layer", "conc_qty") %in% input_vars)) {
    err_msg <- "one but not both of \"conc_layer\" and \"conc_qty\" are in your data"
    stop(str_c("[sum_concentration_by] ", err_msg))
  }

  if ("conc_layer" %in% input_vars) {
    msg("summing raster layers (`conc_layer`)")
    summed_data <- summarise(
      grouped_data,
      conc_layer = list(raster_sum(conc_layer, cache = cache, verbose = verbose)),
      .groups = .groups)
  } else if ("conc_qty" %in% input_vars) {
    msg("summing quantities (`conc_qty`)")
    summed_data <- summarise(
      grouped_data,
      conc_qty = sum(conc_qty, na.rm = na.rm),
      .groups = .groups)
    summed_data <- restore_units(
      summed_data,
      from = input_data)
  }

  return(summed_data)

}

# # Memoised for speed during interactive work
# sum_concentration_by <-
#   memoise::memoise(
#     sum_concentration_by_,
#     cache = cache_memory())
#sum_concentration_by <- sum_concentration_by_

# Just a simple alias for readability's sake
sum_concentration <- sum_concentration_by
