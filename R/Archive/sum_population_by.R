sum_population_by <- function (
  input_data,
  ...,
  na.rm = TRUE,
  .groups = "drop",
  verbose = getOption("verbose", default = FALSE)
) {
  grouped <- group_by(input_data, across(c(...)))
  summed <- sum_population(grouped, na.rm = na.rm, .groups = .groups, verbose = verbose)
  return(summed)
}
