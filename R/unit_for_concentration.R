unit_for_concentration <- function (
  pollutant,
  format,
  default = str_ugm3
) {

  unit <- exptools::unit_for_concentration(pollutant, format)

  i <- which(is.na(unit))
  if (any(i)) {
    if (is.function(default)) {
      default <- default(format = format)
    }
    unit[i] <- rep(default, length(i))
  }

  return(unit)

}
