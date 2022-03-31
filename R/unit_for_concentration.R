unit_for_concentration <- function (
  pollutant,
  format
) {

  unit <- exptools::unit_for_concentration(pollutant, format)

  if (is.na(unit)) {
    unit <- str_ugm3(format = format)
  }

  return(unit)

}
