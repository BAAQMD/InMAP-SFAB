unit_for_exposure <- function (
  pollutant,
  format = c("character", "plotmath", "html", "expression")
) {

  format <- match.arg(format)

  if (format == "character") {
    formatted <-
      case_when(
        (pollutant %in% c("NOx", "NOx")) ~ "ppb",
        (pollutant %in% c("PM2.5", "PM25", "DPM")) ~ "person*ug/m^3",
        (pollutant == "CANCRISK") ~ "burden",
        (pollutant == "CHRONHI") ~ "unitless")
  } else if (format == "html") {
    formatted <- unit_for_exposure(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m^3"), "µg/m<sup>3</sup>")
    formatted <- str_replace_all(formatted, fixed("*"), "&middot;")
  } else if (format == "plotmath") {
    formatted <- unit_for_exposure(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m^3"), "µg/m^3")
    formatted <- str_replace_all(formatted, " ", "~")
    formatted <- str_replace_all(formatted, fixed("*10^-6"), "%*%10^-6")
    formatted <- str_replace_all(formatted, fixed("person*"), "person%.%")
  } else if (format == "expression") {
    formatted <- unit_for_exposure(pollutant, format = "plotmath")
    formatted <- scales::parse_format()(formatted)
  } else {
    stop("format \"", format, "\" not yet implemented")
  }

  return(formatted)

}
