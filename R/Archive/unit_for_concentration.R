unit_for_concentration <- function (
  pollutant,
  format = c("character", "plotmath", "html", "expression", "markdown", "TeX")
) {

  format <- match.arg(format)

  if (format == "character") {
    formatted <-
      case_when(
        (pollutant %in% c("NOX", "NOx")) ~ "ppb",
        (pollutant %in% c("PM25", "PM2.5")) ~ str_ugm3(format = format),
        (pollutant == "DPM") ~ "risk", # WAS: "ug/m3",
        (pollutant == "CANCRISK") ~ "risk",
        (pollutant == "CHRONHI") ~ "unitless")
  } else if (format == "html") {
    formatted <- unit_for_concentration(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m3"), "µg/m<sup>3</sup>")
    formatted <- str_replace_all(formatted, fixed("*"), "&middot;")
  } else if (format == "plotmath") {
    formatted <- unit_for_concentration(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m3"), "µg/m^3")
    formatted <- str_replace_all(formatted, " ", "~")
    formatted <- str_replace_all(formatted, fixed("*10^-6"), "%*%10^-6")
    formatted <- str_replace_all(formatted, fixed("person*"), "person%.%")
  } else if (format == "expression") {
    formatted <- unit_for_concentration(pollutant, format = "plotmath")
    formatted <- scales::parse_format()(formatted)
  } else if (format == "TeX") {
    formatted <- unit_for_concentration(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m3"), str_ugm3(format = "TeX"))
  } else if (format == "markdown") {
    formatted <- unit_for_concentration(pollutant, format = "character")
    formatted <- str_replace_all(formatted, fixed("ug/m3"), "µg/m^3^")
    #formatted <- str_replace_all(formatted, fixed("*"), "&middot;")
  } else {
    stop("format \"", format, "\" not yet implemented")
  }

  return(formatted)

}
