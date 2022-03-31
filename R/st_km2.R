st_km2 <- function (..., drop_units = TRUE) {
  km2 <- set_units(st_area(...), "km^2")
  if (isTRUE(drop_units)) {
    km2 <- units::drop_units(km2)
  }
  return(km2)
}
