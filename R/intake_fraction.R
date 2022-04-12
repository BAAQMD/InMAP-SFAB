intake_fraction <- function (
  pop_qty,
  conc_qty,
  ems_qty,
  breathing_rate = set_units(14.5, "m^3/d/person"),
  unit = "ppm"
) {
  conc_qty <- set_units(conc_qty, "ug/m^3") # if not supplied
  ems_qty <- set_units(ems_qty, "ton/yr")   # if not supplied
  iF <- pop_qty * conc_qty * breathing_rate / ems_qty
  return(set_units(iF, unit, mode = "character"))
}
