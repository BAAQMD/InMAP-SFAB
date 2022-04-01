find_pop_qty_vars <- function (input_data) {

  input_vars <-
    names(ungroup(input_data))

  pop_name_vars <-
    tidyselect::vars_select(
      input_vars,
      matches("pop_qty"),
      .strict = FALSE)

  pop_unit_vars <-
    names(select(ungroup(input_data), where(~ has_units(., "person"))))

  pop_qty_vars <-
    union(pop_name_vars, pop_unit_vars)

  if (length(pop_qty_vars) == 0) {
    err_msg <- "no variable(s) with units of \"person\" or named \"pop_qty\" found in your data"
    stop(err_msg)
  }

  return(pop_qty_vars)

}
