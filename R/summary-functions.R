summarise_and_spread <- function (
  .data,
  .fun,
  values_from,
  rows_from,
  cols_from,
  label = "(all)",
  .groups = "keep"
) {

  grp_vars <- group_vars(.data)

  # cell_data <- local({
  #   summarised <- .fun(.data, all_of(c(grp_vars, rows_from, cols_from)))
  #   select(summarised, all_of(c(grp_vars, rows_from, cols_from, values_from)))
  # })

  # To summarise `.data` across `margin` (1 or more)
  f <- function (.data, margin = character(0)) {
    relabeled <- mutate(.data, across(all_of(margin), ~ label))
    summarised <- .fun(relabeled, all_of(c(grp_vars, rows_from, cols_from)))
    selected <- select(summarised, all_of(c(grp_vars, rows_from, cols_from, values_from)))
    for (j in margin) {
      old_levels <- levels(factor(.data[[j]]))
      selected[[j]] <- factor(selected[[j]], levels = c(old_levels, label))
    }
    return(selected)
  }

  .data <-
    mutate(.data, across(all_of(rows_from), factor))

  table_data <-
    bind_rows(
      f(.data), # cell data
      f(.data, rows_from), # row margins
      f(.data, cols_from), # column margins
      f(.data, c(rows_from, cols_from))) # grand margin

  spread_data <-
    pivot_wider(
      table_data,
      values_from = all_of(values_from),
      names_from = all_of(cols_from)) %>%
    arrange(
      across(all_of(c(grp_vars, rows_from))))

  if (.groups == "keep") {
    spread_data <- group_by(spread_data, across(all_of(grp_vars)))
  }

  return(spread_data)

}


summarise_county_x_raceeth <- function (.data, .fun, values_from) {
  require_data(CMAQ_ID_VARS)
  if ("cnty_name" %not_in% names(.data)) {
    if (all(CMAQ_ID_VARS %in% names(.data))) {
      .data <- with_cell_county(.data)
    } else if ("tract_id" %in% names(.data)) {
      .data <- with_tract_county(.data)
    } else {
      stop("[summarise_county_x_raceeth] don't know how to add cnty_* to your data")
    }
  }
  summarised <- .fun(.data, pol_abbr, cnty_name, pop_h1)
  select(summarised, pol_abbr, cnty_name, pop_h1, all_of(values_from))
}

summarise_county_x_raceeth_with_margins <- function (.data, .fun, values_from) {
  by_county_x_raceeth <- .data %>% summarise_county_x_raceeth(.fun, values_from)
  by_county <- mutate(.data, pop_h1 = label) %>% summarise_county_x_raceeth(.fun, values_from)
  by_raceeth <- mutate(.data, cnty_name = label) %>% summarise_county_x_raceeth(.fun, values_from)
  overall <- mutate(.data, pop_h1 = label, cnty_name = label) %>% summarise_county_x_raceeth(.fun, values_from)
  bind_rows(by_county_x_raceeth, by_county, by_raceeth, overall)
}

spread_county_x_raceeth <- function (
  .data,
  values_from,
  county_levels = COUNTY_LEVELS
) {

  pop_levels <-
    union(
      levels(factor(.data$pop_h1)),
      label)

  .data %>%
    mutate(
      pol_abbr = fct_rev(factor(pol_abbr)),
      pop_h1 = factor(pop_h1, levels = pop_levels)) %>%
    pivot_wider(
      names_from = "pop_h1",
      values_from = all_of(values_from)) %>%
    arrange(
      pol_abbr,
      cnty_name)
}
