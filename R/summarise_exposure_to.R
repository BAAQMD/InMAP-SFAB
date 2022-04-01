summarise_exposure_to <- function (
  pop_data,
  conc_data,
  by,
  id_var = NULL,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[summarise_exposure_to] ", ...)

  .by <- rlang::enexpr(by)
  find_by_vars <- function (.data, .by) {
    tidyselect::vars_select(names(.data), !!.by, .strict = FALSE)
  }
  pop_by_vars <- find_by_vars(pop_data, .by)
  conc_by_vars <- find_by_vars(conc_data, .by)

  if (is.null(id_var)) {
    pop_id_var <- vartools::find_id_var(pop_data)
    conc_id_var <- vartools::find_id_var(pop_data)
    id_var <- intersect(pop_id_var, conc_id_var)
    stopifnot(length(id_var) == 1)
    msg("auto-detected id_var: ", id_var)
  }

  # Store for later use
  pop_group_vars <- group_vars(pop_data)
  conc_group_vars <- group_vars(conc_data)

  # # For testing, we might want to look a just one block.
  # # Otherwise, let's restrict to just those ids that `pop_data`
  # # and `conc_data` have in common.
  # if (isTRUE(is.null(keep_ids))) {
  #   keep_ids <- unique(intersect(pull(pop_data, id_var), pull(conc_data, id_var)))
  #   msg("n = ", format_count(length(keep_ids)), " ", id_var, " in common")
  # }
  # pop_data <- filter(pop_data, across(all_of(id_var), ~ . %in% keep_ids))
  # conc_data <- filter(conc_data, across(all_of(id_var), ~ . %in% keep_ids))

  if ("exp/pop" %in% names(conc_data)) {
    conc_data <- rename(conc_data, conc_qty = `exp/pop`)
  }

  conc_summary_data <-
    sum_concentration_by(
      drop_units(ungroup(conc_data)),
      all_of(id_var),
      all_of(conc_group_vars),
      all_of(conc_by_vars))

  pop_summary_data <-
    sum_population_by(
      drop_units(ungroup(pop_data)),
      all_of(id_var),
      all_of(pop_group_vars),
      all_of(pop_by_vars))

  joined_data <-
    inner_join(
      filter(conc_summary_data, conc_qty > 0),
      filter(pop_summary_data, pop_qty > 0),
      by = id_var)

  summarised_data <-
    joined_data %>%
    group_by(
      across(c(
        all_of(conc_group_vars),
        all_of(conc_by_vars),
        all_of(pop_group_vars),
        all_of(pop_by_vars)))) %>%
    summarise(
      conc_qty = weighted.mean(conc_qty, pop_qty),
      pop_qty = sum(pop_qty),
      .groups = "drop")

  tidied_data <-
    summarised_data %>%
    select(
      all_of(find_by_vars(summarised_data, .by)),
      everything()) %>%
    restore_units(
      from = pop_data) %>%
    restore_units(
      from = conc_data) %>%
    group_by(
      across(c(
        all_of(pop_group_vars),
        all_of(conc_group_vars)))) %>%
    rename(
      `exp/pop` = conc_qty)

  return(tidied_data)

}
