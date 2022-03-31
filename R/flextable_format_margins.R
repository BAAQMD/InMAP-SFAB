flextable_format_margins <- function (
  table_object,
  pattern = regex("\\(all|average|mean|avg\\)")
) {

  body_data <-
    table_object$body$dataset

  # Pretty straightforward
  margin_column <-
    names(body_data) %>%
    str_detect(pattern) %>%
    which()

  #'
  #' **FIXME**: come up with a not-terrible heuristic for `h_var`
  #'
  #' Note: can be more than one "margin row" if table is grouped
  #'
  margin_rows <-
    body_data %>%
    map(str_detect, pattern) %>%
    reduce(`|`) %>% # note: (TRUE | NA) is TRUE
    which()

  # stopifnot(
  #   length(margin_rows) >= 1)

  vline_j <- margin_column - 1
  hline_i <- margin_rows - 1

  hline_j <-
    body_data[margin_rows,] %>%
    map_lgl(~ !all(is.na(.))) %>%
    which()

  table_object %>%
    flextable::italic(
      j = margin_column) %>%
    flextable::italic(
      i = margin_rows) %>%
    flextable::hline(
      i = hline_i,
      j = hline_j) %>%
    flextable::vline(
      j = vline_j,
      part = "body")

}
