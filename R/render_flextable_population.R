render_flextable_population <- function(
  table_data,
  caption,
  ...,
  format = "character"
) {

  is_count <- function (x) has_units(x, "person")
  count_vars <- names(select(table_data, where(is_count)))
  count_formatters <- rep_list_along(count_vars, format_count)

  is_percentage <- function (x) has_units(x, "%")
  pct_vars <- names(select(table_data, where(is_percentage)))
  pct_formatters <- rep_list_along(pct_vars, partial(format_percentage, digits = 1))

  # pct_formatters <-
  #   rep_list(
  #     partial(format_percentage, digits = 1),
  #     length(pct_vars)) %>%
  #   set_names(pct_vars)

  table_object <-
    table_data %>%
    drop_units() %>%
    render_flextable(
      ...,
      padding = 1,
      caption = caption) %>%
    flextable::set_formatter(
      values = append(
        count_formatters,
        pct_formatters)) %>%
    flextable::set_header_labels(
      pol_abbr = " ",
      cnty_name = " ") %>%
    flextable_align_where(
      is_bare_double,
      align = "center") %>%
    # flextable::align(
    #   j = which(names(table_data) == "(all)"),
    #   align = "right",
    #   part = "all") %>%
    flextable::align(
      j = which(names(table_data) %in% count_vars),
      align = "right",
      part = "all") %>%
    flextable::padding(
      padding.left = 3,
      padding.right = 3) %>%
    flextable_format_margins()

  return(table_object)

}
