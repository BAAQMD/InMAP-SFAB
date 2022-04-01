render_flextable_exposure <- function(
  table_data,
  caption,
  ...,
  format = "character"
) {

  pollutant_label <- function (x, unit = NULL, format = "markdown", na = "") {
    pol <- str_extract(x, regex("NOx|PM2\\.?5", ignore_case = TRUE))
    humanized <- humanize_pollutant(x, format = format)
    if (is.null(unit)) {
      unit <- unit_for_concentration(pol, format = format)
    }
    glued <- str_glue("{humanized}, {unit}")
    spliced <- if_else(is.na(x), na, as.character(glued))
    return(spliced)
  }

  table_object <-
    table_data %>%
    mutate(
      pol_abbr = pollutant_label(
        pol_abbr, format = "markdown")) %>%
    render_flextable(
      ...,
      caption = caption) %>%
    # flextable::set_formatter(
    #   pol_abbr = format_pollutant) %>%
    flextable::set_formatter(
      pop_qty = function (...) "Population",
      part = "header")

  table_object <-
    table_object %>%
    ftExtra::colformat_md(
      j = "pol_abbr")

  table_object <-
    table_object %>%
    flextable::set_header_labels(
      pol_abbr = " ",
      pop_h1 = " ",
      src_h1 = " ",
      cnty_name = " ") %>%
    flextable_align_where(
      is_bare_double,
      align = "c") %>%
    flextable::padding(
      padding.left = 3,
      padding.right = 3) %>%
    flextable_format_margins()

  return(table_object)

}
