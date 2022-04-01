render_flextable <- function (
  input_data,
  id = NULL, # bookmark ID
  align = NULL,
  caption = NULL,
  digits = flextable::get_flextable_defaults()$digits,
  na = "-",
  style = c("basic", "hover", "condensed"),
  font_size = 16,
  position = "center",
  full_width = FALSE,
  escape = FALSE,
  header = NULL,
  formats = NULL,
  col.width = NULL, # inches
  row.height = NULL, # inches
  padding = 1,
  rename_with = NULL,
  theme_fun = flextable::theme_booktabs,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  table_data <- input_data
  grp_vars <- group_vars(input_data)

  if (length(grp_vars) > 0) {
    table_data <-
      flextable::as_grouped_data(
        table_data,
        groups = grp_vars)
  }

  if (isFALSE(is.null(formats))) {
    if (is.null(names(formats)) || isFALSE(is.list(formats))) {
      stop("formats must be a named list of functions")
    }
    for (j in intersect(names(formats), names(table_data))) {
      f <- formats[[j]]
      table_data[[j]] <- f(table_data[[j]])
    }
  }

  flextable::init_flextable_defaults()

  flextable::set_flextable_defaults(
    # padding.left = padding,
    # padding.right = padding,
    # padding.bottom = padding,
    # padding.top = padding,
    theme_fun = theme_fun,
    ...)

  if (is.null(align)) {
    align <- replace(
      rep("l", ncol(table_data)),
      which(sapply(table_data, is.numeric)),
      "r")
  } else {
    align <- unlist(str_split(align, ""))
    if (length(align) == 1) {
      align <- rep(align, ncol(table_data))
    }
  }

  if (is.null(col.width) && is.null(row.height)) {
    table_object <-
      flextable::flextable(
        table_data) %>%
      flextable::autofit()
  } else {
    if (is.finite(col.width) && is.finite(row.height)) {
      table_object <-
        flextable::flextable(
          table_data,
          cwidth = col.width,
          cheight = row.height)
    } else {
      stop("[render_flextable] col.width and row.height must either be both NULL or both numeric")
    }
  }

  table_object <-
    table_object %>%
    flextable::colformat_double(
      digits = digits,
      na_str = na)}

  #
  # Default alignments for columns are controlled by the `align` argument
  #
  table_object <-
    table_object %>%
    flextable::align(j = which(align == "l"), align = "left", part = "all") %>%
    flextable::align(j = which(align == "r"), align = "right", part = "all") %>%
    flextable::align(j = which(align == "c"), align = "center", part = "all")

  #
  # Special considerations for grouped input
  #
  if (length(grp_vars) > 0) {

    cell_is_na <- table_data %>% select(-any_of(grp_vars)) %>% is.na()
    i <- which(rowMeans(cell_is_na) == 1)
    j <- match(grp_vars, names(table_data))

    table_object <-
      table_object %>%
      flextable::merge_h_range(i = i, j1 = 1, j2 = ncol(table_data)) %>%
      flextable::bold(i = i) %>%
      flextable::align(i = i, align = "left") %>%
      flextable::align(j = j, align = "left", part = "all") %>%
      flextable::set_header_labels(values = replace(names(table_data), j, "foo")) %>%
      flextable::hline(i = i) %>%
      flextable::width(j = j, 0.25, "in")

  }

  table_object <-
    table_object %>%
    flextable::padding(
      padding.left   = padding,
      padding.right  = padding,
      padding.bottom = padding,
      padding.top    = padding,
      part           = "all") %>%
    flextable::padding(
      padding.bottom = padding + 1,
      padding.top    = padding + 1,
      part           = "header") # add a little extra padding to header

  table_object <-
    flextable::bold(
      table_object,
      part = "header")

  table_object <-
    flextable::set_caption(
      table_object,
      caption = caption,
      autonum = officer::run_autonum(bkm = id))

  # attr(table_object, "data") <-
  #   input_data

  return(table_object)

}
