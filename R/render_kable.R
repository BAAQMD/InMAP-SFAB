render_kable <- function (
  input_data,
  align = NULL,
  caption = NULL,
  style = c("basic", "hover", "condensed"),
  font_size = 16,
  position = "center",
  full_width = FALSE,
  escape = FALSE,
  header = NULL,
  formats = NULL,
  rename_with = NULL,
  ...,
  verbose = getOption("verbose", default = FALSE)
) {

  msg <- function (...) if(isTRUE(verbose)) message("[render_table] ", ...)

  if (isFALSE(is.null(formats))) {
    if (is.null(names(formats)) || isFALSE(is.list(formats))) {
      stop("formats must be a named list of functions")
    }
    for (j in intersect(names(formats), names(input_data))) {
      f <- formats[[j]]
      input_data[[j]] <- f(input_data[[j]])
    }
  }

  grp_var <-
    dplyr::group_vars(input_data)

  kable_data <-
    ungroup(input_data)

  if (nrow(kable_data) > 1) {

    if (length(grp_var) == 1) {

      kable_data <-
        kable_data %>%
        arrange_at(vars(one_of(grp_var))) %>%
        select(-one_of(grp_var))

    } else if (length(grp_var) == 0) {
      # pass
    } else {
      stop("Don't know how to handle multiple grouping variables in render_kable() yet.")
    }

    which_same <- which(map_lgl(kable_data, all_same))
    msg("which_same is: ", str_csv(which_same))

    blank_except_first <- function (x) replace(as.character(x), 2:length(x), "")

    if (length(which_same) == 1) {

      msg("packing column ", which_same)
      kable_data <-
        kable_data %>%
        mutate(
          across(where(all_same), blank_except_first))

    } else if (length(which_same) > 1) {
      warning("Dont know how to handle mutiple columns that are all_same")
    }

  }

  if (is.null(align)) {
    align <- c("l", rep("r", ncol(kable_data) - 1))
  }

  # kable_object <-
  #   kable(
  #     kable_data,
  #     align = align,
  #     caption = caption,
  #     escape = escape)

  if (isFALSE(is.null(rename_with))) {
    kable_data <- dplyr::rename_with(kable_data, rename_with)
  }


  kable_object <-
    kableExtra::kbl(
      kable_data,
      caption = caption,
      escape = escape,
      align = align,
      ...)

  styled_kable_object <-
    kableExtra::kable_styling(
      kable_object,
      bootstrap_options = style,
      full_width = full_width,
      font_size = font_size,
      position = position)

  if (is.character(header) || is.list(header)) {

    header_chr <- names(kable_data) %>% codec::decode(header) %>% replace_na(" ")
    header_colspans <- set_names(rle(header_chr)$lengths, rle(header_chr)$values)

    styled_kable_object <-
      kableExtra::add_header_above(
        styled_kable_object,
        header_colspans)

  }

  if (length(grp_var) > 0) {
    grp_vals <- pull(input_data, grp_var)
    grp_idx <- table(grp_vals)
    #print(grp_idx)
    styled_kable_object <- styled_kable_object %>% kableExtra::pack_rows(index = grp_idx)
  }

  return(styled_kable_object)

}
