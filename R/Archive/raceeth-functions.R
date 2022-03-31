#'----------------------------------------------------------------------
#'
#' Palette definitions.
#'
CENSUS_RACEETH_COLORS <- c(
  White    = "#73B2FF",
  HspLt    = "#FFAA00",
  AABlk    = "#A1D209",
  AsnPI    = "#E7452D",
  Other    = "#895A44")

BENMAP_RACEETH_COLORS <- c(
  White    = CENSUS_RACEETH_COLORS[["White"]],
  Hispanic = CENSUS_RACEETH_COLORS[["HspLt"]],
  Black    = CENSUS_RACEETH_COLORS[["AABlk"]],
  Asian    = CENSUS_RACEETH_COLORS[["AsnPI"]],
  NatAmer  = CENSUS_RACEETH_COLORS[["Other"]])

RACEETH_COLORS <- c(
  White    = CENSUS_RACEETH_COLORS[["White"]],
  Hispanic = BENMAP_RACEETH_COLORS[["Hispanic"]],
  HspLt    = CENSUS_RACEETH_COLORS[["HspLt"]],
  Black    = BENMAP_RACEETH_COLORS[["Black"]],
  AABlk    = CENSUS_RACEETH_COLORS[["AABlk"]],
  Asian    = BENMAP_RACEETH_COLORS[["Asian"]],
  AsnPI    = CENSUS_RACEETH_COLORS[["AsnPI"]],
  NatAmer  = BENMAP_RACEETH_COLORS[["NatAmer"]],
  Other    = CENSUS_RACEETH_COLORS[["Other"]])

#'----------------------------------------------------------------------

#' @seealso raceeth_name()
raceeth_abbr <- function (
  x,
  codec = c(
    White = "W",
    POC   = "PoC",
    HspLt = "H",
    Hispanic = "H",
    AsnPI = "A",
    Asian = "A",
    AABlk = "B",
    Black = "B",
    NatAm = "N",
    Multi = "M",
    Other = "O")
) {
  dplyr::recode(x, !!!codec)
}

#'----------------------------------------------------------------------

raceeth_Census_h2 <- function (
  x,
  codec = c(
    White = "P0020005",
    HspLt = "P0020002",
    Asian = "P0020008",
    PcIsl = "P0020009",
    AABlk = "P0020006",
    NatAm = "P0020007",
    Multi = "P0020011",
    Other = "P0020010"),
  ...
) {
  recoded <- fct_decode(x, codec, ...)
  return(recoded)
}

testthat::expect_equal(
  raceeth_Census_h2(c("P0020007", "P0020011", "P0020009", "P0020010")),
  factor(c("NatAm", "Multi", "PcIsl", "Other"), levels = c("PcIsl", "NatAm", "Multi", "Other")))

#'----------------------------------------------------------------------

raceeth_Census_h1 <- function (
  x,
  codec = c(
    "White" = "White",
    "HspLt" = "HspLt",
    "AsnPI" = "Asian",
    "AsnPI" = "PcIsl",
    "AABlk" = "AABlk",
    "Other" = "NatAm",
    "Other" = "Multi",
    "Other" = "Other"),
  ...
) {
  recoded <- fct_decode(raceeth_Census_h2(x), codec, ...)
  return(recoded)
}

testthat::expect_equal(
  raceeth_Census_h1(c("P0020007", "P0020011", "P0020009", "P0020010")),
  factor(c("Other", "Other", "AsnPI", "Other"), levels = c("AsnPI", "Other")))

#'----------------------------------------------------------------------

#' raceeth_name
#'
#' Provides more human-friendly labels.
#'
#' @param x (character) short labels
#' @param codec (named character) mapping between short and long labels
#'
#' @details
#' - If `x` is a factor, then:
#'     - levels of `x` that are not present in the values of `x` will be dropped; and
#'     - the order of the (remaining) levels will be preserved.
#' - If `x` is not a factor, then the names of `codec` will determin the ordering of the levels of the result.
#'
#' @value longer labels (factor)
#'
#' @seealso raceeth_abbr()
raceeth_name <- function (
  x,
  codec = list(
    "White" = "White",
    "People of Color" = "POC",
    "Hispanic/Latino" = "HspLt",
    "Hispanic/Latino" = "Hispanic",
    "Asian/Pacific Islander" = "AsnPI",
    "Asian/Pacific Islander" = "Asian",
    "African-American/Black" = "AABlk",
    "African-American/Black" = "Black",
    "Native American" = "NatAmer",
    "Other/Multi" = "Other")
) {
  recoded <- fct_decode(x, codec)
  return(recoded)
}

testthat::expect_equal(
  raceeth_name(
    c("AsnPI", "Black", "White")),
  factor(
    c("Asian/Pacific Islander", "African-American/Black", "White"),
    levels = c("White", "Asian/Pacific Islander", "African-American/Black")))

testthat::expect_equal(
  raceeth_name(
    factor(
      c("Asian/Pacific Islander", "White"),
      levels = c("Asian/Pacific Islander", "White"))),
  factor(
    c("Asian/Pacific Islander", "White"),
    levels = c("Asian/Pacific Islander", "White")))

#'----------------------------------------------------------------------

str_raceeth <- function (x, ...) {
  translated <- fct_raceeth(x, ...)
  return(as.character(translated))
}

#'----------------------------------------------------------------------

fct_raceeth <- function (
  x,
  format = c("name", "letter"),
  abbreviate = FALSE
) {

  format <-
    match.arg(format)

  raceeth <-
    stringr::str_to_lower(as.character(x))

  if (format == "name") {
    translated <- raceeth_name(x)
  } else if (format == "abbr") {
    translated <- raceeth_abbr(x)
  }

  if (isTRUE(abbreviate)) {
    translated <- fct_relabel(translated, str_replace, "African-American", "Afr.-Amer.")
    translated <- fct_relabel(translated, str_replace, "Pacific Islander", "Pac. Isl.")
    translated <- fct_relabel(translated, str_replace, "Native American", "Native Amer.")
  }

  if (any(is.na(translated))) {
    unknown <- unique(x[which(is.na(translated))])
    warning("[str_raceeth] don't know how to translate ", str_or(unknown))
  }

  return(translated)

}

testthat::expect_equal(
  fct_raceeth(
    c("Other", "AsnPI", "AABlk", "AABlk"),
    format = "name",
    abbr = TRUE),
  factor(
    c("Other/Multi", "Asian/Pac. Isl.", "Afr.-Amer./Black", "Afr.-Amer./Black"),
    levels = c("Asian/Pac. Isl.", "Afr.-Amer./Black", "Other/Multi")))

#'----------------------------------------------------------------------

scale_x_raceeth <- function (
  name = NULL,
  labels = raceeth_abbr,
  ...
) {
  scale_x_discrete(
    name = name,
    labels = labels,
    ...)
}

#'----------------------------------------------------------------------

scale_fill_raceeth <- function (
  name = NULL,
  ...,
  values = RACEETH_COLORS,
  limits = force, # workaround for https://github.com/tidyverse/ggplot2/issues/4723
  labels = function (x) str_raceeth(x, abbr = TRUE)
) {
  scale_fill_manual(
    name = name,
    ...,
    values = values,
    limits = limits,
    labels = labels)
}

#'----------------------------------------------------------------------

scale_color_raceeth <- function (
  name = NULL,
  ...,
  values = RACEETH_COLORS,
  limits = force, # workaround for https://github.com/tidyverse/ggplot2/issues/4723
  labels = raceeth_name
) {
  scale_color_manual(
    name = name,
    ...,
    values = values,
    limits = limits,
    labels = labels)
}

