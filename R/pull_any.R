#' pull_any
#'
#' @param .data tabular data
#' @param ... [dplyr::select()] syntax; should match exactly 1 column
#'
#' @return single vector
#' @export
#'
#' pull_any(mtcars, starts_with("cy"))
pull_any <- function (.data, ...) {
  try(.data <- st_drop_geometry(.data)) # HACK
  j <- names(select(.data, any_of(...)))
  stopifnot(length(j) == 1)
  x <- pull(.data, !!j)
  return(x)
}
