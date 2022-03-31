#' progressively
#'
#' Wrap a function in a progress bar.
#' Good when the function is called repeatedly, as with [purrr::map()].
#'
#' @param .f function
#' @param title (character) prepended to progress bar
#' @param format passed to [progress::progress_bar]
#' @param width passed to [progress::progress_bar]
#' @param ... further arguments to [progress::progress_bar]
#'
#' @importFrom progress progress_bar
#'
#' @export
progressively <- function (
  .f,
  total,
  title = "",
  format = "[:bar] :percent (:current/:total) elapsed: :elapsedfull eta: :eta",
  width = 80,
  show_after = 0,
  ...
) {

  captured_args <- list(...)

  if (rlang::is_formula(.f)) {
    .f <- rlang::as_function(.f)
  }

  .pb <-
    progress::progress_bar$new(
      total = total,
      format = paste(title, format),
      width = width,
      show_after = show_after)

  fun <- function (...) {
    fun_args <- list(...)
    dot_args <- c(fun_args, captured_args)
    result <- rlang::exec(.f, !!!dot_args)
    .pb$tick()
    return(result)
  }

  return(fun)

}
