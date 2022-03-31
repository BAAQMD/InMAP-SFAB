#' msg
#'
#' Display a diagnostic message from within a function.
#'
#' @param ... passed to [stringr::str_glue()]
#'
#' @details
#' - If `verbose` is not `TRUE` in the context of the enclosing function, and `options("verbose")` is not `TRUE`, then a message will not be displayed.
#' - The message will be prefixed with the name of the enclosing function surrounded by square brackets.
#'
#' @importFrom rlang trace_back call_name
#' @importFrom stringr str_glue
#'
#' @examples
#' foo <- function (..., verbose = TRUE) {
#'     msg("hello world")
#' }
#' foo()
#'
#' @export
msg <- function (...) {

  verbose <- get0("verbose", ifnotfound = getOption("verbose", default = TRUE))
  if (isFALSE(isTRUE(verbose))) return("verbose is FALSE")

  context <- NULL

  # First, try getting the path of the currently open document in RStudio
  if (is.null(context)) {
    active <- rstudioapi::getActiveDocumentContext()
    if (active[["id"]] == "#console") {
      context <- "console"
    } else if (nchar(active[["path"]]) > 0) {
      context <- fs::path_rel(active[["path"]], here::here())
    }
  }

  # If that fails, try getting the name of the current function
  if (is.null(context)) {
    pf <- base::parent.frame(n = 2)
    print(pf)
    tb <- rlang::trace_back(top = pf)
    print(tb)
    context <- rlang::call_name(tb$calls[[1]])
  }

  msg_txt <- stringr::str_glue("[{context}] ", ...)
  message(msg_txt)

}
