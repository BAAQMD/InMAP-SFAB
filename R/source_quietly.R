source_quietly <- function (path, ...) {
  message("Sourcing ", fs::path_rel(path, here::here()))
  invisible(purrr::quietly(source)(path, ...))
}
