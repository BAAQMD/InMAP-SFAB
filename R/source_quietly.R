source_quietly <- function (
  path,
  recurse = FALSE,
  pattern = regex("\\.R$", ignore_case = TRUE)
) {
  if (fs::is_dir(path)) {
    paths <- fs::dir_ls(path, recurse = recurse, regexp = pattern)
    purrr::walk(paths, source_quietly, recurse = FALSE, pattern = pattern)
  } else {
    message("Sourcing ", fs::path_rel(path, here::here()))
    invisible(purrr::quietly(source)(path))
  }
}
