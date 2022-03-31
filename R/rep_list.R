rep_list <- function (x, length.out) {
  if (length.out == 0) return(NULL)
  lapply(1:length.out, function (...) x)
}

rep_list_along <- function (along, x) {
  n <- length(along)
  if (n == 0) return(NULL)
  lst <- lapply(1:n, function (...) x)
  try(names(lst) <- unname(along))
  return(lst)
}
