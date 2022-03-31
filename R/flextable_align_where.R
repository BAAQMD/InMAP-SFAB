flextable_align_where <- function (
  table_object,
  j,
  align = c("left", "center", "right"),
  part = "all",
  ...
) {

  if (is.function(j)) {
    table_data <- table_object$body$dataset
    j <- which(sapply(table_data, j))
  }

  align <- match.arg(align)

  flextable::align(
    table_object,
    j = j,
    align = align,
    part = part,
    ...)

}
