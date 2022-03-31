fct_decode <- function (x, codec, relevel = FALSE) {
  i <- match(x, codec)
  if (isFALSE(relevel)) {
    i <- sort(i)
  }
  if (is.factor(x)) {
    decoded <- fct_recode(x, !!!codec[i])
  } else {
    decoded <- factor(x, levels = unname(codec[i]), labels = names(codec[i]))
  }
  return(decoded)
}
