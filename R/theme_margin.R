theme_margin <- function (x = c(t = 0, r = 0, b = 0, l = 0), unit = "cm") {
  if (length(x) == 1) x <- rep(x, 4)
  args <- append(as.list(x), list(unit = unit))
  m <- rlang::exec(ggplot2::margin, !!!append(x, unit))
  return(theme(plot.margin = m))
}
