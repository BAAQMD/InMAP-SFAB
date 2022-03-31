theme_tight <- function (
  margin = ggplot2::margin(0, 0, 0, 0, "cm"),
  spacing = grid::unit(0, "cm")
) {
  theme(
    plot.margin = margin,
    panel.spacing = spacing,
    legend.margin = margin,
    legend.box.margin = margin,
    legend.box.spacing = spacing,
    legend.spacing = spacing)
}
