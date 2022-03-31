annotate_watermark <- function (
  label = str_draft(),
  x = NULL,
  y = NULL,
  hjust = 0,
  vjust = -1,
  color = gray(0.8),
  size = I(3),
  ...
) {

  # bb <- st_bbox(RSP_block_polygons)
  # if (is.null(x)) x <- bb["xmin"]
  # if (is.null(y)) y <- bb["ymin"]

  annotate(
    geom = "text",
    label = label,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    color = color,
    size = size,
    ...)
}
