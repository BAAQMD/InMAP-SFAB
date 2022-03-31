annotate_scalebar <- function (
  data = require_data(SFAB_boundary), # required by `ggsn::scalebar()`; TODO: work around this
  dist = 1000,
  dist_unit = "m",
  ...
) {

  envelope <-
    st_sf(st_buffer(st_envelope(data), -30))

  bb <- as.list(st_bbox(envelope))
  x_span <- with(bb, (xmax - xmin))

  scalebar_anchor <-
    with(bb, c(x = xmax, y = ymin)) %>%
    add(c((-x_span / 2) + 2, 0))

  # scalebar_object <-
  #   ggsn::scalebar(
  #     envelope,
  #     dist = dist * 1000,
  #     dist_unit = dist_unit,
  #     #st.bottom = FALSE,
  #     #st.size	= 10,
  #     #height = 0.01,
  #     #border.size = 0.2,
  #     location = "bottomright",
  #     anchor = scalebar_anchor,
  #     transform = TRUE)

  scalebar_object <-
    ggspatial::annotation_scale(
      #data = envelope,
      location = "bl",
      style = "ticks",
      text_cex = 1,
      pad_x = unit(7, "line"),
      text_pad = unit(1, "line"),
      ...)

  return(scalebar_object)

}
