gg_map_SFAB <- function (
  ...,
  #value_var = NULL,
  #limits = c(0, NA),
  #opacity = 1.0,
  #water_color = alpha("gray", 0.2),
  #mask = SFAB_rst_land_mask,
  #breaks = waiver(),
  #extent = st_extent(SFAB_block_polygons),
  color = gray(0.6),
  fill = "white",
  background = "white",
  size = 0.2,
  extent = st_extent(st_buffer(require_data(SFAB_boundary), 1)), # 1 km buffer
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  crs = require_data(CMAQ_CRS),
  watermark = str_draft()
) {

  map_description <-
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption)

  map_theme <-
    theme_void(
      base_size = 12) +
    theme_tight() +
    theme(
      legend.text = element_text(size = 12, color = color),
      legend.title = element_text(color = gray(0.2)),
      legend.box.margin = margin(l = 1, unit = "cm"),
      legend.margin = margin(t=12, r=12, b=12, l = -12, "pt")) +
    theme(
      plot.title = element_text(face = "bold"),
      #plot.subtitle = element_text(vjust = unit(-22, "cm")),
      plot.caption = element_text(
        color = color, hjust = 0,
        size = rel(0.75), margin = margin(0,0,0,0, unit = "lines")),
      plot.margin = margin(1, 1, 1, 1, unit = "cm")) +
    theme_remove(
      "strip", "text", c("x", "y")) +
    theme_remove(
      "axis", c("title", "text", "line", "ticks"), c("x", "y"))

  map_coord <- coord_sf(crs = crs)
  map_coord$default <- TRUE # suppress subsequent messages about "Coordinate system already present"

  map_object <-
    ggplot(...) +
    map_coord +
    geom_SFAB_land(fill = fill) +
    geom_SFAB_outline(size = size, color = color) +
    map_theme +
    map_description +
    annotate_watermark(
      label = watermark,
      x = extent[1],
      y = extent[3]) +
    scale_extent(
      extent)

  return(map_object)

}
