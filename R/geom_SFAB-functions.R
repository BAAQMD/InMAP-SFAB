geom_SFAB_land <- function (
  color = NA,
  fill = "white",
  size = 0.2
) {
  geom_sf(
    color = color,
    size = size,
    fill = fill,
    data = require_data(SFAB_boundary))
}

#'----------------------------------------------------------------------

geom_SFAB_outline <- function (
  color = gray(0.5),
  size = 0.2
) {
  geom_sf(
    color = color,
    size = size,
    fill = NA,
    data = require_data(SFAB_boundary))
}

#'----------------------------------------------------------------------

geom_SFAB_water <- function (
  color = NA,
  fill = alpha("gray", 0.2),
  size = 0.2
) {
  geom_sf(
    color = color,
    fill = fill,
    size = size,
    data = st_stencil(
      require_data(SFAB_boundary), 10)) # 10 km buffer
}
