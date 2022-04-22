write_geojson <- function (x, dsn, ..., crs = 4326, overwrite = TRUE) {
  if (fs::file_exists(dsn)) {
    if (isTRUE(overwrite)) {
      fs::file_delete(dsn) # FIXME: move out of the way, don't delete (in case of failure)
    }
  }
  st_write(
    st_transform(st_as_sf(x), crs = crs),
    dsn,
    ...,
    append = FALSE)
}
