write_topojson <- function (x, dsn, ..., crs = 4326, overwrite = TRUE) {

  if (fs::file_exists(dsn)) {
    if (isTRUE(overwrite)) {
      fs::file_delete(dsn) # FIXME: move out of the way, don't delete (in case of failure)
    }
  }

  geojson_tmp_path <-
    tempfile(fileext = ".geojson")

  write_geojson(
    x, geojson_tmp_path, crs = crs, ...)

  geojson_content <-
    xfun::read_utf8(geojson_tmp_path) %>%
    str_remove_all("\n") %>%
    str_c(collapse = "")

  topojson_content <-
    geojsonio::geo2topo(
      geojson_content)

  xfun::write_utf8(
    topojson_content,
    dsn)

  return(invisible(dsn))

}
