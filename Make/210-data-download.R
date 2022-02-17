#'----------------------------------------------------------------------
#'
#' Check if the "California" ISRM NetCDF is already downloaded.
#' If not, go ahead and download it.
#'
#' **FIXME**: Not working as of 2022-02-16.
#' GDrive doesn't seem to like GET or POST requests like this.
#'
#'----------------------------------------------------------------------

# if (isFALSE(fs::file_exists(ISRM_CA_nc_path))) {
#
#   message(
#     "[210-data-download] ",
#     fs::path_rel(ISRM_CA_nc_path, here::here()),
#     " does not yet exist; downloading now")
#
#   response <- httr::GET(ISRM_CA_nc_url)
#   html_content <- httr::content(response)
#
#   parsed_url <- httr::parse_url(ISRM_CA_nc_url)
#
#   response <-
#     httr::POST(
#       "https://drive.google.com/u/0/uc",
#       query = parsed_url$query,
#       httr::write_disk(ISRM_CA_nc_path),
#       httr::progress())
#
#   httr::stop_for_status(response) # ensure we got a "200 OK"
#
# }

#'----------------------------------------------------------------------
#'
#' Check if the "full" ISRM NetCDF is already downloaded.
#' If not, go ahead and download it.
#'
#' NOTE: Zenodo seems to throttle aggressively---expect hours
#' download this, even though it's only 12 GB zipped.
#'
#'----------------------------------------------------------------------

if (isFALSE(fs::file_exists(ISRM_full_nc_path))) {

  message(
    "[210-data-download] ",
    fs::path_rel(ISRM_full_nc_path, here::here()),
    " does not yet exist; downloading now")

  nc_zip_path <-
    tempfile(fileext = ".zip")

  response <-
    httr::GET(
      ISRM_full_nc_url,
      httr::write_disk(nc_zip_path),
      httr::progress())

  unzip(
    nc_zip_path,
    exdir = here::here("Data"))

}

