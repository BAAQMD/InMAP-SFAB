write_data <- function (x, verbose = TRUE) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_data] ", ...)
  obj_name <- str_remove_all(lazyeval::expr_text(x), "[\"']")

  if (inherits(x, "SpatRaster")) {
    outfile <- str_glue("Build/RData/{obj_name}.grd")
    fs::dir_create(dirname(here::here(outfile)))
    terra::writeRaster(x, filename = here::here(outfile), overwrite = TRUE)
  } else {
    outfile <- str_glue("Build/RData/{obj_name}.Rds")
    fs::dir_create(dirname(here::here(outfile)))
    write_rds(x, here::here(outfile))
  }

  msg(str_glue("wrote {obj_name} to {outfile}"))

}
