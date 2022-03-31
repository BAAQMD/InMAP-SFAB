require_data <- function (x, verbose = TRUE) {

  msg <- function (...) if(isTRUE(verbose)) message("[require_data] ", ...)
  obj_name <- str_remove_all(lazyeval::expr_text(x), "[\"']")

  if (exists(obj_name)) {

    # just get it from the working environment; it's already in there
    obj <- get(obj_name)

  } else {

    root_path <- here::here("Build", "RData")
    rdata_paths <- dir(root_path, full.names = TRUE)
    obj_path <- keep(rdata_paths, ~ fs::path_ext_remove(fs::path_file(.)) == obj_name)

    if (length(obj_path) == 0) {
      err_msg <- str_c("[require_data] nothing named ", obj_name, " found in ", root_path)
      stop(err_msg)
    } else if (length(obj_path) > 1) {
      if (setequal(fs::path_ext(obj_path), c("grd", "gri"))) {
        p <- unique(fs::path_ext_remove(obj_path))
        stopifnot(length(p) == 1)
        obj_path <- fs::path_ext_set(p, "grd")
      }
    }

    has_raster_ext <- function (path, pattern = "grd|tif") {
      str_detect(fs::path_ext(path), regex(pattern, ignore_case = TRUE))
    }

    if (has_raster_ext(obj_path)) {
      obj <- terra::rast(obj_path)
    } else {
      obj <- read_rds(obj_path)
    }

    msg(str_glue("read {obj_name} from {obj_path}"))
    assign(obj_name, obj, envir = .GlobalEnv)

  }

  return(invisible(obj))

}
