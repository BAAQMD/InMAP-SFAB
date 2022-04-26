#'----------------------------------------------------------------------
#'
#' Import `CA_ISRM_cell_geometries` from UW-supplied shapefile.
#'
#' FIXME: is `CA_ISRM_id = JoinID` correct?
#' - See https://github.com/BAAQMD/InMAP-SFAB/issues/5#issuecomment-1110000473)
#'
#'----------------------------------------------------------------------

CA_ISRM_cell_geometries <-
  data_path("UW", "2022-04-25", "InMAP_gridCells.shp") %>%
  read_shp() %>%
  ensurer::ensure(
    nrow(.) == CA_ISRM_CELL_COUNT) %>%
  rename(
    CA_ISRM_id = JoinID) # FIXME

#'----------------------------------------------------------------------
#'
#' Filter `CA_ISRM_cell_geometries`,
#' using `CMAQ_LCC_envelope`,
#' yielding `CA_ISRM_SFAB_cell_geodata` (n = 2,553 cells).
#'
#'----------------------------------------------------------------------

msg("filtering `CA_ISRM_cell_geometries` using `CMAQ_LCC_envelope`")

CA_ISRM_SFAB_cell_geometries <-
  CA_ISRM_cell_geometries %>%
  st_filter(
    st_transform(
      CMAQ_LCC_envelope, st_crs(.))) %>%
  ensurer::ensure(
    nrow(.) == 5486)

CA_ISRM_SFAB_cell_ids <-
  CA_ISRM_SFAB_cell_geometries %>%
  pull_any(ISRM_ID_VARS)

#'----------------------------------------------------------------------
#'
#' Extract deltas for the "SFAB" domain, from the "Cali" ISRM dataset,
#'   into an in-memory array `CA_ISRM_SFAB_array`.
#'
#' - Per variable, this step consumes ~50 MB and takes ~2 seconds.
#' - See `R/extract_ISRM_array.R` for more details.
#'
#'----------------------------------------------------------------------

CA_ISRM_SFAB_array <- local({

  CA_ISRM_ncdf4_obj <-
    ncdf4::nc_open(
      CA_ISRM_NC_PATH)

  conc_vars <- c(
    "PrimaryPM25",
    "SOA") # , "pNH4", "pNO3", "pSO4")

  extract_L0_varid <- function (varid) {
    extract_ISRM_array(
      CA_ISRM_ncdf4_obj,
      CA_ISRM_SFAB_cell_ids,
      varid = varid,
      layer = 1)
  }

  array_list <-
    conc_vars %>%
    map(progressively(
      extract_L0_varid,
      total = length(.)))

  ncdf4::nc_close(
    CA_ISRM_ncdf4_obj)

  bind_arrays(
    array_list,
    along = "varid")

})

#'----------------------------------------------------------------------
#'
#' A `tbl_cube` might be friendlier than an array, and it has about the
#' same memory footprint. But, it can't "just do" array multiplication.
#' Let's build it anyway (this is cheap, for the time being at least).
#'
#'----------------------------------------------------------------------

CA_ISRM_SFAB_cube <-
  tbl_cube(
    dimensions = dimnames(CA_ISRM_SFAB_array),
    measures = list(value = CA_ISRM_SFAB_array))

#'----------------------------------------------------------------------
#'
#' Extract baseline data and join it to cell geometries, yielding:
#'
#' - `CA_ISRM_cell_geodata`; and
#' - `CA_ISRM_SFAB_cell_geodata`
#'     - A subset of `ISRM_full_cell_geodata`
#'         - Where `ISRM_id` is in `US_ISRM_SFAB_cell_ids`
#'
#'----------------------------------------------------------------------

CA_ISRM_cell_geodata <- local({

  CA_ISRM_tidync_obj <-
    tidync::tidync(
      CA_ISRM_NC_PATH)

  CA_ISRM_baseline_data <-
    CA_ISRM_tidync_obj %>%
    tidync::activate("D3") %>%
    hyper_tibble() %>%
    filter(
      Layer == 0) %>%
    ensurer::ensure(
      nrow(.) == CA_ISRM_CELL_COUNT,
      all(.$allcells == 1:nrow(.))) %>%
    mutate(
      CA_ISRM_id := allcells)

  rm(CA_ISRM_tidync_obj)

  powerjoin::power_left_join(
    CA_ISRM_cell_geometries,
    CA_ISRM_baseline_data,
    check = powerjoin::check_specs(
      column_conflict = "abort",
      unmatched_keys_left = "abort",
      unmatched_keys_right = "abort",
      na_keys = "abort"),
    by = "CA_ISRM_id")

})

CA_ISRM_SFAB_cell_geodata <-
  CA_ISRM_cell_geodata %>%
  filter(across(
    any_of(ISRM_ID_VARS),
    ~ . %in% CA_ISRM_SFAB_cell_ids))

#'----------------------------------------------------------------------
#'
#' Write objects to disk.
#'
#'----------------------------------------------------------------------

write_data(CA_ISRM_cell_geometries)
write_data(CA_ISRM_cell_geodata)
write_data(CA_ISRM_SFAB_cell_geodata)
