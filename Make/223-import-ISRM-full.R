#'----------------------------------------------------------------------
#'
#' Extract deltas for the "SFAB" domain, from the **full** ISRM dataset,
#'   into an in-memory array `ISRM_SFAB_array`.
#'
#' - Per variable, this step consumes ~50 MB and takes ~2 seconds.
#' - See `R/extract_ISRM_array.R` for more details.
#'
#'----------------------------------------------------------------------

ISRM_SFAB_cell_ids <-
  ISRM_SFAB_cell_geometries %>%
  pull(isrm)

ISRM_SFAB_array <- local({

  ISRM_full_ncdf4_obj <-
    ncdf4::nc_open(
      ISRM_FULL_NC_PATH)

  varids <- c(
    "PrimaryPM25",
    "SOA",
    "pNH4",
    "pNO3",
    "pSO4")

  array_list <-
    map(varids,
        ~ extract_ISRM_array(
          ISRM_full_ncdf4_obj,
          ISRM_SFAB_cell_ids,
          varid = .,
          layer = 1))

  ncdf4::nc_close(
    ISRM_full_ncdf4_obj)

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

ISRM_SFAB_cube <-
  tbl_cube(
    dimensions = dimnames(ISRM_SFAB_array),
    measures = list(value = ISRM_SFAB_array))

#'----------------------------------------------------------------------
#'
#' Extract baseline data and join it to cell geometries, yielding:
#'
#' - `ISRM_full_cell_geodata`; and
#' - `ISRM_CA_cell_geodata`
#'     - A subset of `ISRM_full_cell_geodata`
#'         - Where `isrm` is in `ISRM_SFAB_cell_ids`
#'
#'----------------------------------------------------------------------

ISRM_full_cell_geodata <- local({

  ISRM_full_tidync_obj <-
    tidync::tidync(
      ISRM_FULL_NC_PATH)

  ISRM_full_baseline_data <-
    ISRM_full_tidync_obj %>%
    tidync::activate("D3") %>%
    hyper_tibble() %>%
    filter(
      Layer == 0) %>%
    ensurer::ensure(
      nrow(.) == ISRM_FULL_CELL_COUNT,
      all(.$allcells == 1:nrow(.))) %>%
    mutate(
      isrm = allcells - 1L) # **NOTE**: `isrm` starts at 0; `allcells` starts at 1

  rm(ISRM_full_tidync_obj)

  powerjoin::power_left_join(
    ISRM_full_cell_geometries,
    ISRM_full_baseline_data,
    check = powerjoin::check_specs(
      column_conflict = "abort",
      unmatched_keys_left = "abort",
      unmatched_keys_right = "abort",
      na_keys = "abort"),
    by = "isrm")

})

ISRM_SFAB_cell_geodata <-
  ISRM_full_cell_geodata %>%
  filter(
    isrm %in% ISRM_SFAB_cell_ids)
