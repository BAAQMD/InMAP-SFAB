require_data(SFAB_ISRM_demo_data)
require_data(SFAB_ISRM_demo_conc_geodata)
require_data(SFAB_ISRM_demo_conc_data)
require_data(SFAB_ISRM_demo_ems_data)

#'----------------------------------------------------------------------
#'
#' Mapview of `TotalPM25` from residential wood combustion ("Res Wood").
#'
#' Using `st_as_sf()` to read WKT polygons directly from CSV.
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_data %>%
  st_as_sf(
    wkt = "geometry",
    crs = WGS84_GPS) %>%
  filter(
    src_h1 == "Res Wood") %>%
  mapview::mapview(
    stroke = FALSE,
    smoothFactor = 0,
    zcol = "PM25_TOT")

#'----------------------------------------------------------------------
#'
#' `gg_map_SFAB()` of `PM25_TOT` from "Res Wood".
#'
#' Visually, it matches the `mapview` variant above. No obvious errors.
#'
#'----------------------------------------------------------------------

local({

  layer_geodata <-
    SFAB_ISRM_demo_conc_geodata %>%
    drop_units() %>%
    filter(
      src_h1 == "Res Wood")

  gg_map_SFAB() +
    aes(fill = PM25_TOT) +
    scale_fill_sepia(
      limits = c(0, NA)) +
    geom_sf(
      color = NA,
      data = layer_geodata) +
    geom_SFAB_outline()

})

#'----------------------------------------------------------------------
#'
#' Summary of NEI-based RWC emissions, and comparison to CMAQ RWC emissions.
#' (Looks like NEI-based SFAB RWC emissions are +40% higher.)
#'
#'----------------------------------------------------------------------

CMAQ_EMS_VARS <-
  c("PM25", "NOX", "SOX", "VOC", "NH3") # omitting CO and CH4 for now

SFAB_CMAQ_RWC_ems_stobj <- local({

  nc_path <-
    fs::path("/Volumes", "AIM", "AB617", "Richmond",
             "Regional Modeling", "Woodsmoke Impacts",
             "Regional Emissions", "Annual_Tot_RWC_Emis.NewRWC_Base.2018.nc")

  CMAQ_stobj <-
    nc_path %>%
    import_CMAQ_ncdf(
      variables = CMAQ_EMS_VARS) %>%
    mutate(across(
      all_of(CMAQ_EMS_VARS),
      set_units, "ton/yr"))

  # Clip/mask to `SFAB_boundary`
  CMAQ_stobj %>%
    .[require_data(SFAB_boundary)]

})

SFAB_CMAQ_RWC_ems_data <-
  SFAB_CMAQ_RWC_ems_stobj %>%
  as_tibble() %>%
  drop_units() %>%
  mutate(
    src_h1 = "RWC") %>%
  pivot_longer(
    all_of(CMAQ_EMS_VARS),
    names_to = "pol_abbr",
    values_to = "ems_qty") %>%
  mutate(
    pol_abbr = factor(pol_abbr, levels = CMAQ_EMS_VARS)) %>%
  filter(
    is.finite(ems_qty),
    ems_qty > 0) %>%
  mutate(
    ems_unit = "ton/yr")

SFAB_CMAQ_RWC_ems_data %>%
  filter(
    src_h1 == "RWC") %>%
  sum_emissions_by(
    pol_abbr, src_h1)

SFAB_ISRM_demo_ems_data %>%
  filter(
    src_h1 == "rwc") %>%
  sum_emissions_by(
    pol_abbr, src_h1)

#'----------------------------------------------------------------------
#'
#' Summary of (ISRM x Census 2020) RWC exposures.
#'
#' - `exp/pop` for `PM25_TOT` due to RWC ≈ +0.306 ug/m3
#' - 97% of that is `PM25_PRI`
#'
#' See `Scratch/scratch-demo-exp.R` for a similar tabulation of
#' exposures for all demo sectors (not just RWC).
#'
#'----------------------------------------------------------------------

SFAB_ISRM_demo_2020_exp_data %>%
  filter(
    src_h1 == "Res Wood") %>%
  summarise_exposure_by(
    pol_abbr, src_h1)
