#'----------------------------------------------------------------------
#'
#' Map of total PM2.5, according to the CA ISRM.
#'
#'----------------------------------------------------------------------

MAP_PM25_BREAKS <-
  seq(0, 20, by = 5) %>%
  set_names(
    str_suffix(., " or more", length(.)))

map_scale <- list(
  theme(
    legend.text.align = 0),
  scale_fill_sepia(
    limits = range(MAP_PM25_BREAKS),
    breaks = MAP_PM25_BREAKS,
    labels = names(MAP_PM25_BREAKS),
    oob = scales::oob_squish))

fig1_object <- local({

  map_geodata <-
    require_data(
      CA_ISRM_SFAB_cell_geodata)

  map_var <-
    "BaselineTotalPM25"

  gg_map_SFAB() +
    geom_sf(
      aes_string(fill = map_var),
      color = NA,
      data = map_geodata) +
    geom_SFAB_outline(
      color = "black") +
    scale_extent(
      st_extent(SFAB_boundary)) +
    map_scale +
    theme(
      legend.position = "top") +
    annotate_scalebar() +
    labs(
      title = str_glue(
        "CA ISRM"),
      subtitle = str_glue(
        "Basis: {comment(map_geodata)}",
        "Mapped values are extracted from netCDF and joined to shapefile.",
        .sep = "\n"),
      caption = str_draft())

})

#'----------------------------------------------------------------------
#'
#' Map of total PM2.5, according to the "demo" dataset.
#'
#' - WARNING: sectors may be missing.
#' - See https://github.com/BAAQMD/InMAP-SFAB/issues/1#issuecomment-1105453126
#'
#'----------------------------------------------------------------------

fig2_object <- local({

  map_geodata <-
    require_data(
      SFAB_ISRM_demo_conc_data) %>%
    sum_concentration_by(
      pol_abbr,
      any_of(ISRM_ID_VARS)) %>%
    spread(
      pol_abbr,
      conc_qty) %>%
    inner_join(
      US_ISRM_SFAB_cell_geometries,
      .,
      by = intersect(names(.), ISRM_ID_VARS)) %>%
    set_comment(
      comment(SFAB_ISRM_demo_conc_data))

  comment(map_geodata)
  map_var <-
    "TotalPM25"

  gg_map_SFAB() +
    geom_sf(
      aes_string(fill = map_var),
      color = NA,
      data = drop_units(map_geodata)) +
    geom_SFAB_outline(
      color = "black") +
    map_scale +
    scale_extent(
      st_extent(SFAB_boundary)) +
    theme(
      legend.position = "top") +
    annotate_scalebar() +
    labs(
      title = str_glue(
        "Demo"),
      subtitle = str_glue(
        "Basis: {comment(map_geodata)}",
        "CSV is split by Sector; mapped values are sums, by cell, across Sector.",
        .sep = "\n"),
      caption = str_draft())

})

#'----------------------------------------------------------------------
#'
#' Paste both maps side-by-side
#'
#'----------------------------------------------------------------------

library(patchwork)

ggtools::write_png(
  (fig1_object + fig2_object),
  build_path("Demo", "scratch-demo-CA-PM25-map.png"),
  height = 12)
