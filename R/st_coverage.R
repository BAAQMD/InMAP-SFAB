st_coverage <- function (x, y, id_var) {
  int_obj <- suppressWarnings(mutate(x, x_area = st_area(x)) %>% st_intersection(y))
  cov_obj <- int_obj %>% mutate(coverage = drop_units(st_area(.) / x_area))
  joined <- left_join(st_drop_geometry(x), st_drop_geometry(cov_obj), by = id_var) %>% select(any_of(id_var), coverage)
  return(joined %>% replace_na(list(coverage = 0)) %>% pull(coverage))
}
