with(SFAB_ISRM_demo_data, {
  testthat::expect_equal(Area, TotalPM25 / TotalPM_area, tol = 0.001)
  testthat::expect_equal(White_per, White / Total)
  testthat::expect_equal(DeathsK, DeathsK_White + DeathsK_Black + DeathsK_Asian + DeathsK_Hispanic)
  #testthat::expect_equal(Total, White + Black + NatAmer + Asian + PcIsl + Other + Multi + Hispanic)
})

SFAB_ISRM_demo_data %>%
  mutate(x = White + Black + NatAmer + Asian + PcIsl + Other + Multi) %>%
  drop_units() %>%
  gg_scatterplot(
    aes(x, Total))

SFAB_ISRM_demo_data %>%
  mutate(x = DeathsK_White + DeathsK_Black + DeathsK_Asian + DeathsK_Hispanic) %>%
  drop_units() %>%
  gg_scatterplot(
    aes(x, DeathsK))

