humanize_pollutant <- function (x, ...) {

  codec <- c(
    "NOx" = "NOx",
    "PM2.5 (Total)" = "PM25_TOT",
    "PM2.5 (Secondary)" = "PM25_SEC",
    "PM2.5 (Primary)" = "PM25_PRI",
    "SOA" = "SOA",
    "pNH4" = "pNH4",
    "pNO3" = "pNO3",
    "pSO4" = "pSO4")

  names(codec) <-
    names(codec) %>%
    str_replace_all("PM2.5", str_PM25(...)) %>%
    str_replace_all("NOx", str_NOx(...))

  codec <- codec[which(codec %in% unique(x))]
  recoded <- fct_recode(x, !!!codec)
  releveled <- factor(recoded, levels = names(codec))

  return(releveled)

}
