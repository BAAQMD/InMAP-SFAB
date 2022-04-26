tidy_InMAP_names <- function (
  input_data,
  codec = c(
    "src_h1" = "Sector",
    "SCC_id" = "SCC",
    "TotalPM25" = "TotalPM25",
    "PrimaryPM25" = "PrimaryPM25",
    "NatAm" = "Native_American",
    "PcIsl" = "Pacific_Islander",
    "Multi" = "Multiple")
) {

  found_vars <-
    tidyselect::vars_select(
      names(input_data),
      all_of(codec),
      .strict = FALSE)

  tidied_data <-
    rename(
      input_data,
      !!!found_vars)

  if (any(ISRM_ID_VARS %in% names(tidied_data))) {
    tidied_data <- mutate(tidied_data, across(any_of(ISRM_ID_VARS), as.integer))
  }

  if ("SCC_id" %in% names(tidied_data)) {
    if (is.numeric(tidied_data[["SCC_id"]])) {
      tidied_data <- mutate(tidied_data, SCC_id = format_SCC(SCC_id, digits = 10))
    }
  }

  tidied_data <-
    select(
      tidied_data,
      any_of(ISRM_ID_VARS),
      any_of(ISRM_EMS_VARS),
      any_of(ISRM_CONC_VARS),
      any_of(ISRM_POP_VARS),
      everything())

  if ("geometry" %in% names(tidied_data)) {
    tidied_data <- tbltools::select_last(tidied_data, geometry)
  }

  return(tidied_data)

}
