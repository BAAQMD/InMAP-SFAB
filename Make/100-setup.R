library(geotools)   # devtools::install_github("BAAQMD/geotools")
library(unittools)  # devtools::install_github("BAAQMD/unittools")
library(tbltools)   # devtools::install_github("BAAQMD/tbltools")
library(lltools)    # devtools::install_github("BAAQMD/lltools")
library(exptools)   # devtools::install_github("BAAQMD/exptools")
library(ggtools)    # devtools::install_github("BAAQMD/ggtools")
library(SFBA)

library(ncdf4)
library(ncmeta)
library(tidync)
library(cubelyr)

mapview::mapviewOptions(
  fgb = FALSE)  # per answer at [https://stackoverflow.com/questions/65485747/mapview-points-not-showing-in-r]

source(here::here("R", "source_quietly.R"))
source_quietly(here::here("R"), recurse = FALSE)

data_path <- function (...) {
  here::here("Data", ...)
}

ISRM_ID_VARS <- "isrm"
ISRM_SRC_VARS <- c("src_h1", "SCC_id")
ISRM_CONC_VARS <- c("PM25_TOT", "PM25_PRI", "SOA", "pNH4", "pNO3", "pSO4")
ISRM_EMS_VARS <- c("PM25", "NOx", "SOx", "VOC", "NH3")

ISRM_CRS <- WGS84_GPS # FIXME: assumption!

# The full ISRM (about 166 GB on disk)
ISRM_FULL_CELL_COUNT <- 52411
ISRM_FULL_NC_URL <- "https://zenodo.org/record/2589760/files/isrm_v1.2.1.zip?download=1"
ISRM_FULL_NC_PATH <- here::here("Data", "isrm_v1.2.1.ncf")

# Cell geometry definitions for the full ISRM (minimum cell size ≈ 1 km^2)
ISRM_FULL_LATLON_CSV_PATH <- here::here("Data", "ISRM", "isrm_boundaries_latlons.csv")

# The California "hi-res" prototype,
# via https://drive.google.com/drive/folders/1WmLRz7iWo2MjtSikgHEig7M0NvK2sOns
ISRM_CA_CELL_COUNT <- 9001
ISRM_CA_NC_URL <- "https://drive.google.com/u/0/uc?id=1v__TPKJeiL8Q4zLz8Dyx5HcWSqR2NDx4&export=download&confirm=t"
ISRM_CA_NC_PATH <- here::here("Data", "ca_isrm.ncf")

