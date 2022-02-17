library(geotools)   # devtools::install_github("BAAQMD/geotools")
library(unittools)  # devtools::install_github("BAAQMD/unittools")
library(tbltools)   # devtools::install_github("BAAQMD/tbltools")
library(lltools)    # devtools::install_github("BAAQMD/lltools")
library(ncdf4)
library(ncmeta)
library(tidync)
library(cubelyr)

source(here::here("R", "source_quietly.R"))
walk(fs::dir_ls(here::here("R"), glob = "*.R"), source_quietly)

mv <- mapview::mapview # convenient shortcut for interactive use

ISRM_CRS <- WGS84_GPS # FIXME: assumption!

ISRM_FULL_CELL_COUNT <- 52411
ISRM_CA_CELL_COUNT <- 9001

# The full ISRM (about 166 GB on disk)
ISRM_FULL_NC_URL <- "https://zenodo.org/record/2589760/files/isrm_v1.2.1.zip?download=1"
ISRM_FULL_NC_PATH <- here::here("Data", "isrm_v1.2.1.ncf")

# The California "hi-res" prototype,
# via https://drive.google.com/drive/folders/1WmLRz7iWo2MjtSikgHEig7M0NvK2sOns
ISRM_CA_NC_URL <- "https://drive.google.com/u/0/uc?id=1v__TPKJeiL8Q4zLz8Dyx5HcWSqR2NDx4&export=download&confirm=t"
ISRM_CA_NC_PATH <- here::here("Data", "ca_isrm.ncf")
