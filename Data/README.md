# README for Data/

:warning: This directory contains datasets used as _input_ in the "make" process. 

- For scripts that use these as input, see [Make/].
- For output, see [Build/].

## Contents of `Data/BAAQMD/`

The shapefile [CMAQ-LCC-1km-grid/BAAQMD_1km_toxic_164X224.zip][BAAQMD_1km_toxic_164X224.zip] contains an effective definition of the 1 km grid used by the Air District for CMAQ modeling.

See also, in `Build/Geodata/`:

- [CMAQ_LCC.prj], an SRS definition in PROJ format.
- [CMAQ_LCC_1km_grid.geojson], a re-export of the same cell geometries found in the [BAAQMD_1km_toxic_164X224.zip] shapefile.
- [CMAQ_raster_template.tif], a GeoTIFF version of the CMAQ cells.

## Contents of `Data/UW/`

See [Data/UW/README.md](./UW/README.md).

## Contents of `Data/Zenodo/`

`marginal_values/` is the unzipped contents of [marginal_values.zip][Zenodo], published alongside the US ISRM v1.2.1 on Zenodo. For details, see the [Zenodo] page.

## Contents of `Data/Socrata/`

Here you'll find copies of Census 2020 counts, by race/ethnicity, for the SF Bay Area.
These were mirrored from the internal BAAQMD/MTC Socrata portal on 2022-01-22.
The shapefile contains hard-to-recognize Census variables like `P0020005`.
The logic that translates and combines these into abbreviations of more familiar names for racial/ethnic categories, like `AsnPI` (Asian/Pacific Islander), is in the [exptools] R package.

## Large external files

The following large files are _not_ in this GH repo, but are part of the build. 
There's some code in `Make/` that attempts to download them, but it seems really hard to do programmatically, as there's a point where Google asks you to confirm and uses a one-time code in that process. 
The code in `Make/` expects to find these files in a certain location; see `Make/*.R` for details.

- [Big ISRM][Zenodo]
- [CA ISRM][ca_isrm1]

[BAAQMD_1km_toxic_164X224.zip]: ./BAAQMD/CMAQ-LCC-1km-grid/BAAQMD_1km_toxic_164X224.zip
[CMAQ_LCC.prj]: ../Build/Geodata/CMAQ_LCC.prj
[CMAQ_raster_template.tif]: ../Build/Geodata/CMAQ_raster_template.tif
[CMAQ_LCC_1km_grid.geojson]: ../Build/Geodata/CMAQ_LCC_1km_grid.geojson
[Build/]: ../Build/
[Make/]: ../Make/
[References/Handoff/]: https://github.com/BAAQMD/ISRM-SFAB/blob/master/References/Handoff
[Zenodo]: https://zenodo.org/record/2589760#.YgREvS2ZPEa
[ca_isrm1]: https://drive.google.com/drive/folders/1WmLRz7iWo2MjtSikgHEig7M0NvK2sOns?usp=sharing
[ca_isrm2]: https://drive.google.com/drive/folders/1jO5saBcQW1-qmiv-pjsICNiulud9SCU5
[Bay Area ISRM/]: https://www.dropbox.com/sh/0bwdu3vnfsmrrzg/AAA87bGHmcv5Fr3GOTWXczZva?dl=0
[exptools]: https://github.com/BAAQMD/exptools
