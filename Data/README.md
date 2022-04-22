# README for Data/

This directory contains datasets used as input. For output, see [Build/].

## Contents of `Data/Zenodo/`

`marginal_values/` is the unzipped contents of [marginal_values.zip][Zenodo], published alongside the full ISRM v1.2.1 on Zenodo. For details, see the [Zenodo] page.

## Contents of `Data/UW/`

**`2022-03-07/`** contents were supplied by UW team via Dropbox folder [Bay Area ISRM/].
They were subsequently mirrored to this folder.

**`2022-02-10/`** contents were supplied by UW team to Phil Martien via email with link to Dropbox folder:

- Data/ISRM/ca_isrm_gridcells.csv
- Data/ISRM/isrm_boundaries_latlons.csv

For additional documentation, see `[References/Handoff/]`. 

:warning: **Large external files.** 
The following large files are _not_ in this GH repo, but are part of the build. 
There's some code in `Make/` that attempts to download them, but it seems really hard to do programmatically, as there's a point where Google asks you to confirm and uses a one-time code in that process. 
The code in `Make/` expects to find these files in a certain location; see `Make/*.R` for details.

- [Big ISRM][Zenodo]
- [CA ISRM]

## Contents of `Data/Socrata/`

Here you'll find copies of Census 2020 counts, by race/ethnicity, for the SF Bay Area.
These were mirrored from the internal BAAQMD/MTC Socrata portal on 2022-01-22.
The shapefile contains hard-to-recognize Census variables like `P0020005`.
The logic that translates and combines these into abbreviations of more familiar names for racial/ethnic categories, like `AsnPI` (Asian/Pacific Islander), is in the [exptools] R package.

[Build/]: https://github.com/BAAQMD/ISRM-SFAB/blob/master/Build/
[References/Handoff/]: https://github.com/BAAQMD/ISRM-SFAB/blob/master/References/Handoff
[Zenodo]: https://zenodo.org/record/2589760#.YgREvS2ZPEa
[CA ISRM]: https://drive.google.com/drive/folders/1WmLRz7iWo2MjtSikgHEig7M0NvK2sOns?usp=sharing
[Bay Area ISRM/]: https://www.dropbox.com/sh/0bwdu3vnfsmrrzg/AAA87bGHmcv5Fr3GOTWXczZva?dl=0
[exptools]: https://github.com/BAAQMD/exptools
[marginal_values.zip]
