# InMAP-SFAB

InMAP analyses scoped to SF air basin.

## Project / Task Management

- We can use GH [issues] liberally.
- There's a modest [project board][project]. We can use this, if it's helpful. Maybe as the project grows, it'll be more helpful.

## Notable Files and Folders

As of 2022-04-21:

- "Demo" inputs (from the UW team) and outputs are in [Data/UW/] and [Build/Demo/], respectively.
- GIS definitions are exported to [Build/Geodata/].
- R utility functions, written by @dholstius, are in [R/].
- A series of R scripts to "make" intermediate datasets is in [Make/].
    - Stuff is written to `Build/RData/` in R format.
    - Stuff is written to other subdirectories of `Build/` in other formats, like GeoJSON, CSV, HTML, or what have you. 

[issues]: https://github.com/BAAQMD/InMAP-SFAB/issues
[project]: https://github.com/orgs/BAAQMD/projects/7/views/4
[Data/UW/]: ../../tree/master/Data/UW
[Build/Demo/]: ../../tree/master/Build/Demo
[Build/Geodata/]: ../../tree/master/Build/Geodata
[R/]: ../../tree/master/R
[Make/]: ../../tree/master/Make
