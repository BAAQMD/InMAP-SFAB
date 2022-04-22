# ISRM

The big ISRM needs 166gb and the California ISRM needs 28gb of free space. You need a computer with at least 16gb of RAM.

* Download big ISRM: https://zenodo.org/record/2589760#.YgREvS2ZPEa
* Download Cali ISRM: https://drive.google.com/drive/folders/1WmLRz7iWo2MjtSikgHEig7M0NvK2sOns?usp=sharing

Because the ISRM is an extremely large file, it is easier to take slices of the ISRM and work with smaller netCDF4 files that contain the relevant parts of the matrix.

## Slicing the ISRM

Issue a command in the following format to slice the ISRM
```
>> ncks -v varName -d layer,# big_isrm.ncf new_file.nc
```

For example, if you wanted to extract the 0th layer of the ISRM's SO4 values:
```
>> ncks -v pSO4 -d layer,0 big_isrm.ncf pSO4L0.nc
```

Other variables (the other emissions besides SO4) are `pNO3`, `PNH4`, `PrimaryPM25`, and `SOA`

# DATASETS

* `ISRM_boundaries_latlons.csv` - ISRM grid cell numbers with latitudes and longitudes. `Lat0` and `lon0` are lattitudes and longitudes of grid cell center, and `lat1,lat2,lat3,lat4,lon1,lon2,lon3` and `lon4` are latitude and longitudes of the polygons.
* `ca_isrm_gridcells.csv` - ISRM grid cell numbers that are located in California state. Column "ism" stands for the ISRM grid cell numbers.

# CODE

IRSM example run.ipynb - Examples of loading sum slices and doing basic calculations.
You would need to download netcdf, geopandas, pandas packages to run these simple calculations. If you get any errors that mentions "memory", restart the notebook and run only few isrm slices to save your computer RAM.
