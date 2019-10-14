# R code
This folder contains the necessary R functions to fit the LCAR, AHC and DBSC models described in Santafé et al. (2019) using a simulated data set over the n=508 municipalities of the Spanish autonomous regions of Navarre and the basque Country.

The [NA_PV_simulatedData.RData](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/R/NA_PV_simulatedData.RData) file contains the following R objects:
- ```Carto.MUN```: SpatialPolygonDataFrame object with the cartography of the 508 municipalities of Navarre and the Basque Country.
- ```Datos```: data.frame object with the IDs of the areas, year, observed cases, expected cases and standardized mortality ratios (SMR) variables.
- ```W```: spatial adjacency matrix of the 508 municipalities.

<br>

A detailed example of how to fit these models and plot the maps with estimated relative risks can be found in [Example_FitModels.R](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/R/Example_FitModels.R).
