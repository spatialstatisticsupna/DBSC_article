# Density-based spatial clustering (DBSC) algorithm
This repository contains the Supplementary Material and R code to fit the models described in the paper entitle _"Dealing with risk discontinuities to estimate cancer mortality risks when the number of small areas is large"_ (Santafé et al., 2019)

## Table of contents

- [Supplementary Material](#SupplementaryMaterial)
- [R code](#Rcode)
- [References](#References)


# Supplementary Material
This supplementary material contains some results of the simulation study described in Santafé et al. (2019).

- [Figure S1](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario1A.pdf): True risks and average values of the relative risks ![equation](https://latex.codecogs.com/gif.latex?%24r_i%24) posterior median estimates for the simulation study of Scenario 1A for LCAR, AHC and DBSC (with ![equation](https://latex.codecogs.com/gif.latex?%24%5Cell%3D1%2C2%2C3%24)) models. 


# R code
R code to fit the following spatial disease mapping models:
- LCAR model (Leroux et al., 1999)
- AHC model (Adin et al., 2019)
- DBSC model (Santafé et al., 2019)


# References
[Adin, A., Lee, D., Goicoa, T., and Ugarte, M.D. (2019). A two-stage approach to estimate spatial and spatio-temporal disease risks in the presence of local discontinuities and clusters. _Statistical Methods in Medical Research_, __28(9)__, 2595-2613.](https://doi.org/10.1177/0962280218767975)

[Leroux, B.G., Lei, X., and Breslow, N. (1999). Estimation of disease rates in small areas: A new mixed model for spatial dependence. In Halloran, M. and Berry, D. (eds), _Statistical Models in Epidemiology, the Environment, and Clinical Trials_, pp. 179-191. Springer-Verlag: New York.](https://doi.org/10.1007/978-1-4612-1284-3_4)

Santafé, G., Adin, A., Lee, D., and Ugarte, M.D. (2019). Dealing with risk discontinuities to estimate cancer mortality risks when the number of small areas is large. _Submitted on 14 Oct 2019._ 
