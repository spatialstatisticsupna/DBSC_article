# Density-based spatial clustering (DBSC) algorithm
This repository contains the Supplementary Material and R code to fit the models described in the paper entitle [_"Dealing with risk discontinuities to estimate cancer mortality risks when the number of small areas is large"_ (Santafé et al., 2020)](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/UGARTE_AND_COAUTHORS_SMMR.pdf)


## Table of contents

- [Supplementary Material](#Supplementary-Material)
- [R code](#R-code)
- [References](#References)


# Supplementary Material
Figures S1-S9 contains some results of the __simulation study__ described in Santafé et al. (2020).

__Scenario 1__
- [Figure S1](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario1A.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 1A for LCAR, AHC and DBSC models.
- [Figure S2](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario1B.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 1B for LCAR, AHC and DBSC models.
- [Figure S3](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario1C.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 1C for LCAR, AHC and DBSC models.

__Scenario 2__
- [Figure S4](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario2A.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 2A for LCAR, AHC and DBSC models.
- [Figure S5](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario2B.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 2B for LCAR, AHC and DBSC models.
- [Figure S6](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario2C.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 2C for LCAR, AHC and DBSC models.

__Scenario 3__
- [Figure S7](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario3A.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 3A for LCAR, AHC and DBSC models.
- [Figure S8](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario3B.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 3B for LCAR, AHC and DBSC models.
- [Figure S9](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/SimulationStudy_Scenario3C.pdf): True risks and average values of the relative risk posterior median estimates for the simulation study of Scenario 3C for LCAR, AHC and DBSC models.

<br>

Figures S10-S13 contains some results of the __motivating application__ described in Santafé et al. (2020).

- [Figure S10](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/MotivatingApplication_Males_PosteriorRisks.pdf): Maps of posterior median estimates for stomach cancer mortality risks in the municipalities of Spain for male population during the period 2011-2015.
- [Figure S11](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/MotivatingApplication_Males_ExceedenceProbabilities.pdf): Maps of posterior exceedence probabilities ![equation](https://latex.codecogs.com/gif.latex?P%28r_i%20%3E1%20%7C%20%7B%5Cbf%20O%7D%29) of stomach cancer mortality risks in the municipalities of Spain for male population during the period 2011-2015.

- [Figure S12](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/MotivatingApplication_Females_PosteriorRisks.pdf): Maps of posterior median estimates for stomach cancer mortality risks in the municipalities of Spain for female population during the period 2011-2015.
- [Figure S13](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/pdf/MotivatingApplication_Females_ExceedenceProbabilities.pdf): Maps of posterior exceedence probabilities ![equation](https://latex.codecogs.com/gif.latex?P%28r_i%20%3E1%20%7C%20%7B%5Cbf%20O%7D%29) of stomach cancer mortality risks in the municipalities of Spain for female population during the period 2011-2015.


# R code
R code to fit the following spatial disease mapping models
- LCAR model (Leroux et al., 1999)
- AHC model (Adin et al., 2019)
- DBSC model (Santafé et al., 2020)

has been included [here](https://github.com/spatialstatisticsupna/DBSC_article/blob/master/R/).


# References
[Adin, A., Lee, D., Goicoa, T., and Ugarte, M.D. (2019). A two-stage approach to estimate spatial and spatio-temporal disease risks in the presence of local discontinuities and clusters. _Statistical Methods in Medical Research_, __28(9)__, 2595-2613.](https://doi.org/10.1177/0962280218767975)

[Leroux, B.G., Lei, X., and Breslow, N. (1999). Estimation of disease rates in small areas: A new mixed model for spatial dependence. In Halloran, M. and Berry, D. (eds), _Statistical Models in Epidemiology, the Environment, and Clinical Trials_, pp. 179-191. Springer-Verlag: New York.](https://doi.org/10.1007/978-1-4612-1284-3_4)

Santafé, G., Adin, A., Lee, D., and Ugarte, M.D. (2020). Dealing with risk discontinuities to estimate cancer mortality risks when the number of small areas is large. _Submitted on 14 Oct 2019._ 
