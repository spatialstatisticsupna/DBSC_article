rm(list=ls())
library(INLA)
library(maptools)
library(spdep)
library(RColorBrewer)
library(tmap)


setwd("./")
fileIn <- "./NA_PV_simulatedData.RData"
fileOut <- "./results_NA_PV.RData"

## load data
load(fileIn)


results <- vector("list",5)
model.names <- c("LCAR",
                 "AHC",
                 "DBSCl1",
                 "DBSCl2.nb",
                 "DBSCl4")
names(results) <- model.names


# Number of areas
n <- ncol(W)


source("../Clustering_functions/Model_INLA.R")
source("../Clustering_functions/AHC_algorithm.R")
source("../Clustering_functions/DBSC_algorithm.R")
source("../Clustering_functions/ApproximatedDIC.R")


Qs <- diag(apply(W,2,sum))-W

################
## LCAR model ##
################
R.Leroux <- diag(n)-Qs

sdunif="expression:
      logdens=-log_precision/2;
      return(logdens)"

lunif = "expression:
      a = 1;
      b = 1;
      beta = exp(theta)/(1+exp(theta));
      logdens = lgamma(a+b)-lgamma(a)-lgamma(b)+(a-1)*log(beta)+(b-1)*log(1-beta);
      log_jacobian = log(beta*(1-beta));
      return(logdens+log_jacobian)"


data.INLA <- data.frame(Y.real=Datos[Datos$year==4,"obs"],
                        E.real=Datos[Datos$year==4,"exp"],
                        region=1:n)

formula <- Y.real ~ f(region, model="generic1", Cmatrix = R.Leroux, constr=TRUE,
                      hyper=list(prec=list(prior=sdunif),beta=list(prior=lunif, initial=0)))

model <- inla(formula, family="poisson", data=data.INLA, E=E.real,
               control.compute=list(dic=TRUE, mlik=TRUE, cpo=TRUE, waic=TRUE, config=TRUE),
               control.predictor=list(compute=TRUE, cdf=c(log(1))),
               control.inla=list(strategy="simplified.laplace"))

results$LCAR <- model


###############
## AHC model ##
###############
set.seed(20191014)

# prepare input data for AHC clustering
SMR.prior <- cbind(log(Datos[Datos$year==1,"SMR"]+0.0001),
                   log(Datos[Datos$year==2,"SMR"]+0.0001),
                   log(Datos[Datos$year==3,"SMR"]+0.0001))
cluster.prior <- clustering.function(SMR.prior, W)
cluster.partition <- cluster.prior$cluster.store[seq(dim(W)[1],2),]

## Model fitting ##
cluster.model <- model.selection(cluster.partition=cluster.partition, strategy="simplified.laplace",
                                 final.cluster=85, plot.dic=FALSE, # Remove this line to fit a model for each cluster configuration candidate
                                 Y.real=Datos[Datos$year==4,"obs"],
                                 E.real=Datos[Datos$year==4,"exp"],
                                 C=Qs, Carto=Carto.MUN)

results$AHC <- cluster.model$model.final


################
## DBSC model ##
################
# prepare input data for DBSC clustering 
multivariate.data <- matrix(log(Datos[,"SMR"]+0.0001),nrow=n)
colnames(multivariate.data) <- unique(Datos$year)
clustering.inputdata <- as.data.frame(multivariate.data)
backgroundData <- rep(log(0.0001),ncol(multivariate.data))


##############################################
## Example: DBSC-l1 (with background cluster)
##############################################
# clustering method. The parameters used are:
#   x: input data for the clustering algorithm
#   W: adjacencyMatrix of the areas
#   l: neighborhood level as defined in the paper
#   backgroundData: The centroid of the background cluster (a vector with the same length as the objects in x and also measured in the same scale).
partition <- getClusterPartition(x=clustering.inputdata, W=W, l=1, backgroundData=backgroundData)

## Model fitting ##
cluster.model <- model.selection(cluster.partition=matrix(partition,nrow=1),
                                 strategy="simplified.laplace",
                                 Y.real=Datos[Datos$year==4,"obs"],
                                 E.real=Datos[Datos$year==4,"exp"],
                                 C=Qs, Carto=Carto.MUN)

results$DBSCl1 <- cluster.model$model.final


#################################################
## Example: DBSC-l2 (without background cluster)
#################################################
# clustering method: The use of no background cluster can be obtained by omitting the "backgroundData" parameter 
# in the function getClusteringPartition or by using "backgroundData=NULL"
a <- 2
partition <- getClusterPartition(x=clustering.inputdata, W=W, l=2)

## Model fitting ##
cluster.model <- model.selection(cluster.partition=matrix(partition,nrow=1),
                                 strategy="simplified.laplace",
                                 Y.real=Datos[Datos$year==4,"obs"],
                                 E.real=Datos[Datos$year==4,"exp"],
                                 C=Qs, Carto=Carto.MUN)

results$DBSCl2.nb <- cluster.model$model.final


# For high neighborhood levels, the calculation of the adjacency matrix for the neighborhood can be computationally expensive. 
# However, as this matrix only depends on the geographical arrangement of the areas and not on the specific observed data, a 
# pre-computation of these matrices is highly recommended. Additionally, these matrices can be saved for future uses.

# For a given neighborhood level (for example l=4), its adjacency matrix can be computed as
adjacencyMatrix.neighborhoodLevel <- calculateAdjacencyMatrix.neighborhoodLevel(adjacencyMatrix=W, neighborhoodLevel=4)
# where "adjacencyMatrix" is the area-level adjacency matrix and "l" is the desired neighborhood-level 


# On the other hand, if several consecutive neighborhood levels are going to be taken into consideration (for example l=1,...,5), 
# neighborhood-level adjacency matrices can be incrementally obtained in a more efficient way.
adjacencyMatrix.neighborhoodLevel = list(W)
for(l in 2:5){
  adjacencyMatrix.neighborhoodLevel[[l]] <- calculateAdjacencyMatrix.neighborhoodLevel(adjacencyMatrix.neighborhoodLevel[[l-1]])
}


# Once neighborhood-level adjacency matrix have been calculated, to obtain, for instance, DBSCl4 model with no background cluster, we can use:
partition <- getClusterPartition(x=clustering.inputdata, W=W, 
                                 adjacencyMatrix.neighborhoodLevel=adjacencyMatrix.neighborhoodLevel[[4]])

# Note that, if "adjacencyMatrix.neighborhoodLevel" parameter is provided, parameter "l" can be ignored.


## Model fitting ##
cluster.model <- model.selection(cluster.partition=matrix(partition,nrow=1),
                                 strategy="simplified.laplace",
                                 Y.real=Datos[Datos$year==4,"obs"],
                                 E.real=Datos[Datos$year==4,"exp"],
                                 C=Qs, Carto=Carto.MUN)

results$DBSCl4 <- cluster.model$model.final


# save models
save(results, file=fileOut)



########################################################
##################        RESULTS     ##################
########################################################

############################
## Model fitting criteria ##
############################
# In order to calculate some model fitting criteria such as DIC, WAIC or LS we can use:

pD <- unlist(lapply(results, function(x) x$dic$p.eff))
DIC <- unlist(lapply(results, function(x) x$dic$dic))
WAIC <- unlist(lapply(results, function(x) x$waic$waic))
LS <- unlist(lapply(results, function(x) -sum(log(x$cpo$cpo))))
print(cbind(pD,DIC,WAIC,LS))


##########################################
## Risk maps (posterior mean estimates) ##
##########################################
# To print the risk maps and the maps with the posterior exceedence probabilities - P(r_i>1 | O)- we can use :

mean.risk <- lapply(results, function(x) x$summary.fitted.values$'0.5quant')
mean.risk$true.risk <- exp(risk)
mean.risk$muni <- Carto.MUN$muni
mean.risk$SMR <- Datos[Datos$year==4,"SMR"]

Carto <- merge(Carto.MUN,mean.risk)
paleta <- brewer.pal(8,"RdYlGn")[8:1]
values <- c(-Inf,0.67,0.77,0.91,1,1.10,1.30,1.50,Inf)
 
pdf("results.pdf")
tmap_mode("plot")
Map <- tm_shape(Carto) + 
   tm_polygons(col=c("true.risk","SMR",model.names),
               palette=paleta, title="", legend.show=T, border.alpha=0,
               legend.reverse=T, style="fixed", breaks=values, interval.closure="left",
               labels=c("[0-0.67)","[0.67-0.77)","[0.77-0.91)","[0.91-1)",
                        "[1-1.10)","[1.10-1.30)","[1.30-1.50)","[1.50-Inf)")) +
   tm_grid(n.x=2, n.y=2, alpha=0.2, labels.format=list(scientific=T), labels.inside.frame=F, labels.col="white") +
   tm_layout(main.title="Mean Risks", main.title.position=0.2,
             panel.labels=c("true.risk","SMR",model.names),
             legend.outside=T, legend.outside.position="right", legend.frame=F, legend.outside.size=0.2,
             outer.margins=c(0.02,0.01,0.02,0.01)) + 
tm_facets(ncol=4, nrow=2)
print(Map)


###########################################
## Posterior exceedence probability maps ##
###########################################
probs <- lapply(results, function(x) x$summary.fitted.values$`1 cdf`)
probs$muni <- Carto.MUN$muni

Carto <- merge(Carto.MUN,probs)
paleta <- brewer.pal(8,"RdYlGn")[8:1]
values <- c(-Inf,0.67,0.77,0.91,1,1.10,1.30,1.50,Inf)

Map <- tm_shape(Carto) + 
  tm_polygons(col=model.names,
              palette=paleta, title="", legend.show=T, border.alpha=0,
              legend.reverse=T, style="fixed", breaks=values, interval.closure="left",
              labels=c("[0-0.67)","[0.67-0.77)","[0.77-0.91)","[0.91-1)",
                       "[1-1.10)","[1.10-1.30)","[1.30-1.50)","[1.50-Inf)")) +
  tm_grid(n.x=2, n.y=2, alpha=0.2, labels.format=list(scientific=T), labels.inside.frame=F, labels.col="white") +
  tm_layout(main.title="Mean Risks", main.title.position=0.2,
            panel.labels=model.names,
            legend.outside=T, legend.outside.position="right", legend.frame=F, legend.outside.size=0.2,
            outer.margins=c(0.02,0.01,0.02,0.01)) + 
  tm_facets(ncol=3, nrow=2)
print(Map)
dev.off()

