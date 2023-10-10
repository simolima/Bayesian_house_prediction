library(MASS)
library(coda)
library(spam)
library(truncdist)
library(CARBayes)
library(boot)
library(deldir)
library(foreign)
library(grid)
library(maptools)
library(Matrix)
library(nlme)
library(shapefiles)
library(sp)
library(spdep)
library(ape)
library(splines)
library(dplyr)
library(cluster)

#We run the Leroux CAR model (where each house in the dataset represent a different areal unit) on R with the package 'CARBayes' that uses the ??? algorithm to create the MCMC
#since the running of the model on Stan was too slowly due to the huge Proximity matrix we were working with (see 'Proximity Matrix Without' R code for creation of the matrix)

train <- read.csv("traintransf.csv", header=TRUE, sep=";")
W <- read.csv("matrixposout.csv", header=TRUE, sep=";")
W <- as.matrix(W)

f <- train$price ~ train$Overall.Qual + train$Garage.Area + train$Total.Bsmt.SF + train$X1st.Flr.SF + train$Full.Bath + train$AgeofHouse + train$Mas.Vnr.Area + train$TotRms.AbvGrd  + train$Fireplaces + train$Wood.Deck.SF
M <- S.CARleroux(formula=f, family="gaussian", data=train, W=W, burnin=1000, n.sample=15000, thin=2)
M

#95% credible intervals for betas
beta.samples.matrix <- M$samples$beta
colnames(beta.samples.matrix) <- c("Intercept", "Overall.Qual", "Garage.Area", "Total.Bsmt.SF", "X1st.Flr.SF", "Full.Bath", "AgeofHouse", "Mas.Vnr.Area", "TotRms.AbvGrd", "Fireplaces", "Wood.Deck.SF")
round(t(rbind(apply(beta.samples.matrix, 2, mean), apply(beta.samples.matrix,2, quantile, c(0.025, 0.975)))), 6)
#No credible interval for beta contains zero 
#--> all the covariates are significant

save(M,file='Leroux.dat')
#load('Leroux.dat')

beta.samples <- mcmc.list(M$samples$beta)
x11()
plot(beta.samples[,1:4])
x11()
plot(beta.samples[,5:8])
x11()
plot(beta.samples[,9:11])
#good mixing and convergence of the chains

#Plot of the posterior distributions for nu2, tau2 and rho
x11()
plot(M$samples$nu2)
x11()
plot(M$samples$tau2)
x11()
plot(M$samples$rho)
#from the traceplot of the spatial autocorrelation parameter rho we see there is no a good mixing of the chain
#random effects have not modeled enough spatial autocorrelation, as the posterior mean for rho is 0.2918

#So we try the Multilevel Leroux model, where each areal unit is represented by each different Neighborhood (25), 
#taking into consideration all the houses for each Neighborhood.
#We wrote and made run our model in Stan! --> see Stan code 'MultilevelLeroux'








