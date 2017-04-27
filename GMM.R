# exercise 11.1.1
rm(list=ls())
source("setup.R")
#library(mixtools) # install.packages("mixtools") # package that can be used to fit a gaussian mixture model. This package does allow random starts of the algorithm.
library(mclust) # install.packages("mclust") # package that can be used to fit a gaussian mixture model. This package does not allow random starts of the algorithm. It is faster than the algorithm in mixtools.

# Load data
library(R.matlab)
dat <- readMat(file.path('Data', 'synth1.mat'))
X <- dat$X
N <- dat$N
attributeNames <- as.vector(unlist(dat$attributeNames))
M <- dat$M
y <- dat$y
C <- dat$C
classNames <- as.vector(unlist(dat$classNames))

# substitute spaces with dots to make handling of columns in data matrix easier
attributeNames <- gsub(' ', '.', attributeNames)
Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames

## Gaussian mixture model

# Number of clusters
K = 4; #minimum value is 2

# Fit model
model <- Mclust(data=Xdf, G=K) # using the mclust package
# model <- mvnormalmixEM(x=Xdf, k=K, maxit=100, epsilon=1e-2, verb=TRUE) # using the mixtools package. Defaults for maxit and epsilon are 500 and 1e-8, respectively. Avoid extreme running times by allowing fewer iterations and deeming convergence earlier by setting maxit and epsilon as done here. The argument verb=TRUE makes the method write output from the EM algorithm at each iteration. The argument verb is FALSE by default.

# Get clustering
i = model$classification # using the mclust package
#i = max_idx(model$posterior) # using the mixtools package

# Get cluster centers
Xc = t(model$parameters$mean) # using the mclust package
#Xc = matrix(unlist(model$mu), nrow=length(model$mu), byrow=TRUE) # using the mixtools package

## Plot results
# Plot clustering
clusterplot(Xdf, y, i, Xc, main='GMM: Clustering');
