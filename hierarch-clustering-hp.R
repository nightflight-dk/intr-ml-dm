source("setup.R")
source("load-data.R")

library(R.matlab)
library(nnet)
library(cvTools)
library(mclust) # install.packages("mclust") # package that can be used to fit a gaussian mixture model. This package does not allow random starts of the algorithm. It is faster than the algorithm in mixtools.

graphics.off()

c(dat, X, names, NAs) := loaddata(normalise = TRUE, removeNAs = TRUE, oneofKenc = TRUE, datadir="data")

y <- dat$powerclass
N <- nrow(cars)
M <- length(X)

attributeNames <- colnames(X)

Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames

############### hierarchical

## Hierarchical clustering
# Maximum number of clusters
Maxclust = 4;

# Compute hierarchical clustering
# Methods <- c("ward","ward.D2","single","complete","average")
hc <- hclust(dist(Xdf), method="ward.D2")

# Compute clustering by thresholding the dendrogram
i <- cutree(hc, k = Maxclust)

## Plot results

# Plot dendrogram
plot(hc)

# Plot data
#dev.new()
clusterplot(Xdf, y, i, main='Hierarchical')

classError(i, y)

