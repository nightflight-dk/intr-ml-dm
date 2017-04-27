# exercise 10.2.1
rm(list=ls())
graphics.off()
source("setup.R")

# Load data
dat <- read.csv(file.path('Data', 'iris.csv'))
X <- dat[1:4]
N <- nrow(X)
attributeNames <- names(dat)[1:4]
M <- ncol(X)
y <- dat[[5]]
C <- length(unique(y))
classNames <- as.vector(unique(y))

#X <- standardize(X)

# substitute spaces with dots to make handling of columns in data matrix easier
Xdf <- data.frame(X)

## Hierarchical clustering
# Maximum number of clusters
Maxclust = 3;

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

