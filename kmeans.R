# exercise 10.1.1
rm(list=ls())
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
attributeNames <- gsub(' ', '.', attributeNames)
Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames

## K-means clustering

# Number of clusters
K = 3;

# Run k-means
res = kmeans(Xdf, K);
i <- res$cluster
Xc <- res$centers

## Plot results
# Plot data
clusterplot(Xdf, y, i, Xc, main='K-means')
