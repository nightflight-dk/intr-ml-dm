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

# Maximum number of clusters
K = 10;

# Allocate variables
Rand = rep(NA, times=K)
Jaccard = rep(NA, times=K)
NMI = rep(NA,times = K)
for(k in 1:K){
  # Run k-means
  kmeansres = kmeans(Xdf, k, iter.max=100);
  i <- kmeansres$cluster
  # Compute cluster validities
  res <- clusterval(y, i);
  
  Rand[k] <- res$Rand
  Jaccard[k] <- res$Jaccard
  NMI[k] <- res$NMI
}

## Plot results
cols <- c('blue', 'green', 'red', 'lightblue')
maxy <- max(c(Rand, Jaccard,NMI))
miny <- min(c(Rand, Jaccard,NMI))
plot(c(1,K), c(miny, maxy), type='n', main='Cluster validity', xlab='Number of clusters', ylab='')
lines(1:K, Rand, col=cols[1]);
lines(1:K, Jaccard, col=cols[2]);
lines(1:K, NMI, col=cols[3]);
legend('bottomright', legend=c('Rand', 'Jaccard','NMI'), fill=cols)
