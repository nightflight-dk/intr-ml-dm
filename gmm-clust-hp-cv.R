source("setup.R")

library(R.matlab)
library(nnet)
library(cvTools)
library(mclust) # install.packages("mclust") # package that can be used to fit a gaussian mixture model. This package does not allow random starts of the algorithm. It is faster than the algorithm in mixtools.

graphics.off()

c(dat, X, names, NAs) := loaddata(normalise = TRUE, removeNAs = TRUE, oneofKenc = TRUE)

cars <- data.frame(X)

X <- cars
y <- cars$powerclass
X <- cars[,!(names(cars) == "powerclass")]
N <- nrow(cars)
M <- length(X)

attributeNames <- colnames(X)

Xdf <- data.frame(X)
colnames(Xdf) <- attributeNames

## Gaussian mixture model

# Range of K's to try
KRange = 3:12;
T = length(KRange);

# Allocate variables
BIC = rep(NA, times=T)
AIC = rep(NA, times=T)
CVE = rep(0, times=T)

# Create crossvalidation partition for evaluation
set.seed(1234) # for reproducibility
NumTestSets <- 10
CV <- cvFolds(N, K=NumTestSets)
CV$NumTestSets <- NumTestSets
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# For each model order (number of clusters)
for(t in 1:T){
  # Get the current K
  K = KRange[t];
  
  # Display information
  print(paste('Fitting model for K =', K));
  
  model <- Mclust(data=X, G=K) # if using the package mclust to fit the model
  if(FALSE){ # this block of code, within the "if" conditional, is only relevant if random restarts are allowed, as is the case for the package mixtools but not for the package mclust
    # Fit model
    reps <- 10
    models = vector('list', reps)
    logliks <- rep(NA, times=reps)
    startsigma <- replicate(K, diag(apply(X, 2, sd)), simplify=FALSE)
    for(irep in 1:reps){          
      err <- TRUE
      while(err){
        randobs <- sample(x=1:N, size=K)
        randmu <- lapply(X=randobs, function(X, dat) dat[X,], dat=X)
        emres <- try(mvnormalmixEM(X, maxit=50, , eps=1e-2, mu=randmu, sigma=startsigma), silent=TRUE)
        if(class(emres)=="mixEM"){
          err <- FALSE
        }
      }
      model <- emres
      models[[irep]] <- model
      logliks[irep] <- model$loglik
    }
    whichmaxloglik <- which.max(logliks)
    model <- models[[whichmaxloglik]]
  } # end of block only relevant for the package mixtools
  
  
  # Get BIC and AIC
  BIC[t] = BIC(model);
  AIC[t] = AIC(model);
  
  # For each crossvalidation fold
  for(k in 1:CV$NumTestSets){
    # Extract the training and test set
    X_train <- X[CV$which!=k, ];
    X_test <- X[CV$which==k, ];
    
    # Fit model to training set
    model <- Mclust(data=X_train, G=K) # if using the package mclust to fit the model
    if(FALSE){ # this block of code, within the "if" conditional, is only relevant if random restarts are allowed, as is the case for the package mixtools but not for the package mclust
      reps <- 5
      models = vector('list', reps)
      logliks <- rep(NA, times=reps)
      startsigma <- replicate(K, diag(apply(X_train, 2, sd)), simplify=FALSE)
      for(irep in 1:reps){
        err <- FALSE
        while(!err){
          randobs <- sample(x=1:dim(X_train)[1], size=K)
          randmu <- lapply(X=randobs, function(X, dat) dat[X,], dat=X_train)
          emres <- try(mvnormalmixEM(X_train, maxit=50, , eps=1e-2, mu=randmu, sigma=startsigma), silent=TRUE)
          if(class(emres)!="mixEM"){
            err <- TRUE
          }
        }
        model <- emres
        models[[irep]] <- model
        logliks[irep] <- model$loglik
      }
      whichmaxloglik <- which.max(logliks)
      model <- models[[whichmaxloglik]]
    }
    
    # Evaluation crossvalidation error
    res <- gmmposterior(model, X_test);
    NLOGL <- res$ll
    CVE[t] = CVE[t]+NLOGL;
  }
}

## Plot results
pdf("../GMM-crossval.pdf",width = 6, height = 4.5)
cols <- c('blue', 'darkgreen', 'red')
miny <- min(c(BIC, AIC, 2*CVE))
maxy <- max(c(BIC, AIC, 2*CVE))
plot(c(KRange[1], KRange[length(KRange)]), c(miny, maxy),
     ylab='Log-likelihood', main='GMM: Number of clusters', xlab='K', type='n')
points(KRange, BIC, col=cols[1], pch=16)
points(KRange, AIC, col=cols[2], pch=16)
points(KRange, 2*CVE, col=cols[3], pch=16)
legend('topleft', legend=c('BIC', 'AIC', 'Crossvalidation'), fill=cols, bg = 'white')
dev.off()
