#### helper for HP clusters
powerclass <- function(hp){
  if (is.na(hp))
    return(4)
  if (hp < 60) 
    return(0)
  if (hp < 120)
    return(1)
  if (hp < 160)
    return(2)
  return(3)
}

loaddata <- function(normalise = FALSE, removeNAs = FALSE, oneofKenc = FALSE, binarize = FALSE, mycols = NA, datadir = "../Project") {
  #### Read data and attribute names ####
  # source('setup.R')
  names <- readLines(paste0(datadir, "/names.txt"))
  dat <- read.csv(paste0(datadir, "/imports-85.data"), na.strings = "?", check.names = FALSE, header = FALSE, col.names = names)
  
  if(!all(is.na(mycols))) {
    dat <- dat[mycols]
    names <- names(dat)
  }
  
  X <- dat
  ## Calculate number of NAs in each column
  NAs <- colSums(is.na(X))
  # Find columns containing numeric data
  numericcols <- !sapply(X, is.factor)
  # Calculate average values
  means <- numeric(0); means[numericcols] <- colMeans(X[numericcols], na.rm = TRUE)
  
  ## Convert cylinders and doors to numbers
  nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve")
  if(all(is.na(mycols)) || any(mycols == "num-of-cylinders")) {
    X[["num-of-cylinders"]] <- match(X[["num-of-cylinders"]], nums)
  }
  if(all(is.na(mycols)) || any(mycols == "num-of-doors")) {
    X[["num-of-doors"]] <- match(X[["num-of-doors"]], nums)
  }
  
  #classify horsepower
  powerclass <- lapply(dat$horsepower, powerclass)
  
  ## Normalise with standardize(X)
  if(normalise) {
    # Substract mean and divide by standard deviation column-wise. Return list
    l <- lapply(X, function(x) {if(is.factor(x)){ x } else { (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE) }})
    # Convert returned list to data frame
    X <- do.call(rbind.data.frame, args = list(l, "make.row.names" = FALSE))
    colnames(X)<-names
  }
  
  if(removeNAs && !normalise) {
    for (i in which(as.logical(NAs))) {
      # If not numeric, set NAs as the mode of the vector
      X[is.na(X[i]), i] <- ifelse(numericcols[i], mean(X[[i]], na.rm = TRUE), names(which.max(table(X[[i]]))))
    }
  } else if(removeNAs) {
    # If the data is already standardised, the mean is just 0
    for (i in which(as.logical(NAs))) {
      # If not numeric, set NAs as the mode of the vector
      X[is.na(X[i]), i] <- ifelse(numericcols[i], 0, names(which.max(table(X[[i]]))))
    }
  }
  if(all(is.na(mycols)) || any(mycols == "num-of-doors")) {
    X[["num-of-doors"]] <- as.integer(X[["num-of-doors"]])
  }
  if(binarize) {
    numericcols <- !sapply(X, is.factor)
    for(col in names(numericcols)[numericcols == TRUE]) {
      med  <- median(X[[col]])
      
      low  <- numeric(nrow(X))
      low[X[[col]] < med]  <- 1
      high <- numeric(nrow(X))
      high[X[[col]] >= med]  <- 1
      
      lowname  <- paste0(col, ".low")
      highname <- paste0(col, ".high")
      X[[lowname]]  <- low
      X[[highname]] <- high
      
      X[[col]] <- NULL
    }
  }
  
  #### Convert to One-out-of-K encodings ####
  if(oneofKenc) {
    atts <- c("make", "fuel-type", "aspiration", "body-style", "drive-wheels", "engine-location", "engine-type", "fuel-system")
    for (att in atts) {
      if(!any(names(X) == att)) {
        #warning("'", att, "' not found. Skipping attribute...") 
      } else {
        facs <- names(table(X[[att]]))
        if(binarize) {
          k = 1
        } else {
          k = 1/sqrt(length(facs))  # Normalise to number of columns
        }
        for (f in facs) {
          newatt <- paste(att, f, sep = ".")
          X[[newatt]] <- as.integer(X[[att]] == f) * k
        }
        X[[att]] <- NULL
      }
    }
  }
  
  # add the powerclass column
  dat$powerclass=unlist(powerclass)
  
  list(dat, X, names, NAs)
}
':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}