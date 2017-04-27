
#### Format Data ####
# Number of observations
N <- nrow(X)
# Attribute (Feature) Names
attributeNames <- gsub('-', '.', names(X))

# Number of features
M <- ncol(X)