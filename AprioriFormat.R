AprioriSetFormat <- function(string) {
  library(stringr)
  
  sets = unlist(lapply(strsplit(string, split = '[', fixed = TRUE), "[", 1))
  
  sups = paste0('[', unlist(lapply(strsplit(string, split = '[', fixed = TRUE), "[", 2)))
  sups = as.numeric(str_extract(sups, "[0-9]+"))
  
  sets <- str_split_fixed(sets, ' ', n = Inf)
  sets <- matrix(names(X)[as.numeric(sets)], nrow = nrow(sets), ncol = ncol(sets))
  sets <- apply(sets, 1, function(X) paste(na.omit(X), collapse = ' + '))
  
  Itemsets <- data.frame( 1:length(sups), sets, paste(sups, " %"))
  names(Itemsets) <- c("Set #", "Attributes", "Support")
  return(Itemsets)
}

AprioriAssocFormat <- function(string) {
  library(stringr)
  sups = paste0('[', unlist(lapply(strsplit(string, split = '[', fixed = TRUE), "[", 2)))
  
  confs <- unlist(lapply(str_extract_all(sups, "[0-9]+"), "[", 2))
  sups  <- unlist(lapply(str_extract_all(sups, "[0-9]+"), "[", 1))
  
  sets <- unlist(lapply(strsplit(string, split = '[', fixed = TRUE), "[", 1))
  sets <- str_replace_all(sets, "[0-9]+", function(index) names(X)[as.numeric(index)])
  sets <- str_replace(sets, "<- $", "<- $\\\\varnothing$")
  
  AssocSets <- data.frame( 1:length(sups), sets, paste(sups, " %"), paste(confs, " %"))
  names(AssocSets) <- c("Rule #", "Attributes", "Support", "Confidence")
  return(AssocSets)
}
