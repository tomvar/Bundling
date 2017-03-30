# Initiation of list of required packages
requiredPackages = c("foreach", "doParallel") # List of required packages
for(i in requiredPackages){
  if(!require(i,character.only = TRUE)) install.packages(i)
  library(i,character.only = TRUE) }
