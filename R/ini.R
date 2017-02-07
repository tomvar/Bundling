# Initiation of list of required packages
requiredPackages = c("foreach", "doParallel") # List of required packages
for(i in requiredPackages){
  if(!require(i,character.only = TRUE)) install.packages(i)
  library(i,character.only = TRUE) }

# za³odowanie potrzebnych pakietów
#
#  require(foreach)
#  require(doParallel)
#
#
#  if(require("foreach")){
#    print("foreach is loaded correctly")
#  } else {
#    print("trying to install foreach")
#    install.packages("foreach")
#    if(require(foreach)){
#      print("foreach installed and loaded")
#    } else {
#      stop("could not install foreach")
#    }
#  }
#
#  if(require("doParallel")){
#    print("doParallel is loaded correctly")
#  } else {
#    print("trying to install doParallel")
#    install.packages("doParallel")
#    if(require(doParallel)){
#      print("doParallel installed and loaded")
#    } else {
#      stop("could not install doParallel")
#    }
#  }
