
#To set the working directory use Source buttom
cleandir = TRUE

if (cleandir) {
  rm(list = ls())
}


# Function that installs packages for necessary libraries
pkgload = function(libs){
  isinstalled = (libs) %in% installed.packages()
  if (sum(isinstalled) < length(libs)){
    #If a package is not found, then install it
    install.packages( libs[!isinstalled])
  }
  #Load libraries
  temp = sapply( libs, function(x)  library(x, character.only = TRUE) )
}


# List of libraries 
libs = c("tidyverse", "usdarnass", "readxl", "lubridate", "zoo", "tidyr", "fixest",
         "nloptr", "kableExtra", "gridExtra", "grid", "writexl", "usdampr", "qpdf",
         "knitr", "openxlsx", "dplyr", "officer", "lpSolve")

pkgload(libs)


#Get the Working directory
home = getwd()

if (is.null(home)) {
  warning( "Unable to set the working directory" )   
}else{
  #Success
  message("The working directory has been set: \n", getwd())
}

options(warn=-1)
