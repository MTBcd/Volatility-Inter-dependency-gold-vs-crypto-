# Conditional installation of packages
packages_needed <- c("quantmod", "dplyr", "tidyverse", "rugarch", "xts", "PerformanceAnalytics", 
                     "timetk", "data.table", "XML", "xml2", "rvest", "httr", "readxl", 
                     "timeDate", "FinTS", "vars", "fUnitRoots", "strucchange", "forecast", 
                     "dygraphs", "plyr", "texreg", "fImport", "ARDL")

# Function to check and install packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# apply the function for each package
sapply(packages_needed, install_if_missing)

