##  ------------------------------------------------------------  ##
# Daubenmire Project - Summarize
##  ------------------------------------------------------------  ##
# Purpose:
## Summarize data within various groups

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

# Load data
sry_v01 <- read.csv(file =  file.path("data", "01_daub-tidy.csv"))

# Check structure
dplyr::glimpse(sry_v01)




# End ----

