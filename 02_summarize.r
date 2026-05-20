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

##  ------------------------------------------  ##
# Prep the Data ----
##  ------------------------------------------  ##

# Reshape the data into long format
sry_v02 <- sry_v01 %>% 
  tidyr::pivot_longer(cols = -year:-patch,
    names_to = "variable", values_to = "value")

# Check structure
dplyr::glimpse(sry_v02)

##  ------------------------------------------  ##
# Summarize the Data ----
##  ------------------------------------------  ##

# Calculate the summary of all variables
sry_v03 <- supportR::summary_table(data = sry_v02,
  groups = setdiff(names(sry_v02), "value"),
  response = "value", drop_na = TRUE)

# Check structure
dplyr::glimpse(sry_v03)

# End ----

