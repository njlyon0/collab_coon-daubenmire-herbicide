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
sry_v01 <- read.csv(file =  file.path("data", "02_daub-filtered.csv"))

# Check structure
dplyr::glimpse(sry_v01)

##  ------------------------------------------  ##
# Prep the Data ----
##  ------------------------------------------  ##

# Remove unwanted columns and reshape to long format
sry_v02 <- sry_v01 %>% 
  dplyr::select(-pasture, -patch, -burn_cohort) %>% 
  tidyr::pivot_longer(cols = -year:-time.since.herbicide_years,
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

##  ------------------------------------------  ## 
# Export ----
##  ------------------------------------------  ## 
# Make one final object
sry_v99 <- sry_v03

# Check structure
dplyr::glimpse(sry_v99)

# Export
write.csv(x = sry_v99, row.names = FALSE, na = '',
    file = file.path("data", "03_daub-summarized.csv"))

# End ----

