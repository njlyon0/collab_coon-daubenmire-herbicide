##  ------------------------------------------------------------  ##
# Daubenmire Project - Filter
##  ------------------------------------------------------------  ##
# Purpose:
## Subset to only data relevant to this manuscript

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

# Load data
sub_v01 <- read.csv(file =  file.path("data", "01_daub-tidy.csv"))

# Check structure
dplyr::glimpse(sub_v01)

##  ------------------------------------------  ##
# Site Subset ----
##  ------------------------------------------  ##

# Subset to only desired sites
sub_v02 <- sub_v01 %>% 
    dplyr::filter(pasture %in% c("BSH", "DUN", "GIL", "LTR", "PYW", "RC2", "STE"))

# What sites are lost?
supportR::diff_check(old = unique(sub_v01$pasture), new = unique(sub_v02$pasture))

# What sites are left?
sort(unique(sub_v02$pasture))

# How many rows are lost?
message(nrow(sub_v01) - nrow(sub_v02), " rows (", 100 - floor(nrow(sub_v02)/nrow(sub_v01) * 100), "% of total) lost.")

##  ------------------------------------------  ##
# Treatment Subset ----
##  ------------------------------------------  ##

# Subset to only desired sites
sub_v03 <- sub_v02 %>% 
    dplyr::filter(treatment_fescue %in% c("Con", "Spr", "SnS"))

# What sites are lost?
supportR::diff_check(old = unique(sub_v02$treatment_fescue), new = unique(sub_v03$treatment_fescue))

# What sites are left?
sort(unique(sub_v03$treatment_fescue))

# How many rows are lost?
message(nrow(sub_v02) - nrow(sub_v03), " rows (", 100 - floor(nrow(sub_v03)/nrow(sub_v02) * 100), "% of total) lost.")

##  ------------------------------------------  ## 
# Export ----
##  ------------------------------------------  ## 
# Make one final object
sub_v99 <- sub_v03

# Check structure
dplyr::glimpse(sub_v99)

# Export
write.csv(x = sub_v99, row.names = FALSE, na = '',
    file = file.path("data", "02_daub-filtered.csv"))

# End ----
