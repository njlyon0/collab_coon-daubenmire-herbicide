##  ------------------------------------------------------------  ##
# Checks - Seedmix Thresholds
##  ------------------------------------------------------------  ##
# Purpose:
## Prior to 2017, we only counted % seed-mix plants of FORBS
## Obviously, we actually care about how much of the TOTAL of each quadrat is seed-mix, not of FORBS
## In 2017 we collected both % seed-mix of FORBS and of TOTAL, and we always collect % forbs of total
## This script is aimed at testing how well we can "back-calculate" % seed-mix of total

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

# Load data
smx_v01 <- read.csv(file = file.path("data", "raw", "01_daub-partial-tidy.csv"))

# Check structure
dplyr::glimpse(smx_v01)

##  ------------------------------------------  ##
# Prep the Data ----
##  ------------------------------------------  ##

# Remove unwanted columns and rows
smx_v02 <- smx_v01 %>% 
    dplyr::select(year = Year,
        pasture = Pasture,
        patch = Pasture_Patch,
        forbs_binned.perc,
        dplyr::contains("seedmix")) %>%
    dplyr::select(-seedmix.forbs_prop.above.25.perc.cover) %>% 
    dplyr::filter(year == 2017)

# What does that drop?
supportR::diff_check(old = names(smx_v01), new = names(smx_v02))

# Check structure
dplyr::glimpse(smx_v02)

##  ------------------------------------------  ##
# Test Other Thresholds ----
##  ------------------------------------------  ##

# Calculate the 'prop above X perc cover' for several other thresholds
smx_v03 <- smx_v02 %>% 
    dplyr::mutate(
        calc_seedmix = forbs_binned.perc / seedmix.forbs_binned.perc.of.all.forbs,
        actual_seedmix = seedmix.forbs_binned.perc) %>%     
    dplyr::mutate(seedmix.forbs_binned.perc = dplyr::case_when(
        calc_seedmix == Inf ~ 0, # Division by zero artifact
        calc_seedmix > 0 & calc_seedmix <= 1 ~ 1,
        calc_seedmix > 1 & calc_seedmix <= 5 ~ 3,
        calc_seedmix > 5 & calc_seedmix <= 25 ~ 16,
        calc_seedmix > 25 & calc_seedmix <= 50 ~ 38,
        calc_seedmix > 50 & calc_seedmix <= 75 ~ 63,
        calc_seedmix > 75 & calc_seedmix <= 95 ~ 86,
        calc_seedmix > 95 & calc_seedmix <= 100 ~ 98,
        TRUE ~ NA)) %>% 
  dplyr::mutate(
    calc_prop.above.01.perc.cover = ifelse(calc_seedmix >= 3, yes = 1, no = 0),
    calc_prop.above.05.perc.cover = ifelse(calc_seedmix >= 16, yes = 1, no = 0),
    calc_prop.above.25.perc.cover = ifelse(calc_seedmix >= 38, yes = 1, no = 0),
    calc_prop.above.50.perc.cover = ifelse(calc_seedmix >= 63, yes = 1, no = 0),
    calc_prop.above.75.perc.cover = ifelse(calc_seedmix >= 86, yes = 1, no = 0),
    actual_prop.above.01.perc.cover = ifelse(actual_seedmix >= 3, yes = 1, no = 0),
    actual_prop.above.05.perc.cover = ifelse(actual_seedmix >= 16, yes = 1, no = 0),
    actual_prop.above.25.perc.cover = ifelse(actual_seedmix >= 38, yes = 1, no = 0),
    actual_prop.above.50.perc.cover = ifelse(actual_seedmix >= 63, yes = 1, no = 0),
    actual_prop.above.75.perc.cover = ifelse(actual_seedmix >= 86, yes = 1, no = 0))

# Check structure
dplyr::glimpse(smx_v03)

# End ----
