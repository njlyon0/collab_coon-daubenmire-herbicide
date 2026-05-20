##  ------------------------------------------------------------  ##
# Daubenmire Project - Data Download
##  ------------------------------------------------------------  ##
# Purpose:
## Download raw data from Google Drive

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Make Folders ----
##  ------------------------------------------  ##

# List files in relevant Drive folder
(drive_raw <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1PBA3pYOvpK1CSWS0FUxzBVooU3i1ZeJS")) %>% 
    dplyr::filter(name == "daubenmire-project_raw-data.csv"))

# Download that file
googledrive::drive_download(file = drive_raw$id, overwrite = TRUE,
    path = file.path("data", "raw", drive_raw$name))

# End ----
