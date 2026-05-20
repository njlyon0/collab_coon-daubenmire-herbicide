##  ------------------------------------------------------------  ##
# Daubenmire Project - Setup
##  ------------------------------------------------------------  ##
# Purpose:
## Do generally-useful setup tasks

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment
rm(list = ls()); gc()

##  ------------------------------------------  ##
# Make Folders ----
##  ------------------------------------------  ##

# Create needed folder(s)
dir.create(path = file.path("data", "raw"),
    showWarnings = FALSE, recursive = TRUE)
dir.create(path = file.path("graphs"), showWarnings = FALSE)

# End ----
