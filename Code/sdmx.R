##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## Prior to 2017, we only counted % seed-mix plants of FORBS
  ## Obviously, we actually care about how much of the TOTAL of each quadrat is seed-mix, not of FORBS
  ## In 2017 we collected both % seed-mix of FORBS and of TOTAL, and we always collect % forbs of total
  ## This script is aimed at testing how well we can "back-calculate" % seed-mix of total

# Algebra thinking:
  ## % seed-mix of TOTAL = % forbs of TOTAL * % seed-mix of FORBS

# Required libraries
library(plyr); library(stringr)

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff (just in case)
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                # Test Seed-mix Data Back-Calculation ####
##  ---------------------------------------------------------------------------------------------  ##
# Get data
daub.v0 <- read.csv("./Data/daubdata_raw.csv")

# Remove all but 2017 from the dataframe
daub <- subset(daub.v0, daub.v0$Year == 2017)

# Let's get the % seed-mix of FORBS into a decimal format for easy multiplication
daub$Sdmx.for.Calc <- paste0(".", daub$Seed_mix)
sort(unique(daub$Sdmx.for.Calc))
daub$Sdmx.for.Calc <- as.numeric(gsub("^.1$", ".01", daub$Sdmx.for.Calc))
daub$Sdmx.for.Calc <- as.numeric(gsub("^.3$", ".03", daub$Sdmx.for.Calc))
sort(unique(daub$Sdmx.for.Calc))

# Now get a dataframe of the bare minimum of stuff we need
sdmx <- data.frame(
  Quadrat.ID <- 1:nrow(daub),
  Forbs <- daub$Forbs,
  Sdmx.of.Forbs <- daub$Seed_mix,
  Sdmx.for.Calc <- daub$Sdmx.for.Calc,
  Sdmx.of.Total.TRUE <- daub$Violets)
  ## We recorded % seed-mix of TOTAL under the "Violets" heading because adding a new field to the
  ## Trimble units we were using was too difficult to integrate into the older data
  ## Less than ideal, but well-documented internally

# Weird column names are distracting, fix that
colnames(sdmx)
colnames(sdmx) <- c("Quadrat.ID", "Forbs", "Sdmx.of.Forbs", "Sdmx.for.Calc", "Sdmx.of.Total.TRUE")
colnames(sdmx)

# Make the TRUE % seed-mix of TOTAL into a number (you'll want it later)
sort(unique(sdmx$Sdmx.of.Total.TRUE))
sdmx$Sdmx.of.Total.TRUE <- as.numeric(as.character(sdmx$Sdmx.of.Total.TRUE))
sort(unique(sdmx$Sdmx.of.Total.TRUE))

# Do the calculation
sdmx$Sdmx.of.Total.CALC <- sdmx$Forbs * sdmx$Sdmx.for.Calc
sort(unique(sdmx$Sdmx.of.Total.CALC))

# Now modify the derived values to fit into our pseudo-continuous category system
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 0 & sdmx$Sdmx.of.Total.CALC <= 1] <- 1
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 1 & sdmx$Sdmx.of.Total.CALC <= 5] <- 3
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 5 & sdmx$Sdmx.of.Total.CALC <= 25] <- 16
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 25 & sdmx$Sdmx.of.Total.CALC <= 50] <- 38
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 50 & sdmx$Sdmx.of.Total.CALC <= 75] <- 63
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 75 & sdmx$Sdmx.of.Total.CALC <= 95] <- 86
sdmx$Sdmx.of.Total.CALC[sdmx$Sdmx.of.Total.CALC > 95 & sdmx$Sdmx.of.Total.CALC <= 100] <- 98
sort(unique(sdmx$Sdmx.of.Total.CALC))

# Check accuracy on a per-quadrat basis
sdmx$Accuracy <- sdmx$Sdmx.of.Total.TRUE == sdmx$Sdmx.of.Total.CALC
count(sdmx$Accuracy)

# What's that in % format?
(227 / nrow(sdmx)) * 100
  ## wrong for 15% of the quadrats

# What if we back-calculated to a threshold rather than specific values?
  ## Would likely increase accuracy, but let's test that

##  ---------------------------------------------------------------------------------------------  ##
                        # Threshold Evaluation (25%) ####
##  ---------------------------------------------------------------------------------------------  ##
# Get a new dataframe for threshold testing so you only have to go back to here if problems happen
sdmx2 <- sdmx

# Convert the calculated % seed-mix of TOTAL to a threshold
sdmx2$Sdmx.of.Total.CALC[sdmx2$Sdmx.of.Total.CALC >= 0 & sdmx2$Sdmx.of.Total.CALC <= 16] <- 0
sdmx2$Sdmx.of.Total.CALC[sdmx2$Sdmx.of.Total.CALC > 16 & sdmx2$Sdmx.of.Total.CALC <= 100] <- 1

# Do the same for the TRUE % seed-mix of TOTAL
sdmx2$Sdmx.of.Total.TRUE[sdmx2$Sdmx.of.Total.TRUE >= 0 & sdmx2$Sdmx.of.Total.TRUE <= 16] <- 0
sdmx2$Sdmx.of.Total.TRUE[sdmx2$Sdmx.of.Total.TRUE > 16 & sdmx2$Sdmx.of.Total.TRUE <= 100] <- 1

# Check accuracy on a per-quadrat basis
sdmx2$Accuracy <- sdmx2$Sdmx.of.Total.TRUE == sdmx2$Sdmx.of.Total.CALC
count(sdmx2$Accuracy)

# What's that in % format?
(39 / nrow(sdmx2)) * 100
  ## Wrong only 2.6% of the time!

# Let's test all possible thresholds though to see if there's a *best* one to be using

##  ---------------------------------------------------------------------------------------------  ##
                      # Threshold Evaluation (5%) ####
##  ---------------------------------------------------------------------------------------------  ##
# Get a new dataframe for threshold testing so you only have to go back to here if problems happen
sdmx3 <- sdmx

# Convert the calculated % seed-mix of TOTAL to a threshold
sdmx3$Sdmx.of.Total.CALC[sdmx3$Sdmx.of.Total.CALC >= 0 & sdmx3$Sdmx.of.Total.CALC <= 3] <- 0
sdmx3$Sdmx.of.Total.CALC[sdmx3$Sdmx.of.Total.CALC > 3 & sdmx3$Sdmx.of.Total.CALC <= 100] <- 1

# Do the same for the TRUE % seed-mix of TOTAL
sdmx3$Sdmx.of.Total.TRUE[sdmx3$Sdmx.of.Total.TRUE >= 0 & sdmx3$Sdmx.of.Total.TRUE <= 3] <- 0
sdmx3$Sdmx.of.Total.TRUE[sdmx3$Sdmx.of.Total.TRUE > 3 & sdmx3$Sdmx.of.Total.TRUE <= 100] <- 1

# Check accuracy on a per-quadrat basis
sdmx3$Accuracy <- sdmx3$Sdmx.of.Total.TRUE == sdmx3$Sdmx.of.Total.CALC
count(sdmx3$Accuracy)

# What's that in % format?
(170 / nrow(sdmx3)) * 100

##  ---------------------------------------------------------------------------------------------  ##
                          # Threshold Evaluation (50%) ####
##  ---------------------------------------------------------------------------------------------  ##
# Get a new dataframe for threshold testing so you only have to go back to here if problems happen
sdmx4 <- sdmx

# Convert the calculated % seed-mix of TOTAL to a threshold
sdmx4$Sdmx.of.Total.CALC[sdmx4$Sdmx.of.Total.CALC >= 0 & sdmx4$Sdmx.of.Total.CALC <= 38] <- 0
sdmx4$Sdmx.of.Total.CALC[sdmx4$Sdmx.of.Total.CALC > 38 & sdmx4$Sdmx.of.Total.CALC <= 100] <- 1

# Do the same for the TRUE % seed-mix of TOTAL
sdmx4$Sdmx.of.Total.TRUE[sdmx4$Sdmx.of.Total.TRUE >= 0 & sdmx4$Sdmx.of.Total.TRUE <= 38] <- 0
sdmx4$Sdmx.of.Total.TRUE[sdmx4$Sdmx.of.Total.TRUE > 38 & sdmx4$Sdmx.of.Total.TRUE <= 100] <- 1

# Check accuracy on a per-quadrat basis
sdmx4$Accuracy <- sdmx4$Sdmx.of.Total.TRUE == sdmx4$Sdmx.of.Total.CALC
count(sdmx4$Accuracy)

# What's that in % format?
(20 / nrow(sdmx4)) * 100

##  ---------------------------------------------------------------------------------------------  ##
                      # Threshold Evaluation (75%) ####
##  ---------------------------------------------------------------------------------------------  ##
# Get a new dataframe for threshold testing so you only have to go back to here if problems happen
sdmx5 <- sdmx

# Convert the calculated % seed-mix of TOTAL to a threshold
sdmx5$Sdmx.of.Total.CALC[sdmx5$Sdmx.of.Total.CALC >= 0 & sdmx5$Sdmx.of.Total.CALC <= 63] <- 0
sdmx5$Sdmx.of.Total.CALC[sdmx5$Sdmx.of.Total.CALC > 63 & sdmx5$Sdmx.of.Total.CALC <= 100] <- 1

# Do the same for the TRUE % seed-mix of TOTAL
sdmx5$Sdmx.of.Total.TRUE[sdmx5$Sdmx.of.Total.TRUE >= 0 & sdmx5$Sdmx.of.Total.TRUE <= 63] <- 0
sdmx5$Sdmx.of.Total.TRUE[sdmx5$Sdmx.of.Total.TRUE > 63 & sdmx5$Sdmx.of.Total.TRUE <= 100] <- 1

# Check accuracy on a per-quadrat basis
sdmx5$Accuracy <- sdmx5$Sdmx.of.Total.TRUE == sdmx5$Sdmx.of.Total.CALC
count(sdmx5$Accuracy)

# What's that in % format?
(2 / nrow(sdmx5)) * 100

##  ---------------------------------------------------------------------------------------------  ##
                      # Threshold Evaluation (95%) ####
##  ---------------------------------------------------------------------------------------------  ##
# Get a new dataframe for threshold testing so you only have to go back to here if problems happen
sdmx6 <- sdmx

# Convert the calculated % seed-mix of TOTAL to a threshold
sdmx6$Sdmx.of.Total.CALC[sdmx6$Sdmx.of.Total.CALC >= 0 & sdmx6$Sdmx.of.Total.CALC <= 86] <- 0
sdmx6$Sdmx.of.Total.CALC[sdmx6$Sdmx.of.Total.CALC > 86 & sdmx6$Sdmx.of.Total.CALC <= 100] <- 1

# Do the same for the TRUE % seed-mix of TOTAL
sdmx6$Sdmx.of.Total.TRUE[sdmx6$Sdmx.of.Total.TRUE >= 0 & sdmx6$Sdmx.of.Total.TRUE <= 86] <- 0
sdmx6$Sdmx.of.Total.TRUE[sdmx6$Sdmx.of.Total.TRUE > 86 & sdmx6$Sdmx.of.Total.TRUE <= 100] <- 1

# Check accuracy on a per-quadrat basis
sdmx6$Accuracy <- sdmx6$Sdmx.of.Total.TRUE == sdmx6$Sdmx.of.Total.CALC
count(sdmx6$Accuracy)

# never wrong. So not doing the 98 one

##  ---------------------------------------------------------------------------------------------  ##
                  # Threshold Accuracy Comparison ####
##  ---------------------------------------------------------------------------------------------  ##



# END ####

