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
library(plyr); library(stringr); library(ggplot2)

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
  Quadrat.ID = 1:nrow(daub),
  Forbs = daub$Forbs,
  Sdmx.of.Forbs = daub$Seed_mix,
  Sdmx.for.Calc = daub$Sdmx.for.Calc,
  Sdmx.of.Total.TRUE = daub$Violets)
  ## We recorded % seed-mix of TOTAL under the "Violets" heading because adding a new field to the
  ## Trimble units we were using was too difficult to integrate into the older data
  ## Less than ideal, but well-documented internally

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

# Save the number wrong (you'll want it later)
wrng.qdrt <- 227

# What if we back-calculated to a threshold rather than specific values?
  ## Would likely increase accuracy, but let's test that

##  ---------------------------------------------------------------------------------------------  ##
                        # Threshold Evaluation  ####
##  ---------------------------------------------------------------------------------------------  ##
# Because we'll do this iteratively for many potential thresholds, let's write a function
num.wrong <- function(calc.vec, true.vec, thresh){
  ## calc.vec = the vector for calculated quadrat-level values
  ## true.vec = the vector for true quadrat-level values
  ## thresh = the Daubenmire recording category to be used as a threshold
  
  
  # Modify the calculation back to 
  calc.vec[calc.vec >= 0 & calc.vec <= thresh] <- 0
  calc.vec[calc.vec > thresh & calc.vec <= 100] <- 1
  
  # Do the same for the TRUE % seed-mix of TOTAL
  true.vec[true.vec >= 0 & true.vec <= thresh] <- 0
  true.vec[true.vec > thresh & true.vec <= 100] <- 1
  
  # Check fidelity between calculated and true vectors at that threshold
  accuracy <- true.vec == calc.vec
  acc.df <- plyr::count(accuracy)
  
  # Get just the frequency of disagreement between the true and calculated vectors
  number.wrong <- as.numeric(subset(acc.df, acc.df$x == "FALSE")$freq)
  
  return(number.wrong)
  
}

# Now get the number wrong for all possible thresholds
wrng.05 <- num.wrong(calc.vec = sdmx$Sdmx.of.Total.CALC, true.vec = sdmx$Sdmx.of.Total.TRUE, thresh = 3)
wrng.25 <- num.wrong(calc.vec = sdmx$Sdmx.of.Total.CALC, true.vec = sdmx$Sdmx.of.Total.TRUE, thresh = 16)
wrng.50 <- num.wrong(calc.vec = sdmx$Sdmx.of.Total.CALC, true.vec = sdmx$Sdmx.of.Total.TRUE, thresh = 38)
wrng.75 <- num.wrong(calc.vec = sdmx$Sdmx.of.Total.CALC, true.vec = sdmx$Sdmx.of.Total.TRUE, thresh = 63)
wrng.95 <- num.wrong(calc.vec = sdmx$Sdmx.of.Total.CALC, true.vec = sdmx$Sdmx.of.Total.TRUE, thresh = 86)

# What are those values?
wrng.qdrt; wrng.05; wrng.25; wrng.50; wrng.75; wrng.95

# Just for fun; stuff this into a dataframe and plot it so you can see how error changes with threshold
thresh.fit <- data.frame(
  Threshold = c("Qdrt", "5%", "25%", "50%", "75%", "95%"),
  Number.Wrong = c(wrng.qdrt, wrng.05, wrng.25, wrng.50, wrng.75, 0),
  Total.Quadrats = rep.int(nrow(sdmx), 6))

# Calculate the percent wrong for each condition
thresh.fit$Percent.Wrong <- (thresh.fit$Number.Wrong / thresh.fit$Total.Quadrats) * 100

# Do some minor housekeeping of the dataframe
levels(thresh.fit$Threshold)
thresh.fit$Threshold <- factor(thresh.fit$Threshold,
                               levels = c("Qdrt", "5%", "25%", "50%", "75%", "95%"))
levels(thresh.fit$Threshold)

# Now plot the accuracy!
ggplot(thresh.fit, aes(x = Threshold, y = Percent.Wrong, fill = Threshold)) +
  geom_bar(stat = 'identity') +
  labs(x = "Conversion Threshold", y = "% Wrong") +
  scale_fill_manual(values = sp::bpy.colors(length(thresh.fit$Threshold)))+
  theme(legend.position = "none")

# Seems like either 25% or 50% is a nice middle-ground between relevance and accuracy



# END ####

