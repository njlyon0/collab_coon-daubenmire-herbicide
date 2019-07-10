##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                      # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# START ####

# Required libraries
library(plyr); library(stringr); library(gmodels)

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff (just in case)
rm(list = ls())

# Load site history file and more refined herbicide index
hstry <- read.csv("./Data/_Indices/sitehistories.csv")
herbtrt <- read.csv("./Data/_Indices/sns_index.csv")

# Get raw data
daub.v0 <- read.csv("./Data/daub-data_raw.csv")
str(daub.v0)

##  ---------------------------------------------------------------------------------------------  ##
                        # Create Necessary Columns ####
##  ---------------------------------------------------------------------------------------------  ##
# Want to make a quick patch within site code (so overwrite the useless one)
sort(unique(daub.v0$Patch))
daub.v0$Patch <- paste0(daub.v0$Pasture, "_", daub.v0$Patch)
sort(unique(daub.v0$Patch))

# Fix the PAW patch problem (West mistakenly recorded as North in 2014 and '15)
daub.v0$Patch <- gsub("PAW_N", "PAW_W", daub.v0$Patch)
daub.v0$Pasture_Patch_Year <- gsub("PAW_N_2014", "PAW_W_2014", daub.v0$Pasture_Patch_Year)
daub.v0$Pasture_Patch_Year <- gsub("PAW_N_2015", "PAW_W_2015", daub.v0$Pasture_Patch_Year)
   ## Understandable mistake as W is the Northernmost patch in that site
sort(unique(daub.v0$Patch))

# Because the treatments we care about didn't exist before 2014, you can ditch all other years here
daub.v0.5 <- subset(daub.v0, daub.v0$Year >= 2014)

# Add in treatments
daub.v1 <- daub.v0.5
daub.v1$Grazing <- as.factor(tolower(hstry$Grazing[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$GrazingTrmnt <- as.factor(tolower(hstry$GrazingTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$FireTrmnt <- as.factor(tolower(hstry$FireTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$YSB <- as.factor(tolower(hstry$TSF[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
    ## switching from "time since fire" to "years since burn" vocabulary
daub.v1$Herbicide.Treatment <- as.factor(tolower(hstry$HerbTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$TSH <- as.factor(tolower(hstry$TSH[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))

##  ---------------------------------------  ##
  # Treatment Code Improvement
##  ---------------------------------------  ##
# NOW let's create grouping variables we care about

# Get a combo grazing + burning treatment variable
## The interaction term will be found based on the difference in slopes over time
## Keeping them as seperate terms means the potential for a significant 3-way interaction
### that's basically uninterpretable and not really what we're interested in anyway
daub.v1$Treatment <- paste0(daub.v1$FireTrmnt, daub.v1$GrazingTrmnt)
sort(unique(daub.v1$Treatment))
daub.v1$Treatment <- gsub("fbnone", "BO", daub.v1$Treatment)
daub.v1$Treatment <- gsub("fbsls", "GB", daub.v1$Treatment) # GB = graze and burn
daub.v1$Treatment <- gsub("fbies", "IGB", daub.v1$Treatment) # IGB = intensive graze and burn
daub.v1$Treatment <- gsub("pbsls", "PBG", daub.v1$Treatment) # PBG = patch burn graze
daub.v1$Treatment <- gsub("pbies", "PBIG", daub.v1$Treatment) # PBIG = patch burn intense graze
daub.v1$Treatment <- gsub("nonone", "None", daub.v1$Treatment) # Self-explanatory (meant to be hayed but it never happened)
sort(unique(daub.v1$Treatment))

# Make it a factor
daub.v1$Treatment <- as.factor(daub.v1$Treatment)
sort(unique(daub.v1$Treatment))

# Let's upgrade the herbicide treatment column now
daub.v1$Herbicide.Treatment <- herbtrt$Fescue.Treatment[match(daub.v1$Patch, herbtrt$Patch)]
sort(unique(daub.v1$Herbicide.Treatment))

# Now that you have the new-and-improved treatment designations, ditch all unnecessary columns
daub.v2 <- daub.v1[,-c(11:12, 26:30, 32)]
  ## Ditched the Excel-calculated Robel average and SD
  ## Also the "Angle_of_O" column

# Check to make sure your b-e-a-utiful columns are not part of what you just ditched
str(daub.v2)

# Reduce year to just the last two digits (will fit better in the plots eventually)
sort(unique(daub.v2$Year))
daub.v2$Year <- gsub("20", "", daub.v2$Year)
sort(unique(daub.v2$Year))

##  ---------------------------------------------------------------------------------------------  ##
                  # Format Special Response Variables ####
##  ---------------------------------------------------------------------------------------------  ##
##  ---------------------------------------  ##
            # Panic Grass
##  ---------------------------------------  ##
# Modify the panic grass recording to be pure presence/absence
sort(unique(daub.v2$Panic))
daub.v2$Panic <- as.numeric(gsub(2, 1, daub.v2$Panic))
    ## The NAs are  what happens when the "." are coerced into empty cells (ignore the warning message)
sort(unique(daub.v2$Panic))

##  ---------------------------------------  ##
          # Litter Depth (cm)
##  ---------------------------------------  ##
# Get litter depth into numeric format
str(daub.v2$Litter_dep)
sort(unique(daub.v2$Litter_dep))

# Overwrite the factor with itself, but coerced into being treated as a number
daub.v2$Litter_dep <- as.numeric(as.character(daub.v2$Litter_dep))

# Do all of these values make sense?
sort(unique(daub.v2$Litter_dep))
  ## No. We measured with a ruler (34 cm), so the two values above that don't make sense

# Also, 86 cm is just shy of 3 *feet* of litter and 175 cm is almost 6 feet of litter.
  ## Seems most likely that a decimal was skipped during data entry
daub.v2$Litter_dep <- as.numeric(gsub("86", "8.6", daub.v2$Litter_dep))
daub.v2$Litter_dep <- as.numeric(gsub("175", "1.75", daub.v2$Litter_dep))
sort(unique(daub.v2$Litter_dep)) # check
str(daub.v2$Litter_dep)

##  ---------------------------------------  ##
          # Robel Pole (dm)
##  ---------------------------------------  ##
# Calculate Robel averages from the four cardinal direction readings taken
colnames(daub.v2[,7:10])
daub.v2$Robel <- as.vector(rowSums(daub.v2[,7:10]) / 4)
  ## I know these data are in the raw data file, but Excel is not reproducible and this is

##  ---------------------------------------------------------------------------------------------  ##
                # Back-Calculate % Seed-mix of TOTAL ####
##  ---------------------------------------------------------------------------------------------  ##
  ## For justification, see "sdmx.R" in this WD
  ## That script can be used to create a supplementary table for in-manuscript justification if needs be

# Get a special dataframe to just do this operation
drydock <- subset(daub.v2, daub.v2$Year <= 2016)
  ## A dry dock is where ships go for serious repair, 
  ## since this file is merely an intermediary for algebra, the name fits

# Create a column for later multiplication with % forbs
drydock$Seedmix.for.Calc <- paste0(".", drydock$Seed_mix)
drydock$Seedmix.for.Calc <- gsub("^.1$", ".01", drydock$Seedmix.for.Calc)
drydock$Seedmix.for.Calc <- gsub("^.3$", ".03", drydock$Seedmix.for.Calc)
sort(unique(drydock$Seedmix.for.Calc))
drydock$Seedmix.for.Calc <- as.numeric(drydock$Seedmix.for.Calc)
sort(unique(drydock$Seedmix.for.Calc))

# Do the calculation
drydock$Seedmix.of.Total <- drydock$Forbs * drydock$Seedmix.for.Calc
sort(unique(drydock$Seedmix.of.Total))

# These values are likely the actual % seed-mix of total, so you need to convert it back to our categories
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 0 & drydock$Seedmix.of.Total <= 1] <- 1
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 1 & drydock$Seedmix.of.Total <= 5] <- 3
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 5 & drydock$Seedmix.of.Total <= 25] <- 16
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 25 & drydock$Seedmix.of.Total <= 50] <- 38
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 50 & drydock$Seedmix.of.Total <= 75] <- 63
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 75 & drydock$Seedmix.of.Total <= 95] <- 86
drydock$Seedmix.of.Total[drydock$Seedmix.of.Total > 95 & drydock$Seedmix.of.Total <= 100] <- 98
sort(unique(drydock$Seedmix.of.Total))

# And now ditch the "Seedmix.for.Calc" column (because you've gotten what you needed out of it)
drydock1 <- drydock[,-c(ncol(drydock)-1)]

# Get the 2017 data into the same format
drydock2 <- subset(daub.v2, daub.v2$Year >= 2017)
drydock2$Seedmix.of.Total <- drydock2$Violets
  ## We recorded % seed-mix of TOTAL under the violets heading in 2017 and 2018

# Mush them back together
daub.v3 <- rbind(drydock1, drydock2)
daub.v3$Seedmix.of.Total <- as.numeric(daub.v3$Seedmix.of.Total)

# Force that new column into showing a threshold of above/below 25% seemix cover of total
  ## Assign to either 0 or 1 so that an average of each patch will calculate a proportion
sort(unique(daub.v3$Seedmix.of.Total)) # should have all sorts of weirdo decimals
daub.v3$Seedmix.of.Total[daub.v3$Seedmix.of.Total >= 0 & daub.v3$Seedmix.of.Total <= 16] <- 0
daub.v3$Seedmix.of.Total[daub.v3$Seedmix.of.Total > 16 & daub.v3$Seedmix.of.Total <= 100] <- 1
sort(unique(daub.v3$Seedmix.of.Total)) # should have only 0 or 1

##  ---------------------------------------------------------------------------------------------  ##
                        # Heavy Grass Quadrats ###
##  ---------------------------------------------------------------------------------------------  ##
# Want to know how many quadrats have "heavy" fescue/CSG cover (i.e. > some threshold)

# Set threshold
thresh <- 75

# Get a new column for "heavy" cover quadrats
daub.v3$Heavy.Fescue <- ifelse(daub.v3$Fescue > thresh, yes = 1, no = 0)
daub.v3$Heavy.CSG <- ifelse(daub.v3$CSG > thresh, yes = 1, no = 0)
daub.v3$Heavy.WSG <- ifelse(daub.v3$WSG > thresh, yes = 1, no = 0)

##  ---------------------------------------------------------------------------------------------  ##
                        # Get Patch-Level Values ####
##  ---------------------------------------------------------------------------------------------  ##
# Before getting the patch-level averages, check how many quadrats are in some relevant subsets of sites
nrow(subset(daub.v3, (Pasture == "BSH" | Pasture == "DUN" | Pasture == "GIL" | 
                        Pasture == "LTR" | Pasture == "PYW" | Pasture == "RC2" |
                        Pasture == "STE")))

# Get patch-level averages for all the response variables
daub.v4 <- ddply(daub.v3, 
            c("Pasture_Patch_Year", "Patch", "Pasture", "Year",
              "Treatment", "Herbicide.Treatment"), 
            summarise,
            CSG = mean(CSG),
            WSG = mean(WSG),
            Fescue = mean(Fescue),
            Forbs = mean(Forbs),
            Legumes = mean(Legumes),
            Woody = mean(Woody),
            Bare = mean(Bare),
            Litter = mean(Litter),
            Seedmix = mean(Seedmix.of.Total),
            Robel = mean(Robel),
            Panic = mean(Panic),
            LitDep = mean(Litter_dep),
            Hvy.Fesc = sum(Heavy.Fescue),
            Hvy.CSG = sum(Heavy.CSG),
            Hvy.WSG = sum(Heavy.WSG))

# Should lose a *lot* of rows
nrow(daub.v3); nrow(daub.v4)

# Save out this one dataframe with all response variables calculated (just in case)
write.csv(daub.v4, "./Data/daub-data_clean.csv", row.names = F)

##  ---------------------------------------------------------------------------------------------  ##
                     # Get Project-Specific Dataframes ####
##  ---------------------------------------------------------------------------------------------  ##
# Get just the spray and seeded data (but remove RCH too)
herb <- subset(daub.v4, (daub.v4$Herbicide.Treatment == "Con" | 
                 daub.v4$Herbicide.Treatment == "Spr" | 
                 daub.v4$Herbicide.Treatment == "SnS") &
                 daub.v4$Pasture != "RCH")
sort(unique(herb$Pasture)); sort(unique(herb$Herbicide.Treatment))

# Insufficient data for stocking rate comparison, so let's remove that
sort(unique(herb$Treatment))
herb$Treatment <- as.factor(gsub("IGB", "GB", herb$Treatment))
sort(unique(herb$Treatment))
  ## Will receive a substantial asterisk in the paper discussion/methods

# Going to do some fancy footwork to make a grouping variable for eventual plotting
herb$Composite.Variable <- paste0(herb$Year, "-", herb$Herbicide.Treatment)
sort(unique(herb$Composite.Variable))

# Re-order
ncol(herb)
herb.v2 <- herb[,c(1:6, 22, 7:21)]
ncol(herb.v2) # make sure no columns are dropped

# Save out those this 'full data' dataframe!
write.csv(herb.v2, "./Data/sns-data_full.csv", row.names = F)

# Subset the big dataframe into three subsets and save them
  ## 1. 2014 only (to check for absence of pre-treatment differences)
herb.14 <- subset(herb.v2, Year == "14")
write.csv(herb.14, "./Data/sns-data_2014.csv", row.names = F)

  ## 2. everything other than 2014 (to evaluate response to treatment)
herb.pst.trt <- subset(herb.v2, Year != "14")
write.csv(herb.pst.trt, "./Data/sns-data_post-trt.csv", row.names = F)

  ## 3. 2014 and 2018 only (was there a difference between before and the very end?)
herb.comp <- subset(herb.v2, Year == "14" | Year == "18")
write.csv(herb.comp, "./Data/sns-data_14-vs-18.csv", row.names = F)

##  ---------------------------------------------------------------------------------------------  ##
                           # Get Summarized Dataframes for Plotting ####
##  ---------------------------------------------------------------------------------------------  ##
# Check the structure of the 2014 data
str(herb.14)

# Get treatment-level averages for *all* variables (and standard errors too)
herb.14.sumzd <- ddply(herb.14, c("Year", "Treatment", "Herbicide.Treatment"), summarise,
                       Avg.CSG = mean(CSG),
                       SE.CSG = (sd(CSG)/nrow(herb.14)), 
                       CI.CSG = abs(ci(CSG)[1] - abs(ci(CSG)[2])),
                       Avg.WSG = mean(WSG),
                       SE.WSG = (sd(WSG)/nrow(herb.14)), 
                       CI.WSG = abs(ci(WSG)[1] - abs(ci(WSG)[2])),
                       Avg.Fescue = mean(Fescue),
                       SE.Fescue = (sd(Fescue)/nrow(herb.14)), 
                       CI.Fescue = abs(ci(Fescue)[1] - abs(ci(Fescue)[2])),
                       Avg.Forbs = mean(Forbs),
                       SE.Forbs = (sd(Forbs)/nrow(herb.14)), 
                       CI.Forbs = abs(ci(Forbs)[1] - abs(ci(Forbs)[2])),
                       Avg.Legumes = mean(Legumes),
                       SE.Legumes = (sd(Legumes)/nrow(herb.14)), 
                       CI.Legumes = abs(ci(Legumes)[1] - abs(ci(Legumes)[2])),
                       Avg.Woody = mean(Woody),
                       SE.Woody = (sd(Woody)/nrow(herb.14)), 
                       CI.Woody = abs(ci(Woody)[1] - abs(ci(Woody)[2])),
                       Avg.Bare = mean(Bare),
                       SE.Bare = (sd(Bare)/nrow(herb.14)), 
                       CI.Bare = abs(ci(Bare)[1] - abs(ci(Bare)[2])),
                       Avg.Litter = mean(Litter),
                       SE.Litter = (sd(Litter)/nrow(herb.14)), 
                       CI.Litter = abs(ci(Litter)[1] - abs(ci(Litter)[2])),
                       Avg.Seedmix = mean(Seedmix),
                       SE.Seedmix = (sd(Seedmix)/nrow(herb.14)), 
                       CI.Seedmix = abs(ci(Seedmix)[1] - abs(ci(Seedmix)[2])),
                       Avg.Robel = mean(Robel),
                       SE.Robel = (sd(Robel)/nrow(herb.14)), 
                       CI.Robel = abs(ci(Robel)[1] - abs(ci(Robel)[2])),
                       Avg.Panic = mean(Panic),
                       SE.Panic = (sd(Panic)/nrow(herb.14)), 
                       CI.Panic = abs(ci(Panic)[1] - abs(ci(Panic)[2])),
                       Avg.LitDep = mean(LitDep),
                       SE.LitDep = (sd(LitDep)/nrow(herb.14)), 
                       CI.LitDep = abs(ci(LitDep)[1] - abs(ci(LitDep)[2])),
                       Avg.Hvy.Fesc = mean(Hvy.Fesc),
                       SE.Hvy.Fesc = (sd(Hvy.Fesc)/nrow(herb.14)), 
                       CI.Hvy.Fesc = abs(ci(Hvy.Fesc)[1] - abs(ci(Hvy.Fesc)[2])),
                       Avg.Hvy.CSG = mean(Hvy.CSG),
                       SE.Hvy.CSG = (sd(Hvy.CSG)/nrow(herb.14)), 
                       CI.Hvy.CSG = abs(ci(Hvy.CSG)[1] - abs(ci(Hvy.CSG)[2])),
                       Avg.Hvy.WSG = mean(Hvy.WSG),
                       SE.Hvy.WSG = (sd(Hvy.WSG)/nrow(herb.14)),
                       CI.Hvy.WSG = abs(ci(Hvy.WSG)[1] - abs(ci(Hvy.WSG)[2])) )

# Save the new dataframe for use in figure creation!
write.csv(herb.14.sumzd, "./Data/sns-data_2014_summarized.csv", row.names = F)



# END ####


