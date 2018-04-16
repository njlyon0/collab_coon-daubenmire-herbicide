##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                              # Daubenmire Data from the Grand River Grasslands
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
##  ---------------------------------------------------------------------------------------------  ##
                         # Herbicide Project
##  ---------------------------------------------------------------------------------------------  ##
# Project Leads
  ## Jaime J Coon & Nicholas J Lyon

# Main Project Questions
  ## How do plants respond to SnS (dumb phrasing, will return)?

# Common Abbreviations
  # Grazing
    ## IES = intensive early stocking
    ## SLS = season-long stocking
    ## CGR = "cattle grazed restoration" (any site with cattle)
    ## UGR = "un-grazed restoration" (sites without cattle)
  # Herbicide.Treatment
    ## Con = control patch of spray and seed sites
    ## Spr = patch of spray and seed sites sprayed with glyphosate (RoundUp) in November of 2014
    ## SnS = patch of spray and seed sites both sprayed with glyphosate and subsequently seeded in March of 2014
    ## No = any patch of a site not used in the spray and seed project
  # "Treatment"
    ## BO = burn only
    ## GB = graze (via SLS) and burn full site (FB)
    ## IGB = intensive grazing (IES) and burn full site
    ## PBG = patch-burn graze (with SLS)
    ## PBIG = patch burn intense graze (with IES)

# Code written by Nicholas J Lyon

# START ####
  ## Housekeeping

# Required libraries
library(plyr); library(stringr) # Cleaning

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/_Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                              # Data Cleaning ####
##  ---------------------------------------------------------------------------------------------  ##
# Load site history file and more refined herbicide index
hstry <- read.csv("./Data/_Indices/sitehistories.csv")
herbtrt <- read.csv("./Data/_Indices/sns_index.csv")

# Get raw data
daub.v0 <- read.csv("./Data/daubdata_raw.csv")
str(daub.v0)

##  ---------------------------------------  ##
       # Preliminary Cleaning
##  ---------------------------------------  ##
# Want to make a quick patch within site code (so overwrite the useless one)
daub.v0$Patch <- paste0(daub.v0$Pasture, "_", daub.v0$Patch)

# Fix the PAW patch problem (West mistakenly recorded as North in 2014 and '15)
daub.v0$Patch <- gsub("PAW_N", "PAW_W", daub.v0$Patch)
daub.v0$Pasture_Patch_Year <- gsub("PAW_N_2014", "PAW_W_2014", daub.v0$Pasture_Patch_Year)
daub.v0$Pasture_Patch_Year <- gsub("PAW_N_2015", "PAW_W_2015", daub.v0$Pasture_Patch_Year)
   ## Understandable mistake as W is the Northernmost patch in that site
sort(unique(daub.v0$Patch))

# Add in treatments
daub.v1 <- daub.v0
daub.v1$Grazing <- as.factor(tolower(hstry$Grazing[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$GrazingTrmnt <- as.factor(tolower(hstry$GrazingTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$FireTrmnt <- as.factor(tolower(hstry$FireTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$YSB <- as.factor(tolower(hstry$TSF[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
    ## switching from "time since fire" to "years since burn" vocabulary
daub.v1$Herbicide.Treatment <- as.factor(tolower(hstry$HerbTreat[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))
daub.v1$TSH <- as.factor(tolower(hstry$TSH[match(daub.v1$Pasture_Patch_Year, hstry$Pasture_patch_year)]))

# Now that you have most of the columns you want, it's time to ditch some of 'em
daub.v2 <- daub.v1[,-c(9:10, 24)]
str(daub.v2) # check to make sure your b-e-a-utiful columns are not part of what you just ditched

# Can fix the panic grass recording system here ("2" doesn't mean a consistent thing so it's useless)
daub.v2$Panic <- as.numeric(gsub(2, 1, daub.v2$Panic))
    ## Now it's pure presence/absence data
    ## The NAs are  what happens when the "." are coerced into empty cells (it's fine)

# Calculate Robel averages here
daub.v2$Robel <- as.vector(rowSums(daub.v2[,5:8]) / 4)
  ## I know these data are in the raw data file, but Excel is not reproducible and this is

# Do the back-calculation for % seedmix of TOTAL for years where those data weren't collected
  ## For further explanation of why things in this bit are done the way they are:
    ## see "seedmix.R" in the master branch

drydock <- subset(daub.v2, daub.v2$Year >= 2014 & daub.v2$Year <= 2016)
  ## A dry dock is where ships go for serious repair,
  ## since this file is merely an intermediary for algebra, the name fits

# Create a column for later multiplication with % forbs
drydock$Seedmix.for.Calc <- paste0(".", drydock$Seed_mix)
drydock$Seedmix.for.Calc <- gsub("^.1$", ".01", drydock$Seedmix.for.Calc)
drydock$Seedmix.for.Calc <- gsub("^.3$", ".03", drydock$Seedmix.for.Calc)
drydock$Seedmix.for.Calc <- as.numeric(drydock$Seedmix.for.Calc)
sort(unique(drydock$Seedmix.for.Calc))

# Do the calculation
drydock$Seedmix.of.Total <- drydock$Forbs * drydock$Seedmix.for.Calc

# And now ditch the "Seedmix.for.Calc" column (because you've gotten what you needed out of it)
drydock1 <- drydock[,-c(ncol(drydock)-1)]

# Get the 2017 data into the same format
drydock2 <- subset(daub.v2, daub.v2$Year >= 2017)
drydock2$Seedmix.of.Total <- drydock2$Violets

# And prep the other data for the addition of a new column
drydock3 <- subset(daub.v2, daub.v2$Year <= 2013)
drydock3$Seedmix.of.Total <- rep.int("", nrow(drydock3))

# Mush them together in chronological order
daub.v3 <- rbind(drydock3, drydock1, drydock2)
daub.v3$Seedmix.of.Total <- as.numeric(daub.v3$Seedmix.of.Total)

# Force that new column into showing a threshold of above/below 25% seemix cover of total
  ## Going to assign to either 0 or 1 so that an average of each patch will calculate a proportion
  ## That will range from 0 to 1 and will mean the relative abundance of quadrats with >25% seedmix forb cover (of total area)
sort(unique(daub.v3$Seedmix.of.Total)) # should have all sorts of weirdo decimals
daub.v3$Seedmix.of.Total[daub.v3$Seedmix.of.Total >= 0 & daub.v3$Seedmix.of.Total <= 16] <- 0
daub.v3$Seedmix.of.Total[daub.v3$Seedmix.of.Total >= 17 & daub.v3$Seedmix.of.Total <= 100] <- 1
sort(unique(daub.v3$Seedmix.of.Total)) # should have a binary of 0/1

# Save yourself some heartache, and ditch all pre-2014 data here
daub.v3.5 <- subset(daub.v3, daub.v3$Year >= 2014)

# Get patch-level averages for all the response variables that are consistently collected
daub.v4 <- ddply(daub.v3.5, 
            c("Pasture_Patch_Year", "Patch", "Pasture", "Year",
              "Grazing", "GrazingTrmnt", "FireTrmnt", "Herbicide.Treatment"), 
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
            Panic = mean(Panic))

# For some  reason, litter depth is treated funnily, so need to get it's averages seperately
# Remove the "." that caused a problem with Panic earlier
ltdp.v1 <- subset(daub.v3.5, daub.v3.5$Litter_dep != ".")

# Now make sure R is treating it as a number, not a factor or some crazy nonsense like that)
ltdp.v1$Litter_dep <- as.numeric(ltdp.v1$Litter_dep)

# Get the average of Litterdepth within patch
ltdp.v2 <- aggregate(Litter_dep ~ Pasture_Patch_Year + Patch + Pasture + Year +
                       Grazing + GrazingTrmnt + FireTrmnt + Herbicide.Treatment,
                     data = ltdp.v1, FUN = mean)

# Now mush this variable back into the larger dataframe
daub.v4$LitDep <- ltdp.v2$Litter_dep[match(daub.v4$Pasture_Patch_Year, ltdp.v2$Pasture_Patch_Year)]
    # Using "match" here means that entries are placed in the right row
    # and places where litter depth wasn't collected are left empty
  
  ##  ---------------------------------------  ##
            # Nick's Opinions #### 
  ##  ---------------------------------------  ##
# TWO OPINION BITS BEFORE WE MOVE ON

# First, I don't think any of the 235 data from post-herbicide application are good and am dropping them here
  ## "Inexact" is a good way of saying "inconsistent";
  ## I don't consider it good evidence in any story we want to tell
daub.v5 <- subset(daub.v4, daub.v4$Pasture != "235")
sort(unique(daub.v5$Pasture))

# Second, I don't think the RCH data are good because we re-drew the patches partway through
  ## Potential for legacy effects of our old treatments in the two patches that were previously the whole site
  ## AND no records for what happened to the patch that was added later before it was added
daub.v6 <- subset(daub.v5, daub.v5$Pasture != "RCH2014" & daub.v5$Pasture != "RCH")
sort(unique(daub.v6$Pasture)) # check for success

##  ---------------------------------------  ##
    # Grouping Variable Creation
##  ---------------------------------------  ##
# Create a new dataframe to be sure nothing gets messed up in this next bit
daub.v7 <- daub.v6

# NOW let's create grouping variables we care about

# Get a combo grazing + burning treatment variable
  ## The interaction term will be found based on the difference in slopes over time
  ## Keeping them as seperate terms means the potential for a significant 3-way interaction
    ### that's basically uninterpretable and not really what we're interested in anyway
daub.v7$Treatment <- paste0(daub.v7$FireTrmnt, daub.v7$GrazingTrmnt)
daub.v7$Treatment <- gsub("fbnone", "BO", daub.v7$Treatment)
daub.v7$Treatment <- gsub("fbsls", "GB", daub.v7$Treatment) # GB = graze and burn
daub.v7$Treatment <- gsub("fbies", "IGB", daub.v7$Treatment) # IGB = intensive graze and burn
daub.v7$Treatment <- gsub("pbsls", "PBG", daub.v7$Treatment) # PBG = patch burn graze
daub.v7$Treatment <- gsub("pbies", "PBIG", daub.v7$Treatment) # PBIG = patch burn intense graze
daub.v7$Treatment <- gsub("nonone", "None", daub.v7$Treatment) # Self-explanatory (meant to be hayed but it never happened)
sort(unique(daub.v7$Treatment)) # worked?

daub.v7$Treatment <- as.factor(daub.v7$Treatment)

# Let's upgrade the herbicide treatment column now
daub.v7$Herbicide.Treatment <- herbtrt$Fescue.Treatment[match(daub.v7$Patch, herbtrt$Patch)]
sort(unique(daub.v7$Herbicide.Treatment))

# Re-order your dataframe so your new columns get to have pride of place with the other grouping variables
daub.v8 <- daub.v7[,c(2, 4, 21, 8, 9:20)]

# Save out this one dataframe with all response variables calculated (just in case)
write.csv(daub.v8, "./Data/daubdata_clean.csv", row.names = F)

##  ---------------------------------------------------------------------------------------------  ##
                         # Create SnS-Only Dataframe ####
##  ---------------------------------------------------------------------------------------------  ##
# NOTE
  ## Gone as far are you want to go with the mega dataframe,
  ## now can split it up so you only have the data you want

# Get just the spray and seeded data
herb <- subset(daub.v8, daub.v8$Herbicide.Treatment == "Con" | 
                 daub.v8$Herbicide.Treatment == "Spr" | 
                 daub.v8$Herbicide.Treatment == "SnS")

# Insufficient data for IES vs. SLS response comparison, so ditch it
  ## Will need a substantial asterisk in the eventual paper discussion/methods
herb$Treatment <- gsub("IGB", "GB", herb$Treatment)

# Going to do some fancy footwork to make a grouping variable for eventual plotting
herb$Composite.Variable <- paste0(herb$Treatment, "-", herb$Herbicide.Treatment)
sort(unique(herb$Composite.Variable))

# And kick Sterner (STE) because of its weirdo burn/herbicide timing
sort(unique(herb$Patch))
herb.v2 <- subset(herb, herb$Patch != "STE_N" & herb$Patch != "STE_S" & herb$Patch != "STE_W")
sort(unique(herb.v2$Patch))

# Before you save, why don't you go ahead and ditch the grouping variables we are uninterested in?
colnames(herb.v2)
herb.v3 <- herb.v2[, c(1:4, 17, 5:16)]
colnames(herb.v3)

# Save out those this dataframe because it is ready to roll!
write.csv(herb.v3, "./Data/snsdata.csv", row.names = F)

# END ####

