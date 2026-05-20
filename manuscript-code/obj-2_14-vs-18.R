##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                        # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# Objective 2
  ## Grass Dominated Quadrat Fequency Response to Treatment
  ## Comparing pre- (2014) and post-treatment (2018)

# START ####

# Required libraries
library(RRPP) # Analysis

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                              # Housekeeping ####
##  ---------------------------------------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/sns-data_14-vs-18.csv")

# Re-level the factors too though
unique(sns$Herbicide.Treatment)
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# General analytical procedure
  ## 1) Fit model with interaction term and assess *ONLY* the interaction term
  ## 2) If insignificant, run a new model without it (if significant, stop there, you're done)
  ## 3) If either explanatory variable is significant, fit a separate model of just that one
  ## 4) Run pairwise comparisons on that single-variable model

##  ---------------------------------------------------------------------------------------------  ##
                          # Heavy CSG Quadrats ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## NS

# Pairwise comparisons
csg.cgr.fit <- lm.rrpp(Hvy.CSG ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(csg.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## trt = sig

# Pairwise comparisons
csg.ugr.fit <- lm.rrpp(Hvy.CSG ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(csg.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                        # Warm Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## NS

# Pairwise comparisons
wsg.cgr.fit <- lm.rrpp(Hvy.WSG ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(wsg.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## NS

# Pairwise comparisons
wsg.ugr.fit <- lm.rrpp(Hvy.WSG ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(wsg.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                              # Heavy Fescue ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## year = sig

# Pairwise comparisons
fsc.cgr.fit <- lm.rrpp(Hvy.Fesc ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(fsc.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Un-grazed
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## year = marginal

# Pairwise comparisons
fsc.ugr.fit <- lm.rrpp(Hvy.Fesc ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(fsc.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

# END ####
