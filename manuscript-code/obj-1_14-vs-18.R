##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                      # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# Objective 1
  ## Functional Group Response to Treatment
  ## Comparing pre- (2014) and post-treatment (2018)

# START ####

# Required libraries
library(RRPP) # Analysis

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")
setwd("/cloud/project/")

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
                         # Cool Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(CSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(CSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## NS

# Pairwise comparisons
csg.cgr.fit <- lm.rrpp(CSG ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(csg.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

anova(lm.rrpp(CSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(CSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## year = sig

# Pairwise comparisons
csg.trt.ugr.fit <- lm.rrpp(CSG ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(csg.trt.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                        # Warm Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(WSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(WSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## NS

# Pairwise
wsg.cgr.fit <- lm.rrpp(WSG ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(wsg.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

anova(lm.rrpp(WSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(WSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")

# Pairwise comparisons
wsg.ugr.fit <- lm.rrpp(WSG ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(wsg.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                                # Fescue ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Fescue ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Fescue ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## year = sig

# Pairwise
fsc.cgr.fit <- lm.rrpp(Fescue ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(fsc.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

anova(lm.rrpp(Fescue ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Fescue ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## year = sig

# Pairwise
fsc.ugr.fit <- lm.rrpp(Fescue ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(fsc.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                        # Seedmix Threshhold ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Seedmix ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Seedmix ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F") # year = sig

# Pairwise
smx.cgr.fit <- lm.rrpp(Seedmix ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(smx.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Seedmix ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Seedmix ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F") # year = sig

# Pairwise
smx.ugr.fit <- lm.rrpp(Seedmix ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(smx.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                                # Forbs ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Forbs ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F") # int = NS
anova(lm.rrpp(Forbs ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F") # yr = sig

# Pairwise
frb.cgr.fit <- lm.rrpp(Forbs ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(frb.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Forbs ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Forbs ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F") # NS

# Pairwise
frb.ugr.fit <- lm.rrpp(Forbs ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(frb.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                              # Legumes ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Legumes ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Legumes ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F") # year = sig

# Pairwise
lgm.cgr.fit <- lm.rrpp(Legumes ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(lgm.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Legumes ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Legumes ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F") # NS

# Pairwise
lgm.ugr.fit <- lm.rrpp(Legumes ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(lgm.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  ---------------------------------------------------------------------------------------------  ##
                                # Woody ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Woody ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Woody ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F") # NS

# Pairwise
wdy.cgr.fit <- lm.rrpp(Woody ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(wdy.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Woody ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Woody ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F") # yr = sig

# Pairwise
wdy.ugr.fit <- lm.rrpp(Woody ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(wdy.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

##  -----------------------------------------  ##
             # Panic ####
##  -----------------------------------------  ##
# Analysis
anova(lm.rrpp(Panic ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Panic ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F") # yr = sig

# Pairwise
pnc.cgr.fit <- lm.rrpp(Panic ~ Composite.Variable, data = cgr, iter = 9999)
summary(pairwise(pnc.cgr.fit, fit.null = NULL, groups = cgr$Composite.Variable))

# Analysis
anova(lm.rrpp(Panic ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F") # interxn = NS
anova(lm.rrpp(Panic ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F") # NS

# Pairwise
pnc.ugr.fit <- lm.rrpp(Panic ~ Composite.Variable, data = ugr, iter = 9999)
summary(pairwise(pnc.ugr.fit, fit.null = NULL, groups = ugr$Composite.Variable))

# END ####

