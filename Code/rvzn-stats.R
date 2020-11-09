##  ------------------------------------------------------------------------------------------  ##
                # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  ------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE ####
  ## The revise & resubmit review we got at Rangeland Ecology and Management requires a ∆ of stats
  ## So, rather that changing the *many* tests we ran prior to this
  ## I'm just going to write a totally new script for just the final version of this paper
  ## If you're a reader of the paper, this includes 100% of the results in the publication

# Set the working directory
setwd("~/Documents/School/_Publications/2020_Coon_Daubenmire SnS/Daubenmire.HerbicideComponent.WD")
  ## if you're not me you will need to re-set this to your own computer.

# Clear the environment
rm(list = ls())

# Load required libraries
library(lme4); library(lmerTest); library(lsmeans); library(emmeans)

## ----------------------------------- ##
       # Pre-Stats Data Prep ####
## ----------------------------------- ##
# Pull in the datasets
sns.vs <- read.csv("./Data/sns-data_14-vs-18.csv")
sns.post <- read.csv("./Data/sns-data_post-trt.csv")

# Re-level the factors of both
unique(sns.vs$Herbicide.Treatment); unique(sns.post$Herbicide.Treatment)
sns.vs$Herbicide.Treatment <- factor(as.character(sns.vs$Herbicide.Treatment),
                                  levels = c("Con", "Spr", "SnS"))
sns.post$Herbicide.Treatment <- factor(as.character(sns.post$Herbicide.Treatment),
                                  levels = c("Con", "Spr", "SnS"))
unique(sns.vs$Herbicide.Treatment); unique(sns.post$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr.vs <- subset(sns.vs, sns.vs$Treatment == "GB")
ugr.vs <- subset(sns.vs, sns.vs$Treatment == "None")
cgr.post <- subset(sns.post, sns.post$Treatment == "GB")
ugr.post <- subset(sns.post, sns.post$Treatment == "None")

## ----------------------------------- ##
 # Post-Treatment Grazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(cgr.post)

# CSG ~ treatment * year
anova(lmer(CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# WSG ~ treatment * year
anova(lmer(WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Fescue ~ treatment * year
anova(lmer(Fescue ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Seedmix ~ treatment * year
anova(lmer(Seedmix ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Forbs ~ treatment * year
anova(lmer(Forbs ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Legumes ~ treatment * year
anova(lmer(Legumes ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Woody ~ treatment * year
anova(lmer(Woody ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Panic Grass ~ treatment * year
anova(lmer(Panic ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Heavy CSG ~ treatment * year
anova(lmer(Hvy.CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Heavy WSG ~ treatment * year
anova(lmer(Hvy.WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Heavy Fescue ~ treatment * year
anova(lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

  ## Pairwise comparisons
cgr.post.hvy.fsc <- lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post)
lsmeans(cgr.post.hvy.fsc, pairwise ~ Herbicide.Treatment)

# Bare  ~ treatment * year
anova(lmer(Bare ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Litter Cover  ~ treatment * year
anova(lmer(Litter ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Robel  ~ treatment * year
anova(lmer(Robel ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

# Litter Depth ~ treatment * year
anova(lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post))

## ----------------------------------- ##
 # Post-Treatment Ungrazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(ugr.post)

# CSG ~ treatment * year
anova(lmer(CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# WSG ~ treatment * year
anova(lmer(WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Fescue ~ treatment * year
anova(lmer(Fescue ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Seedmix ~ treatment * year
anova(lmer(Seedmix ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Forbs ~ treatment * year
anova(lmer(Forbs ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Legumes ~ treatment * year
anova(lmer(Legumes ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Woody ~ treatment * year
anova(lmer(Woody ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Panic Grass ~ treatment * year
anova(lmer(Panic ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Heavy CSG ~ treatment * year
anova(lmer(Hvy.CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Heavy WSG ~ treatment * year
anova(lmer(Hvy.WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Heavy Fescue ~ treatment * year
anova(lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Bare  ~ treatment * year
anova(lmer(Bare ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Litter Cover  ~ treatment * year
anova(lmer(Litter ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Robel  ~ treatment * year
anova(lmer(Robel ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

# Litter Depth ~ treatment * year
anova(lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post))

## ----------------------------------- ##
    # 14 vs. 18 Grazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(cgr.vs)

# CSG ~ treatment * year
anova(lmer(CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# WSG ~ treatment * year
anova(lmer(WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Fescue ~ treatment * year
anova(lmer(Fescue ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Seedmix ~ treatment * year
anova(lmer(Seedmix ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Forbs ~ treatment * year
anova(lmer(Forbs ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Legumes ~ treatment * year
anova(lmer(Legumes ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Woody ~ treatment * year
anova(lmer(Woody ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Panic Grass ~ treatment * year
anova(lmer(Panic ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Heavy CSG ~ treatment * year
anova(lmer(Hvy.CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Heavy WSG ~ treatment * year
anova(lmer(Hvy.WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Heavy Fescue ~ treatment * year
anova(lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Bare  ~ treatment * year
anova(lmer(Bare ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Litter Cover  ~ treatment * year
anova(lmer(Litter ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Robel  ~ treatment * year
anova(lmer(Robel ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

# Litter Depth ~ treatment * year
anova(lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs))

## ----------------------------------- ##
  # 14 vs. 18 Ungrazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(ugr.vs)

# CSG ~ treatment * year
anova(lmer(CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# WSG ~ treatment * year
anova(lmer(WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Fescue ~ treatment * year
anova(lmer(Fescue ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Seedmix ~ treatment * year
anova(lmer(Seedmix ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Forbs ~ treatment * year
anova(lmer(Forbs ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Legumes ~ treatment * year
anova(lmer(Legumes ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Woody ~ treatment * year
anova(lmer(Woody ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Panic Grass ~ treatment * year
anova(lmer(Panic ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Heavy CSG ~ treatment * year
anova(lmer(Hvy.CSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Heavy WSG ~ treatment * year
anova(lmer(Hvy.WSG ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Heavy Fescue ~ treatment * year
anova(lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Bare  ~ treatment * year
anova(lmer(Bare ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Litter Cover  ~ treatment * year
anova(lmer(Litter ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Robel  ~ treatment * year
anova(lmer(Robel ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# Litter Depth ~ treatment * year
anova(lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.vs))

# END ####
