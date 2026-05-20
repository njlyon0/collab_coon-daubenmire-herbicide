##  ------------------------------------------------------------------------------------------  ##
                # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  ------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon and edited by Jaime J Coon



# PURPOSE ####
  ## The revise & resubmit review we got at Rangeland Ecology and Management requires a ∆ of stats
  ## So, rather that changing the *many* tests we ran prior to this
  ## I'm just going to write a totally new script for just the final version of this paper
  ## If you're a reader of the paper, this includes 100% of the results in the publication

# Set the working directory
setwd("/cloud/project/")
  ## if you're not me you will need to re-set this to your own computer.

# Clear the environment
#rm(list = ls())

# Load required libraries

library(lme4); library(lmerTest); library(lsmeans); library(emmeans); library(tidyverse)

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

cgr.vs <- subset(sns.vs, sns.vs$Treatment == "GB")
ugr.vs <- subset(sns.vs, sns.vs$Treatment == "None")

cgr.post$year2=(cgr.post$Year-14)
ugr.post$year2=(ugr.post$Year-14) #recoding year to be 1,2,3,4

#determining forb averages for the 25% threshold

mean(ugr.vs$Forbs) #27%
mean(cgr.vs$Forbs) #20%


## ----------------------------------- ##
 # Post-Treatment Grazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(cgr.post)


# CSG ~ treatment * year
CSG_model_gr=lmer(CSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(CSG_model_gr)
anova(CSG_model_gr)
lsmeans(CSG_model_gr, pairwise ~ Herbicide.Treatment*year2, adjust="none")
        

# WSG ~ treatment * year
WSG_model_gr=(lmer(WSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE))
summary(WSG_model_gr)
anova(WSG_model_gr)
#lsmeans(WSG_model_gr, pairwise ~ Herbicide.Treatment*year2, adjust="none")


# Fescue ~ treatment * year
fescue_model_gr=lmer(Fescue ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(fescue_model_gr)
anova(fescue_model_gr)
lsmeans(fescue_model_gr, pairwise ~ Herbicide.Treatment*year2, adjust="none")

# Seedmix ~ treatment * year
seedmix_model_gr = lmer(Seedmix ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(seedmix_model_gr)
anova(seedmix_model_gr)
#lsmeans(seedmix_model_gr, pairwise ~ Herbicide.Treatment*year2, adjust="none")


# Forbs ~ treatment * year
forb_model_gr = lmer(Forbs ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(forb_model_gr)
anova(forb_model_gr)
#lsmeans(forb_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Legumes ~ treatment * year
legume_model_gr = lmer(Legumes ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(legume_model_gr)
anova(legume_model_gr)
#lsmeans(legume_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Woody ~ treatment * year
woody_model_gr = lmer(Woody ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(woody_model_gr)
anova(woody_model_gr)

# Panic Grass ~ treatment * year
panic_model_gr = lmer(Panic ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
summary(panic_model_gr)
anova(panic_model_gr)
#lsmeans(panic_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Heavy CSG ~ treatment * year
HvyCSG_model_gr = lmer(Hvy.CSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
anova(HvyCSG_model_gr)
summary(HvyCSG_model_gr)
#lsmeans(HvyCSG_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Heavy WSG ~ treatment * year
HvyWSG_model_gr = lmer(Hvy.WSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
anova(HvyWSG_model_gr)
summary(HvyWSG_model_gr)
#lsmeans(HvyWSG_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Heavy Fescue ~ treatment * year
HvyFesc_model_gr = lmer(Hvy.Fesc ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post, REML=FALSE)
anova(HvyFesc_model_gr)
summary(HvyFesc_model_gr)
lsmeans(HvyFesc_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


#does not converge with patch
# Bare  ~ treatment * year
bare_model_gr = lmer(Bare ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
anova(bare_model_gr)
summary(bare_model_gr)
#lsmeans(bare_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Litter Cover  ~ treatment * year
litter_model_gr = lmer(Litter ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
anova(litter_model_gr)
summary(litter_model_gr)
#lsmeans(litter_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Robel  ~ treatment * year
Robel_model_gr = lmer(Robel ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
anova(Robel_model_gr)
summary(Robel_model_gr)
lsmeans(Robel_model_gr, pairwise ~ Herbicide.Treatment*year2, adjust="none",mode="satterthwaite")


# Litter Depth ~ treatment * year
litdep_model_gr = lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.post, REML=FALSE)
anova(litdep_model_gr)
summary(litdep_model_gr)
#lsmeans(litdep_model_gr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

## ----------------------------------- ##
 # Post-Treatment Ungrazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(ugr.post)

# CSG ~ treatment * year
CSG_model_ugr = lmer(CSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(CSG_model_ugr)
anova(CSG_model_ugr)
lsmeans(CSG_model_ugr, pairwise ~ Herbicide.Treatment*year2, adjust="none",mode="satterthwaite")

# WSG ~ treatment * year
WSG_model_ugr = lmer(WSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(WSG_model_ugr)
anova(WSG_model_ugr)
lsmeans(WSG_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Fescue ~ treatment * year
fescue_model_ugr = lmer(Fescue ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(fescue_model_ugr)
anova(fescue_model_ugr)
lsmeans(fescue_model_ugr, pairwise ~ Herbicide.Treatment*year2, adjust="none",mode="satterthwaite")


# Seedmix ~ treatment * year
seedmix_model_ugr = lmer(Seedmix ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(seedmix_model_ugr)
anova(seedmix_model_ugr)
#lsmeans(seedmix_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Forbs ~ treatment * year
forb_model_ugr = lmer(Forbs ~ Herbicide.Treatment * Year + (1|Pasture), data = ugr.post, REML=FALSE)
summary(forb_model_ugr)
anova(forb_model_ugr)
lsmeans(forb_model_ugr, pairwise ~ Herbicide.Treatment*year2, adjust="none",mode="satterthwaite")


# Legumes ~ treatment * year
legume_model_ugr = lmer(Legumes ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(legume_model_ugr)
anova(legume_model_ugr)
lsmeans(legume_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Woody ~ treatment * year
woody_model_ugr = lmer(Woody ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(woody_model_ugr)
anova(woody_model_ugr)
#lsmeans(woody_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")


# Panic Grass ~ treatment * year
panic_model_ugr = lmer(Panic ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(panic_model_ugr)
anova(panic_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Heavy CSG ~ treatment * year
HvyCSG_model_ugr = lmer(Hvy.CSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(HvyCSG_model_ugr)
anova(HvyCSG_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Heavy WSG ~ treatment * year
Hvy_WSG_model_ugr = lmer(Hvy.WSG ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(Hvy_WSG_model_ugr)
anova(Hvy_WSG_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Heavy Fescue ~ treatment * year
HvyFesc_model_ugr = lmer(Hvy.Fesc ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(HvyFesc_model_ugr)
anova(HvyFesc_model_ugr)
#lsmeans(HvyFesc_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Bare  ~ treatment * year
bare_model_ugr = lmer(Bare ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(bare_model_ugr)
anova(bare_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Litter Cover  ~ treatment * year
litter_model_ugr = lmer(Litter ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(litter_model_ugr)
anova(litter_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Robel  ~ treatment * year
Robel_model_ugr = lmer(Robel ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(Robel_model_ugr)
anova(Robel_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

# Litter Depth ~ treatment * year
litdep_model_ugr = lmer(LitDep ~ Herbicide.Treatment * year2 + (1|Pasture), data = ugr.post, REML=FALSE)
summary(litdep_model_ugr)
anova(litdep_model_ugr)
#lsmeans(panic_model_ugr, pairwise ~ Herbicide.Treatment, adjust="none",mode="satterthwaite")

## ----------------------------------- ##
    # 14 vs. 18 Grazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(cgr.vs)

# CSG ~ treatment * year
CSG_vs_model_gr = lmer(CSG ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(CSG_vs_model_gr)
anova(CSG_vs_model_gr)
lsmeans(CSG_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# WSG ~ treatment * year
WSG_vs_model_gr = lmer(WSG ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(WSG_vs_model_gr)
anova(WSG_vs_model_gr)
lsmeans(WSG_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Fescue ~ treatment * year
fescue_vs_model_gr = lmer(Fescue ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(fescue_vs_model_gr)
anova(fescue_vs_model_gr)
lsmeans(fescue_vs_model_gr, pairwise ~ Herbicide.Treatment*as.factor(Year), adjust="none",mode="satterthwaite")

# Seedmix ~ treatment * year
seedmix_vs_model_gr = lmer(Seedmix ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(seedmix_vs_model_gr)
anova(seedmix_vs_model_gr)
lsmeans(seedmix_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Forbs ~ treatment * year
forb_vs_model_gr = lmer(Forbs ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(forb_vs_model_gr)
anova(forb_vs_model_gr)
lsmeans(forb_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Legumes ~ treatment * year
legume_vs_model_gr = lmer(Legumes ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(legume_vs_model_gr)
anova(legume_vs_model_gr)
lsmeans(legume_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Woody ~ treatment * year
woody_vs_model_gr = lmer(Woody ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(woody_vs_model_gr)
anova(woody_vs_model_gr)
lsmeans(woody_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite", REML=FALSE)


# Panic Grass ~ treatment * year
panic_vs_model_gr = lmer(Panic ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(panic_vs_model_gr)
anova(panic_vs_model_gr)
lsmeans(panic_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# Heavy CSG ~ treatment * year
HvyCSG_vs_model_gr = lmer(Hvy.CSG ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(HvyCSG_vs_model_gr)
anova(HvyCSG_vs_model_gr)
lsmeans(HvyCSG_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# Heavy WSG ~ treatment * year
HvyWSG_vs_model_gr = lmer(Hvy.WSG ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(HvyWSG_vs_model_gr)
anova(HvyWSG_vs_model_gr)
lsmeans(HvyWSG_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# Heavy Fescue ~ treatment * year
HvyFesc_vs_model_gr = lmer(Hvy.Fesc ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(HvyFesc_vs_model_gr)
anova(HvyFesc_vs_model_gr)
lsmeans(HvyFesc_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# Bare  ~ treatment * year
bare_vs_model_gr = lmer(Bare ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(bare_vs_model_gr)
anova(bare_vs_model_gr)
lsmeans(bare_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Litter Cover  ~ treatment * year
litter_vs_model_gr = lmer(Litter ~ Herbicide.Treatment * as.factor(Year) + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(litter_vs_model_gr)
anova(litter_vs_model_gr)
lsmeans(litter_vs_model_gr, pairwise ~ Herbicide.Treatment* as.factor(Year), adjust="none",mode="satterthwaite")

# Robel  ~ treatment * year
robel_vs_model_gr = lmer(Robel ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(robel_vs_model_gr)
anova(robel_vs_model_gr)
lsmeans(robel_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# Litter Depth ~ treatment * year
litdep_vs_model_gr = lmer(LitDep ~ Herbicide.Treatment * Year + (1|Pasture), data = cgr.vs, REML=FALSE)
summary(litdep_vs_model_gr)
anova(litdep_vs_model_gr)
lsmeans(litdep_vs_model_gr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


## ----------------------------------- ##
  # 14 vs. 18 Ungrazed Analysis ####
## ----------------------------------- ##
# Double check the structure of the data
str(ugr.vs)

# CSG ~ treatment  * as.factor(Year)
CSG_vs_model_ugr = lmer(CSG ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(CSG_vs_model_ugr)
anova(CSG_vs_model_ugr)
lsmeans(CSG_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# WSG ~ treatment  * as.factor(Year)
WSG_vs_model_ugr = lmer(WSG ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(WSG_vs_model_ugr)
anova(WSG_vs_model_ugr)
lsmeans(WSG_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Fescue ~ treatment  * as.factor(Year)
fescue_vs_model_ugr = lmer(Fescue ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(fescue_vs_model_ugr)
anova(fescue_vs_model_ugr)
lsmeans(fescue_vs_model_ugr, pairwise ~ Herbicide.Treatment*(Year), adjust="none",mode="satterthwaite")


# Seedmix ~ treatment  * as.factor(Year)
seedmix_vs_model_ugr = lmer(Seedmix ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(seedmix_vs_model_ugr)
anova(seedmix_vs_model_ugr)
lsmeans(seedmix_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Forbs ~ treatment  * as.factor(Year)
forbs_vs_model_ugr = lmer(Forbs ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(forbs_vs_model_ugr)
anova(forbs_vs_model_ugr)


# Legumes ~ treatment  * as.factor(Year)
legumes_vs_model_ugr = lmer(Legumes ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(legumes_vs_model_ugr)
anova(legumes_vs_model_ugr)
lsmeans(legumes_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Woody ~ treatment  * as.factor(Year)
woody_vs_model_ugr = lmer(Woody ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(woody_vs_model_ugr)
anova(woody_vs_model_ugr)
lsmeans(woody_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Panic Grass ~ treatment  * as.factor(Year)
panic_vs_model_ugr = lmer(Panic ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(panic_vs_model_ugr)
anova(panic_vs_model_ugr)
lsmeans(panic_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Heavy CSG ~ treatment  * as.factor(Year)
HvyCSG_vs_model_ugr = lmer(Hvy.CSG ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(HvyCSG_vs_model_ugr)
anova(HvyCSG_vs_model_ugr)
lsmeans(HvyCSG_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Heavy WSG ~ treatment  * as.factor(Year)
HvyWSG_vs_model_ugr = lmer(Hvy.WSG ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(HvyWSG_vs_model_ugr)
anova(HvyWSG_vs_model_ugr)
lsmeans(HvyWSG_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Heavy Fescue ~ treatment  * as.factor(Year)
HvyFesc_vs_model_ugr = lmer(Hvy.Fesc ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(HvyFesc_vs_model_ugr)
anova(HvyFesc_vs_model_ugr)
lsmeans(HvyFesc_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Bare  ~ treatment  * as.factor(Year)
bare_vs_model_ugr = lmer(Bare ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(bare_vs_model_ugr)
anova(bare_vs_model_ugr)
lsmeans(bare_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Litter Cover  ~ treatment  * as.factor(Year)
litter_vs_model_ugr = lmer(Litter ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(litter_vs_model_ugr)
anova(litter_vs_model_ugr)
lsmeans(litter_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Robel  ~ treatment  * as.factor(Year)
Robel_vs_model_ugr = lmer(Robel ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(Robel_vs_model_ugr)
anova(Robel_vs_model_ugr)
lsmeans(Robel_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")


# Litter Depth ~ treatment  * as.factor(Year)
litdep_vs_model_ugr = lmer(LitDep ~ Herbicide.Treatment  * as.factor(Year) + (1|Pasture), data = ugr.vs, REML=FALSE)
summary(litdep_vs_model_ugr)
anova(litdep_vs_model_ugr)
lsmeans(litdep_vs_model_ugr, pairwise ~ Herbicide.Treatment*Year, adjust="none",mode="satterthwaite")

# END ####
