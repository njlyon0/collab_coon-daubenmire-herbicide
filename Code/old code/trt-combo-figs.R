##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                      # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
## Make publication-quality figures*
  ## That combine grazed and un-grazed plots into a single figure

# START ####

# Required libraries
library(Rmisc) # get summary values for plotting
library(ggplot2); library(cowplot) # Plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
#setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")
setwd("/cloud/project/")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                            # Houskeeping ####
##  ---------------------------------------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/sns-data_post-trt.csv")

# Re-level the treatment factor
unique(sns$Herbicide.Treatment)
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Paste the site-level management and the herbicide treatment together into a combo variable
unique(sns$Composite.Variable)
sns$Composite.Variable <- paste0(sns$Treatment, "-", sns$Herbicide.Treatment)
unique(sns$Composite.Variable)

# Re-level that variable
trt.levels <- c("GB-Con", "GB-Spr", "GB-SnS", "None-Con", "None-Spr", "None-SnS")
sns$Composite.Variable <- factor(sns$Composite.Variable, levels = trt.levels)
levels(sns$Composite.Variable)

# Plotting shortcuts
sns.labs <- c("Con", "Spr", "SnS")
trt.colors <- c("GB-Con" = "#d73027", "GB-Spr" = "#f46d43", "GB-SnS" = "#fdae61",
                "None-Con" = "#4575b4", "None-Spr" = "#74add1", "None-SnS" = "#abd9e9")
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank())

##  ---------------------------------------------------------------------------------------------  ##
            # Objective # 1 --- Functional Group Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                        #  CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
csg.pltdf <- summarySE(data = sns, measurevar = "CSG",
                       groupvars = c("Herbicide.Treatment", "Year", "Treatment", "Composite.Variable"))

ggplot(csg.pltdf, aes(x = Year, y = CSG, color = Composite.Variable, shape = Composite.Variable)) +
  geom_smooth(aes(group = Composite.Variable), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  labs(y = "Cool-Season Grass %", x = "Year") +
  scale_color_manual(values = trt.colors) +
  pref.theme + theme(legend.position = c(0, 0.8))


# END ####


