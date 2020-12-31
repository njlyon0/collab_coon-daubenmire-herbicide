##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures FOR OBJECTIVE #1
  ## Objective # 1 --- Functional Group Response

# START ####

# Required libraries
library(Rmisc) # get summary values for plotting
library(ggplot2); library(cowplot); library(gridExtra); library(egg) # Plotting

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
sns.14 <- read.csv("./Data/sns-data_2014_summarized.csv")

# Re-level the treatment factor
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)
sns.14$Herbicide.Treatment <- factor(as.character(sns.14$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns.14$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")
cgr.14 <- subset(sns.14, sns.14$Treatment == "GB")
ugr.14 <- subset(sns.14, sns.14$Treatment == "None")

# Plotting shortcuts
sns.labs <- c("Con", "Spr", "SnS")
#cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red - draft1
cgr.colors <- c("Con" = "#ac261f", "Spr" = "#f77d73", "SnS" = "#fdbb7a") # shades of red
#ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue - draft 1
ugr.colors <- c("Con" = "#30517d", "Spr" = "#61a2cb", "SnS" = "#bfe2ee") # shades of blue

dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none",
                    axis.text = element_text(size = 14), axis.title = element_text(size = 16))
no.y.axis <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(), axis.line.y = element_blank())

##  -------------------------------------------------------------------------  ##
                                # CSG ####
##  -------------------------------------------------------------------------  ##


#get summary stats
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))


# Plot pre-treatment differences - DONE
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.CSG)) +
  geom_errorbar(aes(ymax = Avg.CSG + SE.CSG, ymin = Avg.CSG - SE.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 55) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 55) +
  labs(x = "Pre-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme; cgr.csg14.plt

#DONE
ugr.csg14.plt <- ggplot(ugr.14 , aes(x = Herbicide.Treatment, y = Avg.CSG)) +
  geom_errorbar(aes(ymax = Avg.CSG + SE.CSG, ymin = Avg.CSG - SE.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 80) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 80) +
  labs(x = "Pre-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme; ugr.csg14.plt


# Plot the post-treatment responses - DONE
cgr.csg.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  geom_text(label = "a", x = 0.9, y = 21) +
  geom_text(label = "ab", x = 1.8, y = 30) +
  geom_text(label = "b", x = 2.9, y = 34) +
  ylim(0, 55) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.csg.plt

#DONE
ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(x = Year, y = CSG, shape = Herbicide.Treatment)) +
  #geom_boxplot(outlier.shape = 21) +
  geom_errorbar(aes(ymax = CSG + se, ymin = CSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  scale_fill_manual(values = ugr.colors) +
  scale_color_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  #geom_text(label = "a", x = 0.9, y = 63) +
  #geom_text(label = "ab", x = 1.8, y = 44) +
  #geom_text(label = "b", x = 2.9, y = 35) +
  ylim(0, 80) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.csg.plt

# Check out all four plots together
egg::ggarrange(cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
csg.gb.plt <- egg::ggarrange(cgr.csg14.plt, cgr.csg.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/CSG_GB.pdf", width = 8, height = 6, units = 'in', plot = csg.gb.plt)

csg.hay.plt <- egg::ggarrange(ugr.csg14.plt, ugr.csg.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/CSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = csg.hay.plt)

##  -------------------------------------------------------------------------  ##
                                 # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff - DONE
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.WSG)) +
  geom_errorbar(aes(ymax = Avg.WSG + SE.WSG, ymin = Avg.WSG - SE.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 40) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 40) +
  labs(x = "Pre-Treatment", y = "Warm-Season Grass (%)") +
  pref.theme; cgr.wsg14.plt

#DONE
ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.WSG)) +
  geom_errorbar(aes(ymax = Avg.WSG + SE.WSG, ymin = Avg.WSG - SE.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 45) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Pre-Treatment", y = "Warm-Season Grass (%)") +
  pref.theme; ugr.wsg14.plt

# Plots - DONE
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = WSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype=2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
 # geom_text(label = "NS", x = 14.8, y = 40, color = "black") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 40) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.wsg.plt

#DONE
ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(x = Year, y = WSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "NS", x = 14.8, y = 45, color = "black") +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 45) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.2, 0.9)) + no.y.axis; ugr.wsg.plt

# Check out all four plots together
egg::ggarrange(cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
wsg.gb.plt <- egg::ggarrange(cgr.wsg14.plt, cgr.wsg.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/WSG_GB.pdf", width = 8, height = 6, units = 'in', plot = wsg.gb.plt)

wsg.hay.plt <- egg::ggarrange(ugr.wsg14.plt, ugr.wsg.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/WSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = wsg.hay.plt)

##  -------------------------------------------------------------------------  ##
                               # Fescue ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff - DONE
cgr.fsc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Fescue)) +
  geom_errorbar(aes(ymax = Avg.Fescue + SE.Fescue, ymin = Avg.Fescue - SE.Fescue, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 75) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 90) +
  labs(x = "Pre-Treatment", y = "Fescue (%)") +
  pref.theme; cgr.fsc14.plt

#DONE
ugr.fsc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Fescue)) +
  geom_errorbar(aes(ymax = Avg.Fescue + SE.Fescue, ymin = Avg.Fescue - SE.Fescue, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 60) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 60) +
  labs(x = "Pre-Treatment", y = "Fescue (%)") +
  pref.theme; ugr.fsc14.plt

# Plots 
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Fescue, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "a", x = 14.7, y = 45, color = "black") +
  geom_text(label = "b", x = 14.7, y = 20, color = "black") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 90) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.fsc.plt

#DONE
ugr.fsc.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Fescue, fill = Herbicide.Treatment)) +
    geom_boxplot(outlier.shape = 21) +
    scale_fill_manual(values = ugr.colors) +
    geom_text(label = "a", x = 0.9, y = 21) +
    geom_text(label = "ab", x = 1.8, y = 30) +
    geom_text(label = "b", x = 2.9, y = 34) +
  #geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se, color = Herbicide.Treatment),
  #              width = 0.3, position = dodge) +
  #geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  #geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "a", x = 14.7, y = 22, color = "black") +
  #geom_text(label = "b", x = 14.7, y = 4, color = "black") +
  #scale_color_manual(values = ugr.colors) +
  #scale_fill_manual(values = ugr.colors) +
 # scale_shape_manual(values = 21:23) +
  ylim(0, 60) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.fsc.plt


# Check out all four plots together
egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt, ugr.fsc.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
fsc.gb.plt <- egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Fescue_GB.pdf", width = 8, height = 6, units = 'in', plot = fsc.gb.plt)

fsc.hay.plt <- egg::ggarrange(ugr.fsc14.plt, ugr.fsc.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Fescue_Hay.pdf", width = 8, height = 6, units = 'in', plot = fsc.hay.plt)

##  -------------------------------------------------------------------------  ##
                         # Seed-mix Proportion ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff - DONE
cgr.smx14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Seedmix)) +
  geom_errorbar(aes(ymax = Avg.Seedmix + SE.Seedmix, ymin = Avg.Seedmix - SE.Seedmix, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 0.13) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 0.2) +
  labs(x = "Pre-Treatment", y = "Seedmix (Proportion >25%)") +
  pref.theme; cgr.smx14.plt

#DONE
ugr.smx14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Seedmix)) +
  geom_errorbar(aes(ymax = Avg.Seedmix + SE.Seedmix, ymin = Avg.Seedmix - SE.Seedmix, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 0.18) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 0.18) +
  labs(x = "Pre-Treatment", y = "Seedmix (Proportion >25%)") +
  pref.theme; ugr.smx14.plt

# Plots - DONE
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(x = Year, y = Seedmix)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment, shape = Herbicide.Treatment), position = dodge, size = 2.5) +
 # geom_text(label = "NS", x = 14.7, y = 0.18, color = "black") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 0.2) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.2, 0.9)) + no.y.axis; cgr.smx.plt

#DONE
ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(x = Year, y = Seedmix, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "NS", x = 14.7, y = 0.18, color = "black") +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 0.18) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.smx.plt

# Check out all four plots together
egg::ggarrange(cgr.smx14.plt, cgr.smx.plt, ugr.smx14.plt, ugr.smx.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
smx.gb.plt <- egg::ggarrange(cgr.smx14.plt, cgr.smx.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Seedmix_GB.pdf", width = 8, height = 6, units = 'in', plot = smx.gb.plt)

smx.hay.plt <- egg::ggarrange(ugr.smx14.plt, ugr.smx.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Seedmix_Hay.pdf", width = 8, height = 6, units = 'in', plot = smx.hay.plt)

##  -------------------------------------------------------------------------  ##
                                # Forbs ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff - DONE
cgr.frb14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Forbs)) +
  geom_errorbar(aes(ymax = Avg.Forbs + SE.Forbs, ymin = Avg.Forbs - SE.Forbs, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 75) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 60) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; cgr.frb14.plt

#DONE
ugr.frb14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Forbs)) +
  geom_errorbar(aes(ymax = Avg.Forbs + SE.Forbs, ymin = Avg.Forbs - SE.Forbs, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 60) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 75) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; ugr.frb14.plt

# Plots - DONE
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(x = Year, y = Forbs, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype=2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "a", x = 14.7, y = 32, color = "black") +
  #geom_text(label = "b", x = 14.7, y = 18, color = "black") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 60) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.frb.plt

#DONE
ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(x = Year, y = Forbs, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype=2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "a", x = 14.7, y = 52, color = "black") +
  #geom_text(label = "b", x = 14.7, y = 27, color = "black") +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 75) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.frb.plt

# Check out all four plots together
egg::ggarrange(cgr.frb14.plt, cgr.frb.plt, ugr.frb14.plt, ugr.frb.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
frb.gb.plt <- egg::ggarrange(cgr.frb14.plt, cgr.frb.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Forbs_GB.pdf", width = 8, height = 6, units = 'in', plot = frb.gb.plt)

frb.hay.plt <- egg::ggarrange(ugr.frb14.plt, ugr.frb.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Forbs_Hay.pdf", width = 8, height = 6, units = 'in', plot = frb.hay.plt)

##  -------------------------------------------------------------------------  ##
                                # Legumes ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))

library("ggeffects")
library("lme4")

#legume_model_gr = lmer(Legumes ~ Herbicide.Treatment * year2 + (1|Pasture), data = cgr.post, REML=FALSE)
#ggpredict(legume_model_gr,c("Herbicide.Treatment", "year2 [1,2,3,4]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE)
#legume_model_gr_Pred = as.data.frame(ggpredict(legume_model_gr,c("Herbicide.Treatment", "year2 [1,2,3,4]"),ci.lvl=0.85, back.transform=TRUE, append=TRUE))
#colnames(legume_model_gr_Pred)=c("Herbicide.Treatment", "Predicted","SE","Lower","Upper","year2") #renames columns
##View(Orth_Pred) 
#confint(Orth_Top, level = 0.85)

#legume_model_gr_sum= legume_model_gr_Pred %>% 
#  group_by(year2) %>% 
#  summarise_at(vars(Predicted, Lower, Upper), mean)

# Pre-treatment stuff -DONE
cgr.lgm14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Legumes)) +
  geom_errorbar(aes(ymax = Avg.Legumes + SE.Legumes, ymin = Avg.Legumes - SE.Legumes, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 50) +
  labs(x = "Pre-Treatment", y = "Legumes (%)") +
  pref.theme; cgr.lgm14.plt

#DONE
ugr.lgm14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Legumes)) +
  geom_errorbar(aes(ymax = Avg.Legumes + SE.Legumes, ymin = Avg.Legumes - SE.Legumes, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(-2, 50) +
  labs(x = "Pre-Treatment", y = "Legumes (%)") +
  pref.theme; ugr.lgm14.plt

# Plots - DONE

#trialplot = ggplot(legume_model_gr_sum, aes(x=year2,y=Predicted))+
#  geom_point(aes(x=year2,y=Predicted))+
#  ylim(0, 50) 
#trialplot

cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(x = Year, y = Legumes)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment, shape=Herbicide.Treatment), position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 50) +
  labs(x = "Post-Treatment")+
  pref.theme + theme(legend.position = c(0.1, 0.9)) + no.y.axis; cgr.lgm.plt

#DONE
ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(x = Year, y = Legumes, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  #geom_text(label = "NS", x = 14.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 50) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.lgm.plt

# Check out all four plots together
egg::ggarrange(cgr.lgm14.plt, cgr.lgm.plt, ugr.lgm14.plt, ugr.lgm.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
lgm.gb.plt <- egg::ggarrange(cgr.lgm14.plt, cgr.lgm.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Legumes_GB.pdf", width = 8, height = 6, units = 'in', plot = lgm.gb.plt)

lgm.hay.plt <- egg::ggarrange(ugr.lgm14.plt, ugr.lgm.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Legumes_Hay.pdf", width = 8, height = 6, units = 'in', plot = lgm.hay.plt)

##  -------------------------------------------------------------------------  ##
                                # Woody ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.wdy14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Woody)) +
  geom_errorbar(aes(ymax = Avg.Woody + SE.Woody, ymin = Avg.Woody - SE.Woody, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 7) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 7) +
  labs(x = "Pre-Treatment", y = "Woody (%)") +
  pref.theme; cgr.wdy14.plt

ugr.wdy14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Woody)) +
  geom_errorbar(aes(ymax = Avg.Woody + SE.Woody, ymin = Avg.Woody - SE.Woody, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 7) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Pre-Treatment", y = "Woody (%)") +
  pref.theme; ugr.wdy14.plt

# Plots
cgr.wdy.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  geom_text(label = "a", x = 0.8, y = 4.3, color = "black") +
  geom_text(label = "ab", x = 1.7, y = 4, color = "black") +
  geom_text(label = "b", x = 2.8, y = 1.4, color = "black") +
  ylim(0, 7) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.wdy.plt 

ugr.wdy.plt <- ggplot(ugr.wdy.pltdf, aes(x = Year, y = Woody, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Woody + se, ymin = Woody - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 7) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 7) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.wdy.plt

# Check out all four plots together
egg::ggarrange(cgr.wdy14.plt, cgr.wdy.plt, ugr.wdy14.plt, ugr.wdy.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
wdy.gb.plt <- egg::ggarrange(cgr.wdy14.plt, cgr.wdy.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Woody_GB.pdf", width = 8, height = 6, units = 'in', plot = wdy.gb.plt)

wdy.hay.plt <- egg::ggarrange(ugr.wdy14.plt, ugr.wdy.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Woody_Hay.pdf", width = 8, height = 6, units = 'in', plot = wdy.hay.plt)

##  -------------------------------------------------------------------------  ##
                    # Panic Proportion Occurrence ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = c("Year","Herbicide.Treatment"))
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff - DONE
cgr.pnc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Panic)) +
  geom_errorbar(aes(ymax = Avg.Panic + SE.Panic, ymin = Avg.Panic - SE.Panic, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 0.85) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 1) +
  labs(x = "Pre-Treatment", y = "Panic (Proportion Present)") +
  pref.theme; cgr.pnc14.plt

#DONE
ugr.pnc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Panic)) +
  geom_errorbar(aes(ymax = Avg.Panic + SE.Panic, ymin = Avg.Panic - SE.Panic, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 0.85) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 1) +
  labs(x = "Pre-Treatment", y = "Panic (Proportion Present)") +
  pref.theme; ugr.pnc14.plt

# Plots - DONE
cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(x = Year, y = Panic)) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F, color="Black") +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment, shape=Herbicide.Treatment), position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 1) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.pnc.plt

#DONE
ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(x = Year, y = Panic, shape = Herbicide.Treatment)) +
  #geom_boxplot(outlier.shape = 21) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment, shape=Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "a", x = 0.8, y = 0.1, color = "black") +
#  geom_text(label = "ab", x = 1.7, y = 0.35, color = "black") +
#  geom_text(label = "b", x = 2.8, y = 0.33, color = "black") +
  scale_fill_manual(values = ugr.colors) +
  scale_color_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 1) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.pnc.plt 

# Check out all four plots together
egg::ggarrange(cgr.pnc14.plt, cgr.pnc.plt, ugr.pnc14.plt, ugr.pnc.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
pnc.gb.plt <- egg::ggarrange(cgr.pnc14.plt, cgr.pnc.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 1/Panic_GB.pdf", width = 8, height = 6, units = 'in', plot = pnc.gb.plt)

pnc.hay.plt <- egg::ggarrange(ugr.pnc14.plt, ugr.pnc.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 1/Panic_Hay.pdf", width = 8, height = 6, units = 'in', plot = pnc.hay.plt)

# END ####


