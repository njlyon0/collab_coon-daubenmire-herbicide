##  ----------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  ----------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon & edited by Jaime J Coon
#paired with stats from: rvzn-stats.R


# PURPOSE ####
  ## Create publication-quality figures with multiple panels
  ## Only the ones that selected for inclusion in the final manuscript

# Required libraries
library(Rmisc) # get summary values for plotting
library(ggplot2); library(cowplot); library(gridExtra); library(egg) # Plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
#setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")
setwd("/cloud/project/")
# Clear environment of other stuff
#rm(list = ls())

##  --------------------------------------------------------------  ##
                            # Data Prep ####
##  --------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/sns-data_post-trt.csv")
sns.14 <- read.csv("./Data/sns-data_2014_summarized.csv")
versus <- read.csv("./Data/sns-data_14-vs-18.csv")

# Re-level the treatment factor
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)
sns.14$Herbicide.Treatment <- factor(as.character(sns.14$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns.14$Herbicide.Treatment)
versus$Herbicide.Treatment <- factor(as.character(versus$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(versus$Herbicide.Treatment)

# Re-level the Composite Variable column for ease of plotting
#versus$Composite.Variable <- factor(versus$Composite.Variable,
#                                    levels = c("14-Con", "14-Spr", "14-SnS", "18-Con", "18-Spr", "18-SnS"))

versus$Composite.Variable <- factor(versus$Composite.Variable,
                                    levels = c("14-Con", "18-Con", "14-Spr", "18-Spr", "14-SnS","18-SnS"))

unique(versus$Composite.Variable)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")
cgr.14 <- subset(sns.14, sns.14$Treatment == "GB")
ugr.14 <- subset(sns.14, sns.14$Treatment == "None")
cgr.vs <- subset(versus, versus$Treatment == "GB")
ugr.vs <- subset(versus, versus$Treatment == "None")

# Plotting shortcuts
sns.labs <- c("Con", "Spr", "SnS")
#cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red - draft 1
cgr.colors <- c("Con" = "#b10026", "Spr" = "#fc4e2a", "SnS" = "#feb24c") # shades of red
#ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue - draft 1
ugr.colors <- c("Con" = "#225ea8", "Spr" = "#1d91c0", "SnS" = "#7fcdbb") # shades of blue
#ugr.colors.vs <-c("14"="#238443","18"="#c2e699")
#cgr.colors.vs <-c("14"="#88419d","18"="#8c96c6")
cgr.colors.vs <-c("14"="#b10026","18"="#feb24c")
ugr.colors.vs <-c("14"="#225ea8","18"="#7fcdbb")


dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none",
                    axis.text = element_text(size = 10), axis.title = element_text(size = 12))
no.y.axis <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(), axis.line.y = element_blank())

##  --------------------------------------------------------------  ##
                    # Figure 1 -- Grasses ####
##  --------------------------------------------------------------  ##
# INCLUDES
  ## 2014 & post-treatment data
  ## Fescue
  ## CSG
  ## WSG

# Get the summary values for Fescue, WSGs, and CSG (not all will be used if year is p>0.1)
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment fescue plots
cgr.fsc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Fescue)) +
  geom_errorbar(aes(ymax = Avg.Fescue + SE.Fescue, ymin = Avg.Fescue - SE.Fescue, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 90) +
  labs(x = "Pre-Treatment", y = "Fescue (%)") +
  pref.theme; cgr.fsc14.plt

ugr.fsc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Fescue)) +
  geom_errorbar(aes(ymax = Avg.Fescue + SE.Fescue, ymin = Avg.Fescue - SE.Fescue, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 60) +
  labs(x = "Pre-Treatment", y = "Fescue (%)") +
  pref.theme; ugr.fsc14.plt

# Post-treatment fescue plots
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Fescue, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  annotate("text", x=14.6,y=45,size=5,label="a") +
  annotate("text", x=14.6,y=20,size=5,label="b") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 90) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis + theme(legend.position = c(0.2, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  annotate("text", x=0.8,y=27,size=5,label="a") +
  annotate("text", x=1.8,y=10,size=5,label="b") +
  annotate("text", x=2.8,y=7,size=5,label="b") +

  #geom_text(label = "a", x = 0.9, y = 21) +
  #geom_text(label = "ab", x = 1.8, y = 30) +
  #geom_text(label = "b", x = 2.9, y = 34) +
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



# Make the first row of the figure
fsc.plt <- egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt, ugr.fsc.plt,
                  nrow = 1, ncol = 4, widths = c(1, 2.5, 1, 2.5), labels = c("A", "",  "",  ""))

# Plot pre-treatment values for CSG
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.CSG)) +
  geom_errorbar(aes(ymax = Avg.CSG + SE.CSG, ymin = Avg.CSG - SE.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 55) +
  labs(x = "Pre-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme; cgr.csg14.plt

ugr.csg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.CSG)) +
  geom_errorbar(aes(ymax = Avg.CSG + SE.CSG, ymin = Avg.CSG - SE.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 80) +
  labs(x = "Pre-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme; ugr.csg14.plt

# Plot the post-treatment responses for CSG - DONE
cgr.csg.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  #geom_text(label = "a", x = 0.9, y = 21, size = 6) +
  annotate("text",label = "a",  x = 0.8, y = 22, size = 4.2) +
  annotate("text",label = "ab", x = 1.7, y = 31, size = 4.2) +
  annotate("text",label = "b",  x = 2.8, y = 35, size = 4.2) +
  ylim(0, 55) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.csg.plt

#
ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(x = Year, y = CSG, shape = Herbicide.Treatment)) +
  #geom_boxplot(outlier.shape = 21) +
  geom_errorbar(aes(ymax = CSG + se, ymin = CSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  scale_fill_manual(values = ugr.colors) +
  scale_color_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  annotate("text",label = "Treatment x Year", x = 17, y = 75, size = 4.2) +
  ylim(0, 80) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.csg.plt
# Make the second row of the figure
csg.plt <- egg::ggarrange(cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt,
                          nrow = 1, ncol = 4, widths = c(1, 2.5, 1, 2.5), labels = c("B", "",  "",  ""))

# Pre-treatment stuff for WSGs
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.WSG)) +
  geom_errorbar(aes(ymax = Avg.WSG + SE.WSG, ymin = Avg.WSG - SE.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 40) +
  labs(x = "Pre-Treatment", y = "Warm-Season Grass (%)") +
  pref.theme; cgr.wsg14.plt

ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.WSG)) +
  geom_errorbar(aes(ymax = Avg.WSG + SE.WSG, ymin = Avg.WSG - SE.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Pre-Treatment", y = "Warm-Season Grass (%)") +
  pref.theme; ugr.wsg14.plt

# Post-treatment WSG responses
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = WSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
 # geom_text(label = "NS", x = 14.8, y = 40, color = "black", size = 6) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 40) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(x = Year, y = WSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 1) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "NS", x = 14.8, y = 45, color = "black", size = 6) +
  annotate("text",label = "Treatment x Year", x = 15.8, y = 30, size = 4.2) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 45) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.wsg.plt

# Make the third-and final-row of the figure
wsg.plt <- egg::ggarrange(cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt,
                          nrow = 1, ncol = 4, widths = c(1, 2.5, 1, 2.5), labels = c("C", "",  "",  ""))

# Make the figure with all of the rows included
fig.1 <- egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt, ugr.fsc.plt,
                        cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt,
                        cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt,
                        nrow = 3, ncol = 4, widths = c(1, 2.5, 1, 2.5),
                        labels = c("A", "",  "",  "",
                                   "B", "",  "",  "",
                                   "C", "",  "",  ""))

# Save out the larger figure
ggplot2::ggsave("./Figures/For Manuscript/Figure-1.pdf", width = 7.5, height = 9, units = 'in', plot = fig.1)

##  --------------------------------------------------------------  ##
                        # Figure 2 -- Photos ####
##  --------------------------------------------------------------  ##
# Not relevant to me, just included here so it is clear I didn't skip #2 accidentally.

##  --------------------------------------------------------------  ##
                # Figure 3 -- Forbs & Seedmix ####
##  --------------------------------------------------------------  ##
# INCLUDES
  ## 2014 & post-treatment data
  ## Forbs
  ## Seedmix

# Get summary stats for plotting
cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.frb14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Forbs)) +
  geom_errorbar(aes(ymax = Avg.Forbs + SE.Forbs, ymin = Avg.Forbs - SE.Forbs, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 80) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; cgr.frb14.plt


ugr.frb14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Forbs)) +
  geom_errorbar(aes(ymax = Avg.Forbs + SE.Forbs, ymin = Avg.Forbs - SE.Forbs, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 80) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; ugr.frb14.plt

# Plots
cgr.frb.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  annotate("text",label = "a",  x = 0.8, y = 28, size = 4.2) +
  annotate("text",label = "b", x = 1.8, y = 41, size = 4.2) +
  annotate("text",label = "b",  x = 2.8, y = 45, size = 4.2) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 80) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.frb.plt


ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(x = Year, y = Forbs, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype=2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  annotate("text", label = "a", x = 14.7, y = 52, size = 4.2) +
  annotate("text", label = "b", x = 14.6, y = 27, size = 4.2) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 80) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.frb.plt

# Pre-treatment seedmix graphs
cgr.smx14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Seedmix)) +
  geom_errorbar(aes(ymax = Avg.Seedmix + SE.Seedmix, ymin = Avg.Seedmix - SE.Seedmix, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 0.2) +
  labs(x = "Pre-Treatment", y = "Seedmix (Proportion >25%)") +
  pref.theme; cgr.smx14.plt

ugr.smx14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Seedmix)) +
  geom_errorbar(aes(ymax = Avg.Seedmix + SE.Seedmix, ymin = Avg.Seedmix - SE.Seedmix, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(-0.001, 0.18) +
  labs(x = "Pre-Treatment", y = "Seedmix (Proportion >25%)") +
  pref.theme; ugr.smx14.plt

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(x = Year, y = Seedmix)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(method = "lm", se = F, color = "black") +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment, shape = Herbicide.Treatment), position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 0.2) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(x = Year, y = Seedmix, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "NS", x = 14.8, y = 0.18, color = "black", size = 6) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(-0.001, 0.18) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.smx.plt

# Make the figure all at once
fig.2 <- egg::ggarrange(cgr.frb14.plt, cgr.frb.plt, ugr.frb14.plt, ugr.frb.plt,
                        cgr.smx14.plt, cgr.smx.plt, ugr.smx14.plt, ugr.smx.plt,
                        nrow = 2, ncol = 4, widths = c(1, 2.5, 1, 2.5),
                        labels = c("A", "",  "",  "",
                                   "B", "",  "",  ""))

# Save out this larger figure
ggplot2::ggsave("./Figures/For Manuscript/Figure-3.pdf", width = 7.5, height = 6, units = 'in', plot = fig.2)

##  --------------------------------------------------------------  ##
              # Figure 4 -- Grasses '14 vs. '18 ####
##  --------------------------------------------------------------  ##
# INCLUDES
  ## 2014 vs. 2018 data
  ## Fescue
  ## CSG
  ## WSG

# Fescue plots
cgr.vs.fsc.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Fescue, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors.vs,labels = c("Pre-treatment (2014)", "Post-treatment (2018)")) +
  labs(x = "Year-Treatment", y = "Fescue (%)")+
#, title = "Grazed") +
  annotate("text", x=1,  y=77, size=5, label="a") +
  annotate("text", x=2,  y=52, size=5, label="b") +
  annotate("text", x=3,  y=74, size=5, label="a") +
  annotate("text", x=4,  y=31, size=5, label="b") +
  annotate("text", x=5,  y=82, size=5, label="a") +
  annotate("text", x=6,  y=27, size=5, label="b") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme + theme(legend.position = c(0.4, 0.92)); cgr.vs.fsc.plt


ugr.vs.fsc.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Fescue, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors.vs, labels = c("Pre-treatment (2014)", "Post-treatment (2018)")) +
  annotate("text", x=1,  y=32, size=5, label="a") +
  annotate("text", x=2,  y=25, size=5, label="ab") +
  annotate("text", x=3,  y=40, size=5, label="ab") +
  annotate("text", x=4,  y=12, size=5, label="ac") +
  annotate("text", x=5,  y=42, size=5, label="b") +
  annotate("text", x=6,  y=8,  size=5, label="c") +
  #geom_segment(aes(x = 5, xend = 6, y = 20, yend = 20), size=1) +
  labs(x = "Year-Treatment", y = "Fescue (%)")+ 
       #title = "Un-Grazed") +
#  geom_text(x = 1, y = 75, label = "NS", size = 6) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  pref.theme + theme(legend.position = c(0.6, 0.9)); ugr.vs.fsc.plt


# Cool season grass plots
cgr.vs.csg.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = CSG, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors.vs) +
  labs(x = "Year-Treatment", y = "CSG (%)")+
  #, title = "Grazed") +
  annotate("text", x=3.5,y=60,size=5, label="a") +
  geom_segment(aes(x = 1, xend = 6, y = 55, yend = 55), size=0.9) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.csg.plt

ugr.vs.csg.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = CSG, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors.vs) +
  labs(x = "Year-Treatment", y = "CSG (%)")+
  #, title = "Un-Grazed") +
  annotate("text", x=1,  y=60, size=5, label="ac") +
  annotate("text", x=2,  y=68, size=5, label="b") +
  annotate("text", x=3,  y=25, size=5, label="a") +
  annotate("text", x=4,  y=45, size=5, label="c") +
  annotate("text", x=5,  y=25, size=5, label="a") +
  annotate("text", x=6,  y=40,  size=5, label="c") +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.csg.plt

# Warm season grass plots
cgr.vs.wsg.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = WSG, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors.vs) +
  labs(x = "Year-Treatment", y = "WSG (%)")+
  #, title = "Grazed") +
  annotate("text", x=1,  y=35, size=5, label="abc") +
  annotate("text", x=2,  y=36, size=5, label="ac") +
  annotate("text", x=3,  y=27, size=5, label="abc") + #same as all
  annotate("text", x=4,  y=29, size=5, label="ac") +
  annotate("text", x=5,  y=15, size=5, label="b") +
  annotate("text", x=6,  y=27, size=5, label="c") +
  ylim(0, 45) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.wsg.plt

ugr.vs.wsg.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = WSG, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors.vs) +
  labs(x = "Year-Treatment", y = "WSG (%)")+
  #, title = "Un-Grazed") +
  annotate("text", x=3,y=38,size=5, label="a") +
  geom_segment(aes(x = 1, xend = 5, y = 34, yend = 34), size=0.9) +
  annotate("text", x=6,y=45,size=5, label="b") +
 # geom_text(x = 1, y = 45, label = "NS", size = 6) +
  ylim(0, 45) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.wsg.plt

# Make the figure
fig.4 <- egg::ggarrange(cgr.vs.fsc.plt, ugr.vs.fsc.plt,
                        cgr.vs.csg.plt, ugr.vs.csg.plt,
                        cgr.vs.wsg.plt, ugr.vs.wsg.plt,
                        nrow = 3, ncol = 2, widths = c(1, 1),
                        labels = c("A", "",
                                   "B", "",
                                   "C", ""))

# Save out the larger figure
ggplot2::ggsave("./Figures/For Manuscript/Figure-4.pdf", width = 7.5, height = 9, units = 'in', plot = fig.4)

##  --------------------------------------------------------------  ##
            # Figure 5 -- Forbs/Seedmix '14 vs. '18 ####
##  --------------------------------------------------------------  ##
# INCLUDES
## 2014 vs. post-treatment data
## Forbs
## Seedmix

# Forb plots
cgr.vs.frb.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Forbs, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors.vs, labels = c("Pre-treatment (2014)", "Post-treatment (2018)")) +
  labs(x = "Year-Treatment", y = "Forbs (%)")+
  annotate("text", x=1,  y=30, size=5, label="a") +
  annotate("text", x=2,  y=30, size=5, label="b") +
  annotate("text", x=3,  y=24, size=5, label="a") + #same as all
  annotate("text", x=4,  y=48, size=5, label="c") +
  annotate("text", x=5,  y=27, size=5, label="a") +
  annotate("text", x=6,  y=55, size=5, label="c") +
#  geom_text(x = 2, y = 29, label = "A", size = 6) +
 # geom_segment(aes(x = 1, xend = 3, y = 25, yend = 25)) +
#  geom_text(x = 5, y = 56, label = "B", size = 6) +
#  geom_segment(aes(x = 4, xend = 6, y = 52, yend = 52)) +
  ylim(0, 75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme + theme(legend.position = c(0.5, 0.9)); cgr.vs.frb.plt

ugr.vs.frb.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Forbs, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors.vs, labels = c("Pre-treatment (2014)", "Post-treatment (2018)")) +
  labs(x = "Year-Treatment", y = "Forbs (%)") +
  #, title = "Un-Grazed") +
  annotate("text", x=1,  y=59, size=5, label="a") +
  annotate("text", x=2,  y=23, size=5, label="b") +
  annotate("text", x=3,  y=50, size=5, label="abc") + #same as all
  annotate("text", x=4,  y=44, size=5, label="ac") +
  annotate("text", x=5,  y=32, size=5, label="b") +
  annotate("text", x=6,  y=35, size=5, label="b") +
  ylim(0, 75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme + theme(legend.position = c(0.6, 0.9)); ugr.vs.frb.plt

# Seedmix plots
cgr.vs.smx.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Seedmix, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors.vs) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)")+
  #, title = "Grazed") +
  annotate("text", x=1,  y=0.02, size=5, label="a") +
  annotate("text", x=2,  y=0.12, size=5, label="bc") +
  annotate("text", x=3,  y=0.02, size=5, label="a") + #same as all
  annotate("text", x=4,  y=0.19, size=5, label="bc") +
  annotate("text", x=5,  y=0.09, size=5, label="ab") +
  annotate("text", x=6,  y=0.15, size=5, label="c") +
  ylim(0, 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.smx.plt


ugr.vs.smx.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Seedmix, fill = as.factor(Year))) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors.vs) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)")+
  #, title = "Un-Grazed") +
  annotate("text", x=1,  y=0.02, size=5, label="a") +
  annotate("text", x=2,  y=0.12, size=5, label="abc") +
  annotate("text", x=3,  y=0.05, size=5, label="ab") + #same as all
  annotate("text", x=4,  y=0.16, size=5, label="bc") +
  annotate("text", x=5,  y=0.02, size=5, label="a") +
  annotate("text", x=6,  y=0.12, size=5, label="c") +
  ylim(0, 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.smx.plt

# Make the figure
fig.5 <- egg::ggarrange(cgr.vs.frb.plt, ugr.vs.frb.plt,
                        cgr.vs.smx.plt, ugr.vs.smx.plt,
                        nrow = 2, ncol = 2, widths = c(1, 1),
                        labels = c("A", "",
                                   "B", ""))

# Save out the larger figure
ggplot2::ggsave("./Figures/For Manuscript/Figure-5.pdf", width = 7.5, height = 6, units = 'in', plot = fig.5)




# END ####

