##  ----------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  ----------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon


# PURPOSE ####
  ## Create publication-quality figures with multiple panels
  ## Specifically only the ones that Jaime has selected for inclusion in the final manuscript

# Required libraries
library(Rmisc) # get summary values for plotting
library(ggplot2); library(cowplot); library(gridExtra); library(egg) # Plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

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
versus$Composite.Variable <- factor(versus$Composite.Variable,
                                    levels = c("14-Con", "14-Spr", "14-SnS", "18-Con", "18-Spr", "18-SnS"))
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
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none",
                    axis.text = element_text(size = 14), axis.title = element_text(size = 16))
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

# Get the summary values for Fescue and WSGs (CSG is a boxplot at present)
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))

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
#  geom_text(label = "a", x = 14.7, y = 45, color = "black") +
 # geom_text(label = "b", x = 14.7, y = 20, color = "black") +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 90) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis + theme(legend.position = c(0.2, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(x = Year, y = Fescue, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "a", x = 14.7, y = 22, color = "black") +
 # geom_text(label = "b", x = 14.7, y = 4, color = "black") +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 60) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis + theme(legend.position = c(0.2, 0.9)); ugr.fsc.plt

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

# Plot the post-treatment responses for CSG
cgr.csg.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
#  geom_text(label = "a", x = 0.9, y = 21, size = 6) +
 # geom_text(label = "ab", x = 1.8, y = 30, size = 6) +
  #geom_text(label = "b", x = 2.9, y = 34, size = 6) +
  ylim(0, 55) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.csg.plt

ugr.csg.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
#  geom_text(label = "a", x = 0.9, y = 63, size = 6) +
 # geom_text(label = "ab", x = 1.8, y = 44, size = 6) +
  #geom_text(label = "b", x = 2.9, y = 35, size = 6) +
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
ggplot2::ggsave("./Figures/For Manuscript/Figure-1.pdf", width = 10, height = 12, units = 'in', plot = fig.1)

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
  ylim(0, 60) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; cgr.frb14.plt

ugr.frb14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Forbs)) +
  geom_errorbar(aes(ymax = Avg.Forbs + SE.Forbs, ymin = Avg.Forbs - SE.Forbs, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 75) +
  labs(x = "Pre-Treatment", y = "Forbs (%)") +
  pref.theme; ugr.frb14.plt

# Plots
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(x = Year, y = Forbs, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "a", x = 14.7, y = 32, color = "black", size = 6) +
 # geom_text(label = "b", x = 14.7, y = 18, color = "black", size = 6) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 60) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis + theme(legend.position = c(0.2, 0.9)); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(x = Year, y = Forbs, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
#  geom_text(label = "a", x = 14.7, y = 52, color = "black", size = 6) +
 # geom_text(label = "b", x = 14.7, y = 27, color = "black", size = 6) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 75) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis + theme(legend.position = c(0.2, 0.9)); ugr.frb.plt

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
ggplot2::ggsave("./Figures/For Manuscript/Figure-3.pdf", width = 10, height = 12, units = 'in', plot = fig.2)

##  --------------------------------------------------------------  ##
              # Figure 4 -- Grasses '14 vs. '18 ####
##  --------------------------------------------------------------  ##
# INCLUDES
  ## 2014 vs. 2018 data
  ## Fescue
  ## CSG
  ## WSG

# Fescue plots
cgr.vs.fsc.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Fescue (%)", title = "Grazed") +
#  geom_text(x = 2, y = 79, label = "A", size = 6) +
 # geom_segment(aes(x = 1, xend = 3, y = 75, yend = 75)) +
  #geom_text(x = 5, y = 51, label = "B", size = 6) +
  #geom_segment(aes(x = 4, xend = 6, y = 47, yend = 47)) +
  ylim(0, 80) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.fsc.plt

ugr.vs.fsc.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Fescue (%)", title = "Un-Grazed") +
#  geom_text(x = 1, y = 75, label = "NS", size = 6) +
  ylim(0, 75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.fsc.plt

# Cool season grass plots
cgr.vs.csg.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "CSG (%)", title = "Grazed") +
#  geom_text(x = 1, y = 65, label = "NS", size = 6) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.csg.plt

ugr.vs.csg.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "CSG (%)", title = "Un-Grazed") +
  #geom_text(x = 2, y = 61, label = "A", size = 6) +
  #geom_segment(aes(x = 1, xend = 3, y = 57, yend = 57)) +
  #geom_text(x = 5, y = 69, label = "B", size = 6) +
  #geom_segment(aes(x = 4, xend = 6, y = 65, yend = 65)) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.csg.plt

# Warm season grass plots
cgr.vs.wsg.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "WSG (%)", title = "Grazed") +
#  geom_text(x = 1, y = 45, label = "NS", size = 6) +
  ylim(0, 45) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.wsg.plt

ugr.vs.wsg.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "WSG (%)", title = "Un-Grazed") +
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
ggplot2::ggsave("./Figures/For Manuscript/Figure-4.pdf", width = 10, height = 12, units = 'in', plot = fig.4)

##  --------------------------------------------------------------  ##
            # Figure 5 -- Forbs/Seedmix '14 vs. '18 ####
##  --------------------------------------------------------------  ##
# INCLUDES
## 2014 vs. post-treatment data
## Forbs
## Seedmix

# Forb plots
cgr.vs.frb.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Forbs (%)", title = "Grazed") +
#  geom_text(x = 2, y = 29, label = "A", size = 6) +
 # geom_segment(aes(x = 1, xend = 3, y = 25, yend = 25)) +
#  geom_text(x = 5, y = 56, label = "B", size = 6) +
#  geom_segment(aes(x = 4, xend = 6, y = 52, yend = 52)) +
  ylim(0, 55) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.frb.plt

ugr.vs.frb.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Forbs (%)", title = "Un-Grazed") +
#  geom_text(x = 0.9, y = 58, label = "NS", size = 6) +
  ylim(0, 60) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.vs.frb.plt

# Seedmix plots
cgr.vs.smx.plt <- ggplot(cgr.vs, aes(x = Composite.Variable, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)", title = "Grazed") +
#  geom_text(x = 2, y = 0.09, label = "A", size = 6) +
 # geom_segment(aes(x = 1, xend = 3, y = 0.08, yend = 0.08)) +
#  geom_text(x = 5, y = 0.19, label = "B", size = 6) +
#  geom_segment(aes(x = 4, xend = 6, y = 0.18, yend = 0.18)) +
  ylim(0, 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.vs.smx.plt

ugr.vs.smx.plt <- ggplot(ugr.vs, aes(x = Composite.Variable, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)", title = "Un-Grazed") +
#  geom_text(x = 2, y = 0.06, label = "A", size = 6) +
 # geom_segment(aes(x = 1, xend = 3, y = 0.05, yend = 0.05)) +
#  geom_text(x = 5, y = 0.15, label = "B", size = 6) +
 # geom_segment(aes(x = 4, xend = 6, y = 0.14, yend = 0.14)) +
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
ggplot2::ggsave("./Figures/For Manuscript/Figure-5.pdf", width = 10, height = 12, units = 'in', plot = fig.5)




# END ####

