##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures FOR OBJECTIVE 2
  ## Objective # 2 --- Heavy Grass Quadrats

# START ####

# Required libraries
library(Rmisc) # get summary values for plotting
library(ggplot2); library(cowplot); library(gridExtra); library(egg) # Plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

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
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none",
                    axis.text = element_text(size = 14), axis.title = element_text(size = 16))
no.y.axis <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(), axis.line.y = element_blank())

##  -------------------------------------------------------------------------  ##
                               # Heavy CSG ####
##  -------------------------------------------------------------------------  ##
# Summary dataframes
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.CSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment graphs
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.CSG)) +
  geom_errorbar(aes(ymax = Avg.Hvy.CSG + SE.Hvy.CSG, ymin = Avg.Hvy.CSG - SE.Hvy.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 55) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 6) +
  labs(x = "Pre-Treatment", y = "Heavy CSG Quadrats (#)") +
  pref.theme; cgr.csg14.plt

ugr.csg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.CSG)) +
  geom_errorbar(aes(ymax = Avg.Hvy.CSG + SE.Hvy.CSG, ymin = Avg.Hvy.CSG - SE.Hvy.CSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 80) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 25) +
  labs(x = "Pre-Treatment", y = "Heavy CSG Quadrats (#)") +
  pref.theme; ugr.csg14.plt

# Plot the post-treatment responses
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(x = Year, y = Hvy.CSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.CSG + se, ymin = Hvy.CSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 6) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 6) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.csg.plt

ugr.csg.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  geom_text(label = "a", x = 0.9, y = 11.5) +
  geom_text(label = "ab", x = 1.8, y = 5) +
  geom_text(label = "b", x = 2.9, y = 2) +
  ylim(0, 25) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.csg.plt

# Check out all four plots together
egg::ggarrange(cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
csg.gb.plt <- egg::ggarrange(cgr.csg14.plt, cgr.csg.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.CSG_GB.pdf", width = 8, height = 6, units = 'in', plot = csg.gb.plt)

csg.hay.plt <- egg::ggarrange(ugr.csg14.plt, ugr.csg.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.CSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = csg.hay.plt)

##  -------------------------------------------------------------------------  ##
                              # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Summary dataframes
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment graphss
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.WSG)) +
  geom_errorbar(aes(ymax = Avg.Hvy.WSG + SE.Hvy.WSG, ymin = Avg.Hvy.WSG - SE.Hvy.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 55) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(-0.8, 10) +
  labs(x = "Pre-Treatment", y = "Heavy WSG Quadrats (#)") +
  pref.theme; cgr.wsg14.plt

ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.WSG)) +
  geom_errorbar(aes(ymax = Avg.Hvy.WSG + SE.Hvy.WSG, ymin = Avg.Hvy.WSG - SE.Hvy.WSG, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 80) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 6) +
  labs(x = "Pre-Treatment", y = "Heavy WSG Quadrats (#)") +
  pref.theme; ugr.wsg14.plt

# Plot the post-treatment responses
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = Hvy.WSG, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 10) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(-0.8, 10) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Hvy.WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  geom_text(label = "a", x = 0.9, y = 0.3) +
  geom_text(label = "ab", x = 1.8, y = 1.3) +
  geom_text(label = "b", x = 2.9, y = 2.8) +
  ylim(0, 6) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.wsg.plt

# Check out all four plots together
egg::ggarrange(cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
wsg.gb.plt <- egg::ggarrange(cgr.wsg14.plt, cgr.wsg.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.WSG_GB.pdf", width = 8, height = 6, units = 'in', plot = wsg.gb.plt)

wsg.hay.plt <- egg::ggarrange(ugr.wsg14.plt, ugr.wsg.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.WSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = wsg.hay.plt)

##  -------------------------------------------------------------------------  ##
                             # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
# Pre-treatment
cgr.fsc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.Fesc)) +
  geom_errorbar(aes(ymax = Avg.Hvy.Fesc + SE.Hvy.Fesc, ymin = Avg.Hvy.Fesc - SE.Hvy.Fesc, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 80) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 20) +
  labs(x = "Pre-Treatment", y = "Heavy Fescue Quadrats (#)") +
  pref.theme; cgr.fsc14.plt

ugr.fsc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Hvy.Fesc)) +
  geom_errorbar(aes(ymax = Avg.Hvy.Fesc + SE.Hvy.Fesc, ymin = Avg.Hvy.Fesc - SE.Hvy.Fesc, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 80) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 2) +
  labs(x = "Pre-Treatment", y = "Heavy Fescue Quadrats (#)") +
  pref.theme; ugr.fsc14.plt

# Post
cgr.fsc.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  geom_text(label = "a", x = 0.9, y = 5.1) +
  geom_text(label = "ab", x = 1.8, y = 2.1) +
  geom_text(label = "b", x = 2.9, y = 2.1) +
  ylim(0, 20) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  geom_text(label = "a", x = 0.9, y = 1.1) +
  geom_text(label = "ab", x = 1.8, y = 0.1) +
  geom_text(label = "b", x = 2.9, y = 0.1) +
  ylim(0, 2) +
  labs(x = "Post-Treatment") +
  pref.theme + no.y.axis; ugr.fsc.plt

# Check out all four plots together
egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt, ugr.fsc.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
fsc.gb.plt <- egg::ggarrange(cgr.fsc14.plt, cgr.fsc.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.Fesc_GB.pdf", width = 8, height = 6, units = 'in', plot = fsc.gb.plt)

fsc.hay.plt <- egg::ggarrange(ugr.fsc14.plt, ugr.fsc.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 2/Hvy.Fesc_Hay.pdf", width = 8, height = 6, units = 'in', plot = fsc.hay.plt)


# END ####


