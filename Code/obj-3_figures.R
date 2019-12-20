##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures FOR OBJECTIVE #3
  ## Objective # 3  --- Structural Response

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
                                    # Bare Cover ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.bar14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Bare)) +
  geom_errorbar(aes(ymax = Avg.Bare + SE.Bare, ymin = Avg.Bare - SE.Bare, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 70) +
  labs(x = "Pre-Treatment", y = "Bare (%)") +
  pref.theme; cgr.bar14.plt

ugr.bar14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Bare)) +
  geom_errorbar(aes(ymax = Avg.Bare + SE.Bare, ymin = Avg.Bare - SE.Bare, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 55) +
  labs(x = "Pre-Treatment", y = "Bare (%)") +
  pref.theme; ugr.bar14.plt

# Plots
cgr.bar.plt <- ggplot(cgr.bar.pltdf, aes(x = Year, y = Bare, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 70) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 70) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.bar.plt

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(x = Year, y = Bare, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 55) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 55) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.bar.plt

# Check out all four plots together
egg::ggarrange(cgr.bar14.plt, cgr.bar.plt, ugr.bar14.plt, ugr.bar.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
bar.gb.plt <- egg::ggarrange(cgr.bar14.plt, cgr.bar.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 3/Bare_GB.pdf", width = 8, height = 6, units = 'in', plot = bar.gb.plt)

bar.hay.plt <- egg::ggarrange(ugr.bar14.plt, ugr.bar.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 3/Bare_Hay.pdf", width = 8, height = 6, units = 'in', plot = bar.hay.plt)

##  -------------------------------------------------------------------------  ##
                             # Litter Cover ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.ltr14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Litter)) +
  geom_errorbar(aes(ymax = Avg.Litter + SE.Litter, ymin = Avg.Litter - SE.Litter, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 100) +
  labs(x = "Pre-Treatment", y = "Litter (%)") +
  pref.theme; cgr.ltr14.plt

ugr.ltr14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Litter)) +
  geom_errorbar(aes(ymax = Avg.Litter + SE.Litter, ymin = Avg.Litter - SE.Litter, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 150) +
  labs(x = "Pre-Treatment", y = "Litter (%)") +
  pref.theme; ugr.ltr14.plt

# Plots
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(x = Year, y = Litter)) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F, linetype = 1, color = "black") +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment, shape = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 100) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 100) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.55, 0.2)) + no.y.axis; cgr.ltr.plt


ugr.ltr.plt <- ggplot(ugr.ltr.pltdf, aes(x = Year, y = Litter, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 100) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 150) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.55, 0.2)) + no.y.axis; ugr.ltr.plt

# Check out all four plots together
egg::ggarrange(cgr.ltr14.plt, cgr.ltr.plt, ugr.ltr14.plt, ugr.ltr.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
ltr.gb.plt <- egg::ggarrange(cgr.ltr14.plt, cgr.ltr.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 3/Litter_GB.pdf", width = 8, height = 6, units = 'in', plot = ltr.gb.plt)

ltr.hay.plt <- egg::ggarrange(ugr.ltr14.plt, ugr.ltr.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 3/Litter_Hay.pdf", width = 8, height = 6, units = 'in', plot = ltr.hay.plt)

##  -------------------------------------------------------------------------  ##
                            # Robel Height (dm) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.rbl14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.Robel)) +
  geom_errorbar(aes(ymax = Avg.Robel + SE.Robel, ymin = Avg.Robel - SE.Robel, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 7) +
  labs(x = "Pre-Treatment", y = "Robel (dm)") +
  pref.theme; cgr.rbl14.plt

ugr.rbl14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.Robel)) +
  geom_errorbar(aes(ymax = Avg.Robel + SE.Robel, ymin = Avg.Robel - SE.Robel, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 12) +
  labs(x = "Pre-Treatment", y = "Robel (dm)") +
  pref.theme; ugr.rbl14.plt

# Plots
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(x = Year, y = Robel, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 7) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 7) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(x = Year, y = Robel, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 12) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 12) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.rbl.plt

# Check out all four plots together
egg::ggarrange(cgr.rbl14.plt, cgr.rbl.plt, ugr.rbl14.plt, ugr.rbl.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
rbl.gb.plt <- egg::ggarrange(cgr.rbl14.plt, cgr.rbl.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 3/Robel_GB.pdf", width = 8, height = 6, units = 'in', plot = rbl.gb.plt)

rbl.hay.plt <- egg::ggarrange(ugr.rbl14.plt, ugr.rbl.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 3/Robel_Hay.pdf", width = 8, height = 6, units = 'in', plot = rbl.hay.plt)

##  -------------------------------------------------------------------------  ##
                            # Litter depth (cm) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ldp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ldp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.ldp14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Avg.LitDep)) +
  geom_errorbar(aes(ymax = Avg.LitDep + SE.LitDep, ymin = Avg.LitDep - SE.LitDep, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 3) +
  labs(x = "Pre-Treatment", y = "LitDep (cm)") +
  pref.theme; cgr.ldp14.plt

ugr.ldp14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Avg.LitDep)) +
  geom_errorbar(aes(ymax = Avg.LitDep + SE.LitDep, ymin = Avg.LitDep - SE.LitDep, color = Herbicide.Treatment),
                width = 0.5, position = dodge) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5, shape = 21:23) +
  #geom_text(label = "NS", x = 0.7, y = 50) + 
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 8) +
  labs(x = "Pre-Treatment", y = "LitDep (cm)") +
  pref.theme; ugr.ldp14.plt

# Plots
cgr.ldp.plt <- ggplot(cgr.ldp.pltdf, aes(x = Year, y = LitDep, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 3) +
  scale_color_manual(values = cgr.colors) +
  scale_fill_manual(values = cgr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 3) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; cgr.ldp.plt

ugr.ldp.plt <- ggplot(ugr.ldp.pltdf, aes(x = Year, y = LitDep, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se, color = Herbicide.Treatment),
                width = 0.3, position = dodge) +
  geom_smooth(aes(color = Herbicide.Treatment), method = 'lm', se = F, linetype = 2) +
  geom_point(aes(fill = Herbicide.Treatment), position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.7, y = 8) +
  scale_color_manual(values = ugr.colors) +
  scale_fill_manual(values = ugr.colors) +
  scale_shape_manual(values = 21:23) +
  ylim(0, 8) +
  labs(x = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)) + no.y.axis; ugr.ldp.plt

# Check out all four plots together
egg::ggarrange(cgr.ldp14.plt, cgr.ldp.plt, ugr.ldp14.plt, ugr.ldp.plt, 
               nrow = 2, ncol = 2, widths = c(1, 2.5))

# Save some relevant combinations of these
ldp.gb.plt <- egg::ggarrange(cgr.ldp14.plt, cgr.ldp.plt, nrow = 1, widths = c(1, 2.5), top = "Grazed")
ggplot2::ggsave("./Figures/Objective 3/LitDep_GB.pdf", width = 8, height = 6, units = 'in', plot = ldp.gb.plt)

ldp.hay.plt <- egg::ggarrange(ugr.ldp14.plt, ugr.ldp.plt, nrow = 1, widths = c(1, 2.5), top = "Un-Grazed")
ggplot2::ggsave("./Figures/Objective 3/LitDep_Hay.pdf", width = 8, height = 6, units = 'in', plot = ldp.hay.plt)

# END ####


