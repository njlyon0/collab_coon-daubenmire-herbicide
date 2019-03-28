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
library(ggplot2); library(cowplot) # Plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                              # Houskeeping ####
##  ---------------------------------------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/sns-data_post-trt.csv")
sns.14 <- read.csv("./Data/sns-data_2014.csv")

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
cgr.ns.color <- "#fdae61"
ugr.ns.color <- "#abd9e9"
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none")

##  -------------------------------------------------------------------------  ##
                    # Heavy CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.CSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 13) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 13) +
  labs(x = "Grazed", y = "Heavy CSG Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; cgr.csg14.plt

ugr.csg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 30) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 30) +
  labs(x = "Un-Grazed", y = "Heavy CSG Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; ugr.csg14.plt

# Plots
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(x = Year, y = Hvy.CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.CSG + se, ymin = Hvy.CSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 13, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 13) +
  labs(x = "Year", y = "Heavy CSG Quadrats (#)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.8, y = 21, color = "black") +
  geom_text(label = "ab", x = 1.8, y = 12.5, color = "black") +
  geom_text(label = "b", x = 2.8, y = 7, color = "black") +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 30) +
  labs(x = "Un-Grazed", y = "Heavy CSG Quadrats (#)", title = "Post-Treatment") +
  pref.theme; ugr.csg.plt

# Save these
plot_grid(cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.CSG.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.csg14.plt, cgr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.CSG_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.csg14.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.CSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Hvy.WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 10) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 10) +
  labs(x = "Grazed", y = "Heavy WSG Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; cgr.wsg14.plt

ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Hvy.WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 14) +
  scale_fill_manual(values = ugr.colors) +
  ylim(-1, 14) +
  labs(x = "Un-Grazed", y = "Heavy WSG Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; ugr.wsg14.plt

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = Hvy.WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 10, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 10) +
  labs(x = "Year", y = "Heavy WSG Quadrats (#)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(x = Year, y = Hvy.WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  ylim(-1, 14) +
  labs(x = "Year", y = "Heavy WSG Quadrats (#)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0, 0.9)); ugr.wsg.plt

# Save these
plot_grid(cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.WSG.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.wsg14.plt, cgr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.WSG_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.wsg14.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.WSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Hvy.Fesc", groupvars = "Year")
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Hvy.Fesc", groupvars = "Year")

# Pre-treatment stuff
cgr.fsc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 30) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 30) +
  labs(x = "Grazed", y = "Heavy Fescue Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; cgr.fsc14.plt

ugr.fsc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 15) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 15) +
  labs(x = "Un-Grazed", y = "Heavy Fescue Quadrats (#)", title = "Pre-Treatment") +
  pref.theme; ugr.fsc14.plt

# Plots
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Hvy.Fesc, color = rep("z", nrow(cgr.fsc.pltdf)))) +
  geom_errorbar(aes(ymax = Hvy.Fesc + se, ymin = Hvy.Fesc - se), width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.ns.color) +
  ylim(0, 30) +
  labs(x = "Year", y = "Heavy Fescue Quadrats (#)", title = "Post-Treatment") +
  pref.theme; cgr.fsc.plt

ugr.fsc.plt1 <- ggplot(ugr.fsc.pltdf, aes(x = Year, y = Hvy.Fesc, color = rep("z", nrow(ugr.fsc.pltdf)))) +
  geom_errorbar(aes(ymax = Hvy.Fesc + se, ymin = Hvy.Fesc - se), width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.ns.color) +
  ylim(0, 15) +
  labs(x = "Year", y = "Heavy Fescue Quadrats (#)", title = "Post-Treatment") +
  pref.theme; ugr.fsc.plt1

ugr.fsc.plt2 <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.8, y = 4, color = "black") +
  geom_text(label = "b", x = 1.8, y = 2, color = "black") +
  geom_text(label = "b", x = 2.8, y = 1, color = "black") +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 15) +
  labs(x = "Un-Grazed", y = "Heavy Fescue Quadrats (#)", title = "Post-Treatment") +
  pref.theme; ugr.fsc.plt2

# Save these
plot_grid(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt,
          plot_grid(ugr.fsc.plt1, ugr.fsc.plt2, labels = c("", ""), nrow = 1, ncol = 2),
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.Fesc.pdf", width = 10, height = 8, units = 'in', plot = last_plot())

plot_grid(cgr.fsc14.plt, cgr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.Fesc_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.fsc14.plt, plot_grid(ugr.fsc.plt1, ugr.fsc.plt2, labels = c("", ""), nrow = 1, ncol = 2),
          labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 2/Hvy.Fesc_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

# END ####


