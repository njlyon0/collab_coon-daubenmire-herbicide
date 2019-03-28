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
                          # CSG ####
##  -------------------------------------------------------------------------  ##
# Plot pre-treatment differences
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 55) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 55) +
  labs(x = "Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; cgr.csg14.plt

ugr.csg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 80) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 80) +
  labs(x = "Un-Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; ugr.csg14.plt

# Plot the post-treatment responses
cgr.csg.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  geom_text(label = "a", x = 0.9, y = 21) +
  geom_text(label = "ab", x = 1.8, y = 30) +
  geom_text(label = "b", x = 2.9, y = 34) +
  ylim(0, 55) +
  labs(x = "Grazed", y = "Cool-Season Grass (%)", title = "Post-Treatment") +
  pref.theme; cgr.csg.plt

ugr.csg.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  geom_text(label = "a", x = 0.9, y = 63) +
  geom_text(label = "ab", x = 1.8, y = 44) +
  geom_text(label = "b", x = 2.9, y = 35) +
  ylim(0, 80) +
  labs(x = "Un-Grazed", y = "Cool-Season Grass (%)", title = "Post-Treatment") +
  pref.theme; ugr.csg.plt

# Save some relevant combinations of these
plot_grid(cgr.csg14.plt, cgr.csg.plt, ugr.csg14.plt, ugr.csg.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/CSG.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.csg14.plt, cgr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/CSG_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.csg14.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/CSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 40) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 40) +
  labs(x = "Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; cgr.wsg14.plt

ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 45) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Un-Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; ugr.wsg14.plt

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 40, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 40) +
  labs(x = "Year", y = "Warm-Season Grass (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(x = Year, y = WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "Warm-Season Grass (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0, 0.9)); ugr.wsg.plt

# Save these
plot_grid(cgr.wsg14.plt, cgr.wsg.plt, ugr.wsg14.plt, ugr.wsg.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/WSG.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.wsg14.plt, cgr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/WSG_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.wsg14.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/WSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Fescue ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.fsc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 75) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 75) +
  labs(x = "Grazed", y = "Fescue (%)", title = "Pre-Treatment") +
  pref.theme; cgr.fsc14.plt

ugr.fsc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 45) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Un-Grazed", y = "Fescue (%)", title = "Pre-Treatment") +
  pref.theme; ugr.fsc14.plt

# Plots
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "a", x = 14.7, y = 45, color = "black") +
  geom_text(label = "b", x = 14.7, y = 20, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 75) +
  labs(x = "Year", y = "Fescue (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "a", x = 14.7, y = 22, color = "black") +
  geom_text(label = "b", x = 14.7, y = 3, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "Fescue (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.fsc.plt

# Save these
plot_grid(cgr.fsc14.plt, cgr.fsc.plt, ugr.fsc14.plt, ugr.fsc.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Fescue.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.fsc14.plt, cgr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Fescue_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.fsc14.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Fescue_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                # Seed-mix Proportion ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = "Year")
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.smx14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 0.13) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 0.13) +
  labs(x = "Grazed", y = "Seedmix Proportion >25%", title = "Pre-Treatment") +
  pref.theme; cgr.smx14.plt

ugr.smx14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 0.18) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 0.18) +
  labs(x = "Un-Grazed", y = "Seedmix Proportion >25%", title = "Pre-Treatment") +
  pref.theme; ugr.smx14.plt

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(x = Year, y = Seedmix, color = rep("z", nrow(cgr.smx.pltdf)))) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.ns.color) +
  ylim(0, 0.13) +
  labs(x = "Year", y = "Seedmix Proportion >25%", title = "Post-Treatment") +
  pref.theme; cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(x = Year, y = Seedmix, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 0.18, color = "black") +
  scale_color_manual(values = ugr.colors) +
  #ylim(0, 0.18) +
  labs(x = "Year", y = "Seedmix Proportion >25%", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.smx.plt

# Save these
plot_grid(cgr.smx14.plt, cgr.smx.plt, ugr.smx14.plt, ugr.smx.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Seedmix.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.smx14.plt, cgr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Seedmix_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.smx14.plt, ugr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Seedmix_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Forbs ####
##  -------------------------------------------------------------------------  ##
# Pre-treatment stuff
cgr.frb14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 60) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 60) +
  labs(x = "Grazed", y = "Forbs (%)", title = "Pre-Treatment") +
  pref.theme; cgr.frb14.plt

ugr.frb14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 75) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 75) +
  labs(x = "Un-Grazed", y = "Forbs (%)", title = "Pre-Treatment") +
  pref.theme; ugr.frb14.plt

# Plots
cgr.frb.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.9, y = 26, color = "black") +
  geom_text(label = "b", x = 1.9, y = 39, color = "black") +
  geom_text(label = "b", x = 2.9, y = 43, color = "black") +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 60) +
  labs(x = "Grazed", y = "Forbs (%)", title = "Post-Treatment") +
  pref.theme; cgr.frb.plt

ugr.frb.plt <- ggplot(ugr, aes(x = Herbicide.Treatment, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.9, y = 39, color = "black") +
  geom_text(label = "b", x = 1.9, y = 58, color = "black") +
  geom_text(label = "b", x = 2.9, y = 58.5, color = "black") +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 75) +
  labs(x = "Un-Grazed", y = "Forbs (%)", title = "Post-Treatment") +
  pref.theme; ugr.frb.plt

# Save these
plot_grid(cgr.frb14.plt, cgr.frb.plt, ugr.frb14.plt, ugr.frb.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Forbs.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.frb14.plt, cgr.frb.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Forbs_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.frb14.plt, ugr.frb.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Forbs_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Legumes ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.lgm14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Legumes, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 50) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 50) +
  labs(x = "Grazed", y = "Legumes (%)", title = "Pre-Treatment") +
  pref.theme; cgr.lgm14.plt

ugr.lgm14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Legumes, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 45) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 50) +
  labs(x = "Un-Grazed", y = "Legumes (%)", title = "Pre-Treatment") +
  pref.theme; ugr.lgm14.plt

# Plots
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(x = Year, y = Legumes, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 50, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 50) +
  labs(x = "Year", y = "Legumes (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(x = Year, y = Legumes, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 50) +
  labs(x = "Year", y = "Legumes (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0, 0.17)); ugr.lgm.plt

# Save these
plot_grid(cgr.lgm14.plt, cgr.lgm.plt, ugr.lgm14.plt, ugr.lgm.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Legumes.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.lgm14.plt, cgr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Legumes_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.lgm14.plt, ugr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Legumes_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Woody ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.wdy14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 6.5) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 6.5) +
  labs(x = "Grazed", y = "Woody (%)", title = "Pre-Treatment") +
  pref.theme; cgr.wdy14.plt

ugr.wdy14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0, y = 7) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Un-Grazed", y = "Woody (%)", title = "Pre-Treatment") +
  pref.theme; ugr.wdy14.plt

# Plots
cgr.wdy.plt <- ggplot(cgr, aes(x = Herbicide.Treatment, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "a", x = 0.8, y = 3.6, color = "black") +
  geom_text(label = "ab", x = 1.7, y = 4, color = "black") +
  geom_text(label = "b", x = 2.8, y = 1.4, color = "black") +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 6.5) +
  labs(x = "Grazed", y = "Woody (%)", title = "Post-Treatment") +
  pref.theme; cgr.wdy.plt

ugr.wdy.plt <- ggplot(ugr.wdy.pltdf, aes(x = Year, y = Woody, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Woody + se, ymin = Woody - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 6.5, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Year", y = "Woody (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.wdy.plt

# Save these
plot_grid(cgr.wdy14.plt, cgr.wdy.plt, ugr.wdy14.plt, ugr.wdy.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Woody.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.wdy14.plt, cgr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Woody_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.wdy14.plt, ugr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Woody_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
              # Panic Proportion Occurrence ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = "Year")
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.pnc14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Panic, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 0.8) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 0.8) +
  labs(x = "Grazed", y = "Panic Proportion", title = "Pre-Treatment") +
  pref.theme; cgr.pnc14.plt

ugr.pnc14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Panic, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 0.85) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 0.85) +
  labs(x = "Un-Grazed", y = "Panic Proportion", title = "Pre-Treatment") +
  pref.theme; ugr.pnc14.plt

# Plots
cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(x = Year, y = Panic, color = rep("z", nrow(cgr.pnc.pltdf)))) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se), width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.ns.color) +
  ylim(0, 0.8) +
  labs(x = "Year", y = "Panic Proportion", title = "Post-Treatment") +
  pref.theme; cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(x = Year, y = Panic, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 0.85, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 0.85) +
  labs(x = "Year", y = "Panic Proportion", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.pnc.plt

# Save these
plot_grid(cgr.pnc14.plt, cgr.pnc.plt, ugr.pnc14.plt, ugr.pnc.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Panic.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.pnc14.plt, cgr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Panic_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.pnc14.plt, ugr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 1/Panic_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

# END ####


