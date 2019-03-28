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
                        # Bare Cover ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.bar14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Bare, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 65) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 65) +
  labs(x = "Grazed", y = "Bare (%)", title = "Pre-Treatment") +
  pref.theme; cgr.bar14.plt

ugr.bar14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Bare, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 55) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 55) +
  labs(x = "Un-Grazed", y = "Bare (%)", title = "Pre-Treatment") +
  pref.theme; ugr.bar14.plt

# Plots
cgr.bar.plt <- ggplot(cgr.bar.pltdf, aes(x = Year, y = Bare, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 65, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 65) +
  labs(x = "Year", y = "Bare (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.bar.plt

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(x = Year, y = Bare, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 55, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 55) +
  labs(x = "Year", y = "Bare (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.6, 0.9)); ugr.bar.plt

# Save these
plot_grid(cgr.bar14.plt, cgr.bar.plt, ugr.bar14.plt, ugr.bar.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Bare.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.bar14.plt, cgr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Bare_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.bar14.plt, ugr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Bare_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Litter Cover ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = "Year")
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.ltr14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Litter, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 90) +
  scale_fill_manual(values = cgr.colors) +
  ylim(20, 90) +
  labs(x = "Grazed", y = "Litter (%)", title = "Pre-Treatment") +
  pref.theme; cgr.ltr14.plt

ugr.ltr14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Litter, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 100) +
  scale_fill_manual(values = ugr.colors) +
  ylim(30, 100) +
  labs(x = "Un-Grazed", y = "Litter (%)", title = "Pre-Treatment") +
  pref.theme; ugr.ltr14.plt

# Plots
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(x = Year, y = Litter, color = rep("z", nrow(cgr.ltr.pltdf)))) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se), width = 0.3, position = dodge) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.ns.color) +
  ylim(20, 90) +
  labs(x = "Year", y = "Litter (%)", title = "Post-Treatment") +
  pref.theme; cgr.ltr.plt

ugr.ltr.plt <- ggplot(ugr.ltr.pltdf, aes(x = Year, y = Litter, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 100, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(30, 100) +
  labs(x = "Year", y = "Litter (%)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.5, 0.2)); ugr.ltr.plt

# Save these
plot_grid(cgr.ltr14.plt, cgr.ltr.plt, ugr.ltr14.plt, ugr.ltr.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Litter.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.ltr14.plt, cgr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Litter_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.ltr14.plt, ugr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Litter_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Robel Height (dm) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.rbl14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = Robel, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 6) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 6) +
  labs(x = "Grazed", y = "Robel (dm)", title = "Pre-Treatment") +
  pref.theme; cgr.rbl14.plt

ugr.rbl14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = Robel, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 12) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 12) +
  labs(x = "Un-Grazed", y = "Robel (dm)", title = "Pre-Treatment") +
  pref.theme; ugr.rbl14.plt

# Plots
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(x = Year, y = Robel, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 6, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 6) +
  labs(x = "Year", y = "Robel (dm)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(x = Year, y = Robel, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 12, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 12) +
  labs(x = "Year", y = "Robel (dm)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.rbl.plt

# Save these
plot_grid(cgr.rbl14.plt, cgr.rbl.plt, ugr.rbl14.plt, ugr.rbl.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Robel.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.rbl14.plt, cgr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Robel_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.rbl14.plt, ugr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/Robel_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Litter depth (cm) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ldp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ldp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.ldp14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = LitDep, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 2.5) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 2.5) +
  labs(x = "Grazed", y = "LitDep (cm)", title = "Pre-Treatment") +
  pref.theme; cgr.ldp14.plt

ugr.ldp14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = LitDep, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 7) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Un-Grazed", y = "LitDep (cm)", title = "Pre-Treatment") +
  pref.theme; ugr.ldp14.plt

# Plots
cgr.ldp.plt <- ggplot(cgr.ldp.pltdf, aes(x = Year, y = LitDep, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 2.5, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 2.5) +
  labs(x = "Year", y = "LitDep (cm)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.2, 0.9)); cgr.ldp.plt

ugr.ldp.plt <- ggplot(ugr.ldp.pltdf, aes(x = Year, y = LitDep, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 14.8, y = 7, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Year", y = "LitDep (cm)", title = "Post-Treatment") +
  pref.theme + theme(legend.position = c(0.2, 0.9)); ugr.ldp.plt

# Save these
plot_grid(cgr.ldp14.plt, cgr.ldp.plt, ugr.ldp14.plt, ugr.ldp.plt,
          labels = c("I", "II", "III", "IV"), nrow = 2, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/LitDep.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.ldp14.plt, cgr.ldp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/LitDep_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.ldp14.plt, ugr.ldp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Objective 3/LitDep_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

# END ####


