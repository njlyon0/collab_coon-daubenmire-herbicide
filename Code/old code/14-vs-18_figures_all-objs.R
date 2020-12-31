##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures FOR 2014 VS. 2018 COMPARISONS
  ## This includes all objectives (1, 2, and 3)

# START ####

# Required libraries
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
sns <- read.csv("./Data/sns-data_14-vs-18.csv")

# Re-level the treatment factor
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Re-level the Composite Variable column for ease of plotting
sns$Composite.Variable <- factor(sns$Composite.Variable,
  levels = c("14-Con", "14-Spr", "14-SnS", "18-Con", "18-Spr", "18-SnS"))
unique(sns$Composite.Variable)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# Plotting shortcuts
#cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red - draft 1
cgr.colors <- c("Con" = "#ac261f", "Spr" = "#f77d73", "SnS" = "#fdbb7a") # shades of red
#ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue - draft 1
ugr.colors <- c("Con" = "#30517d", "Spr" = "#61a2cb", "SnS" = "#bfe2ee") # shades of blue
dodge <- position_dodge(width = 0.5)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank(), legend.position = "none",
                    axis.text = element_text(size = 14), axis.title = element_text(size = 16))

##  -------------------------------------------------------------------------  ##
                        # CSG ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.csg.plt <- ggplot(cgr, aes(x = Composite.Variable, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "CSG (%)", title = "Grazed") +
  geom_text(x = 1, y = 65, label = "NS") +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.csg.plt

ugr.csg.plt <- ggplot(ugr, aes(x = Composite.Variable, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "CSG (%)", title = "Un-Grazed") +
  geom_text(x = 2, y = 59, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 57, yend = 57)) +
  geom_text(x = 5, y = 67, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 65, yend = 65)) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.csg.plt

# Save these plots
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/CSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.wsg.plt <- ggplot(cgr, aes(x = Composite.Variable, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "WSG (%)", title = "Grazed") +
  geom_text(x = 1, y = 45, label = "NS") +
  ylim(0, 45) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr, aes(x = Composite.Variable, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "WSG (%)", title = "Un-Grazed") +
  geom_text(x = 1, y = 45, label = "NS") +
  ylim(0, 45) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.wsg.plt

# Save these plots
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/WSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Fescue ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.fsc.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Fescue (%)", title = "Grazed") +
  geom_text(x = 2, y = 77, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 75, yend = 75)) +
  geom_text(x = 5, y = 49, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 47, yend = 47)) +
  ylim(0, 75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Fescue, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Fescue (%)", title = "Un-Grazed") +
  geom_text(x = 1, y = 75, label = "NS") +
  ylim(0, 75) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.fsc.plt

# Save these plots
plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Fescue.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Seedmix ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.smx.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)", title = "Grazed") +
  geom_text(x = 2, y = 0.09, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 0.08, yend = 0.08)) +
  geom_text(x = 5, y = 0.19, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 0.18, yend = 0.18)) +
  ylim(0, 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.smx.plt

ugr.smx.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Seedmix, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Seedmix (Prop.)", title = "Un-Grazed") +
  geom_text(x = 2, y = 0.06, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 0.05, yend = 0.05)) +
  geom_text(x = 5, y = 0.15, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 0.14, yend = 0.14)) +
  ylim(0, 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.smx.plt

# Save these plots
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Seedmix.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Forbs ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.frb.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Forbs (%)", title = "Grazed") +
  geom_text(x = 2, y = 27, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 25, yend = 25)) +
  geom_text(x = 5, y = 54, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 52, yend = 52)) +
  ylim(0, 55) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.frb.plt

ugr.frb.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Forbs, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Forbs (%)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 57, label = "NS") +
  ylim(0, 55) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.frb.plt

# Save these plots
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Forbs.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Legumes ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.lgm.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Legumes, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Legumes (%)", title = "Grazed") +
  geom_text(x = 2, y = 32, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 30, yend = 30)) +
  geom_text(x = 5, y = 68, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 66, yend = 66)) +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Legumes, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Legumes (%)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 65, label = "NS") +
  ylim(0, 70) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.lgm.plt

# Save these plots
plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Legumes.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Woody ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.wdy.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Woody (%)", title = "Grazed") +
  geom_text(x = 0.8, y = 8, label = "NS") +
  ylim(0, 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.wdy.plt

ugr.wdy.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Woody, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Woody (%)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 8, label = "NS") +
  ylim(0, 8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.wdy.plt

# Save these plots
plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Woody.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Panic Grass ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.pnc.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Panic, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Panic (Prop.)", title = "Grazed") +
  geom_text(x = 2, y = 0.82, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 0.8, yend = 0.8)) +
  geom_text(x = 5, y = 0.72, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 0.70, yend = 0.70)) +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Panic, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Panic (Prop.)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 1, label = "NS") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.pnc.plt

# Save these plots
plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Panic.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy CSG ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.hvy.csg.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Heavy CSG Quadrats (#)", title = "Grazed") +
  geom_text(x = 0.8, y = 9, label = "NS") +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.hvy.csg.plt

ugr.hvy.csg.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Hvy.CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Heavy CSG Quadrats (#)", title = "Un-Grazed") +
  geom_text(x = 0.7, y = 6, label = "A") +
  geom_text(x = 3.7, y = 9.7, label = "A") +
  geom_text(x = 2.5, y = 4, label = "B") +
  geom_segment(aes(x = 2, xend = 3, y = 3, yend = 3)) + 
  geom_text(x = 5.5, y = 3.5, label = "B") +
  geom_segment(aes(x = 5, xend = 6, y = 2.5, yend = 2.5)) +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.hvy.csg.plt

# Save these plots
plot_grid(cgr.hvy.csg.plt, ugr.hvy.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.CSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.hvy.wsg.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Hvy.WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Heavy WSG Quadrats (#)", title = "Grazed") +
  geom_text(x = 0.8, y = 10, label = "NS") +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.hvy.wsg.plt

ugr.hvy.wsg.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Hvy.WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Heavy WSG Quadrats (#)", title = "Un-Grazed") +
  geom_text(x = 2, y = 7.5, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 7, yend = 7)) +
  geom_text(x = 5, y = 6.5, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 6, yend = 6)) +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.hvy.wsg.plt

# Save these plots
plot_grid(cgr.hvy.wsg.plt, ugr.hvy.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.WSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.hvy.fsc.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Heavy Fescue Quadrats (#)", title = "Grazed") +
  geom_text(x = 2, y = 21, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 20, yend = 20)) +
  geom_text(x = 5, y = 9, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 7, yend = 7)) +
  ylim(0, 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.hvy.fsc.plt

ugr.hvy.fsc.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Hvy.Fesc, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Heavy Fescue Quadrats (#)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 25, label = "NS") +
  ylim(0, 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.hvy.fsc.plt

# Save these plots
plot_grid(cgr.hvy.fsc.plt, ugr.hvy.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.Fesc.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Bare ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.bar.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Bare, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Bare (%)", title = "Grazed") +
  geom_text(x = 2, y = 42, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 40, yend = 40)) +
  geom_text(x = 5, y = 77, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 75, yend = 75)) +
  ylim(0, 80) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.bar.plt

ugr.bar.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Bare, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Bare (%)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 80, label = "NS") +
  ylim(0, 80) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.bar.plt

# Save these plots
plot_grid(cgr.bar.plt, ugr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Bare.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Litter (%) ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.ltr.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Litter, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Litter (%)", title = "Grazed") +
  geom_text(x = 0.8, y = 100, label = "NS") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.ltr.plt

ugr.ltr.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Litter, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Litter (%)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 100, label = "NS") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.ltr.plt

# Save these plots
plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Litter.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Robel ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.rbl.plt <- ggplot(cgr, aes(x = Composite.Variable, y = Robel, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Robel (dm)", title = "Grazed") +
  geom_text(x = 2, y = 3.2, label = "A") +
  geom_segment(aes(x = 1, xend = 3, y = 3, yend = 3)) +
  geom_text(x = 5, y = 5.7, label = "B") +
  geom_segment(aes(x = 4, xend = 6, y = 5.5, yend = 5.5)) +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr, aes(x = Composite.Variable, y = Robel, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Robel (dm)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 10, label = "NS") +
  ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.rbl.plt

# Save these plots
plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Robel.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Litter (Depth) ####
##  -------------------------------------------------------------------------  ##
# Plots
cgr.ldp.plt <- ggplot(cgr, aes(x = Composite.Variable, y = LitDep, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  labs(x = "Year-Treatment", y = "Litter Depth (cm)", title = "Grazed") +
  geom_text(x = 0.8, y = 7, label = "NS") +
  ylim(0, 7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; cgr.ldp.plt

ugr.ldp.plt <- ggplot(ugr, aes(x = Composite.Variable, y = LitDep, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  labs(x = "Year-Treatment", y = "Litter Depth (cm)", title = "Un-Grazed") +
  geom_text(x = 0.8, y = 7, label = "NS") +
  ylim(0, 7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  pref.theme; ugr.ldp.plt

# Save these plots
plot_grid(cgr.ldp.plt, ugr.ldp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/LitDep.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())


# END ####

