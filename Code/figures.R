##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures

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

##  ---------------------------------------------------------------------------------------------  ##
              # Objective # 1 --- Functional Group Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                          # CSG ####
##  -------------------------------------------------------------------------  ##
# Plot pre-treatment differences
cgr.csg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 55) +
  scale_fill_manual(values = cgr.colors) +
  ylim(0, 55) +
  labs(x = "Un-Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; cgr.csg14.plt

ugr.csg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 80) +
  scale_fill_manual(values = ugr.colors) +
  ylim(0, 80) +
  labs(x = "Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
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
ggplot2::ggsave("./Figures/CSG.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(cgr.csg14.plt, cgr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/CSG_GB.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

plot_grid(ugr.csg14.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/CSG_Hay.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Pre-treatment stuff
cgr.wsg14.plt <- ggplot(cgr.14, aes(x = Herbicide.Treatment, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 55) +
  scale_fill_manual(values = cgr.colors) +
  #ylim(0, 55) +
  labs(x = "Un-Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; cgr.wsg14.plt

ugr.wsg14.plt <- ggplot(ugr.14, aes(x = Herbicide.Treatment, y = WSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  geom_text(label = "NS", x = 0.7, y = 80) +
  scale_fill_manual(values = ugr.colors) +
  #ylim(0, 80) +
  labs(x = "Grazed", y = "Cool-Season Grass (%)", title = "Pre-Treatment") +
  pref.theme; ugr.wsg14.plt



# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(Year, WSG, color = rep.int("x", nrow(cgr.wsg.pltdf)))) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.wsg.pltdf))), size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Year", y = "WSG % Cover") +
  scale_y_continuous(limits = wsg.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(Year, WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), position = dodge, width = 0.3) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  ugr.vlines + pref.theme + theme(legend.position = c(0.2, 0.9)); ugr.wsg.plt

# Save it.
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/WSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Fescue ####
##  -------------------------------------------------------------------------  ##
# Plotting dataframes
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = "Year")
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = "Year")
fsc.lims <- c(0, 60)

# Plot
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(Year, Fescue, color = rep.int("x", nrow(cgr.fsc.pltdf)))) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.fsc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Fescue % Cover") +
  scale_y_continuous(limits = fsc.lims) +
  geom_text(label = "A", x = 13.75, y = 47.5, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 29, color = "black") +
  geom_text(label = "B", x = 15.75, y = 23, color = "black") +
  geom_text(label = "B", x = 16.75, y = 18, color = "black") +
  geom_text(label = "B", x = 17.75, y = 20, color = "black") +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(Year, Fescue, color = rep.int("x", nrow(cgr.fsc.pltdf)))) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.fsc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = fsc.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.fsc.plt

plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Fescue.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                # Seed-mix Proportion ####
##  -------------------------------------------------------------------------  ##
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = "Year")
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = "Year")
smx.lims <- c(0, 0.15)

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(Year, Seedmix, color = rep.int("x", nrow(cgr.smx.pltdf)))) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), position = dodge, width = 0.3) +
  geom_line(aes(group = rep.int("x", nrow(cgr.smx.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  labs(x = "Year", y = "Proportion >25% Seed-mix") +
  geom_text(label = "A", x = 13.75, y = 0.01, color = "black") +
  geom_text(label = "A", x = 15.35, y = 0.02, color = "black") +
  geom_text(label = "AB", x = 15.7, y = 0.067, color = "black") +
  geom_text(label = "B", x = 16.75, y = 0.105, color = "black") +
  geom_text(label = "AB", x = 18.25, y = 0.057, color = "black") +
  scale_y_continuous(limits = smx.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme +   cgr.vlines + theme(legend.position = "none"); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(Year, Seedmix, color = rep.int("x", nrow(ugr.smx.pltdf)))) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.smx.pltdf))), size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = smx.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.smx.plt

# Save it
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Seedmix.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Forbs ####
##  -------------------------------------------------------------------------  ##
cgr.frb.pltdf1 <- summarySE(data = cgr, measurevar = "Forbs", groupvars = "Year")
cgr.frb.pltdf2 <- summarySE(data = cgr, measurevar = "Forbs", groupvars = "Herbicide.Treatment")
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Year"))
frb.lims <- c(0, 60)

# Plots
cgr.frb.plt1 <- ggplot(cgr.frb.pltdf1, aes(Year, Forbs, color = rep.int("x", nrow(cgr.frb.pltdf1)))) +
  geom_line(aes(group = rep.int("x", nrow(cgr.frb.pltdf1))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  labs(x = "Year", y = "Forbs % Cover") +
  geom_text(label = "A", x = 13.75, y = 15, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 21, color = "black") +
  geom_text(label = "B", x = 15.75, y = 36, color = "black") +
  geom_text(label = "B", x = 16.75, y = 32, color = "black") +
  geom_text(label = "B", x = 18.25, y = 30, color = "black") +
  scale_y_continuous(limits = frb.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.frb.plt1

cgr.frb.plt2 <- ggplot(cgr.frb.pltdf2, aes(Herbicide.Treatment, Forbs, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Forbs % Cover") +
  geom_text(label = "A", x = 0.8, y = 20, color = "black") +
  geom_text(label = "AB", x = 1.75, y = 27.3, color = "black") +
  geom_text(label = "B", x = 2.8, y = 30, color = "black") +
  scale_y_continuous(limits = frb.lims) +
  scale_color_manual(values = cgr.colors) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.frb.plt2

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(Year, Forbs, color = rep.int("Z", nrow(ugr.frb.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(ugr.frb.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  labs(x = "Year", y = "Forbs % Cover") +
  scale_y_continuous(limits = frb.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.frb.plt

# Save it.
plot_grid(plot_grid(cgr.frb.plt1, cgr.frb.plt2, labels = c("I", "II"), nrow = 2, ncol = 1),
          ugr.frb.plt, labels = c("", "III"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Forbs.pdf", width = 8, height = 6, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Legumes ####
##  -------------------------------------------------------------------------  ##
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = "Year")
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = "Year")
lgm.lims <- c(10, 35)

# Plot
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(Year, Legumes, color = rep.int("x", nrow(cgr.lgm.pltdf)))) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.lgm.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Legumes % Cover") +
  scale_y_continuous(limits = lgm.lims) +
  geom_text(label = "A", x = 13.75, y = 12.75, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 19.5, color = "black") +
  geom_text(label = "AB", x = 15.6, y = 17.75, color = "black") +
  geom_text(label = "B", x = 16.75, y = 29.25, color = "black") +
  geom_text(label = "AB", x = 18.25, y = 22.5, color = "black") +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(Year, Legumes, color = rep.int("x", nrow(cgr.lgm.pltdf)))) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.lgm.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = lgm.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.lgm.plt

plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Legumes.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Woody ####
##  -------------------------------------------------------------------------  ##
cgr.wdy.pltdf <- summarySE(data = cgr, measurevar = "Woody", groupvars = "Herbicide.Treatment")
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = "Year")
wdy.lims <- c(0, 5)

# Plot
cgr.wdy.plt <- ggplot(cgr.wdy.pltdf, aes(Herbicide.Treatment, Woody, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Woody + se, ymin = Woody - se), position = dodge, width = 0.3) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  labs(x = "Herbicide Treatment", y = "Woody % Cover") +
  geom_text(label = "A", x = 0.8, y = 2.5, color = "black") +
  geom_text(label = "AB", x = 1.75, y = 2.5, color = "black") +
  geom_text(label = "B", x = 2.8, y = 1.1, color = "black") +
  scale_y_continuous(limits = wdy.lims) +
  scale_color_manual(values = cgr.colors) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.wdy.plt

ugr.wdy.plt <- ggplot(ugr.wdy.pltdf, aes(Year, Woody, color = rep.int("x", nrow(ugr.wdy.pltdf)))) +
  geom_errorbar(aes(ymin = Woody - se, ymax = Woody + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.wdy.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  geom_text(label = "A", x = 13.75, y = 0.4, color = "black") +
  geom_text(label = "A", x = 15.2, y = 0.75, color = "black") +
  geom_text(label = "A", x = 15.7, y = 0.3, color = "black") +
  geom_text(label = "B", x = 16.8, y = 3, color = "black") +
  geom_text(label = "AB", x = 17.7, y = 1.2, color = "black") +
  scale_y_continuous(limits = wdy.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.wdy.plt

plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Woody.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
              # Panic Proportion Occurrence ####
##  -------------------------------------------------------------------------  ##
# Plotting dataframes
cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = "Year")
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = "Year")
pnc.lims <- c(0, 0.6)

# Plot
cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(Year, Panic, color = rep.int("x", nrow(cgr.pnc.pltdf)))) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.pnc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Panic Proportion Present") +
  scale_y_continuous(limits = pnc.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(Year, Panic, color = rep.int("x", nrow(ugr.pnc.pltdf)))) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.pnc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = pnc.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.pnc.plt

plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Panic.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                # Objective # 2 --- Heavy Grass Quadrats ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                    # Heavy CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.csg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.CSG", groupvars ="Year")
ugr.hvy.csg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.CSG", groupvars = "Herbicide.Treatment")
hvy.csg.lims <- c(0, 16)

# Plots
cgr.hvy.csg.plt <- ggplot(cgr.hvy.csg.pltdf, aes(Year, Hvy.CSG, color = rep.int("x", nrow(cgr.hvy.csg.pltdf)))) +
  geom_errorbar(aes(ymin = Hvy.CSG - se, ymax = Hvy.CSG + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.hvy.csg.pltdf))), size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Year", y = "# Quadrats >50% CSG") +
  scale_y_continuous(limits = hvy.csg.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.hvy.csg.plt

ugr.hvy.csg.plt <- ggplot(ugr.hvy.csg.pltdf, aes(Herbicide.Treatment, Hvy.CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Hvy.CSG - se, ymax = Hvy.CSG + se), width = 0.3, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Herbicide Treatment", y = " ") +
  geom_text(label = "A", x = 0.8, y = 13.3, color = "black") +
  geom_text(label = "AB", x = 1.7, y = 7.2, color = "black") +
  geom_text(label = "B", x = 2.8, y = 3.8, color = "black") +
  scale_y_continuous(limits = hvy.csg.lims) +
  scale_color_manual(values = ugr.colors) +
  pref.theme + theme(legend.position = "none"); ugr.hvy.csg.plt

# Save it
plot_grid(cgr.hvy.csg.plt, ugr.hvy.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy_CSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.wsg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.WSG", groupvars = "Year")
ugr.hvy.wsg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))
wsg.lims <- c(0, 15)

# Plots
cgr.hvy.wsg.plt <- ggplot(cgr.hvy.wsg.pltdf, aes(Year, Hvy.WSG, color = rep.int("x", nrow(cgr.hvy.wsg.pltdf)))) +
  geom_errorbar(aes(ymin = Hvy.WSG - se, ymax = Hvy.WSG + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.hvy.wsg.pltdf))), size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Year", y = "# Quadrats >50% WSG") +
  scale_y_continuous(limits = wsg.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.hvy.wsg.plt

ugr.hvy.wsg.plt <- ggplot(ugr.hvy.wsg.pltdf, aes(Year, Hvy.WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), position = dodge, width = 0.3) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  ugr.vlines + pref.theme + theme(legend.position = c(0.2, 0.9)); ugr.hvy.wsg.plt

# Save it.
plot_grid(cgr.hvy.wsg.plt, ugr.hvy.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy.WSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
# Plotting dataframes
cgr.hvy.fsc.pltdf <- summarySE(data = cgr, measurevar = "Hvy.Fesc", groupvars = "Year")
ugr.hvy.fsc.pltdf <- summarySE(data = ugr, measurevar = "Hvy.Fesc", groupvars = "Year")
hvy.fsc.lims <- c(0, 17)

# Plot
cgr.hvy.fsc.plt <- ggplot(cgr.hvy.fsc.pltdf, aes(Year, Hvy.Fesc, color = rep.int("x", nrow(cgr.hvy.fsc.pltdf)))) +
  geom_errorbar(aes(ymin = Hvy.Fesc - se, ymax = Hvy.Fesc + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.hvy.fsc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "# Quadrats >50% Fescue") +
  scale_y_continuous(limits = hvy.fsc.lims) +
  geom_text(label = "A", x = 13.75, y = 13.2, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 8.3, color = "black") +
  geom_text(label = "B", x = 15.75, y = 5.0, color = "black") +
  geom_text(label = "B", x = 16.75, y = 2.6, color = "black") +
  geom_text(label = "B", x = 17.75, y = 3.1, color = "black") +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.hvy.fsc.plt

ugr.hvy.fsc.plt <- ggplot(ugr.hvy.fsc.pltdf, aes(Year, Hvy.Fesc, color = rep.int("x", nrow(cgr.hvy.fsc.pltdf)))) +
  geom_errorbar(aes(ymin = Hvy.Fesc - se, ymax = Hvy.Fesc + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.hvy.fsc.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = hvy.fsc.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.hvy.fsc.plt

plot_grid(cgr.hvy.fsc.plt, ugr.hvy.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy.Fsc.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                # Objective # 3  --- Structural Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                        # Bare Cover ####
##  -------------------------------------------------------------------------  ##
cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = "Year")
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = "Year")
bar.lims <- c(5, 50)

# Plot
cgr.bar.plt <- ggplot(cgr.bar.pltdf, aes(Year, Bare, color = rep.int("x", nrow(cgr.bar.pltdf)))) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.bar.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Bare % Cover") +
  scale_y_continuous(limits = bar.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.bar.plt

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(Year, Bare, color = rep.int("x", nrow(ugr.bar.pltdf)))) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.bar.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = bar.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.bar.plt

plot_grid(cgr.bar.plt, ugr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Bare.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Litter Cover ####
##  -------------------------------------------------------------------------  ##
cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = "Year")
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = "Year")
ltr.lims <- c(15, 100)

# Plot
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(Year, Litter, color = rep.int("x", nrow(cgr.ltr.pltdf)))) +
  geom_errorbar(aes(ymin = Litter - se, ymax = Litter + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.ltr.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Litter % Cover") +
  geom_text(label = "A", x = 13.75, y = 76, color = "black") +
  geom_text(label = "B", x = 15.2, y = 33, color = "black") +
  geom_text(label = "AB", x = 15.7, y = 58, color = "black") +
  geom_text(label = "B", x = 16.8, y = 82, color = "black") +
  geom_text(label = "AB", x = 18.25, y = 59, color = "black") +
  scale_y_continuous(limits = ltr.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.ltr.plt

ugr.ltr.plt <- ggplot(ugr.ltr.pltdf, aes(Year, Litter, color = rep.int("x", nrow(cgr.ltr.pltdf)))) +
  geom_errorbar(aes(ymin = Litter - se, ymax = Litter + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.ltr.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = ltr.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.ltr.plt

plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Litter.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Robel Height (dm) ####
##  -------------------------------------------------------------------------  ##
# Plotting dataframes
cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = "Year")
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = "Year")
rbl.lims <- c(1, 10)

# Plot
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(Year, Robel, color = rep.int("x", nrow(cgr.rbl.pltdf)))) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.rbl.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Robel % Cover") +
  scale_y_continuous(limits = rbl.lims) +
  geom_text(label = "A", x = 13.75, y = 2, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 2, color = "black") +
  geom_text(label = "AB", x = 15.6, y = 3.5, color = "black") +
  geom_text(label = "B", x = 16.75, y = 3.9, color = "black") +
  geom_text(label = "AB", x = 17.75, y = 3.6, color = "black") +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(Year, Robel, color = rep.int("x", nrow(cgr.rbl.pltdf)))) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.rbl.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  scale_y_continuous(limits = rbl.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.rbl.plt

plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Robel.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Litter depth (cm) ####
##  -------------------------------------------------------------------------  ##
cgr.ldp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = "Year")
ugr.ldp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = "Year")
ldp.lims <- c(0, 6)

cgr.ldp.plt <- ggplot(cgr.ldp.pltdf, aes(Year, LitDep, color = rep.int("x", nrow(cgr.ldp.pltdf)))) +
  geom_errorbar(aes(ymin = LitDep - se, ymax = LitDep + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.ldp.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Litter Depth (cm)") +
  scale_y_continuous(limits = ldp.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.ldp.plt

ugr.ldp.plt <- ggplot(ugr.ldp.pltdf, aes(Year, LitDep, color = rep.int("x", nrow(ugr.ldp.pltdf)))) +
  geom_errorbar(aes(ymin = LitDep - se, ymax = LitDep + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(ugr.ldp.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = " ") +
  geom_text(label = "A", x = 13.75, y = 1.5, color = "black") +
  geom_text(label = "AB", x = 15.35, y = 1.7, color = "black") +
  geom_text(label = "AB", x = 15.7, y = 2.5, color = "black") +
  geom_text(label = "B", x = 16.75, y = 4.5, color = "black") +
  geom_text(label = "AB", x = 17.75, y = 1.8, color = "black") +
  scale_y_continuous(limits = ldp.lims) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + ugr.vlines + theme(legend.position = "none"); ugr.ldp.plt

plot_grid(cgr.ldp.plt, ugr.ldp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/LitDep.pdf", plot = last_plot())

# END ####


