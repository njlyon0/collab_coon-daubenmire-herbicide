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
setwd("~/Documents/School/1. Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                              # Houskeeping ####
##  ---------------------------------------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/snsdata.csv")

# Re-level the factors too though
unique(sns$Herbicide.Treatment)
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# Plotting shortcuts
yr.labs <- c("14", "15", "16", "17", "18")
sns.labs <- c("Con", "Spr", "SnS")
yr.colors <- c("14" = "#969696", "15" = "#f768a1", "16" = "#dd3497",
               "17" = "#ae017e", "18" = "#7a0177")
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
cgr.ns.color <- "#fdae61"
ugr.ns.color <- "#abd9e9"
dodge <- position_dodge(width = 0.5)
cgr.vlines <- geom_vline(xintercept = c(14.4, 14.6, 17.5), linetype = c(1, 2, 1))
ugr.vlines <- geom_vline(xintercept = 14.55, linetype = 2)
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank())

##  ---------------------------------------------------------------------------------------------  ##
              # Objective # 1 --- Functional Group Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                          # CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = "Year")
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = "Herbicide.Treatment")
csg.lims <- c(10, 50)

# Plots
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(Year, CSG, color = rep.int("x", nrow(cgr.csg.pltdf)))) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, position = dodge) +
  geom_line(aes(group = rep.int("x", nrow(cgr.csg.pltdf))), size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  labs(x = "Year", y = "CSG % Cover") +
  scale_y_continuous(limits = csg.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(Herbicide.Treatment, CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  geom_text(label = "A", x = 0.8, y = 44.5, color = "black") +
  geom_text(label = "AB", x = 1.7, y = 29.5, color = "black") +
  geom_text(label = "B", x = 2.8, y = 21.5, color = "black") +
  labs(x = "Herbicide Treatment", y = "CSG % Cover") +
  scale_y_continuous(limits = csg.lims) +
  scale_color_manual(values = ugr.colors) +
  pref.theme + theme(legend.position = "none"); ugr.csg.plt

# Save it
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/CSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
wsg.lims <- c(0, 45)

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
  geom_line(aes(group = rep.int("Z", nrow(cgr.smx.pltdf))), size = 1, position = dodge) +
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

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(Year, Seedmix, color = rep.int("x", nrow(cgr.smx.pltdf)))) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, position = dodge) +
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
  geom_point(stat = 'identity', size = 2, position = dodge) +
  labs(x = "Year", y = "Panic Proportion Present") +
  scale_y_continuous(limits = pnc.lims) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + cgr.vlines + theme(legend.position = "none"); cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(Year, Panic, color = rep.int("x", nrow(cgr.pnc.pltdf)))) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, position = dodge) +
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
cgr.hvy.csg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.CSG", groupvars = c("Herbicide.Treatment"))
ugr.hvy.csg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.CSG", groupvars = c("Herbicide.Treatment"))
hvy.csg.lims <- minmax(c(cgr.hvy.csg.pltdf[,3], ugr.hvy.csg.pltdf[,3]))

# Plots
cgr.hvy.csg.plt <- ggplot(cgr.hvy.csg.pltdf, aes(Herbicide.Treatment, Hvy.CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Hvy.CSG - se, ymax = Hvy.CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("# Quadrats ≥ 50% CSG", limits = hvy.csg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); cgr.hvy.csg.plt

ugr.hvy.csg.plt <- ggplot(ugr.hvy.csg.pltdf, aes(Herbicide.Treatment, Hvy.CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Hvy.CSG - se, ymax = Hvy.CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("# Quadrats ≥ 50% CSG", limits = hvy.csg.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.hvy.csg.plt

# Save it
plot_grid(cgr.hvy.csg.plt, ugr.hvy.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy_CSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.wsg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.WSG", groupvars = c("Herbicide.Treatment"))
ugr.hvy.wsg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.hvy.wsg.pltdf$Year <- as.numeric(as.character(ugr.hvy.wsg.pltdf$Year))
hvy.wsg.lims <- minmax(c(cgr.hvy.wsg.pltdf[,3], ugr.hvy.wsg.pltdf[,3]), slack = 10)

# Plots
cgr.hvy.wsg.plt <- ggplot(cgr.hvy.wsg.pltdf, aes(Herbicide.Treatment, Hvy.WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Hvy.WSG - se, ymax = Hvy.WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("# Quadrats ≥ 50% WSG", limits = hvy.wsg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); cgr.hvy.wsg.plt

ugr.hvy.wsg.plt <- ggplot(ugr.hvy.wsg.pltdf, aes(Year, Hvy.WSG, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), position = dodge, width = 0.3) +
  scale_y_continuous("# Quadrats ≥ 50% WSG", limits = hvy.wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  ugr.vlines + pref.theme + 
  theme(legend.position = c(0.65, 0.9)); ugr.hvy.wsg.plt

# Save it.
plot_grid(cgr.hvy.wsg.plt, ugr.hvy.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy.WSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
cgr.hvy.fsc.pltdf <- summarySE(data = cgr, measurevar = "Hvy.Fesc", groupvars = c("Year"))
ugr.hvy.fsc.pltdf <- summarySE(data = ugr, measurevar = "Hvy.Fesc", groupvars = c("Herbicide.Treatment"))
hvy.fsc.lims <- minmax(c(cgr.hvy.fsc.pltdf[,4], ugr.hvy.fsc.pltdf[,4]), slack = 20)

# Plot
cgr.hvy.fsc.plt <- ggplot(cgr.hvy.fsc.pltdf, aes(Year, Hvy.Fesc, color = Year)) +
  geom_errorbar(aes(ymin = Hvy.Fesc - se, ymax = Hvy.Fesc + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("# Quadrats ≥ 50% Fescue", limits = hvy.fsc.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.hvy.fsc.plt

ugr.hvy.fsc.plt <- ggplot(ugr.hvy.fsc.pltdf, aes(Herbicide.Treatment, Hvy.Fesc, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Hvy.Fesc - se, ymax = Hvy.Fesc + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("# Quadrats ≥ 50% Fescue", limits = hvy.fsc.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.hvy.fsc.plt

plot_grid(cgr.hvy.fsc.plt, ugr.hvy.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Hvy.Fesc.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                # Objective # 3  --- Structural Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                        # Bare Cover ####
##  -------------------------------------------------------------------------  ##
cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Herbicide.Treatment"))
bar.lims <- minmax(c(cgr.bar.pltdf[,3], ugr.bar.pltdf[,3]))

# Plot
cgr.bar.plt <- ggplot(cgr.bar.pltdf, aes(Year, Bare, color = Year)) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Bare % Cover", limits = bar.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.bar.plt

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(Herbicide.Treatment, Bare, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("Bare % Cover", limits = bar.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.bar.plt

plot_grid(cgr.bar.plt, ugr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Bare.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Litter Cover ####
##  -------------------------------------------------------------------------  ##
cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = c("Year"))
ltr.lims <- minmax(c(cgr.ltr.pltdf[,4], ugr.ltr.pltdf[,3]))

# Plot
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(Herbicide.Treatment, Litter, color = Year)) +
  geom_errorbar(aes(ymin = Litter - se, ymax = Litter + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Litter % Cover", limits = ltr.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.ltr.plt

ugr.ltr.plt <- ggplot(ugr.ltr.pltdf, aes(Year, Litter, color = Year)) +
  geom_errorbar(aes(ymin = Litter - se, ymax = Litter + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Litter % Cover", limits = ltr.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.ltr.plt

plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Litter.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Robel Height (dm) ####
##  -------------------------------------------------------------------------  ##
cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("Year"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("Herbicide.Treatment"))
rbl.lims <- minmax(c(cgr.rbl.pltdf[,3], ugr.rbl.pltdf[,3]), slack = 1)

# Plot
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(Year, Robel, color = Year)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Robel Height (dm)", limits = rbl.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(Herbicide.Treatment, Robel, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Robel Height (dm)", limits = rbl.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = ugr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.rbl.plt

plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/Robel.pdf", plot = last_plot())

##  -------------------------------------------------------------------------  ##
                  # Litter depth (cm) ####
##  -------------------------------------------------------------------------  ##
cgr.ltrdp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year"))
ugr.ltrdp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year"))
ltrdp.lims <- minmax(c(cgr.ltrdp.pltdf[,3], ugr.ltrdp.pltdf[,3]), slack = 5)

cgr.ltrdp.plt <- ggplot(cgr.ltrdp.pltdf, aes(Year, LitDep, color = Year)) +
  geom_errorbar(aes(ymin = LitDep - se, ymax = LitDep + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Litter Depth (cm)", limits = ltrdp.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.ltrdp.plt

ugr.ltrdp.plt <- ggplot(ugr.ltrdp.pltdf, aes(Year, LitDep, color = Year)) +
  geom_errorbar(aes(ymin = LitDep - se, ymax = LitDep + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Litter Depth (cm)", limits = ltrdp.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.ltrdp.plt

plot_grid(cgr.ltrdp.plt, ugr.ltrdp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/LitDep.pdf", plot = last_plot())

# END ####


