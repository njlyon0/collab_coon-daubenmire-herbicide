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

# Make sure year is considered a factor
unique(sns$Year)
sns$Year <- as.factor(sns$Year)
unique(sns$Year)

# Re-level the factors too though
unique(sns$Herbicide.Treatment)
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# Plotting shortcuts
yr.labs <- c("2014", "2015", "2016", "2017")
sns.labs <- c("Con", "Spr", "SnS")
yr.colors <- c("2014" = "#969696", "2015" = "#f768a1", "2016" = "#dd3497",
               "2017" = "#ae017e", "2018" = "#7a0177")
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
cgr.ns.color <- "#fdae61"
ugr.ns.color <- "#abd9e9"
dodge <- position_dodge(width = 0.5)
cgr.vlines <- geom_vline(xintercept = c(2014.4, 2014.5, 2014.6, 2017.4), linetype = c(1, 2, 3, 1))
ugr.vlines <- geom_vline(xintercept = c(2014.5, 2014.6), linetype = c(2, 3))
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank())

# Helpful axis limit-defining function
minmax <- function(x, dig = 0, slack = 10){
  ## x = vector for checking (can be concatenated from multiple sources but must be single object)
  ## dig = digits to round min and max to (default is 0 to yield integers)
  ## slack = how much room you want on either end of the true min and max?
  
  # Get and round minimum and maximums
  min.val <- round(min(x), digits = dig)
  max.val <- round(max(x), digits = dig)
  
  # Get 'em into a single value with slack accounted for
  bounds <- c((min.val - slack), (max.val + slack))
  
  # Make sure it can never get below 0
  bounds[bounds < 0] <- 0
  
  return(bounds)
  
}

##  ---------------------------------------------------------------------------------------------  ##
              # Objective # 1 --- Functional Group Response ####
##  ---------------------------------------------------------------------------------------------  ##
##  -------------------------------------------------------------------------  ##
                          # CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = c("Herbicide.Treatment"))
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = c("Herbicide.Treatment"))
csg.lims <- minmax(c(cgr.csg.pltdf[,3], ugr.csg.pltdf[,3]))

# Plots
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(Herbicide.Treatment, CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("CSG % Cover", limits = csg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(Herbicide.Treatment, CSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("CSG % Cover", limits = csg.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.csg.plt

# Save it
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/CSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf$Year <- as.numeric(as.character(ugr.wsg.pltdf$Year))
wsg.lims <- c(0, 45)

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(Herbicide.Treatment, WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(Year, WSG, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), position = dodge, width = 0.3) +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  ugr.vlines + pref.theme + 
  theme(legend.position = c(0.65, 0.9)); ugr.wsg.plt

# Save it.
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/WSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Fescue ####
##  -------------------------------------------------------------------------  ##
# Plotting dataframes
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Herbicide.Treatment"))
fsc.lims <- minmax(c(cgr.fsc.pltdf[,4], ugr.fsc.pltdf[,4]), slack = 30)

# Plot
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(Year, Fescue, color = Year)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(Herbicide.Treatment, Fescue, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.fsc.plt

plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Fescue.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                # Seed-mix Proportion ####
##  -------------------------------------------------------------------------  ##
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Year"))
cgr.smx.pltdf$Year <- as.numeric(as.character(cgr.smx.pltdf$Year))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Herbicide.Treatment"))
smx.lims <- minmax(c(cgr.smx.pltdf[,3], ugr.smx.pltdf[,3]), slack = 0.15)

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(Year, Seedmix, color = rep.int("Z", nrow(cgr.smx.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(cgr.smx.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(14.4, 14.5, 14.6, 17.4), linetype = c(1, 2, 3, 1)) +
  scale_y_continuous("Proportion Seed-mix ≥25% Cover", limits = smx.lims) +
  scale_x_continuous(breaks = seq(14, 18, 1)) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + theme(legend.position = "none"); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(Herbicide.Treatment, Seedmix, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("Proportion Seed-mix ≥25% Cover", limits = smx.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.smx.plt

# Save it.
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Seedmix.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Forbs ####
##  -------------------------------------------------------------------------  ##
cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("Herbicide.Treatment", "Year"))
cgr.frb.pltdf$Year <- as.numeric(as.character(cgr.frb.pltdf$Year))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Year"))
ugr.frb.pltdf$Year <- as.numeric(as.character(ugr.frb.pltdf$Year))
frb.lims <- minmax(c(cgr.frb.pltdf[,4], ugr.frb.pltdf[,4]))

# Plots
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(Year, Forbs, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(14.4, 14.5, 14.6, 17.4), linetype = c(1, 2, 3, 1)) +
  scale_y_continuous("Forb % Cover", limits = frb.lims) +
  scale_x_continuous(breaks = seq(14, 18, 1)) +
  scale_color_manual(values = cgr.colors) +
  pref.theme + theme(legend.position = c(0.65, 0.9)); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(Year, Forbs, color = rep.int("Z", nrow(ugr.frb.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(ugr.frb.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(14.5, 14.6), linetype = c(2, 3)) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  scale_x_continuous(breaks = seq(14, 18, 1)) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + theme(legend.position = "none"); ugr.frb.plt

# Save it.
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Forbs.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Legumes ####
##  -------------------------------------------------------------------------  ##
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year"))
cgr.lgm.pltdf$Year <- as.numeric(as.character(cgr.lgm.pltdf$Year))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Herbicide.Treatment"))
lgm.lims <- minmax(c(cgr.lgm.pltdf[,4], ugr.lgm.pltdf[,4]), slack = 20)

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(Herbicide.Treatment, Legumes, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("Legumes % Cover", limits = lgm.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.lgm.plt

plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Legumes.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Woody ####
##  -------------------------------------------------------------------------  ##
cgr.wdy.pltdf <- summarySE(data = cgr, measurevar = "Woody", groupvars = c("Herbicide.Treatment"))
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year"))
wdy.lims <- minmax(c(cgr.wdy.pltdf[,3], ugr.wdy.pltdf[,3]))

# Plot
cgr.wdy.plt <- ggplot(cgr.wdy.pltdf, aes(Herbicide.Treatment, Woody, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Woody - se, ymax = Woody + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Woody % Cover", limits = wdy.lims) +
  xlab("Grazed") +
  scale_color_manual(values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.wdy.plt

ugr.wdy.plt <- ggplot(ugr.wdy.pltdf, aes(Year, Woody, color = Year)) +
  geom_errorbar(aes(ymin = Woody - se, ymax = Woody + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Woody % Cover", limits = wdy.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.wdy.plt

plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Woody.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
              # Panic Proportion Occurrence ####
##  -------------------------------------------------------------------------  ##
cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = c("Herbicide.Treatment"))
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("Herbicide.Treatment"))
pnc.lims <- minmax(c(cgr.pnc.pltdf[,3], ugr.pnc.pltdf[,3]), slack = 0.75)

cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(Herbicide.Treatment, Panic, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Panic Proportion Occ", limits = pnc.lims) +
  xlab("Grazed") +
  scale_color_manual(values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(Herbicide.Treatment, Panic, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Panic Proportion Occ", limits = pnc.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = ugr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.pnc.plt

plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Panic.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Hvy_CSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Hvy.WSG.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Hvy.Fesc.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Bare.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Litter.pdf", width = 6, height = 4, units = 'in', plot = last_plot())

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
ggplot2::ggsave("./Graphs/Robel.pdf", plot = last_plot())

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
ggplot2::ggsave("./Graphs/LitDep.pdf", plot = last_plot())

# END ####


