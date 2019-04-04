##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                    # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# PURPOSE
  ## Make publication-quality figures FOR 2014 VS. 2018 COMPARISONS
  ## This includes all objectives (1, 2, and 3)

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
sns <- read.csv("./Data/sns-data_14-vs-18.csv")

# Re-level the treatment factor
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# Plotting shortcuts
sns.labs <- c("Con", "Spr", "SnS")
yr.labs <- c("2014 (Pre-Treatment)", "x")
yr.labs <- scale_x_discrete(breaks = c("14", "18"))
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
# Get summary stats for plotting
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(x = Year, y = CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = CSG + se, ymin = CSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 65, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 65) +
  labs(x = "Year", y = "CSG (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.6, 0.85)); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(x = Year, y = CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = CSG + se, ymin = CSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 65) +
  labs(x = "Year", y = "CSG (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.85)); ugr.csg.plt

# Save these plots
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/CSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(x = Year, y = WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 45, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "WSG (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(x = Year, y = WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = WSG + se, ymin = WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 45, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "WSG (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.wsg.plt

# Save these plots
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/WSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Fescue ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 65) +
  labs(x = "Year", y = "Fescue (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 65, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 65) +
  labs(x = "Year", y = "Fescue (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.fsc.plt

# Save these plots
plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Fescue.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Seedmix ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(x = Year, y = Seedmix, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 0.14) +
  labs(x = "Year", y = "Seedmix (Prop.)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.9)); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(x = Year, y = Seedmix, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 0.14) +
  labs(x = "Year", y = "Seedmix (Prop.)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.9)); ugr.smx.plt

# Save these plots
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Seedmix.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Forbs ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(x = Year, y = Forbs, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 50) +
  labs(x = "Year", y = "Forbs (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(x = Year, y = Forbs, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 50, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 50) +
  labs(x = "Year", y = "Forbs (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.frb.plt

# Save these plots
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Forbs.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Legumes ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(x = Year, y = Legumes, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "Legumes (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(x = Year, y = Legumes, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 45, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 45) +
  labs(x = "Year", y = "Legumes (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.lgm.plt

# Save these plots
plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Legumes.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Woody ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.wdy.pltdf <- summarySE(data = cgr, measurevar = "Woody", groupvars = c("Year", "Herbicide.Treatment"))
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.wdy.plt <- ggplot(cgr.wdy.pltdf, aes(x = Year, y = Woody, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Woody + se, ymin = Woody - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 6, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 6) +
  labs(x = "Year", y = "Woody (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.wdy.plt

ugr.wdy.plt <- ggplot(ugr.wdy.pltdf, aes(x = Year, y = Woody, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Woody + se, ymin = Woody - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 6, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 6) +
  labs(x = "Year", y = "Woody (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.wdy.plt

# Save these plots
plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Woody.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Panic Grass ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = c("Year", "Herbicide.Treatment"))
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(x = Year, y = Panic, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 0.8) +
  labs(x = "Year", y = "Panic (Prop.)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(x = Year, y = Panic, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Panic + se, ymin = Panic - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 0.8, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 0.8) +
  labs(x = "Year", y = "Panic (Prop.)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.pnc.plt

# Save these plots
plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Panic.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy CSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.csg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.CSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.hvy.csg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.CSG", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.hvy.csg.plt <- ggplot(cgr.hvy.csg.pltdf, aes(x = Year, y = Hvy.CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.CSG + se, ymin = Hvy.CSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 30, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 30) +
  labs(x = "Year", y = "Heavy CSG Quadrats (#)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.hvy.csg.plt

ugr.hvy.csg.plt <- ggplot(ugr.hvy.csg.pltdf, aes(x = Year, y = Hvy.CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.CSG + se, ymin = Hvy.CSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 30) +
  labs(x = "Year", y = "Heavy CSG Quadrats (#)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.9)); ugr.hvy.csg.plt

# Save these plots
plot_grid(cgr.hvy.csg.plt, ugr.hvy.csg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.CSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Heavy WSG ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.wsg.pltdf <- summarySE(data = cgr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))
ugr.hvy.wsg.pltdf <- summarySE(data = ugr, measurevar = "Hvy.WSG", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.hvy.wsg.plt <- ggplot(cgr.hvy.wsg.pltdf, aes(x = Year, y = Hvy.WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 15, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 15) +
  labs(x = "Year", y = "Heavy WSG Quadrats (#)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.hvy.wsg.plt

ugr.hvy.wsg.plt <- ggplot(ugr.hvy.wsg.pltdf, aes(x = Year, y = Hvy.WSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.WSG + se, ymin = Hvy.WSG - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 15) +
  labs(x = "Year", y = "Heavy WSG Quadrats (#)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.hvy.wsg.plt

# Save these plots
plot_grid(cgr.hvy.wsg.plt, ugr.hvy.wsg.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.WSG.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Heavy Fescue ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.hvy.fsc.pltdf <- summarySE(data = cgr, measurevar = "Hvy.Fesc", groupvars = c("Year", "Herbicide.Treatment"))
ugr.hvy.fsc.pltdf <- summarySE(data = ugr, measurevar = "Hvy.Fesc", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.hvy.fsc.plt <- ggplot(cgr.hvy.fsc.pltdf, aes(x = Year, y = Hvy.Fesc, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.Fesc + se, ymin = Hvy.Fesc - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 25) +
  labs(x = "Year", y = "Heavy Fescue Quadrats (#)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.hvy.fsc.plt

ugr.hvy.fsc.plt <- ggplot(ugr.hvy.fsc.pltdf, aes(x = Year, y = Hvy.Fesc, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Hvy.Fesc + se, ymin = Hvy.Fesc - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 25) +
  labs(x = "Year", y = "Heavy Fescue Quadrats (#)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.hvy.fsc.plt

# Save these plots
plot_grid(cgr.hvy.fsc.plt, ugr.hvy.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Hvy.Fesc.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                          # Bare ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.bar.plt <- ggplot(cgr.bar.pltdf, aes(x = Year, y = Bare, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 55) +
  labs(x = "Year", y = "Bare (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.8)); cgr.bar.plt

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(x = Year, y = Bare, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Bare + se, ymin = Bare - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 55, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 55) +
  labs(x = "Year", y = "Bare (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.8)); ugr.bar.plt

# Save these plots
plot_grid(cgr.bar.plt, ugr.bar.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Bare.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                      # Litter (%) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(x = Year, y = Litter, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 100, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(35, 100) +
  labs(x = "Year", y = "Litter (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.ltr.plt

ugr.ltr.plt <- ggplot(ugr.ltr.pltdf, aes(x = Year, y = Litter, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Litter + se, ymin = Litter - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 100, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(35, 100) +
  labs(x = "Year", y = "Litter (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.ltr.plt

# Save these plots
plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Litter.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                        # Robel ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(x = Year, y = Robel, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "A", x = 14, y = 40, color = "black") +
  #geom_text(label = "B", x = 18, y = 62, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 10) +
  labs(x = "Year", y = "Robel (dm)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.05, 0.9)); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(x = Year, y = Robel, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Robel + se, ymin = Robel - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 10, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 10) +
  labs(x = "Year", y = "Robel (dm)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.rbl.plt

# Save these plots
plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Robel.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())

##  -------------------------------------------------------------------------  ##
                    # Litter (Depth) ####
##  -------------------------------------------------------------------------  ##
# Get summary stats for plotting
cgr.ldp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))
ugr.ldp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.ldp.plt <- ggplot(cgr.ldp.pltdf, aes(x = Year, y = LitDep, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 7, color = "black") +
  scale_color_manual(values = cgr.colors) +
  ylim(0, 7) +
  labs(x = "Year", y = "Litter Depth (cm)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.ldp.plt

ugr.ldp.plt <- ggplot(ugr.ldp.pltdf, aes(x = Year, y = LitDep, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = LitDep + se, ymin = LitDep - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  geom_text(label = "NS", x = 13.8, y = 7, color = "black") +
  scale_color_manual(values = ugr.colors) +
  ylim(0, 7) +
  labs(x = "Year", y = "Litter Depth (cm)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.ldp.plt

# Save these plots
plot_grid(cgr.ldp.plt, ugr.ldp.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/LitDep.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())


# END ####

