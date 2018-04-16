##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                            # Daubenmire Data from the Grand River Grasslands
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
##  ---------------------------------------------------------------------------------------------  ##
                          # Spray and Seed Project
##  ---------------------------------------------------------------------------------------------  ##
# Project Leads
  ## Jaime J Coon & Nicholas J Lyon

# Main Project Questions
  ## How do plants respond to SnS (dumb phrasing, will return)

##  ----------------  ##
  # General Notes
##  ----------------  ##
# NOTE ON STATS:
  # "sig" = significant at alpha = 0.05 (p < 0.05)
      ## This is the only result that will get plotted
  # "marginally sig" = significant at alpha = 0.10 (p < 0.10)
  # "NS" = p > 0.10

# NOTE ON COLORS:
  # Colors are selected to be colorblind safe across all levels of Treatment/Herb Treatment

# Code written by Nicholas J Lyon

##  ---------------------------------------------------------------------------------------------  ##
                       # Necessary Nuts and Bolts
##  ---------------------------------------------------------------------------------------------  ##
# Required libraries
library(geomorph) # Analysis
library(ggplot2); library(cowplot) # Plotting
library(Rmisc) # get summary values for plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/_Daubenmire Project/Daubenmire_Collab_SnS_Branch")

# Tabula rasa
rm(list = ls())

# Pull in the dataset
sns <- read.csv("./Data/snsdata.csv")

# Make sure year is considered a factor
str(sns$Year)
sns$Year <- as.factor(sns$Year)
str(sns$Year)

# Re-level the factors too though
levels(sns$HerbTrmnt)
sns$HerbTrmnt <- as.character(sns$HerbTrmnt)
str(sns$HerbTrmnt)
sns$HerbTrmnt <- factor(sns$HerbTrmnt, levels = c("Con", "Spr", "SnS"))
levels(sns$HerbTrmnt)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

# Plotting shortcuts
yr.labs <- c("2014", "2015", "2016", "2017")
sns.labs <- c("Con", "Spr", "SnS")
yr.colors <- c("2014" = "#969696",
               "2015" = "#f768a1", "2016" = "#dd3497", "2017" = "#ae017e", "2018" = "#7a0177")
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
cgr.ns.color <- "#fdae61"
ugr.ns.color <- "#abd9e9"
dodge <- position_dodge(width = 0.5)

# Will need this custom function to parse the pairwise comparisons if any interaction term comes out sig
sig.reduced <- function(adv.procD.obj, p.dig = 4, thresh = 0.1, sig.thresh = 0.05){
  ## adv.procD.obj = object of a geomorph::advanced.procD.lm function
  ## p.dig = the number of digits you want the p value rounded to
  ## thresh = the upper threshold of the p values you want to keep
  ## sig.thresh = when getting a qualitative assessment of signficance, what is your cut-off?
  
  # Get just the p values 
  ## You can refer to the whole output later to get relevant stats when you know what you're looking for
  pairs <- adv.procD.obj$P.means.dist
  
  # Want to ditch either the top diagonal or the bottom diagonal of the matrix of p-values
  ## These are redundant with the opposite triangle of the matrix
  ## I've arbitrarily chosen to eliminate the lower triangle, but it doesn't matter
  pairs[lower.tri(pairs, diag = T)] <- NA
  
  # Now get the p values out of that matrix
  pvals <- as.vector( round(pairs, digits = p.dig) )
  
  # Get the list of combinations that the matrix includes
  combos <- expand.grid(rownames(adv.procD.obj$P.means.dist), colnames(adv.procD.obj$P.means.dist))
  
  # Get those as vectors in their own right
  var1.vec <- combos$Var1
  var2.vec <- combos$Var2
  
  # And just in case you want one where they're combined...
  var.vecs <- paste(combos$Var1, "-", combos$Var2)
  
  # THE FOLLOWING LINES ARE ONLY RELEVANT TO THIS PROJECT
  
  # Want to get the information in each composite variable on its own to subset later
  require(stringr)
  yr1 <- str_sub(var1.vec, 1, 4)
  trt1 <- str_sub(var1.vec, 6, 8)
  yr2 <- str_sub(var2.vec, 1, 4)
  trt2 <- str_sub(var2.vec, 6, 8)
  
  # Okay, now make a dataframe of both your p values and your newly created variable vectors
  results <- data.frame(Comparisons = var.vecs,
                        Year.1 = yr1, Year.2 = yr2,
                        Treat.1 = trt1, Treat.2 = trt2,
                        P.Values = pvals)
  
  # NOW BACK TO NORMAL RELEVANCE
  
  # Ditch the NAs you inserted (aka the pvalues along the diagonal and in the triangle you eliminated)
  results2 <- results[complete.cases(results),]
  
  # And ditch all the rows for p values above some threshold (user-defined)
  results3 <- subset(results2, results2$P.Values < thresh)
  
  # Though you probably want to know if the stuff is significant at a glance
  results3$Significance <- results3$P.Values
  results3$Significance <- ifelse(results3$Significance >= sig.thresh, "Marginal", "Sig")
  
  # Get rid of the bothersome and distracting row numbering
  row.names(results3) <- NULL
  
  # And spit out the result
  return(results3)
  
}

##  ---------------------------------------------------------------------------------------------  ##
                            # Analysis & Plotting
##  ---------------------------------------------------------------------------------------------  ##

##  ----------------------------------------------------  ##
                  # Analysis
##  ----------------------------------------------------  ##
# Question 1: What was the effect of the spray and seed project* on veg characteristics
  ## * = ON GRAZED SITES (CGRs)

# Question 2: What was the effect of the spray and seed project* on veg characteristics
  ## * = ON UN-GRAZED SITES (UGRs)

##  --------------------------  ##
  # Cool Season Grasses
##  --------------------------  ##
# Analysis
procD.lm(CSG ~ HerbTrmnt * Year, data = cgr) # NS
procD.lm(CSG ~ HerbTrmnt * Year, data = ugr) # trt = sig
advanced.procD.lm(CSG ~ HerbTrmnt + Year, ~ Year, ~ HerbTrmnt, data = ugr)
  ## Con = A | Spr = AB | SnS = B

# Get summary stats for plotting
cgr.csg.pltdf <- summarySE(data = cgr, measurevar = "CSG", groupvars = c("HerbTrmnt"))
ugr.csg.pltdf <- summarySE(data = ugr, measurevar = "CSG", groupvars = c("HerbTrmnt"))
csg.lims <- c(10, 50)

# Plots
cgr.csg.plt <- ggplot(cgr.csg.pltdf, aes(HerbTrmnt, CSG, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("CSG % Cover", limits = csg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(HerbTrmnt, CSG, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = CSG - se, ymax = CSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("CSG % Cover", limits = csg.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.csg.plt

# Save it.
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = c("", "**"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/CSG.pdf",plot = last_plot())

##  --------------------------  ##
 # Warm Season Grasses
##  --------------------------  ##
# Analysis
procD.lm(WSG ~ HerbTrmnt * Year, data = cgr) # NS
procD.lm(WSG ~ HerbTrmnt * Year, data = ugr) # trt = sig
advanced.procD.lm(WSG ~ HerbTrmnt + Year, ~ Year, ~ HerbTrmnt, data = ugr)
  ## Con = A | Spr = A | SnS = B

# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("HerbTrmnt"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("HerbTrmnt"))
wsg.lims <- c(0, 25)

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(HerbTrmnt, WSG, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(HerbTrmnt, WSG, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.wsg.plt

# Save it.
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = c("", "**"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/WSG.pdf",plot = last_plot())

##  --------------------------  ##
         # Fescue
##  --------------------------  ##
# Analysis
procD.lm(Fescue ~ HerbTrmnt * Year, data = cgr) # yr = sig
advanced.procD.lm(Fescue ~ HerbTrmnt + Year, ~ 1, ~ Year, data = cgr)
  ## 14 = A | 15 = B | 16 = B | 17 = B

procD.lm(Fescue ~ HerbTrmnt * Year, data = ugr) # yr = marginally sig
advanced.procD.lm(Fescue ~ HerbTrmnt + Year, ~ 1, ~ Year, data = ugr)
  ## 14 = A | 15 = A | 16 = A | 17 = B

cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "HerbTrmnt"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "HerbTrmnt"))
fsc.lims <- c(0, 65)

# Plot
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(HerbTrmnt, Fescue, color = Year)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(HerbTrmnt, Fescue, color = Year)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.fsc.plt

plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("**", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Fescue.pdf", plot = last_plot())

##  --------------------------  ##
  # Seedmix Threshhold
##  --------------------------  ##
# Analysis
procD.lm(Seedmix ~ HerbTrmnt * Year, data = cgr) # year = marginally sig
procD.lm(Seedmix ~ HerbTrmnt * Year, data = ugr) # NS

cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("HerbTrmnt"))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("HerbTrmnt"))
smx.lims <- c(0, 0.15)

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(HerbTrmnt, Seedmix, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("Seedmix Threshold", limits = smx.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(HerbTrmnt, Seedmix, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("Seedmix Threshold", limits = smx.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.smx.plt

# Save it.
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = c("", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Seedmix.pdf", plot = last_plot())
##  --------------------------  ##
         # Forbs
##  --------------------------  ##
procD.lm(Forbs ~ HerbTrmnt * Year, data = cgr) # yr & trt = sig
frb.posthoc <- advanced.procD.lm(Forbs ~ HerbTrmnt * Year, ~ 1, ~ as.factor(paste(cgr$Year, cgr$HerbTrmnt)), data = cgr)
sig.reduced(frb.posthoc) # not parsed as of right now

procD.lm(Forbs ~ HerbTrmnt * Year, data = ugr) # NS

cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("HerbTrmnt", "Year"))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("HerbTrmnt", "Year"))
frb.lims <- c(5, 70)

# Plots
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(HerbTrmnt, Forbs, color = Year)) +
  geom_errorbar(aes(ymin = Forbs - se, ymax = Forbs + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(HerbTrmnt, Forbs, color = Year)) +
  geom_errorbar(aes(ymin = Forbs - se, ymax = Forbs + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.frb.plt

# Save it.
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = c("**", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Forbs.pdf", plot = last_plot())

##  --------------------------  ##
        # Legumes
##  --------------------------  ##
# Analysis
procD.lm(Legumes ~ HerbTrmnt * Year, data = cgr) # yr = marginally sig
advanced.procD.lm(Legumes ~ HerbTrmnt + Year, ~ 1, ~ Year, data = cgr)
  ## 14 = A | 15 = A | 16 = A | 17 = B

procD.lm(Legumes ~ HerbTrmnt * Year, data = ugr) # interxn = SIG
lgm.posthoc <- advanced.procD.lm(Legumes ~ HerbTrmnt * Year, ~ 1, ~ as.factor(paste(ugr$Year, ugr$HerbTrmnt)), data = ugr)
sig.reduced(lgm.posthoc) # not parsed as of right now

cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year", "HerbTrmnt"))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "HerbTrmnt"))
lgm.lims <- c(0, 65)

# Plot
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(HerbTrmnt, Legumes, color = Year)) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Legumes % Cover", limits = c()) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(HerbTrmnt, Legumes, color = Year)) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Legumes % Cover", limits = lgm.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.lgm.plt

plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = c("", "**"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Legumes.pdf", plot = last_plot())

##  --------------------------  ##
         # Woody
##  --------------------------  ##
# Analysis
procD.lm(Woody ~ HerbTrmnt * Year, data = cgr) # trt = marginally sig
advanced.procD.lm(Woody ~ HerbTrmnt + Year, ~ 1, ~ HerbTrmnt, data = cgr)
  ## Con = AB | Spr = A | SnS = B

procD.lm(Woody ~ HerbTrmnt * Year, data = ugr) # yr = sig
advanced.procD.lm(Woody ~ HerbTrmnt + Year, ~ 1, ~ Year, data = ugr)
  ## 14 = A | 15 = A | 16 = A | 17 = B

cgr.wdy.pltdf <- summarySE(data = cgr, measurevar = "Woody", groupvars = c("HerbTrmnt"))
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year"))
wdy.lims <- c(0, 5)

# Plot
cgr.wdy.plt <- ggplot(cgr.wdy.pltdf, aes(HerbTrmnt, Woody, color = HerbTrmnt)) +
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

plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = c("**", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Woody.pdf", plot = last_plot())

##  --------------------------  ##
       # Bare Cover
##  --------------------------  ##
procD.lm(Bare ~ HerbTrmnt * Year, data = cgr) # yr = sig
advanced.procD.lm(Bare ~ HerbTrmnt + Year, ~ 1, ~ Year, data = cgr)
## 14 = A | 15 = B | 16 = A | 17 = A

procD.lm(Bare ~ HerbTrmnt * Year, data = ugr) # NS

cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Year"))
bar.lims <- c(0, 60)

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

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(Year, Bare, color = Year)) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Bare % Cover", limits = bar.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.bar.plt

plot_grid(cgr.bar.plt, ugr.bar.plt, labels = c("**", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Bare.pdf", plot = last_plot())

##  --------------------------  ##
      # Litter Cover
##  --------------------------  ##
procD.lm(Litter ~ HerbTrmnt * Year, data = cgr) # interxn = sig
ltr.posthoc <- advanced.procD.lm(Legumes ~ HerbTrmnt * Year, ~ 1, ~ as.factor(paste(cgr$Year, cgr$HerbTrmnt)), data = cgr)
sig.reduced(ltr.posthoc) # not parsed as of right now

procD.lm(Litter ~ HerbTrmnt * Year, data = ugr) # NS

cgr.ltr.pltdf <- summarySE(data = cgr, measurevar = "Litter", groupvars = c("Year", "HerbTrmnt"))
ugr.ltr.pltdf <- summarySE(data = ugr, measurevar = "Litter", groupvars = c("Year"))
ltr.lims <- c(0, 100)

# Plot
cgr.ltr.plt <- ggplot(cgr.ltr.pltdf, aes(HerbTrmnt, Litter, color = Year)) +
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

plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = c("**", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Litter.pdf", plot = last_plot())

##  --------------------------  ##
       # Robel (dm)
##  --------------------------  ##
procD.lm(Robel ~ HerbTrmnt * Year, data = cgr) # NS

procD.lm(Robel ~ HerbTrmnt * Year, data = ugr) # NS

cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("HerbTrmnt"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("HerbTrmnt"))
rbl.lims <- c(0, 10)

# Plot
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(HerbTrmnt, Robel, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Robel % Cover", limits = rbl.lims) +
  xlab("Grazed") +
  scale_color_manual(values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(HerbTrmnt, Robel, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Robel % Cover", limits = rbl.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = ugr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.rbl.plt

plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = c("", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Robel.pdf", plot = last_plot())

##  --------------------------  ##
          # Panic
##  --------------------------  ##
procD.lm(Panic ~ HerbTrmnt * Year, data = cgr) # NS

procD.lm(Panic ~ HerbTrmnt * Year, data = ugr) # NS

cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = c("HerbTrmnt"))
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("HerbTrmnt"))
pnc.lims <- c(0, 1)

# Plot
cgr.pnc.plt <- ggplot(cgr.pnc.pltdf, aes(HerbTrmnt, Panic, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Panic Proportion Occ", limits = pnc.lims) +
  xlab("Grazed") +
  scale_color_manual(values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.pnc.plt

ugr.pnc.plt <- ggplot(ugr.pnc.pltdf, aes(HerbTrmnt, Panic, color = HerbTrmnt)) +
  geom_errorbar(aes(ymin = Panic - se, ymax = Panic + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Panic Proportion Occ", limits = pnc.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = ugr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.pnc.plt

plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = c("", ""), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Panic.pdf", plot = last_plot())

##  --------------------------  ##
    # Litter Depth (cm)
##  --------------------------  ##
procD.lm(LitDep ~ HerbTrmnt * Year, data = cgr) # yr = sig
advanced.procD.lm(LitDep ~ HerbTrmnt + Year, ~ 1, ~ Year, data = cgr)
  ## 14 = A | 15 = B | 16 = A | 17 = A

procD.lm(LitDep ~ HerbTrmnt * Year, data = ugr) # yr = sig
advanced.procD.lm(LitDep ~ HerbTrmnt + Year, ~ 1, ~ Year, data = ugr)
  ## 14 = A | 15 = A | 16 = AB | 17 = B

cgr.ltrdp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year"))
ugr.ltrdp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year"))
ltrdp.lims <- c(0, 75)

# Plot
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

plot_grid(cgr.ltrdp.plt, ugr.ltrdp.plt, labels = c("**", "**"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/LitDep.pdf", plot = last_plot())
