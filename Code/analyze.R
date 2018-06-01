##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                             # Daubenmire Data - Herbicide Treatment Project
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Project Leads
  ## Jaime J Coon & Nicholas J Lyon

# Main Project Question(s)
  ## How do plants respond to anti-fescue herbicide treatments?

# NOTE ON STATS:
  # "sig" = significant at alpha = 0.05 (p < 0.05)
      ## This is the only result that will get plotted
  # "marginally sig" = significant at alpha = 0.10 (p < 0.10)
  # "NS" = p > 0.10

# NOTE ON COLORS:
  # Colors are selected to be colorblind safe

# Code written by Nicholas J Lyon

##  ---------------------------------------------------------------------------------------------  ##
                            # Nuts & Bolts ####
##  ---------------------------------------------------------------------------------------------  ##
# Required libraries
library(geomorph) # Analysis
library(ggplot2); library(cowplot) # Plotting
library(Rmisc) # get summary values for plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")
                            
# Clear environment of other stuff
rm(list = ls())

# Pull in the dataset
sns <- read.csv("./Data/snsdata.csv")

# Make sure year is considered a factor
str(sns$Year)
sns$Year <- as.factor(sns$Year)
str(sns$Year)

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
panel.labs <- c("i", "ii")
dodge <- position_dodge(width = 0.5)

# Helpful custom functions
  ## Simplify advanced.procD.lm output and do multiple comparison adjustment (run as wrapper)
simp.procD <- function(adv.procD.obj, p.dig = 4, crit.dig = 4){
  ## adv.procD.obj = object of a geomorph::advanced.procD.lm function
  ## p.dig = the number of digits you want the p value rounded to
  ## thresh = the upper threshold of the p values you want to keep
  
  # Get just the p values 
  pairs <- adv.procD.obj$P.means.dist
  ## You can refer to the whole output later to get relevant stats when you know what you're looking for
  
  # Want to ditch either the top diagonal or the bottom diagonal of the matrix of p-values
  ## These are the mirror image of the opposite triangle of the matrix
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
  
  # Okay, now make a dataframe of both your p values and your newly created variable vectors
  results <- data.frame(Comparisons = var.vecs,
                        Factor.1 = var1.vec, Factor.2 = var2.vec,
                        P.Values = pvals)
  
  # Ditch the NAs you inserted (aka the pvalues along the diagonal and in the triangle you eliminated)
  results2 <- results[complete.cases(results),]
  
  # Sequential Bonferroni Bit
  
  # For sequential Bonferroni you need to rank the pairs based on ascending p value
  results3 <- results2[order(results2$P.Values),] # order the comparisons
  rank <- c(1:length(results3$Comparisons)) # assign them a rank based on this order
  
  # Modify the critical point based on the rank of each sequential p value
  results3$Crit.Pt <- round( with(results3, ( (0.05 / (length(results3$Comparisons) + 1 - rank)) ) ), 
                             digits = crit.dig)
  ## Sequential bonferroni is calculated as show above, but in plain English it is like this:
  ## Each comparison gets it's own, sequential, critical point
  ## This is determined by dividing the standard critical point (0.05) by
  ## the total number of comparisons plus 1, minus the "rank" of the p value
  ## where lower p values have a lower rank
  ## The final pairwise comparison will always have a critical point of 0.05 in this method
  ### E.g. 6 pairwise comparisons + 1 - 6 (for the sixth one) = 1
  ### And 0.05 / 1 = 0.05 (duh)
  
  # Though you probably want to know if the stuff is significant at a glance
  results3$"P/Crit" <- results3$P.Values / results3$Crit.Pt
  
  # Now get the ranges of "significance" to be reduced to qualitative bits
  results3$Sig <- ifelse(test = results3$"P/Crit" > 2, yes = " ",
                         no = ifelse(test = results3$"P/Crit" > 0.2, yes = ".",
                                     no = ifelse(test = results3$"P/Crit" > 0.02, yes = "*",
                                                 no = ifelse(test = results3$"P/Crit" > 0.002, yes = "**", no = "***"))))
  ## Viewer discretion is advized when using this bonus column
  
  # Just in case you don't want to look in the guts of this function to see what * vs. ** means:
  message("Sig codes: P / Crit > 2 = ''
          0.2 < P/C ≤ 2 = '.'
          0.02 < P/C ≤ 0.2 = '*'
          0.002 < P/C ≤ 0.02 = '**'
          P/C ≤ 0.002 = '***'")
  
  # Get rid of the bothersome and distracting row numbering
  row.names(results3) <- NULL
  
  # And spit out the result
  return(results3)
  
}
  ## Get's min and max values from a supplied vector, good for setting plot limits
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

# RESEARCH QUESTIONS ####

# Question 1: What was the effect of the spray and seed project* on veg characteristics
  ## * = ON GRAZED SITES (CGRs)

# Question 2: What was the effect of the spray and seed project* on veg characteristics
  ## * = ON UN-GRAZED SITES (UGRs)

##  -----------------------------------------  ##
     # Cool Season Grasses ####
##  -----------------------------------------  ##
# Analysis
procD.lm(CSG ~ Herbicide.Treatment * Year, data = cgr) # NS
procD.lm(CSG ~ Herbicide.Treatment * Year, data = ugr) # trt = sig
simp.procD(advanced.procD.lm(CSG ~ Herbicide.Treatment + Year, ~ Year, ~ Herbicide.Treatment, data = ugr))
  ## Con = A | Spr = AB | SnS = B

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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.csg.plt

ugr.csg.plt <- ggplot(ugr.csg.pltdf, aes(Herbicide.Treatment, CSG, color = Herbicide.Treatment)) +
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
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/CSG.pdf",plot = last_plot())

##  -----------------------------------------  ##
     # Warm Season Grasses ####
##  -----------------------------------------  ##
# Analysis
procD.lm(WSG ~ Herbicide.Treatment * Year, data = cgr) # NS
procD.lm(WSG ~ Herbicide.Treatment * Year, data = ugr) # trt = sig
simp.procD(advanced.procD.lm(WSG ~ Herbicide.Treatment + Year, ~ Year, ~ Herbicide.Treatment, data = ugr))
  ## Con = A | Spr = B | SnS = C

# Get summary stats for plotting
cgr.wsg.pltdf <- summarySE(data = cgr, measurevar = "WSG", groupvars = c("Herbicide.Treatment"))
ugr.wsg.pltdf <- summarySE(data = ugr, measurevar = "WSG", groupvars = c("Herbicide.Treatment"))
wsg.lims <- minmax(c(cgr.wsg.pltdf[,3], ugr.wsg.pltdf[,3]))

# Plots
cgr.wsg.plt <- ggplot(cgr.wsg.pltdf, aes(Herbicide.Treatment, WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(Herbicide.Treatment, WSG, color = Herbicide.Treatment)) +
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
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/WSG.pdf", plot = last_plot())

##  -----------------------------------------  ##
            # Fescue ####
##  -----------------------------------------  ##
# Analysis
procD.lm(Fescue ~ Herbicide.Treatment * Year, data = cgr) # yr = sig
simp.procD(advanced.procD.lm(Fescue ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = B | 16 = B | 17 = B

procD.lm(Fescue ~ Herbicide.Treatment * Year, data = ugr) # yr = marginally sig
simp.procD(advanced.procD.lm(Fescue ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = ugr))
  ## 14 = A | 15 = A | 16 = A | 17 = B

cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
fsc.lims <- minmax(c(cgr.fsc.pltdf[,4], ugr.fsc.pltdf[,4]))

# Plot
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(Herbicide.Treatment, Fescue, color = Year)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(Herbicide.Treatment, Fescue, color = Year)) +
  geom_errorbar(aes(ymin = Fescue - se, ymax = Fescue + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.fsc.plt

plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Fescue.pdf", plot = last_plot())

##  -----------------------------------------  ##
      # Seedmix Threshhold ####
##  -----------------------------------------  ##
# Analysis
procD.lm(Seedmix ~ Herbicide.Treatment * Year, data = cgr) # year = marginally sig
simp.procD(advanced.procD.lm(Seedmix ~ Herbicide.Treatment * Year, ~ 1, ~ Year, data = cgr))
  ## 2014 = A | 2015 = AB | 2016 = B | 2017 = B
procD.lm(Seedmix ~ Herbicide.Treatment * Year, data = ugr) # NS

cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Herbicide.Treatment"))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Herbicide.Treatment"))
smx.lims <- minmax(c(cgr.smx.pltdf[,3], ugr.smx.pltdf[,3]), slack = 0.1)

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(Herbicide.Treatment, Seedmix, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Grazed") +
  scale_y_continuous("Seedmix Proportion", limits = smx.lims) +
  scale_color_manual(values = cgr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(Herbicide.Treatment, Seedmix, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Seedmix - se, ymax = Seedmix + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("Seedmix Proportion", limits = smx.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.smx.plt

# Save it.
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Seedmix.pdf", plot = last_plot())

##  -----------------------------------------  ##
            # Forbs ####
##  -----------------------------------------  ##
procD.lm(Forbs ~ Herbicide.Treatment * Year, data = cgr) # yr & trt = sig
simp.procD(advanced.procD.lm(Forbs ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = cgr))
  ## Only marginal significance for some pairwise comparisons (after adjustment)

procD.lm(Forbs ~ Herbicide.Treatment * Year, data = ugr) # NS

cgr.frb.pltdf <- summarySE(data = cgr, measurevar = "Forbs", groupvars = c("Herbicide.Treatment", "Year"))
ugr.frb.pltdf <- summarySE(data = ugr, measurevar = "Forbs", groupvars = c("Herbicide.Treatment", "Year"))
frb.lims <- minmax(c(cgr.frb.pltdf[,4], ugr.frb.pltdf[,4]))

# Plots
cgr.frb.plt <- ggplot(cgr.frb.pltdf, aes(Herbicide.Treatment, Forbs, color = Year)) +
  geom_errorbar(aes(ymin = Forbs - se, ymax = Forbs + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(Herbicide.Treatment, Forbs, color = Year)) +
  geom_errorbar(aes(ymin = Forbs - se, ymax = Forbs + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.frb.plt

# Save it.
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Forbs.pdf", plot = last_plot())

##  -----------------------------------------  ##
          # Legumes ####
##  -----------------------------------------  ##
# Analysis
procD.lm(Legumes ~ Herbicide.Treatment * Year, data = cgr) # yr = marginally sig
simp.procD(advanced.procD.lm(Legumes ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = B | 16 = A | 17 = B

procD.lm(Legumes ~ Herbicide.Treatment * Year, data = ugr) # interxn = SIG
simp.procD(advanced.procD.lm(Legumes ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = ugr))
  ## Some marginal significance after adjustment

cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
lgm.lims <- minmax(c(cgr.lgm.pltdf[,4], ugr.lgm.pltdf[,4]))

# Plot
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(Herbicide.Treatment, Legumes, color = Year)) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Legumes % Cover", limits = c()) +
  xlab("Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(Herbicide.Treatment, Legumes, color = Year)) +
  geom_errorbar(aes(ymin = Legumes - se, ymax = Legumes + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Legumes % Cover", limits = lgm.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.lgm.plt

plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Legumes.pdf", plot = last_plot())

##  -----------------------------------------  ##
            # Woody ####
##  -----------------------------------------  ##
# Analysis
procD.lm(Woody ~ Herbicide.Treatment * Year, data = cgr) # trt = marginally sig
simp.procD(advanced.procD.lm(Woody ~ Herbicide.Treatment + Year, ~ 1, ~ Herbicide.Treatment, data = cgr))
  ## Con = AB | Spr = A | SnS = B

procD.lm(Woody ~ Herbicide.Treatment * Year, data = ugr) # yr = sig
simp.procD(advanced.procD.lm(Woody ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = ugr))
  ## 14 = A | 15 = A | 16 = A | 17 = B

cgr.wdy.pltdf <- summarySE(data = cgr, measurevar = "Woody", groupvars = c("Herbicide.Treatment"))
ugr.wdy.pltdf <- summarySE(data = ugr, measurevar = "Woody", groupvars = c("Year"))
wdy.lims <-minmax(c(cgr.wdy.pltdf[,3], ugr.wdy.pltdf[,3]))

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

plot_grid(cgr.wdy.plt, ugr.wdy.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Woody.pdf", plot = last_plot())

##  -----------------------------------------  ##
          # Bare Cover ####
##  -----------------------------------------  ##
procD.lm(Bare ~ Herbicide.Treatment * Year, data = cgr) # yr = sig
simp.procD(advanced.procD.lm(Bare ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = B | 16 = A | 17 = A

procD.lm(Bare ~ Herbicide.Treatment * Year, data = ugr) # NS

cgr.bar.pltdf <- summarySE(data = cgr, measurevar = "Bare", groupvars = c("Year"))
ugr.bar.pltdf <- summarySE(data = ugr, measurevar = "Bare", groupvars = c("Year"))
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

ugr.bar.plt <- ggplot(ugr.bar.pltdf, aes(Year, Bare, color = Year)) +
  geom_errorbar(aes(ymin = Bare - se, ymax = Bare + se), width = 0.3, size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2, position = dodge) +
  scale_y_continuous("Bare % Cover", limits = bar.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = yr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.bar.plt

plot_grid(cgr.bar.plt, ugr.bar.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Bare.pdf", plot = last_plot())

##  -----------------------------------------  ##
          # Litter Cover ####
##  -----------------------------------------  ##

procD.lm(Litter ~ Herbicide.Treatment * Year, data = cgr) # interxn = sig
simp.procD(advanced.procD.lm(Legumes ~ Herbicide.Treatment * Year, ~ 1, ~ Composite.Variable, data = cgr))
  ## Some marginal significance after adjustment

procD.lm(Litter ~ Herbicide.Treatment * Year, data = ugr) # NS

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

plot_grid(cgr.ltr.plt, ugr.ltr.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Litter.pdf", plot = last_plot())

##  -----------------------------------------  ##
          # Robel (dm) ####
##  -----------------------------------------  ##
procD.lm(Robel ~ Herbicide.Treatment * Year, data = cgr) # NS

procD.lm(Robel ~ Herbicide.Treatment * Year, data = ugr) # NS

cgr.rbl.pltdf <- summarySE(data = cgr, measurevar = "Robel", groupvars = c("Herbicide.Treatment"))
ugr.rbl.pltdf <- summarySE(data = ugr, measurevar = "Robel", groupvars = c("Herbicide.Treatment"))
rbl.lims <- minmax(c(cgr.rbl.pltdf[,3], ugr.rbl.pltdf[,3]), slack = 1)

# Plot
cgr.rbl.plt <- ggplot(cgr.rbl.pltdf, aes(Herbicide.Treatment, Robel, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Robel % Cover", limits = rbl.lims) +
  xlab("Grazed") +
  scale_color_manual(values = cgr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); cgr.rbl.plt

ugr.rbl.plt <- ggplot(ugr.rbl.pltdf, aes(Herbicide.Treatment, Robel, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = Robel - se, ymax = Robel + se), width = 0.3, size = 1) +
  geom_point(stat = 'identity', size = 2) +
  scale_y_continuous("Robel % Cover", limits = rbl.lims) +
  xlab("Un-Grazed") +
  scale_color_manual(values = ugr.colors) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none", legend.title = element_blank()); ugr.rbl.plt

plot_grid(cgr.rbl.plt, ugr.rbl.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Robel.pdf", plot = last_plot())

##  -----------------------------------------  ##
             # Panic ####
##  -----------------------------------------  ##
procD.lm(Panic ~ Herbicide.Treatment * Year, data = cgr) # NS

procD.lm(Panic ~ Herbicide.Treatment * Year, data = ugr) # NS

cgr.pnc.pltdf <- summarySE(data = cgr, measurevar = "Panic", groupvars = c("Herbicide.Treatment"))
ugr.pnc.pltdf <- summarySE(data = ugr, measurevar = "Panic", groupvars = c("Herbicide.Treatment"))
pnc.lims <- minmax(c(cgr.pnc.pltdf[,3], ugr.pnc.pltdf[,3]), slack = 0.1)

# Plot
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

plot_grid(cgr.pnc.plt, ugr.pnc.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Panic.pdf", plot = last_plot())

##  -----------------------------------------  ##
      # Litter Depth (cm) ####
##  -----------------------------------------  ##
procD.lm(LitDep ~ Herbicide.Treatment * Year, data = cgr) # yr = sig
simp.procD(advanced.procD.lm(LitDep ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = B | 16 = A | 17 = A

procD.lm(LitDep ~ Herbicide.Treatment * Year, data = ugr) # yr = sig
simp.procD(advanced.procD.lm(LitDep ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = ugr))
  ## 14 = A | 15 = A | 16 = AB | 17 = B

cgr.ltrdp.pltdf <- summarySE(data = cgr, measurevar = "LitDep", groupvars = c("Year"))
ugr.ltrdp.pltdf <- summarySE(data = ugr, measurevar = "LitDep", groupvars = c("Year"))
ltrdp.lims <- minmax(c(cgr.ltrdp.pltdf[,3], ugr.ltrdp.pltdf[,3]), slack = 5)

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

plot_grid(cgr.ltrdp.plt, ugr.ltrdp.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/LitDep.pdf", plot = last_plot())

# END ####


