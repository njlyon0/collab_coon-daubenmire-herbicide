##  --------------------------------------------------------------------------------------------------------------------------------------  ##
                      # Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# START ####

# Required libraries
library(RRPP) # Analysis
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
panel.labs <- c("A)", "B)")
dodge <- position_dodge(width = 0.5)
cgr.vlines <- geom_vline(xintercept = c(2014.4, 2014.5, 2014.6, 2017.4), linetype = c(1, 2, 3, 1))
ugr.vlines <- geom_vline(xintercept = c(2014.5, 2014.6), linetype = c(2, 3))
pref.theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.title = element_blank())

# Helpful custom functions
  ## Modification of RRPP's summary function to do multiple comparison adjustment as a matter of course
simp.rrpp <- function (object, test.type = c("dist", "VC", "var"), angle.type = c("rad", "deg"),
                       stat.table = T, confidence = 0.95, show.vectors = F, crit.dig = 3, ...) {
  
  test.type <- match.arg(test.type)
  angle.type <- match.arg(angle.type)
  x <- object
  if (test.type != "var") { # if you don't specify that the test.type is "var" (which means variances), see what the object should take
    if (is.null(x$LS.means)) 
      type = "slopes"  # this would be appropriate for linear regression analyses
    if (is.null(x$slopes)) 
      type = "means" # this would be appropriate for ANOVAs. For my data, that turned it into type = 'means'
  }
  else type <- "var" # ignore for my data
  RRPP:::print.pairwise(x) # Print onscreen the output from the fitted object
  cat("\n") # add a line in the output
  vars <- object$vars # needed. not sure why but setting something up with iterations I think
  if (type == "var") { # ignore for my data
    var.diff <- lapply(1:NCOL(vars), function(j) {
      v <- as.matrix(vars[, j])
      as.matrix(dist(v))
    })
    L <- d.summary.from.list(var.diff)
    cat("\nObserved variances by group\n\n")
    print(vars[, 1])
    if (stat.table) {
      tab <- makePWDTable(L)
      cat("\nPairwise distances between variances, plus statistics\n")
      print(tab)
    }
    else {
      cat("\nPairwise distances between variances\n")
      print(L$D)
      cat("\nPairwise", paste(L$confidence * 100, "%", 
                              sep = ""), "upper confidence limits between variances\n")
      print(L$CL)
      cat("\nPairwise effect sizes (Z) between variances\n")
      print(L$Z)
      cat("\nPairwise P-values between variances\n")
      print(L$P)
    }
  }
  if (type == "means") { # this is appropriate for my data
    cat("LS means:\n") 
    if (show.vectors) 
      print(x$LS.means[[1]])
    else cat("Vectors hidden (use show.vectors = TRUE to view)\n") # print out message for LS means output
    if (test.type == "dist") { # if type = dist (like my data)
      L <- RRPP:::d.summary.from.list(x$means.dist)  # THIS IS WHERE THE P VALUE LIST IS MADE - L$P
      if (stat.table) { # if you ask for it in a table, this is how it's made
        tab <- RRPP:::makePWDTable(L) # making the table
        cat("\nPairwise distances between means, plus statistics\n")
        print(tab) 
      }
      else { # ignore
        cat("\nPairwise distances between means\n")
        print(L$D)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits between means\n")
        print(L$CL)
        cat("\nPairwise effect sizes (Z) between means\n")
        print(L$Z)
        cat("\nPairwise P-values between means\n")
        print(L$P)
      }
    }
    if (test.type == "VC") {
      L <- r.summary.from.list(x$means.vec.cor)
      if (stat.table) {
        tab <- makePWCorTable(L)
        cat("\nPairwise statistics based on mean vector correlations\n")
        if (angle.type == "deg") {
          tab$angle <- tab$angle * 180/pi
          tab[, 3] <- tab[, 3] * 180/pi
        }
        print(tab)
      }
      else {
        cat("\nPairwise vector correlations between mean vectors\n")
        print(L$r)
        cat("\nPairwise angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$angle * 180/pi)
        else print(L$angle)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits for angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$aCL * 180/pi)
        else print(L$aCL)
        cat("\nPairwise effect sizes (Z) for angles between mean vectors\n")
        print(L$Z)
        cat("\nPairwise P-values for angles between mean vectors\n")
        print(L$P)
      }
    }
  }
  if (type == "slopes") {
    cat("Slopes (vectors of variate change per one unit of covariate change, by group):\n")
    if (show.vectors) 
      print(x$slopes[[1]])
    else cat("Vectors hidden (use show.vectors = TRUE to view)\n")
    if (test.type == "dist") {
      cat("\nSlope vector lengths\n")
      print(x$slopes.length[[1]])
      L <- d.summary.from.list(x$slopes.dist)
      if (stat.table) {
        tab <- makePWDTable(L)
        cat("\nPairwise absolute difference (d) between vector lengths, plus statistics\n")
        print(tab)
      }
      else {
        cat("\nPairwise absolute differences (d) between slope lengths\n")
        print(L$D)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits between slope lengths\n")
        print(L$CL)
        cat("\nPairwise effect sizes (Z) between slope lengths\n")
        print(L$Z)
        cat("\nPairwise P-values between slope lengths\n")
        print(L$P)
      }
    }
    if (test.type == "VC") {
      L <- r.summary.from.list(x$slopes.vec.cor)
      cat("\nPairwise statistics based on slopes vector correlations (r) and angles, acos(r)")
      cat("\nThe null hypothesis is that r = 1 (parallel vectors).")
      cat("\nThis null hypothesis is better treated as the angle between vectors = 0\n")
      if (stat.table) {
        tab <- makePWCorTable(L)
        if (angle.type == "deg") {
          tab$angle <- tab$angle * 180/pi
          tab[, 3] <- tab[, 3] * 180/pi
        }
        print(tab)
      }
      else {
        cat("\nPairwise vector correlations between slope vectors\n")
        print(L$r)
        cat("\nPairwise angles between slope vectors\n")
        if (angle.type == "deg") 
          print(L$angle * 180/pi)
        else print(L$angle)
        cat("\nPairwise", paste(L$confidence * 100, "%", 
                                sep = ""), "upper confidence limits for angles between mean vectors\n")
        if (angle.type == "deg") 
          print(L$aCL * 180/pi)
        else print(L$aCL)
        cat("\nPairwise effect sizes (Z) for angles between slope vectors\n")
        print(L$Z)
        cat("\nPairwise P-values for angles between slope vectors\n")
        print(L$P)
      }
    }
  }
  
  # Make new dataframe
  df <- tab
  
  # The following steps are necessary for performing Sequential Bonferroni multiple comparison adjustment
  ## Order the rows from lowest to highest p value
  results <- df[order(df$"Pr > d"), ]
  
  ## Assign a rank based on that order
  rank <- c(1:length(results$P))
  
  # Now modify the critical point based on that rank (hence "sequential" Bonferroni)
  results$Alpha <- round( with(results, ( (0.05 / (length(results$"Pr > d") + 1 - rank)) ) ), digits = crit.dig)
  
  # Helpful to know how much larger the p value is than its critical point
  results$"P/Alpha" <- round( (results$"Pr > d" / results$Alpha), digits = crit.dig)
  
  # Now get the ranges of "significance" to be reduced to qualitative bits
  results$Sig <- ifelse(test = results$"P/Alpha" > 2, yes = " ",
                        no = ifelse(test = results$"P/Alpha" > 1, yes = ".",
                                    no = ifelse(test = results$"P/Alpha" > 0.2, yes = "*",
                                                no = ifelse(test = results$"P/Alpha" > 0.02, yes = "**",
                                                            no = ifelse(test = results$"P/Alpha" > 0.002, yes = "***", no = "****")))))
  ## Viewer discretion is advized when using this bonus column
  
  # Just in case you don't want to look in the guts of this function to see what * vs. ** means:
  message("Sig codes: P / Crit > 2 = ''
          1 < P/C ≤ 2 = '.'
          0.2 < P/C ≤ 1 = '*'
          0.02 < P/C ≤ 0.2 = '**'
          0.002 < P/C ≤ 0.02 = '***'
          P/C ≤ 0.002 = '****'")
  
  # And spit out the result
  return(results)
  
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

##  ---------------------------------------------------------------------------------------------  ##
                         # Cool Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
procD.lm(CSG ~ Herbicide.Treatment * Year, data = cgr) # interxn = NS
procD.lm(CSG ~ Herbicide.Treatment + Year, data = cgr) # NS

procD.lm(CSG ~ Herbicide.Treatment * Year, data = ugr) # interxn = NS
procD.lm(CSG ~ Herbicide.Treatment + Year, data = ugr) # treat = sig, year = marginal
simp.procD(advanced.procD.lm(CSG ~ Herbicide.Treatment + Year, ~ 1, ~ Herbicide.Treatment, data = ugr))
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
plot_grid(cgr.csg.plt, ugr.csg.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/CSG.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                        # Warm Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
procD.lm(WSG ~ Herbicide.Treatment * Year, data = cgr) # interxn = NS
procD.lm(WSG ~ Herbicide.Treatment + Year, data = cgr) # NS

procD.lm(WSG ~ Herbicide.Treatment * Year, data = ugr) # interxn = marginal
procD.lm(WSG ~ Herbicide.Treatment + Year, data = ugr) # treat = sig
simp.procD(advanced.procD.lm(WSG ~ Herbicide.Treatment + Year, ~ 1, ~ Herbicide.Treatment, data = ugr))
  ## Con = A | Spr = A | SnS = B

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
  pref.theme + theme(legend.position = "none"); cgr.wsg.plt

ugr.wsg.plt <- ggplot(ugr.wsg.pltdf, aes(Herbicide.Treatment, WSG, color = Herbicide.Treatment)) +
  geom_errorbar(aes(ymin = WSG - se, ymax = WSG + se), width = 0.3, size = 1, position = dodge) +
  geom_point(position = 'identity', size = 2.5) +
  xlab("Un-Grazed") +
  scale_y_continuous("WSG % Cover", limits = wsg.lims) +
  scale_color_manual(values = ugr.colors) +
  scale_x_discrete(limits = sns.labs, labels = sns.labs) +
  pref.theme + theme(legend.position = "none"); ugr.wsg.plt

# Save it.
plot_grid(cgr.wsg.plt, ugr.wsg.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/WSG.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                                # Fescue ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
procD.lm(Fescue ~ Herbicide.Treatment * Year, data = cgr) # interxn = NS
procD.lm(Fescue ~ Herbicide.Treatment + Year, data = cgr) # year = sig
simp.procD(advanced.procD.lm(Fescue ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = AB | 16 = B | 17 = B

procD.lm(Fescue ~ Herbicide.Treatment * Year, data = ugr) # interxn = NS
procD.lm(Fescue ~ Herbicide.Treatment + Year, data = ugr) # year = marginal
simp.procD(advanced.procD.lm(Fescue ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = ugr))
  ## 14 = A | 15 = AB | 16 = AB | 17 = B

cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
cgr.fsc.pltdf$Year <- as.numeric(as.character(cgr.fsc.pltdf$Year))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf$Year <- as.numeric(as.character(ugr.fsc.pltdf$Year))
fsc.lims <- minmax(c(cgr.fsc.pltdf[,4], ugr.fsc.pltdf[,4]), slack = 15)

# Plot
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(Year, Fescue, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), position = dodge, width = 0.3) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = cgr.colors) +
  cgr.vlines + pref.theme + 
  theme(legend.position = c(0.65, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(Year, Fescue, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), position = dodge, width = 0.3) +
  scale_y_continuous("Fescue % Cover", limits = fsc.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = ugr.colors) +
  ugr.vlines + pref.theme + 
  theme(legend.position = c(0.65, 0.9)); ugr.fsc.plt

plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Fescue.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                        # Seedmix Threshhold ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
procD.lm(Seedmix ~ Herbicide.Treatment * Year, data = cgr) # interxn = NS
procD.lm(Seedmix ~ Herbicide.Treatment + Year, data = cgr) # year = sig
simp.procD(advanced.procD.lm(Seedmix ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 2014 = A | 2015 = AB | 2016 = BC | 2017 = C

procD.lm(Seedmix ~ Herbicide.Treatment * Year, data = ugr) # interxn = NS
procD.lm(Seedmix ~ Herbicide.Treatment + Year, data = ugr) # NS

cgr.smx.pltdf <- summarySE(data = cgr, measurevar = "Seedmix", groupvars = c("Year"))
cgr.smx.pltdf$Year <- as.numeric(as.character(cgr.smx.pltdf$Year))
ugr.smx.pltdf <- summarySE(data = ugr, measurevar = "Seedmix", groupvars = c("Year"))
ugr.smx.pltdf$Year <- as.numeric(as.character(ugr.smx.pltdf$Year))
smx.lims <- minmax(c(cgr.smx.pltdf[,3], ugr.smx.pltdf[,3]), slack = 0.15)

# Plots
cgr.smx.plt <- ggplot(cgr.smx.pltdf, aes(Year, Seedmix, color = rep.int("Z", nrow(cgr.smx.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(cgr.smx.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(2014.4, 2014.5, 2014.6, 2017.4), linetype = c(1, 2, 3, 1)) +
  scale_y_continuous("Proportion Seed-mix ≥25% Cover", limits = smx.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + theme(legend.position = "none"); cgr.smx.plt

ugr.smx.plt <- ggplot(ugr.smx.pltdf, aes(Year, Seedmix, color = rep.int("Z", nrow(cgr.smx.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(cgr.smx.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Seedmix + se, ymin = Seedmix - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(2014.5, 2014.6), linetype = c(2, 3)) +
  scale_y_continuous("Proportion Seed-mix ≥25% Cover", limits = smx.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + theme(legend.position = "none"); ugr.smx.plt

# Save it.
plot_grid(cgr.smx.plt, ugr.smx.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Seedmix.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                                # Forbs ####
##  ---------------------------------------------------------------------------------------------  ##
procD.lm(Forbs ~ Herbicide.Treatment * Year, data = cgr) # all sig

procD.lm(Forbs ~ Herbicide.Treatment * Year, data = ugr) # interxn = NS
procD.lm(Forbs ~ Herbicide.Treatment + Year, data = ugr) # NS

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
  geom_vline(xintercept = c(2014.4, 2014.5, 2014.6, 2017.4), linetype = c(1, 2, 3, 1)) +
  scale_y_continuous("Forb % Cover", limits = frb.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = cgr.colors) +
  pref.theme + theme(legend.position = c(0.65, 0.9)); cgr.frb.plt

ugr.frb.plt <- ggplot(ugr.frb.pltdf, aes(Year, Forbs, color = rep.int("Z", nrow(ugr.frb.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(ugr.frb.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Forbs + se, ymin = Forbs - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(2014.5, 2014.6), linetype = c(2, 3)) +
  scale_y_continuous("Forbs % Cover", limits = frb.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = ugr.ns.color) +
  pref.theme + theme(legend.position = "none"); ugr.frb.plt

# Save it.
plot_grid(cgr.frb.plt, ugr.frb.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Forbs.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                              # Legumes ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
procD.lm(Legumes ~ Herbicide.Treatment * Year, data = cgr) # interxn = NS
procD.lm(Legumes ~ Herbicide.Treatment + Year, data = cgr) # year = sig
simp.procD(advanced.procD.lm(Legumes ~ Herbicide.Treatment + Year, ~ 1, ~ Year, data = cgr))
  ## 14 = A | 15 = A | 16 = A | 17 = B

procD.lm(Legumes ~ Herbicide.Treatment * Year, data = ugr) # interxn = sig

# Get plottings dataframes
cgr.lgm.pltdf <- summarySE(data = cgr, measurevar = "Legumes", groupvars = c("Year"))
cgr.lgm.pltdf$Year <- as.numeric(as.character(cgr.lgm.pltdf$Year))
ugr.lgm.pltdf <- summarySE(data = ugr, measurevar = "Legumes", groupvars = c("Year", "Herbicide.Treatment"))
ugr.lgm.pltdf$Year <- as.numeric(as.character(ugr.lgm.pltdf$Year))
lgm.lims <- minmax(c(cgr.lgm.pltdf[,4], ugr.lgm.pltdf[,4]))

# Plot
cgr.lgm.plt <- ggplot(cgr.lgm.pltdf, aes(Year, Legumes, color = rep.int("Z", nrow(cgr.lgm.pltdf)))) +
  geom_line(aes(group = rep.int("Z", nrow(cgr.lgm.pltdf))), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(2014.4, 2014.5, 2014.6, 2017.4), linetype = c(1, 2, 3, 1)) +
  scale_y_continuous("Legumes % Cover", limits = lgm.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = cgr.ns.color) +
  pref.theme + theme(legend.position = "none"); cgr.lgm.plt

ugr.lgm.plt <- ggplot(ugr.lgm.pltdf, aes(Year, Legumes, color = Herbicide.Treatment)) +
  geom_line(aes(group = Herbicide.Treatment), size = 1, position = dodge) +
  geom_point(stat = 'identity', size = 2.5, position = dodge) +
  geom_errorbar(aes(ymax = Legumes + se, ymin = Legumes - se), position = dodge, width = 0.3) +
  geom_vline(xintercept = c(2014.5, 2014.6), linetype = c(2, 3)) +
  scale_y_continuous("Legume % Cover", limits = lgm.lims) +
  scale_x_continuous(breaks = seq(2014, 2018, 1)) +
  scale_color_manual(values = ugr.colors) +
  pref.theme + theme(legend.position = c(0.65, 0.9)); ugr.lgm.plt

plot_grid(cgr.lgm.plt, ugr.lgm.plt, labels = panel.labs, nrow = 1, ncol = 2)
ggplot2::ggsave("./Graphs/Legumes.pdf", plot = last_plot())

##  ---------------------------------------------------------------------------------------------  ##
                                # Woody ####
##  ---------------------------------------------------------------------------------------------  ##
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


