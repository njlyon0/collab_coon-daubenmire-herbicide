##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# Objective 2
  ## Grass Dominated Quadrat Fequency Response to Treatment

# START ####

# Required libraries
library(RRPP) # Analysis
library(ggplot2); library(cowplot) # Plotting
library(Rmisc) # get summary values for plotting

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/1. Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                          # Housekeeping ####
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
yr.labs <- c("14", "15", "16", "17", "18")
sns.labs <- c("Con", "Spr", "SnS")
yr.colors <- c("14" = "#969696", "15" = "#f768a1", "16" = "#dd3497",
               "17" = "#ae017e", "18" = "#7a0177")
cgr.colors <- c("Con" = "#d73027", "Spr" = "#f46d43", "SnS" = "#fdae61") # shades of red
ugr.colors <- c("Con" = "#4575b4", "Spr" = "#74add1", "SnS" = "#abd9e9") # shades of blue
cgr.ns.color <- "#fdae61"
ugr.ns.color <- "#abd9e9"
dodge <- position_dodge(width = 0.5)
cgr.vlines <- geom_vline(xintercept = c(14.4, 14.5, 14.6, 17.4), linetype = c(1, 2, 3, 1))
ugr.vlines <- geom_vline(xintercept = c(14.5, 14.6), linetype = c(2, 3))
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

# General analytical procedure
  ## 1) Fit model with interaction term and assess *ONLY* the interaction term
  ## 2) If insignificant, run a new model without it (if significant, stop there, you're done)
  ## 3) If either explanatory variable is significant, fit a separate model of just that one
  ## 4) Run pairwise comparisons on that single-variable model

##  ---------------------------------------------------------------------------------------------  ##
                          # Heavy CSG Quadrats ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## NS

anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.CSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## treat = sig, year = sig

# Fit models
csg.trt.fit <- lm.rrpp(Hvy.CSG ~ Herbicide.Treatment, data = ugr, iter = 9999)
csg.yr.fit <- lm.rrpp(Hvy.CSG ~ Year, data = ugr, iter = 9999)

# Get pairwise comps
csg.trt.pairs <- simp.rrpp(pairwise(csg.trt.fit, fit.null = NULL, groups = ugr$Herbicide.Treatment))
csg.yr.pairs <- simp.rrpp(pairwise(csg.yr.fit, fit.null = NULL, groups = ugr$Year))

# Report
csg.trt.pairs
## Con = A | Spr = AB | SnS = B
csg.yr.pairs
## NS after correction

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

##  ---------------------------------------------------------------------------------------------  ##
                        # Warm Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
## NS

anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
## interxn = sig

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

##  ---------------------------------------------------------------------------------------------  ##
                              # Heavy Fescue ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## year = sig & trt = sig
hvy.fsc.yr.cgr.fit <- lm.rrpp(Hvy.Fesc ~ Year, data = cgr, iter = 9999)
hvy.fsc.yr.cgr.pairs <- simp.rrpp(pairwise(hvy.fsc.yr.cgr.fit, fit.null = NULL, groups = cgr$Year))
hvy.fsc.yr.cgr.pairs

# Un-grazed
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## year = marginal
hvy.fsc.yr.ugr.fit <- lm.rrpp(Hvy.Fesc ~ Year, data = ugr, iter = 9999)
hvy.fsc.yr.ugr.pairs <- simp.rrpp(pairwise(hvy.fsc.yr.ugr.fit, fit.null = NULL, groups = ugr$Year))
hvy.fsc.yr.ugr.pairs
  ## NS

# Plotting dataframes
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


# END ####
