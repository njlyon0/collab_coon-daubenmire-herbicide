##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Vegetation Response to Anti-Tall Fescue Herbicide Treatments
##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Code written by Nicholas J Lyon

# Objective 2
  ## Grass Dominated Quadrat Fequency Response to Treatment
  ## 2015, 16, 17, and 18 (i.e., post-treatment response)

# START ####

# Required libraries
library(RRPP) # Analysis

# Set working directory (Also, "Session" menu to "Set Working Directory" works)
setwd("~/Documents/School/Iowa State/Collaborations/'Daubenmire Herbicide Bit/Daubenmire.HerbicideComponent.WD")

# Clear environment of other stuff
rm(list = ls())

##  ---------------------------------------------------------------------------------------------  ##
                          # Housekeeping ####
##  ---------------------------------------------------------------------------------------------  ##
# Pull in the dataset
sns <- read.csv("./Data/sns-data_post-trt.csv")

# Re-level the factors too though
unique(sns$Herbicide.Treatment)
sns$Herbicide.Treatment <- factor(as.character(sns$Herbicide.Treatment), levels = c("Con", "Spr", "SnS"))
unique(sns$Herbicide.Treatment)

# Further separate into cattle-grazed restorations (CGRs) and un-grazed restorations (UGRs)
cgr <- subset(sns, sns$Treatment == "GB")
ugr <- subset(sns, sns$Treatment == "None")

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
  ## treat = sig

# Fit models
csg.trt.cgr.fit <- lm.rrpp(Hvy.CSG ~ Herbicide.Treatment, data = cgr, iter = 9999)
csg.trt.ugr.fit <- lm.rrpp(Hvy.CSG ~ Herbicide.Treatment, data = ugr, iter = 9999)

# Pairwise for CGR
simp.rrpp(pairwise(csg.trt.cgr.fit, fit.null = NULL, groups = cgr$Herbicide.Treatment))

# Get pairwise comps (UGR)
simp.rrpp(pairwise(csg.trt.ugr.fit, fit.null = NULL, groups = ugr$Herbicide.Treatment))
  ## Con = A | Spr = AB | SnS = B

##  ---------------------------------------------------------------------------------------------  ##
                        # Warm Season Grasses ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## year = marginal

anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.WSG ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## treat = sig

# Pairwise comparisons
wsg.trt.cgr.fit <- lm.rrpp(Hvy.WSG ~ Herbicide.Treatment, data = cgr, iter = 9999)
simp.rrpp(pairwise(wsg.trt.cgr.fit, fit.null = NULL, groups = cgr$Herbicide.Treatment))

wsg.trt.ugr.fit <- lm.rrpp(Hvy.WSG ~ Herbicide.Treatment, data = ugr, iter = 9999)
simp.rrpp(pairwise(wsg.trt.ugr.fit, fit.null = NULL, groups = ugr$Herbicide.Treatment))

##  ---------------------------------------------------------------------------------------------  ##
                              # Heavy Fescue ####
##  ---------------------------------------------------------------------------------------------  ##
# Analysis
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = cgr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = cgr, iter = 9999), effect.type = "F")
  ## both sig

fsc.trt.cgr.fit <- lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment, data = cgr, iter = 9999)
fsc.trt.cgr.pairs <- simp.rrpp(pairwise(fsc.trt.cgr.fit, fit.null = NULL, groups = cgr$Herbicide.Treatment))
fsc.trt.cgr.pairs

# Un-grazed
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment * Year, data = ugr, iter = 9999), effect.type = "F")
  ## interxn = NS
anova(lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment + Year, data = ugr, iter = 9999), effect.type = "F")
  ## treat = sig

fsc.trt.ugr.fit <- lm.rrpp(Hvy.Fesc ~ Herbicide.Treatment, data = ugr, iter = 9999)
fsc.trt.ugr.pairs <- simp.rrpp(pairwise(fsc.trt.ugr.fit, fit.null = NULL, groups = ugr$Herbicide.Treatment))
fsc.trt.ugr.pairs
  ## NS

# END ####
