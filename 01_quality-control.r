##  ------------------------------------------------------------  ##
# Daubenmire Project - Wrangle Data
##  ------------------------------------------------------------  ##
# Purpose:
## Wrangle data (e.g., perform quality contorl, calculate needed metrics)

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, supportR)

# Get set up
source("-setup.r")

# Clear environment
rm(list = ls()); gc()

# Load data
qc_v01 <- read.csv(file = file.path("data", "raw", "daubenmire-project_raw-data.csv"))

# Check structure
dplyr::glimpse(qc_v01)

##  ------------------------------------------  ##
# Fix Column Class Issues ----
##  ------------------------------------------  ##

# Reshape data to get all ostensibly numeric columns into one column
qc_v02 <- qc_v01 %>% 
  dplyr::mutate(row_id = 1:nrow(.), .before = dplyr::everything()) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(),
    .fns = as.character)) %>% 
  tidyr::pivot_longer(cols = c(Year, Robel.N:Angle_of_O),
    names_to = "vars", values_to = "values")

# Check for non-numeric characters
supportR::num_check(data = qc_v02, col = "values")

# Replace problem values and re-reshape back to original format
qc_v03 <- qc_v02 %>% 
  dplyr::mutate(values = ifelse(test = values %in% c("", " ", "."),
    yes = NA, no = values)) %>% 
  dplyr::mutate(values = as.numeric(values)) %>% 
  tidyr::pivot_wider(names_from = vars, values_from = values) %>% 
  dplyr::relocate(Year, .after = Patch)

# Check structure
dplyr::glimpse(qc_v03)

##  ------------------------------------------  ##
# Re-Calculate Robel Information ----
##  ------------------------------------------  ##

# Re-calculate average/std. dev of Robel
robel_v01 <- qc_v03 %>% 
  dplyr::select(-Avg_Robel, -SD_Robel) %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("Robel.")) %>% 
  dplyr::group_by(Pasture_Patch_Year_Transect) %>% 
  dplyr::summarize(mean.robel_dm = mean(value, na.rm = TRUE),
    std.dev.robel_dm = sd(value, na.rm = TRUE),
    .groups = "drop") 

# Check structure
dplyr::glimpse(robel_v01)

# Update Robel data in original dataset
qc_v04 <- qc_v03 %>%
  dplyr::select(-Avg_Robel, -SD_Robel) %>% 
  dplyr::rename(robel.north_dm = Robel.N,
    robel.east_dm = Robel.E,
    robel.south_dm = Robel.S,
    robel.west_dm = Robel.W) %>% 
  dplyr::left_join(y = robel_v01, by = "Pasture_Patch_Year_Transect") %>% 
  dplyr::relocate(dplyr::contains("robel"), .before = WSG)

# Re-check structure
dplyr::glimpse(qc_v04)

##  ------------------------------------------  ##
# Repair Daubenmire Categories ----
##  ------------------------------------------  ##

# Remaining vegetation categories were quantified as pseudo-categorical percents
## Only allowed values are: 0, 1, 3, 16, 38, 63, 86, 98
veg_v01 <- qc_v04 %>% 
  dplyr::select(row_id, Pasture_Patch_Year_Transect, WSG:Litter, Seed_mix) %>% 
  tidyr::pivot_longer(cols = -row_id:-Pasture_Patch_Year_Transect)

# Check values
sort(unique(veg_v01$value))

# Repair back into allowed bins & make better column names
veg_v02 <- veg_v01 %>% 
  dplyr::mutate(value = dplyr::case_when(
    value == 2 ~ 3,
    value %in% c(6, 15, 165) ~ 16,
    value %in% c(28, 36, 58) ~ 38,
    value == 80 ~ 86,
    value == 96 ~ 98,
    TRUE ~ value)) %>% 
  dplyr::mutate(name = dplyr::case_when(
    name == "Bare" ~ "bare.ground_binned.perc",
    name == "CSG" ~ "cool.season.grass_binned.perc",
    name %in% c("Fescue", "Forbs", "Legumes", 
        "Sedges") ~ paste0(tolower(name), "_binned.perc"),
    name == "Seed_mix" ~ "seedmix.forbs_binned.perc.of.all.forbs",
    name == "Litter" ~ "plant.litter_binned.perc",
    name == "Violets" ~ "prairie.violets_binned.perc",
    name == "Woody" ~ "woody.plants_binned.perc",
    name == "WSG" ~ "warm.season.grass_binned.perc",
    TRUE ~ name)) %>% 
  tidyr::pivot_wider(names_from = name, values_from = value)

# Check structure
dplyr::glimpse(veg_v02)

# Re-attach to broader daubenmire data
qc_v05 <- qc_v04 %>% 
  dplyr::select(-WSG:-Litter, -Seed_mix) %>% 
  dplyr::left_join(y = veg_v02, by = c("row_id", "Pasture_Patch_Year_Transect"))

# Re-check structure
dplyr::glimpse(qc_v05)

##  ------------------------------------------  ##
# Fix Seedmix Ambiguity ----
##  ------------------------------------------  ##

# We only measured prairie violets through 2016
# From 2017-on, the 'violets' column was used to measure "seedmix % of total"
qc_v06 <- qc_v05 %>% 
  dplyr::mutate(seedmix.forbs_binned.perc = ifelse(test = Year >= 2017,
    yes = prairie.violets_binned.perc, no = NA),
    .before = seedmix.forbs_binned.perc.of.all.forbs) %>% 
  dplyr::mutate(prairie.violets_binned.perc = ifelse(Year >= 2017,
    yes = NA, no = prairie.violets_binned.perc))

# Check structure
dplyr::glimpse(qc_v06)

##  ------------------------------------------  ##
# Calculate 'Seedmix % of Total' ----
##  ------------------------------------------  ##

# From 2014-2017, we only measured seedmix as a percent of forbs
## but we can back-calculate that now with simple algebra
qc_v07 <- qc_v06 %>% 
  dplyr::mutate(seedmix.forbs_binned.perc = ifelse(Year < 2017 & is.na(seedmix.forbs_binned.perc),
    yes = forbs_binned.perc / seedmix.forbs_binned.perc.of.all.forbs,
    no = seedmix.forbs_binned.perc)) %>% 
  dplyr::mutate(seedmix.forbs_binned.perc = dplyr::case_when(
    seedmix.forbs_binned.perc == Inf ~ 0, # Division by zero artifact
    seedmix.forbs_binned.perc > 0 & seedmix.forbs_binned.perc <= 1 ~ 1,
    seedmix.forbs_binned.perc > 1 & seedmix.forbs_binned.perc <= 5 ~ 3,
    seedmix.forbs_binned.perc > 5 & seedmix.forbs_binned.perc <= 25 ~ 16,
    seedmix.forbs_binned.perc > 25 & seedmix.forbs_binned.perc <= 50 ~ 38,
    seedmix.forbs_binned.perc > 50 & seedmix.forbs_binned.perc <= 75 ~ 63,
    seedmix.forbs_binned.perc > 75 & seedmix.forbs_binned.perc <= 95 ~ 86,
    seedmix.forbs_binned.perc > 95 & seedmix.forbs_binned.perc <= 100 ~ 98,
    TRUE ~ NA)) %>% 
  dplyr::mutate(seedmix.forbs_prop.above.25.perc.cover = ifelse(
    seedmix.forbs_binned.perc >= 16, yes = 1, no = 0),
    .after = seedmix.forbs_binned.perc)

# Check structure
dplyr::glimpse(qc_v07)

##  ------------------------------------------  ## 
# Identify 'Heavy' Grass Quadrats ----
##  ------------------------------------------  ## 
# Identify quadrats above some threshold percentage of grass cover
qc_v08 <- qc_v07 %>% 
  dplyr::mutate(
    cool.season.grass_prop.above.75.perc.cover = ifelse(cool.season.grass_binned.perc > 75, yes = 1, no = 0),
    warm.season.grass_prop.above.75.perc.cover = ifelse(warm.season.grass_binned.perc > 75, yes = 1, no = 0),
    fescue_prop.above.75.perc.cover = ifelse(fescue_binned.perc > 75, yes = 1, no = 0),
    .after = fescue_binned.perc)

# Check structure
dplyr::glimpse(qc_v08)

##  ------------------------------------------  ## 
# Summarize within Patch ----
##  ------------------------------------------  ## 
# Summarize within patch (i.e., across quadrats from 2 transects / patch)
qc_v09 <- qc_v08 %>% 
  dplyr::select(-row_id, -Patch, -Pasture_Patch_Year,
    -Pasture_Patch_Year_Transect, -Angle_of_O) %>% 
  dplyr::rename(year = Year,
    pasture = Pasture,
    patch = Pasture_Patch,
    litter.depth_cm = Litter_dep) %>% 
  tidyr::pivot_longer(cols = robel.north_dm:seedmix.forbs_binned.perc.of.all.forbs) %>% 
  dplyr::group_by(year, pasture, patch, name) %>% 
  dplyr::summarize(value = mean(value, na.rm = TRUE),
    .groups = "drop") %>% 
  tidyr::pivot_wider()

# Re-check structure
dplyr::glimpse(qc_v09)

##  ------------------------------------------  ## 
# Clarify Panic Grass Data ----
##  ------------------------------------------  ## 
qc_v10 <- qc_v09 %>% 
  dplyr::mutate(panic.grass_pres.abs = dplyr::case_when(
    Panic > 0 ~ 1,
    is.na(Panic) ~ NA, 
    TRUE ~ Panic)) %>% 
  dplyr::select(-Panic)

# Check structure
dplyr::glimpse(qc_v10)

##  ------------------------------------------  ##
# Reorder Columns ----
##  ------------------------------------------  ## 
# Reorder remaining columns
qc_v11 <- qc_v10 %>% 
  dplyr::relocate(dplyr::starts_with("robel."), .after = patch) %>% 
  dplyr::relocate(dplyr::contains("\\.robel"), .after = robel.west_dm) %>% 
  dplyr::relocate(panic.grass_pres.abs, .after = std.dev.robel_dm) %>% 
  dplyr::relocate(dplyr::starts_with("bare.ground"), dplyr::starts_with("plant.litter"),
    dplyr::contains("season.grass"), dplyr::starts_with("fescue"),
    dplyr::starts_with("sedges"), dplyr::starts_with("woody.plants"),
    dplyr::starts_with("legumes"),
    dplyr::starts_with("forbs"), dplyr::starts_with("seedmix"),
    dplyr::starts_with("prairie.violets"),
    .after = panic.grass_pres.abs) %>% 
  dplyr::relocate(dplyr::contains("prop.above"), .after = dplyr::everything()) %>% 
  dplyr::relocate(litter.depth_cm, .after = std.dev.robel_dm)
  
# Make sure no columns are lost accidentally
supportR::diff_check(old = names(qc_v10), new = names(qc_v11))

# Re-check structure
dplyr::glimpse(qc_v11)

##  ------------------------------------------  ## 
# Attach Indices ----
##  ------------------------------------------  ## 

# Read in all indices
history_v01 <- read.csv(file = file.path("indices", "site-history.csv"))
burn_v01 <- read.csv(file = file.path("indices", "burn-cohort.csv"))
sns_v01 <- read.csv(file = file.path("indices", "spray-and-seed-treatments.csv"))

# Check structure
dplyr::glimpse(history_v01)
dplyr::glimpse(burn_v01)
dplyr::glimpse(sns_v01)

# Attach them to the data and do subsequent column tidying
qc_v12 <- qc_v11 %>% 
  dplyr::left_join(x = ., y = history_v01,
    by = c("year" = "Year", "pasture" = "Pasture", "patch" = "Pasture_patch")) %>% 
  dplyr::left_join(x = ., y = burn_v01,
    by = c("year" = "Year", "patch" = "Pasture_patch", "Pasture_patch_year")) %>% 
  dplyr::left_join(x = ., y = sns_v01,
    by = c("patch" = "Patch")) %>% 
  dplyr::select(-Patch, -Pasture_patch_year) %>% 
  dplyr::rename(treatment_fire = FireTreat,
    treatment_herbicide = HerbTreat,
    treatment_grazing = GrazingTreat,
    grazing_binary = Grazing,
    time.since.fire_years = TSF,
    time.since.herbicide_years = TSH,
    burn_cohort = Burn_Cohort,
    treatment_fescue = Fescue.Treatment) %>%
  dplyr::relocate(treatment_fescue, .before = grazing_binary) %>% 
  dplyr::relocate(treatment_fire:burn_cohort, .after = patch)

# Check structure
dplyr::glimpse(qc_v12)

##  ------------------------------------------  ## 
# Export ----
##  ------------------------------------------  ## 
# Make one final object
qc_v99 <- qc_v12

# Check structure
dplyr::glimpse(qc_v99)

# Export
write.csv(x = qc_v99, row.names = FALSE, na = '',
    file = file.path("data", "01_daub-tidy.csv"))

# Also export pre-summarization raw data
write.csv(x = qc_v08, row.names = FALSE, na = '',
  file = file.path("data", "raw", "01_daub-partial-tidy.csv"))

# End ----
