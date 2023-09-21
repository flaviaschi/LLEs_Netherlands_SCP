#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# The following script contains codes for running Sensitivity analysis for 
# Sceanrio 1 (Maximize ES Scenario) and Scenario 2 (Minimize Costs Scenario)
#===============================================================================

# Import libraries
library(tidyselect)
library(raster)
library(stringr)
library(remotes)
library(edmaps)
library(rgdal)
library(slam)
library(gurobi) 
library(remotes)
library(prioritizr)
library(scales)
library(dplyr)
library(bnspatial)
library(rasterize)
library(tictoc)
library(foreach)
library(doParallel)


# ------------------------------------------------------------------------------
# Load in data
# ------------------------------------------------------------------------------

# Management cost
management_cost <- raster("Data/cost_management_agri.tif")
names(management_cost) <- c("Costs")

#Crop type cost in Noord Brabant (NB)
opportunity_cost <- raster("Data/Cost_crop_HedgeDensity_agri.tif")
names(opportunity_cost) <- c("Costs")

# NORMAL COSTS
expenses <- raster("Data/Costs.tif")
names(expenses) <- c("Costs")

# ESS flipped
ESS_costs <- raster("Data/ESS_flipped.tif")
names(ESS_costs) <- c("Costs")

# ESS 
ESS_saved<- raster("Data/ESS.tif")

# Pollination flipped
polli_flipped <- raster("Data/Pollination_flipped.tif")
names(polli_flipped) <- c("Costs")

# Carbon sequestration flipped
cseq_flipped <- raster("Data/CSequestration_flipped.tif")
names(cseq_flipped) <- c("Costs")

# Import species names
sp_35quantile_names <- readRDS("Data/Names_sp_q35")

# Import sp areas
species_areas <- readRDS("Data/species_areas")

# Import names
sp_10quantile_names <- readRDS("Data/Names_sp_q10")

# Import areas
species_areas1 <- readRDS("Data/species_areas1")

# Import species presence in PA 
cover <- readRDS("Data/cover")

# Load in conservation features (sp probability and sp density P/A binary maps)
con_features<-list.files(path= "Data/All_conservation_features/", 
                         pattern='*.tif$', full.names = TRUE)
all_confeatures <- lapply(con_features, raster)
conservation_features<- stack(all_confeatures)


# ------------------------------------------------------------------------------
# Scenario 1
# ------------------------------------------------------------------------------

##### Range of values for LB
  l <- log10(6000)/log10(2)
  b <- log10(100)/log10(2)
  a <- seq(from= b, to= l,  by= 0.45)   # 0.3 - alternative with more numbers
  x <- 2^a
plot(x, main = "Lower Bound", ylab= "Species Area (km2)")
  
  ##### Range of values for HB 
  d <- log10(7000)/log10(3)
  f <- log10(20000)/log10(3)
  s <- seq(from=d , to= f,  by= 0.073)    #0.1 - alternative with more numbers
  z <- 3^s
plot(z, main = "Upper Bound", ylab= "  Species Area (km2)") 

scen1 <- expand.grid(LB = x, HB = z, cost = c("a", "b", "c"))


source("R/1.SpeciesTargetsScenario1.R")
source("R/3.Scenario1.R")
registerDoParallel(5)

foreach(i=1:588) %dopar% {
 # for (i in 434) {
  tic(paste0("Iteration ", i))
  lb <- scen1[i, 1]
  hb <- scen1[i, 2]
  cost <- scen1[i, 3]
  targets <- speciestargets(lb, hb)
  print(targets)
  if (scen1[i, 3] == "a") {
    c <- ESS_costs
  } else if (scen1[i, 3] == "b") {
    c <- polli_flipped
  } else if (scen1[i, 3] == "c") {
    c <- cseq_flipped
  }

  out <- scenario1(c, targets, lb, hb, cost)
  print(paste0("Finished scenario ", i, ": with parameters", lb, hb, cost))
  toc()
}

# For runnimg the additional target sceanrios 10% and 100%
scen1.1 <- expand.grid(target = c(0.1, 1), cost = c("a", "b", "c"))
for (i in 1:nrow(scen1.1)) {
  tic(paste0("Iteration ", i))
  cost <- scen1.1[i, 2]
  # targets <- speciestargets(lb, hb)
  targets <- c(rep(scen1.1[i, 1], 23))
  print(targets)
  if (scen1.1[i, 2] == "a") {
    c <- ESS_costs
  } else if (scen1.1[i, 2] == "b") {
    c <- polli_flipped
  } else if (scen1.1[i, 2] == "c") {
    c <- cseq_flipped
  }

  out <- scenario1(c, targets, "_target", paste0("_", scen1.1[i, 1]), cost)
  print(paste0("Finished scenario ", i, ": with parameters", lb, hb, cost))
  toc()
}


# ------------------------------------------------------------------------------
# Scenario 2
# ------------------------------------------------------------------------------


##### Range of values for LB
l <- log10(6000)/log10(2)
b <- log10(100)/log10(2)
a <- seq(from= b, to= l,  by= 0.45)   # 0.3 - alternative with more numbers
x <- 2^a
plot(x, main = "Minimize Cost Scenario - Lower Bound")

##### Range of values for HB 
d <- log10(7000)/log10(3)
f <- log10(20000)/log10(3)
s <- seq(from=d , to= f,  by= 0.073)    #0.1 - alternative with more numbers
z <- 3^s
plot(z, main = "Minimize Cost Scenario - Upper Bound")

scen2 <- expand.grid(LB = x, HB = z, cost = c("a", "b", "c"))


source("R/2.SpeciesTargtesScenario2.R")
source("R/4.Scenario2.R")

registerDoParallel(5)

foreach(i=1:588) %dopar% {
# for (i in 1:3) {  # was 588
  tic(paste0("Iteration ", i))
  lb <- scen2[i, 1]
  hb <- scen2[i, 2]
  cost <- scen2[i, 3]
  targets <- speciestargets(lb, hb)
  print(targets)
  if (scen2[i, 3] == "a") {
    c <- expenses
  } else if (scen2[i, 3] == "b") {
    c <- management_cost
  } else if (scen2[i, 3] == "c") {
    c <- opportunity_cost
  }
  out <- scenario2(c, targets, lb, hb, cost)
  print(paste0("Finished scenario", i, ": with parameters", lb, hb, cost))
  toc()
}

# For runnimg the additional target sceanrios 10% and 100%
scen2.1 <- expand.grid(target = c(0.1, 1), cost = c("a", "b", "c"))
for (i in 1:nrow(scen2.1)) {
  tic(paste0("Iteration ", i))
  cost <- scen2.1[i, 2]
  targets <- c(rep(scen2.1[i, 1], 23))
  print(targets)
  if (scen2.1[i, 2] == "a") {
    c <- ESS_costs
  } else if (scen2.1[i, 2] == "b") {
    c <- polli_flipped
  } else if (scen2.1[i, 2] == "c") {
    c <- cseq_flipped
  }
  
  out <- scenario2(c, targets, "_target", paste0("_", scen2.1[i, 1]), cost)
  print(paste0("Finished scenario ", i, ": with parameters", lb, hb, cost))
  toc()
}


