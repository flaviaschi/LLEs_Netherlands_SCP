#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# This script contains calculation for creating the cost layers in agricultural
# areas of the analysis for the region on Noord-Branbant
#===============================================================================


library(sf)
library(raster)
library(rgdal)
library(tibble)
library(bnspatial)

#===============================================================================
#Raster calculations for creating cost management layer 
#===============================================================================

#In Agricultural areas
Hed_den_agri<-raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")
plot(Hed_den_agri)

# Multiply constant facor 581.9 (for resultion 500x500) to find costs 
# See Methods section for additonal information
cost_man_agri<-Hed_den_agri*581.9
plot(cost_man_agri)

# save raster
writeRaster(cost_man_agri, "Data/CleanData/Costs/cost_management_agri.tif", format ="GTiff", overwrite=TRUE)

#===============================================================================
#Crop type cost in Noord Brabant (NB)
#===============================================================================

#Import hedgerows density raster 
Hed_den_agri<-raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")

# Assume that on average hesgerows are 2.5 m wide (that's why 0.0025 km)
adimentional_hedge_quantity <- Hed_den_agri * 0.0025 

# Calculate the surface of hedges in m2 per gridcell (500x500m)
hedge_surface_grid <- adimentional_hedge_quantity * (500 * 500)

#Import crop_cost_eurperha_NB.tif (€/ha) 
crop_cost_eurperha_NB <- raster("Data/RawData/crop_cost_eurperha_NB.tif")

#turn the raster into €/m2 - divide by 10.000.
crop_cost_eurperm2_NB <- crop_cost_eurperha_NB / 10000

# Multiply €/m2 with hedge surface per grid - and find the cost per grid 
# based on hedgerows density
library(terra)
crop_price_grid_hedge <- hedge_surface_grid * crop_cost_eurperm2_NB
plot(crop_price_grid_hedge)

#Save file
writeRaster(crop_price_grid_hedge, "Data/CleanData/Costs/Normal_costs/Cost_crop_HedgeDensity_agri.tif")

# Turn NAs into 0s to sum up the layers
crop_price_grid_hedge[is.na(crop_price_grid_hedge)] <- 0
cost_man_agri[is.na(cost_man_agri)] <- 0

# Sum costs
cost <- crop_price_grid_hedge + cost_man_agri

#Reset the NAs in the cost layer- woth function extract by mask
costs <- extractByMask(cost, msk = Hed_den_agri, spatial = TRUE)
plot(costs)
# Give approiate name to layer
names(costs) <- c("Costs")

# save raster
writeRaster(costs, "Data/CleanData/Costs/Normal_costs/Costs.tif")


#Ecosystem services as cost layers
#===============================================================================

# ------------------------------------------------------------------------------
# POLLINATION - straight
# ------------------------------------------------------------------------------

#Import data
hedge_agri <- raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")
pol_cbs<- raster("Data/RawData/CBSData/Pollination_euro_ha_2018.tif")
plot(pol_cbs)
# Extract values only in agri areas
poll <- extractByMask(pol_cbs, msk = hedge_agri, spatial = T)

# Adjust for grid size
poll_grid <- poll*25

# Save raster
writeRaster(poll_grid, "Data/CleanData/Costs/ESS/pollation_straight.tif")

# Import raster
polli <- raster("Data/CleanData/Costs/ESS/pollation_straight.tif")
plot(polli)
# ------------------------------------------------------------------------------
# CARBON SEQUESTRATION - straight
# ------------------------------------------------------------------------------

# Import data
hedge_agri <- raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")
cseq_cbs <- raster("Data/RawData/CBSData/Cseq_euro_ha_2018.tif")

# Extract values only in agri areas
cseq <- extractByMask(cseq_cbs, msk = hedge_agri, spatial = T)

# Adjust for grid size
cseq_GRID <- cseq*25
# Save raster
writeRaster(cseq_GRID, "Data/CleanData/Costs/ESS/cseqestration_straight.tif")

#Import raster
cseq <- raster("Data/CleanData/Costs/ESS/cseqestration_straight.tif")

# ------------------------------------------------------------------------------
# SUM OF ESS
# ------------------------------------------------------------------------------

# Turn NAs into 0s to sum up the layers
cseq[is.na(cseq)] <-0
polli[is.na(polli)] <-0

#Sum layers
ESS <- polli + cseq
plot(ESS)

#Import PU dataset - agri areas
hedge_density_agri <- raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")

#Reset the NAs in the cost layer- woth function extract by mask
ESSs <- extractByMask(ESS, msk = hedge_density_agri, spatial = TRUE)
plot(ESSs, main= "ESS")

# Give approiate name to layer
names(ESSs) <- c("ESS")

#Save cost layer of Carbon Sequesation and Pollination summed
writeRaster(ESSs, "Data/CleanData/Costs/ESS/ESS.tif")

# 2) ESS flipped - caluclated in GIS with raster calculator funtion from "ESS.tif" file


