#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# The script contains codes for creating the maps of the species in Noord Barabant
# based on the species quantile maps shared by Henk Siedsema
# (I use the dataset of hedge density not filtered for agricultural areas 
# because I later on need to check how much of the species range falls within 
# existing NNN areas - gap analysis)
#===============================================================================

# Load packages 
library(rgdal)
library(raster)
library(remotes)
library(scales)
library(stringr)


# Load in hedgerows density 
hedge_density <- raster("Data/CleanData/Hedgerows/Hedge_density.tif")
plot(hedge_density)

# Species 35quantile
rastlist_sp<-list.files(path = "Data/RawData/Sp_quantile/q35", pattern='*.asc$', full.names = TRUE)
allrasterquantile<-lapply(rastlist_sp, raster)
species_35quantile<- stack(allrasterquantile)
sp_35quantile_names <- names(species_35quantile)

# extract by mask
library(bnspatial)

sp_35quant <- c()

for (sp in species_35quantile@layers) {
  sp1 <- extractByMask(sp,msk = hedge_density, spatial = TRUE)
  # print(sp1)
  sp_35quant <- stack(sp1, sp_35quant)
}

names(sp_35quant) <- sp_35quantile_names
plot(sp_35quant)


#Saving files
setwd("Data/RawData/Sp_quantile/q35_NB/")
writeRaster(sp_35quant, filename = names(sp_35quant), bylayer= TRUE, format="GTiff", overwrite= TRUE)

#Re-importing raster stack
sp_35quant<-list.files(path = "Data/RawData/Sp_quantile/q35_NB", pattern='*.tif$', full.names = TRUE)
all_sp_35quant<-lapply(sp_35quant, raster)
sp_35quant<- stack(all_sp_35quant)
plot(sp_35quant)

# -------------------------------------

# Species 10quantile
rastlist_sp_10<-list.files(path = "Data/RawData/Sp_quantile/q10", pattern='*.asc$', full.names = TRUE)
allrasterquantile10<-lapply(rastlist_sp_10, raster)
species_10quantile<- stack(allrasterquantile10)
sp_10quantile_names <- names(species_10quantile)

# extract by mask to set extent, resolutoon, crs 

sp_10quant <- c()

for (sp in species_10quantile@layers) {
  sp1 <- extractByMask(sp,msk = hedge_density, spatial = TRUE)
  print(sp1)
  sp_10quant <- stack(sp1, sp_10quant)
}

names(sp_10quant) <- sp_10quantile_names
plot(sp_10quant)

#Saving files
setwd("Data/RawData/Sp_quantile/q10_NB/")
writeRaster(sp_10quant, filename = names(sp_10quant), bylayer= TRUE, format="GTiff", overwrite= TRUE)

#Re-importing raster stack
sp_10quant<-list.files(path = "Data/RawData/Sp_quantile/q10_NB", pattern='*.tif$', full.names = TRUE)
all_sp_10quant<-lapply(sp_10quant, raster)
sp_10quant<- stack(all_sp_10quant)
plot(sp_10quant)

# Put together species stacks 
con_features <- stack(sp_10quant, sp_35quant)
plot(con_features)
names(con_features)

#Saving rasters
setwd("Data/CleanData/Sp_quantile/All_conservation_features")
writeRaster(con_features, filename = names(con_features), bylayer= TRUE, format="GTiff", overwrite=T)

