#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Species conservation targets .
# The following script contains codes for: 
# 1. Calculate species areas for the whole of NL
# 2. Apply log_linear_interpolation() to find the target based on the 
# total species' area 
#===============================================================================

library(tidyselect)
library(raster)
library(stringr)
library(prioritizr)
library(remotes)
library(edmaps)

#=========================================================================================================================================
# Species 35quantile
#=========================================================================================================================================

# 1) SPECIES' RANGE AREA

# Import species quantile maps
rastlist_sp<-list.files(path = "Data/RawData/Sp_quantile/q35", pattern='*.asc$', full.names = TRUE)
allrasterquantile<-lapply(rastlist_sp, raster)
species_35quantile<- stack(allrasterquantile)
plot(species_35quantile)
sp_35quantile_names <- names(species_35quantile)
saveRDS(sp_35quantile_names, file="DataNew/Excel/Names_sp_q35")

# Calculate range area
species_area<-c() 

for (sp in species_35quantile@layers) {
  sp[sp == 0] <- NA
  ncells<-cellStats(sp, stat = sum, na.rm = TRUE )
  area<-ncells*0.0625
  species_area<-rbind(species_area, area)
}

class(species_area)

# 2) FIND SPECIES SPECIFIC TARGETS GIVEN THE SPECIES RANGE

# Convert the matrix into a class numeric
species_areas<- as.numeric(species_area) 
species_area_df <- as.data.frame(species_area)
#Save
saveRDS(species_areas, file="DataNew/Excel/species_areas")

# Calcualate species' specific targets 
spp_target_percentage <-
  loglinear_interpolation(
    x = species_areas,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 10000,
    coordinate_two_y = 0.1)

print(spp_target_percentage)

# Create data frame
species_targets <- as.data.frame(spp_target_percentage)
colnames(species_targets)<-"percentage_target"

# Create a list with names 
sp_list<-as.data.frame(sp_35quantile_names)
colnames(sp_list) <- "Species"

# Bind names wirth targets
species_targets <- cbind(sp_list, species_targets)
species_area_df <- cbind(sp_list, species_area_df)

# Save csv
write.csv2(species_targets, "DataNew/Excel/Species_35quantile_targets.csv")

#=========================================================================================================================================
# Species 10quantile
#=========================================================================================================================================

# 1) SPECIES' RANGE AREA

# Import species quantile maps
rastlist_sp_10<-list.files(path = "Data/RawData/Sp_quantile/q10", pattern='*.asc$', full.names = TRUE)
allrasterquantile10<-lapply(rastlist_sp_10, raster)
species_10quantile<- stack(allrasterquantile10)
sp_10quantile_names <- names(species_10quantile)
saveRDS(sp_10quantile_names, file="DataNew/Excel/Names_sp_q10")

# Calculate range area
species_area1<-c() 

for (sp1 in species_10quantile@layers) {
  sp1[sp1 == 0] <- NA
  ncells1<-cellStats(sp1, stat = sum, na.rm = TRUE )
  area1<-ncells1*0.0625
  species_area1<-rbind(species_area1, area1)
}

class(species_area1)

# 2) FIND SPECIES SPECIFIC TARGETS GIVE THE SPECIES RANGE

# convert the matrix into a class numeric
species_areas1<- as.numeric(species_area1) 
species_area1_df <- data.frame(species_area1)
# Save
saveRDS(species_areas1, file="DataNew/Excel/species_areas1")

# Find target
spp_target_percentage1 <-
  loglinear_interpolation(
    x = species_area1,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 10000,
    coordinate_two_y = 0.1) 

print(spp_target_percentage1)

# Create data frame
species_targets1<-as.data.frame(spp_target_percentage1)
colnames(species_targets1)<-"percentage_target"

# Create a list of species' names
sp_list1<-as.data.frame(sp_10quantile_names)
colnames(sp_list1)<-"Species"

# Bind targets and sp names 
species_targets1<-cbind(sp_list1, species_targets1)
species_area1_df <- cbind(sp_list1, species_area1_df)

# Save csv
write.csv2(species_targets1, "DataNew/Excel/Species_10quantile_targets.csv")

# Create one target df and save  
targets<-rbind(species_targets, species_targets1)
# Arrange alphabetically
targets<-arrange(targets, Species)
#Save 
write.csv2(targets, "DataNew/Excel/Species_quantile_targets.csv")
