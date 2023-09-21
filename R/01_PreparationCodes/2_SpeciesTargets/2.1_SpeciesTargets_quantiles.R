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
# 3.Evaluate appropriate targets boundaries and confront them 
#===============================================================================


library(tidyselect)
library(raster)
library(stringr)
library(prioritizr)
library(remotes)
library(edmaps)

# Import species quantile maps
rastlist_sp<-list.files(path = "Data/RawData/Sp_quantile/q35", pattern='*.asc$', full.names = TRUE)
allrasterquantile<-lapply(rastlist_sp, raster)
species_35quantile<- stack(allrasterquantile)
plot(species_35quantile)
sp_35quantile_names <- names(species_35quantile)

# Calculate range area
species_area<-c() 

for (sp in species_35quantile@layers) {
  sp[sp == 0] <- NA
  ncells<-cellStats(sp, stat = sum, na.rm = TRUE )
  area<-ncells*0.0625
  species_area<-rbind(species_area, area)
}

class(species_area)

# Convert the matrix into a class numeric
species_areas<- as.numeric(species_area) 
species_area_df <- data.frame(species_area)
colnames(species_area_df)<-"area"

# Import species quantile maps
rastlist_sp_10<-list.files(path = "Data/RawData/Sp_quantile/q10", pattern='*.asc$', full.names = TRUE)
allrasterquantile10<-lapply(rastlist_sp_10, raster)
species_10quantile<- stack(allrasterquantile10)
sp_10quantile_names <- names(species_10quantile)

# Calculate range area
species_area1<-c() 

for (sp1 in species_10quantile@layers) {
  sp1[sp1 == 0] <- NA
  ncells1<-cellStats(sp1, stat = sum, na.rm = TRUE )
  area1<-ncells1*0.0625
  species_area1<-rbind(species_area1, area1)
}

class(species_area1)

# convert the matrix into a class numeric
species_areas1<- as.numeric(species_area1) 
species_area1_df <- data.frame(species_area1)
colnames(species_area1_df)<-"area"

# Make unique data frame
species_area_all<-rbind(species_area_df, species_area1_df)
#Re-arrange in ascending order
# species_area_all <- species_area_all%>% arrange(species_area_all$area)

#Set the data frame as a numeric list for the log linear interpol function
species_area_all<-as.list(species_area_all$area)
species_area_all<-as.numeric(species_area_all)

## Calcualate species' specific targets 
spp_target_percentage <-
  loglinear_interpolation(
    x = species_area_all,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 10000,
    coordinate_two_y = 0.1)

print(spp_target_percentage)

## Calcualate species' specific targets 
spp_target_percentage_OLD <-
  loglinear_interpolation(
    x = species_area_all,
    coordinate_one_x = 1000,
    coordinate_one_y = 1,
    coordinate_two_x = 250000,
    coordinate_two_y = 0.1)

print(spp_target_percentage_OLD)


#Difference targets

Dtarget<-spp_target_percentage_OLD - spp_target_percentage
