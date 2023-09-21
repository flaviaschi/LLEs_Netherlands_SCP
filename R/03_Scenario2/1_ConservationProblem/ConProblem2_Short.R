#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Scenario 2 - (Minimize Costs Scenario)
# Conserve species while minimizing the costs 
# -----------------------------------------------------------------------------
# The script contains codes for:  
# 1) Conservation Problem (only selected solutions 1,15)
# 2) Feature representation   
#===============================================================================

library(rgdal)
library(raster)
library(slam)
library(gurobi) 
library(remotes)
library(prioritizr)
library(scales)
library(dplyr)
library(bnspatial)

#===============================================================================
# LOAD IN DATA
#===============================================================================

# A) Costs
#Import raster costs
costs <- raster("Data/CleanData/Costs/Normal_costs/Costs.tif")
names(costs) <- c("Costs")
plot(costs, main="Management and opportunity costs, and PU")


# B) Biodiversity fetures
# Conservation features (sp probability and sp density P/A binary maps)
con_features<-list.files(path= "Data/CleanData/Sp_quantile/All_conservation_features/", 
                         pattern='*.tif$', full.names = TRUE)
all_confeatures <- lapply(con_features, raster)
conservation_features<- stack(all_confeatures)
# plot(conservation_features)
names(conservation_features)

#Remove species not present in NB
conservation_features1 <- dropLayer(conservation_features,  12)
conservation_features1 <- dropLayer(conservation_features1, 14)
conservation_features1 <- dropLayer(conservation_features1, 16)
conservation_features <- dropLayer(conservation_features1, 20)
names(conservation_features) # check names


# c) Targets
# Load in species targets - 
targets_species <- read.csv2("DataNew/Excel/Species_targets_pa.csv", sep= ";")
colnames(targets_species) <- c("x", "targets")
# Select only colum with numeric targets 
targets <- targets_species$target 

#===============================================================================
# CONSERVATION PROBLEM
#===============================================================================

# Set up SF table 
costs[is.na(costs)] <- -9999   # turn NAs into -9999 - to make the SpatialPol work

# Turn into a Spatial Polygon DF
cos <- as(costs, "SpatialPolygonsDataFrame")

#Convert costs and locked in spatial polygon dataframe into an SF OBJECT
cost_sf <- st_as_sf(cos)

#Create planning units (pu) sf object - filter out NAs (the -9999 values)
pu <- cost_sf %>% 
  filter(Costs > -9999) %>%
  mutate (id = row_number())

# plot map of planning unit costs
plot(pu[, "Costs"], main = "Planning unit costs")

# precomputed boundary data
bmat <- boundary_matrix(pu)  # this speeds up a lot the computation!

#Probelm formulation
p1 <- problem(pu, conservation_features, "Costs") %>%
  add_min_set_objective() %>%  
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap= 0.05)   #The gap is relative and expresses the 
# acceptable deviance from the optimal objective.The default is 0.1 (10% from optimality)  

#Adding boundary penalties
p14 <- p1 %>% add_boundary_penalties(10, 1, data= bmat) 

#Find solution
s1 <- solve(p1)
s14 <- solve(p14)

#plot solutions
plot(st_as_sf(s1[, "solution_1"]), main = "Prioritization p1 - no boundary penalty",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s14[, "solution_1"]), main = "Prioritization p3 - Low penalty (10)",
     pal = c("grey90", "darkgreen"))


#Convert the solution into a RasterLayer
library(rasterize)

#Save solution1 
solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
names(solution1)<- c("Solution1")
plot(solution1, main = "Solution 1 - No BP" )
a <- cellStats(solution1, stat = "sum", na.rm= TRUE)
writeRaster(solution1, "Outcome/Scenario2/Solution/Scen2_solution1.tif", overwrite= T)

# solution14
solution14 <- rasterize(s14, costs, res=500, field = c("solution_1"), backround=NA )
names(solution14)<- c("Solution14")
plot(solution14, main = "Solution 14 - BP 20")
t <- cellStats(solution14, stat = "sum", na.rm= TRUE)
writeRaster(solution14, "Outcome/Scenario2/Solution/Scen2_solution14.tif", overwrite= T)

#===============================================================================
# FEATURE REPRESENTATION
#===============================================================================

# 1) Solution 1

# Calculate  feature representation statistics based on the prioritization
tc_s1 <- eval_target_coverage_summary(p1, s1[, "solution_1"])
print(tc_s1)

# Remove species which do not occur in NB
tc_s1 <- tc_s1 %>%
  dplyr::filter(total_amount != "0")

# Summarize representation (values show percent coverage)
summary(tc_s1$relative_held * 100)

# Calculate number of features adequately represented by the prioritization
sum(tc_s1$met)

# Save csv
write.csv2(tc_s1, "Outcome/Scenario2/Solution/SpeciesRepresScen2Sol1.csv")

# Visualize representation  (values show percent coverage)
hist(tc_s1$relative_held * 100,
     main = "Feature representation by prioritization - Scenario 2 No BP",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")

# 2) Solution 14

# Calculate  feature representation statistics based on the prioritization
tc_s14 <- eval_target_coverage_summary(p14, s14[, "solution_1"])
print(tc_s14)

# Remove species which do not occur in NB
tc_s14 <- tc_s14 %>%
  dplyr::filter(total_amount != "0")

# Summarize representation (values show percent coverage)
summary(tc_s14$relative_held * 100)

# Calculate number of features adequately represented by the prioritization
sum(tc_s14$met)

# Save table
write.csv2(tc_s14, "Outcome/Scenario2/Solution/SpeciesRepresScen2Sol14.csv")

# Visualize representation  (values show percent coverage)
hist(tc_s14$relative_held * 100,
     main = "Feature representation by prioritization - Scenario 2 BP 20",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")

# Dataframes with relative held targets  
s2lshRepT <- as.data.frame(tc_s1$feature)
s2lshRepT$held_targets <- tc_s1$relative_held

s2lspRepT <- as.data.frame(tc_s14$feature)
s2lspRepT$held_targets <- tc_s14$relative_held

