#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Scenario 1 (Maximize ES Sceanrio) - Species' based in agricultural areas:
# Conserve species while maximizing ES monetary values 
# The script contains codes for:  
# 1) Conservation Problem (only selected probelms 1,13 , respectively
# land-sharing and land-sparing )
# 2) Feature representation of each solution
#===============================================================================

# Import libraries
library(rgdal)
library(raster)
library(slam)
library(gurobi) 
library(remotes)
library(prioritizr)
library(scales)
library(dplyr)
library(bnspatial)
library(rasterize)

#===============================================================================
# LOAD IN DATA
#===============================================================================

# A) Costs
# Import raster costs
costs <- raster("Data/CleanData/Costs/flip_benefits.tif")
names(costs) <- c("Costs")
plot(costs, main="Benefits, and PU")


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
# targets_species <- select(targets_species, -1,-2)
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
plot(pu[, "Costs"], main = "Planning unit costs - ESS")

# precomputed boundary data
bmat <- boundary_matrix(pu)  # this speeds up a lot the computation!

#Probelm formulation
p1 <- problem(pu, conservation_features, "Costs") %>%
  add_min_set_objective() %>%  
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap= 0.05)   #The gap is relative and expresses the 
  #acceptable deviance from the optimal objective.The default is 0.1 (10% from optimality)  

#Adding boundary penalties
p13<- p1 %>% add_boundary_penalties(250, 1, data= bmat)  

#Find solution
s1 <- solve(p1)
s13 <- solve(p13)


#plot solutions
plot(st_as_sf(s1[, "solution_1"]), main = "Prioritization p1-2 - no boundary penalty",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s13[, "solution_1"]), main = "Prioritization p3 - medium boundary penalty (100)",
     pal = c("grey90", "darkgreen"))


#Convert the solution into a RasterLayer
library(rasterize)

#Save solution1 
solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
names(solution1)<- c("Solution1")
plot(solution1, main = "Maximize ES - Land Sharing")
a <- cellStats(solution1, stat = "sum", na.rm= TRUE)
so1 <- solution1
so1[so1 == 0] <- NA
writeRaster(so1, "Outcome/Scenario1/Solution/So1.tif", overwrite = T)


# solution13
solution13 <- rasterize(s13, costs, res=500, field = c("solution_1"), backround=NA )
names(solution13)<- c("Solution13")
plot(solution13, main = "Maximize ES - Land Sparing")
q <- cellStats(solution13, stat = "sum", na.rm= TRUE)
so13 <- solution13
so13[so13 == 0] <- NA
writeRaster(so13, "Outcome/Scenario1/Solution/So13.tif", overwrite = T)


#===============================================================================
# FEATURE REPRESENTATION
#===============================================================================

# 1) Solution 1

# calculate  feature representation statistics based on the prioritization
tc_s1 <- eval_target_coverage_summary(p1, s1[, "solution_1"])
print(tc_s1)

#Remove species which do not occur in NB
tc_s1 <- tc_s1 %>%
  dplyr::filter(total_amount != "0")

# summarize representation (values show percent coverage)
summary(tc_s1$relative_held * 100)

## calculate number of features adequately represented by the prioritization
sum(tc_s1$met)
class(tc_s1)

# This is to see the difference between the absolute target and what it is
# conserved
{
relative_held <- data.frame(target = tc_s1$relative_held*100)
relative_target <- data.frame(target = tc_s1$relative_target*100)

relative_held$type <- 'relative_held'
relative_target$type <-  'relative_target'

library(ggplot2)
hist <- rbind(relative_held, relative_target)

hist %>%
  mutate(hist$target *100)

ggplot(hist, aes(target, fill = type)) +
  geom_histogram(binwidth=10, alpha = 0.5, position = 'identity') +
  xlim(0, 100)

ggplot(relative_target, aes(target)) +
  geom_histogram(binwidth=10) +
  xlim(0, 100)

c <- ggplot(relative_target, aes(target)) +
  geom_freqpoly(binwidth=10)
}

# Visualize representation  (values show percent coverage)
hist(tc_s1$relative_target * 100,
     main = "Feature representation by Land Sharing scenario ",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")

hist(tc_s1$relative_held * 100,
      main = "Feature representation by Land Sharing scenario ",
      xlim = c(0, 100),
      xlab = "Percent coverage of features (%)") #,
      # col="red",
      # add= T)

write.csv2(tc_s1, "Outcome/Scenario1/Solution/FeatureRepScen1Sol1.csv")


# 2) Solution 13

# Calculate feature representation statistics based on the prioritization
tc_s13 <- eval_target_coverage_summary(p13, s13[, "solution_1"])
print(tc_s13)

# Remove species which do not occur in NB
tc_s13 <- tc_s13 %>%
  dplyr::filter(total_amount != "0")

# Summarize representation (values show percent coverage)
summary(tc_s13$relative_held * 100)

# Calculate number of features adequately represented by the prioritization
sum(tc_s13$met)

# Save csv
write.csv2(tc_s13, "Outcome/Scenario1/Solution/Sol13.csv")

# Visualize representation  (values show percent coverage)
hist(tc_s13$relative_held * 100,
     main = "Feature representation by Land Sparing",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")

write.csv2(tc_s13, "Outcome/Scenario1/Solution/FeatureRepScen1Sol13.csv")

# Dataframes with relative held targets 
s1lshRepT <- as.data.frame(tc_s1$feature)
s1lshRepT$held_targets <- tc_s1$relative_held

s1lspRepT <- as.data.frame(tc_s13$feature)
s1lspRepT$held_targets <- tc_s13$relative_held

# Comparison between scenarios
A <- s1lshRepT$held_targets - s2lshRepT$held_targets
B <- s1lspRepT$held_targets - s2lspRepT$held_targets

# Average conservation targets held per scenario

s1lspRepT$held_targets <- tc_s13$relative_held
s1lshRepT$held_targets <- tc_s1$relative_held



