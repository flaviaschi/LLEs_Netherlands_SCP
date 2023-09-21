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
# 1) Conservation Problem (all of the solutions)
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
p2 <- p1 %>% add_boundary_penalties(0.1, 1, data= bmat) 
p3 <- p1 %>% add_boundary_penalties(0.2, 1, data= bmat) 
p4 <- p1 %>% add_boundary_penalties(0.3, 1, data= bmat) 
p5 <- p1 %>% add_boundary_penalties(0.4, 1, data= bmat) 
p6 <- p1 %>% add_boundary_penalties(0.5, 1, data= bmat) 
p7 <-p1 %>% add_boundary_penalties(1, 1, data= bmat) 
p8 <-p1 %>% add_boundary_penalties(1.5, 1, data= bmat) 
p9 <- p1 %>% add_boundary_penalties(2, 1, data= bmat) 
p10 <- p1 %>% add_boundary_penalties(2.5, 1, data= bmat) 
p11<- p1 %>% add_boundary_penalties(3, 1, data= bmat) 
p12<- p1 %>% add_boundary_penalties(4, 1, data= bmat) 
p13<- p1 %>% add_boundary_penalties(5, 1, data= bmat) 
p14 <- p1 %>% add_boundary_penalties(10, 1, data= bmat) 
p15 <- p1 %>% add_boundary_penalties(20, 1, data= bmat) 
p16 <- p1 %>% add_boundary_penalties(25, 1, data= bmat) 



#Find solution
s1 <- solve(p1)
s2 <- solve(p2)
s3 <- solve(p3)
s4 <- solve(p4)
s5 <- solve(p5)
s6 <- solve(p6)
s7 <- solve(p7)
s8 <- solve(p8)
s9 <- solve(p9)
s10 <- solve(p10)
s11 <- solve(p11)
s12 <- solve(p12)
s13 <- solve(p13)
s14 <- solve(p14)
s15 <- solve(p15)
s16 <- solve(p16)


#plot (some) solutions

plot(st_as_sf(s1[, "solution_1"]), main = "Prioritization p1 - no boundary penalty",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s4[, "solution_1"]), main = "Prioritization p2 - Low penalty (0.3)",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s15[, "solution_1"]), main = "Prioritization p3 - Low penalty (20)",
     pal = c("grey90", "darkgreen"))


#Convert the solution into a RasterLayer
library(rasterize)

#Save solution1 
solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
names(solution1)<- c("Solution1")
plot(solution1, main = "Solution 1 - No BP" )
a <- cellStats(solution1, stat = "sum", na.rm= TRUE)

# solution2
solution2 <- rasterize(s2, costs, res=500, field = c("solution_1"), backround=NA )
names(solution2)<- c("Solution2")
plot(solution2, main = "Solution 2 -BP 0.1" )
b <- cellStats(solution2, stat = "sum", na.rm= TRUE)

# solution3
solution3 <- rasterize(s3, costs, res=500, field = c("solution_1"), backround=NA )
names(solution3)<- c("Solution3")
plot(solution3, main = "Solution 3- BP 0.2")
c <- cellStats(solution3, stat = "sum", na.rm= TRUE)

# solution4
solution4 <- rasterize(s4, costs, res=500, field = c("solution_1"), backround=NA )
names(solution4)<- c("Solution4")
plot(solution4, main = "Solution 4 - BP 0.3")
d <- cellStats(solution4, stat = "sum", na.rm= TRUE)

# solution5
solution5 <- rasterize(s5, costs, res=500, field = c("solution_1"), backround=NA )
names(solution5)<- c("Solution5")
plot(solution5, main = "Solution 5 - BP 0.4")
e <- cellStats(solution5, stat = "sum", na.rm= TRUE)

# solution6
solution6 <- rasterize(s6, costs, res=500, field = c("solution_1"), backround=NA )
names(solution6)<- c("Solution6")
plot(solution6, main = "Solution 6 - BP 0.5")
f <- cellStats(solution6, stat = "sum", na.rm= TRUE)

# solution7
solution7 <- rasterize(s7, costs, res=500, field = c("solution_1"), backround=NA )
names(solution7)<- c("Solution7")
plot(solution7, main = "Solution 7 - BP 1")
g <- cellStats(solution7, stat = "sum", na.rm= TRUE)

# solution8
solution8 <- rasterize(s8, costs, res=500, field = c("solution_1"), backround=NA )
names(solution8)<- c("Solution8")
plot(solution8, main = "Solution 8 - BP 1.5")
h <- cellStats(solution8, stat = "sum", na.rm= TRUE)

# solution9
solution9 <- rasterize(s9, costs, res=500, field = c("solution_1"), backround=NA )
names(solution9)<- c("Solution9")
plot(solution9, main = "Solution 9 - BP 2")
i <- cellStats(solution9, stat = "sum", na.rm= TRUE)

# solution10
solution10 <- rasterize(s10, costs, res=500, field = c("solution_1"), backround=NA )
names(solution10)<- c("Solution10")
plot(solution10, main = "Solution 10 - BP 2.5")
l <- cellStats(solution10, stat = "sum", na.rm= TRUE)

# solution11
solution11 <- rasterize(s11, costs, res=500, field = c("solution_1"), backround=NA )
names(solution11)<- c("Solution11")
plot(solution11, main = "Solution 11 - BP 3")
m <- cellStats(solution11, stat = "sum", na.rm= TRUE)


# solution12
solution12 <- rasterize(s11, costs, res=500, field = c("solution_1"), backround=NA )
names(solution12)<- c("Solution12")
plot(solution12, main = "Solution 12 - BP 4")
o <- cellStats(solution12, stat = "sum", na.rm= TRUE)


# solution13
solution13 <- rasterize(s13, costs, res=500, field = c("solution_1"), backround=NA )
names(solution13)<- c("Solution13")
plot(solution13, main = "Solution 13 - BP 5")
q <- cellStats(solution13, stat = "sum", na.rm= TRUE)

# solution14
solution14 <- rasterize(s14, costs, res=500, field = c("solution_1"), backround=NA )
names(solution14)<- c("Solution14")
plot(solution14, main = "Solution 14 - BP 10")
r <- cellStats(solution14, stat = "sum", na.rm= TRUE)

# solution15
solution15 <- rasterize(s15, costs, res=500, field = c("solution_1"), backround=NA )
names(solution15)<- c("Solution15")
plot(solution15, main = "Solution 15 - BP 20")
t <- cellStats(solution15, stat = "sum", na.rm= TRUE)

# solution16
solution16 <- rasterize(s16, costs, res=500, field = c("solution_1"), backround=NA )
names(solution16)<- c("Solution16")
plot(solution16, main = "Solution 16 - BP 25")
u <- cellStats(solution16, stat = "sum", na.rm= TRUE)

pu_stat <- data.frame(pu_cells = c(a,b,c,d,e,f,g,h,i,l,m,o, q, r,t))

#Save csv
write.csv2(pu_stat, "Outcome/Scenario2/pu_stat_scen2_lowpen.csv")


S1_Sol1<- writeRaster(solution1, "Outcome/Scenario2/Solution/S2_Solution1.tif")
S1_Sol15<- writeRaster(solution15, "Outcome/Scenario2/Solution/S2_Solution15.tif")
S1_Sol14<- writeRaster(solution14, "Outcome/Scenario2/Solution/S2_Solution14.tif")



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

write.csv2(tc_s1, "Outcome/Scenario2/Solution/Sol1.csv")

# Visualize representation  (values show percent coverage)
hist(tc_s1$relative_held * 100,
     main = "Feature representation by prioritization - Scenario 2 No BP",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")


# 2) Solution 15
# Calculate  feature representation statistics based on the prioritization
tc_s15 <- eval_target_coverage_summary(p15, s15[, "solution_1"])
print(tc_s15)

# Remove species which do not occur in NB
tc_s15 <- tc_s15 %>%
  dplyr::filter(total_amount != "0")

# Summarize representation (values show percent coverage)
summary(tc_s15$relative_held * 100)

# Calculate number of features adequately represented by the prioritization
sum(tc_s15$met)

# Save table
write.csv2(tc_s15, "Outcome/Scenario2/Solution/Sol15.csv")

# Visualize representation  (values show percent coverage)
hist(tc_s15$relative_held * 100,
     main = "Feature representation by prioritization - Scenario 2 BP 20",
     xlim = c(0, 100),
     xlab = "Percent coverage of features (%)")




