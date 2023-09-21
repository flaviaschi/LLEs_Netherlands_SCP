#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Scenario I (Maximize ES Sceanrio) 
# Conserve species while maximizing ES monetary values 
# -----------------------------------------------------------------------------
# The script contains codes for:  
# 1) Conservation Problem (all of the 16 possible solutions)
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
#Import raster costs
costs <- raster("Data/CleanData/Costs/ESS_flipped/ESS_flipped.tif")
names(costs) <- c("Costs")
plot(costs, main="Pollination and Carbon Sequestration costs, and PU")

# B) Biodiversity features
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
plot(pu[, "Costs"], main = "Planning unit costs - ESS")

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
p2 <- p1 %>% add_boundary_penalties(0.5, 1, data= bmat) 
p3 <- p1 %>% add_boundary_penalties(1, 1, data= bmat) 
p4 <- p1 %>% add_boundary_penalties(2, 1, data= bmat) 
p5 <- p1 %>% add_boundary_penalties(2.5, 1, data= bmat) 
p6 <- p1 %>% add_boundary_penalties(5, 1, data= bmat) 
p7 <- p1 %>% add_boundary_penalties(10, 1, data= bmat) 
p8 <- p1 %>% add_boundary_penalties(25, 1, data= bmat) 
p9 <- p1 %>% add_boundary_penalties(50, 1, data= bmat) 
p10 <- p1 %>% add_boundary_penalties(100, 1, data= bmat)  
p11<- p1 %>% add_boundary_penalties(150, 1, data= bmat)  
p12<- p1 %>% add_boundary_penalties(200, 1, data= bmat)  
p13<- p1 %>% add_boundary_penalties(250, 1, data= bmat)  
p14 <- p1 %>% add_boundary_penalties(300, 1, data= bmat)  
p15 <- p1 %>% add_boundary_penalties(400, 1, data= bmat)  
# p16 <- p1 %>% add_boundary_penalties(500, 1, data= bmat)  


# Solve solution
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
# s16 <- solve(p16)
 

# Plot (some) solutions

plot(st_as_sf(s1[, "solution_1"]), main = "Prioritization p1 - no boundary penalty",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s3[, "solution_1"]), main = "Prioritization p2 - Low penalty (1)",
     pal = c("grey90", "darkgreen"))

plot(st_as_sf(s13[, "solution_1"]), main = "Prioritization p3 - medium/ high boundary penalty (250)",
     pal = c("grey90", "darkgreen"))


# Convert the solution into a RasterLayer
library(rasterize)

# Save solution1 
solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
names(solution1)<- c("Solution1")
plot(solution1, main = "Solution 1 - No BP")
a <- cellStats(solution1, stat = "sum", na.rm= TRUE)
so1 <- solution1
so1[so1 == 0] <- NA

# solution 2
solution2 <- rasterize(s2, costs, res=500, field = c("solution_1"), backround=NA )
names(solution2)<- c("Solution2")
plot(solution2, main = "Solution 2 - BP 0.5")
b <- cellStats(solution2, stat = "sum", na.rm= TRUE)

# solution 3
solution3 <- rasterize(s3, costs, res=500, field = c("solution_1"), backround=NA )
names(solution3)<- c("Solution3")
plot(solution3, main = "Solution 3 - BP 1")
c <- cellStats(solution3, stat = "sum", na.rm= TRUE)
so3 <- solution3
so3[so3 == 0] <- NA

# solution 4
solution4 <- rasterize(s4, costs, res=500, field = c("solution_1"), backround=NA )
names(solution4)<- c("Solution4")
plot(solution4, main = "Solution 4- BP 2")
d <- cellStats(solution4, stat = "sum", na.rm= TRUE)

# solution 5
solution5 <- rasterize(s5, costs, res=500, field = c("solution_1"), backround=NA )
names(solution5)<- c("Solution5")
plot(solution5, main = "Solution 15 - BP 2.5")
e <- cellStats(solution5, stat = "sum", na.rm= TRUE)

# solution 6
solution6 <- rasterize(s6, costs, res=500, field = c("solution_1"), backround=NA )
names(solution6)<- c("Solution4")
plot(solution6, main = "Solution 6 - BP 5")
f <- cellStats(solution6, stat = "sum", na.rm= TRUE)

# solution 7
solution7 <- rasterize(s7, costs, res=500, field = c("solution_1"), backround=NA )
names(solution7)<- c("Solution7")
plot(solution7, main = "Solution 7 - BP 10")
g <- cellStats(solution7, stat = "sum", na.rm= TRUE)

# solution 8
solution8 <- rasterize(s8, costs, res=500, field = c("solution_1"), backround=NA )
names(solution8)<- c("Solution8")
plot(solution8, main = "Solution 8 - BP 25")
h <- cellStats(solution8, stat = "sum", na.rm= TRUE)

# solution 9
solution9 <- rasterize(s9, costs, res=500, field = c("solution_1"), backround=NA )
names(solution9)<- c("Solution9")
plot(solution9, main = "Solution 9 - BP 50")
i <- cellStats(solution9, stat = "sum", na.rm= TRUE)

# solution 10
solution10 <- rasterize(s10, costs, res=500, field = c("solution_1"), backround=NA )
names(solution10)<- c("Solution10")
plot(solution10, main = "Solution 10 - BP 100")
l <- cellStats(solution10, stat = "sum", na.rm= TRUE)

# solution 11
solution11 <- rasterize(s11, costs, res=500, field = c("solution_1"), backround=NA )
names(solution11)<- c("Solution11")
plot(solution11, main = "Solution 11 - BP 150")
m <- cellStats(solution11, stat = "sum", na.rm= TRUE)

# solution 12
solution12 <- rasterize(s12, costs, res=500, field = c("solution_1"), backround=NA )
names(solution12)<- c("Solution12")
plot(solution12, main = "Solution 12 - BP 200")
o <- cellStats(solution12, stat = "sum", na.rm= TRUE)

# solution 13
solution13 <- rasterize(s13, costs, res=500, field = c("solution_1"), backround=NA )
names(solution13)<- c("Solution13")
plot(solution13, main = "Solution 13 - BP 250")
q <- cellStats(solution13, stat = "sum", na.rm= TRUE)
so13 <- solution13
so13[so13 == 0] <- NA

# solution 14
solution14 <- rasterize(s14, costs, res=500, field = c("solution_1"), backround=NA )
names(solution14)<- c("Solution14")
plot(solution14, main = "Solution 14 - BP 300")
r <- cellStats(solution14, stat = "sum", na.rm= TRUE)

# solution 15
solution15<- rasterize(s15, costs, res=500, field = c("solution_1"), backround=NA )
names(solution15)<- c("Solution15")
plot(solution15, main = "Solution 15 - BP 400")
t <- cellStats(solution15, stat = "sum", na.rm= TRUE)

# solution 16
# solution16 <- rasterize(s16, costs, res=500, field = c("solution_1"), backround=NA )
# names(solution16)<- c("Solution16")
# plot(solution16, main = "Solution 16 - BP 500")
# u <- cellStats(solution16, stat = "sum", na.rm= TRUE)

pu_stat <- data.frame(pu_cells = c(a,b,c,d,e,f,g,h,i,l,m,o,q,r,t))

#Save csv
write.csv2(pu_stat, "Outcome/Scenario1/pu_stat.csv")

S1_Sol1<- writeRaster(solution1, "Outcome/Scenario1/Solution/Solution1.tif")
S1_Sol13<- writeRaster(solution13, "Outcome/Scenario1/Solution/Solution13.tif")
S1_Sol14<- writeRaster(solution14, "Outcome/Scenario1/Solution/Solution14.tif")
