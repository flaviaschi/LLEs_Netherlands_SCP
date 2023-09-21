#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Sensitivity analysis - The following script contains codes for creating 
# functions for running Scenario 2 (Minimize Costs  Scenario)
#===============================================================================

#===============================================================================
# CONSERVATION PROBLEM
#===============================================================================

scenario2 <- function(costs, targets, lb, hb, cost) {
  
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
   # p4 <- p1 %>% add_boundary_penalties(0.3, 1, data= bmat) 
   p14<- p1 %>% add_boundary_penalties(10, 1, data= bmat)  
  
  
  s1 <- solve(p1)
  s14 <- solve(p14)
  
  
  #Convert the solution into a RasterLayer
  
  #Save solution1 
  solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
  names(solution1)<- c("Solution1")
  so1 <- solution1
  so1[so1 == 0] <- NA
  writeRaster(so1, paste0("Outcomes/rasters/Scenario2/Solution_Scen2_noBP_LB", lb, "_HB", hb, "_cost", cost, ".tif"), overwrite = T)
  
  
  # solution14
  solution14 <- rasterize(s14, costs, res=500, field = c("solution_1"), backround=NA )
  names(solution14)<- c("Solution14")
  so14 <- solution14
  so14[so14 == 0] <- NA
  writeRaster(so14, paste0("Outcomes/rasters/Scenario2/Solution_Scen2_BP10_LB", lb, "_HB", hb, "_cost", cost, ".tif"), overwrite = T)
  
  
  #===============================================================================
  # POST HOC
  #===============================================================================
  
  
  # 1)  EUROS SAVED WITH ESS in solutions
  
  # Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
  #also where the cells = 0
  solution1[solution1 == 0] <- NA
  # solution4[solution4 == 0] <- NA
  solution14[solution14 == 0] <- NA
  
  # solution1
  ESS_savedSol1 <- extractByMask(layer = ESS_saved, msk = solution1, spatial = TRUE)
  # plot(ESS_savedSol1)
  ESS_saved_sum_Sol1<-cellStats(ESS_savedSol1, 'sum', na.rm= TRUE)
  
  # solution15
  ESS_savedSol14<- extractByMask(layer = ESS_saved, msk = solution14, spatial = TRUE)
  # plot(ESS_savedSol14)
  ESS_saved_sum_Sol14<-cellStats(ESS_savedSol14, 'sum', na.rm= TRUE)
  
  # 2) MONEY SPENT
  
  # solution1
  euros_spent_Sol1 <- extractByMask(layer = expenses, msk = solution1, spatial = TRUE)
  euros_spent_sum_Sol1<-cellStats(euros_spent_Sol1, 'sum')

  # solution14
  euros_spent_Sol14 <- extractByMask(layer = expenses, msk = solution14, spatial = TRUE)
  euros_spent_sum_Sol14<-cellStats(euros_spent_Sol14, 'sum')
  
  out <- c(ESS_saved_sum_Sol1, ESS_saved_sum_Sol14,
           euros_spent_sum_Sol1, euros_spent_sum_Sol14)
  
  outdf <-  data.frame("ESS_saved_sum_Sol1" = out[1],  "ESS_saved_sum_Sol14"= out[2],
                       "euros_spent_sum_Sol1" = out[3], "euros_spent_sum_Sol14" = out[4])
  
  saveRDS(outdf, file = paste0("Outcomes/tables/Scenario2/Out_Scen2_LB", lb, "_HB", hb, "_cost", cost, ".RDS"))
  outdf
}
