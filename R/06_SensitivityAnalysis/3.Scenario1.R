#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Sensitivity analysis - The following script contains codes for creating 
# functions for running Scenario 1 (Maximize ES Scenario)
#===============================================================================


#===============================================================================
# CONSERVATION PROBLEM
#===============================================================================

scenario1 <- function(costs, targets, lb, hb, cost) {
  
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
  # p3 <- p1 %>% add_boundary_penalties(1, 1, data= bmat) 
  p13<- p1 %>% add_boundary_penalties(250, 1, data= bmat)  
  
  
  s1 <- solve(p1)
  # s3 <- solve(p3)
  s13 <- solve(p13)
  
  
  #Convert the solution into a RasterLayer
  
  #Save solution1 
  solution1 <- rasterize(s1, costs, res=500, field = c("solution_1"), backround=NA )
  names(solution1)<- c("Solution1")
  so1 <- solution1
  so1[so1 == 0] <- NA
  writeRaster(so1, paste0("Outcomes/rasters/Scenario1/Solution_Scen1_noBP_LB", lb, "_HB", hb, "_cost", cost, ".tif"), overwrite = T)
  
  
  # solution13
  solution13 <- rasterize(s13, costs, res=500, field = c("solution_1"), backround=NA )
  names(solution13)<- c("Solution13")
  so13 <- solution13
  so13[so13 == 0] <- NA
  writeRaster(so13, paste0("Outcomes/rasters/Scenario1/Solution_Scen1_BP250_LB", lb, "_HB", hb, "_cost", cost, ".tif"), overwrite = T)
  
  
  #===============================================================================
  # POST HOC
  #===============================================================================
  
  
  # 1)  EUROS SAVED WITH ESS in solutions
  
  # Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
  #also where the cells = 0
  solution1[solution1 == 0] <- NA
  # solution3[solution3 == 0] <- NA
  solution13[solution13 == 0] <- NA
  
  # solution1
  ESS_savedSol1 <- extractByMask(layer = ESS_saved, msk = solution1, spatial = TRUE)
  # plot(ESS_savedSol1)
  ESS_saved_sum_Sol1<-cellStats(ESS_savedSol1, 'sum', na.rm= TRUE)
  
  # solution13
  ESS_savedSol13<- extractByMask(layer = ESS_saved, msk = solution13, spatial = TRUE)
  # plot(ESS_savedSol13)
  ESS_saved_sum_Sol13<-cellStats(ESS_savedSol13, 'sum', na.rm= TRUE)
  
  # 2) MONEY SPENT
  
  # solution1
  euros_spent_Sol1 <- extractByMask(layer = expenses, msk = solution1, spatial = TRUE)
  euros_spent_sum_Sol1<-cellStats(euros_spent_Sol1, 'sum')
  
  # solution13
  euros_spent_Sol13 <- extractByMask(layer = expenses, msk = solution13, spatial = TRUE)
  euros_spent_sum_Sol13<-cellStats(euros_spent_Sol13, 'sum')

  out <- c(ESS_saved_sum_Sol1, ESS_saved_sum_Sol13,
           euros_spent_sum_Sol1, euros_spent_sum_Sol13)
  
  
  outdf <-  data.frame("ESS_saved_sum_Sol1" = out[1], "ESS_saved_sum_Sol13"= out[2], 
                       "euros_spent_sum_Sol1" = out[3], "euros_spent_sum_Sol13" = out[4])
  
  saveRDS(outdf, file = paste0("Outcomes/tables/Scenario1/Out_Scen1_LB", lb, "_HB", hb, "_cost", cost, ".RDS"))
  outdf
}
