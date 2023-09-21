#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# -----------------------------------------------------------------------------
# The following script contains codes for calculating:
# (B) Calculate area (in km2) of hedgerows in Noord Brabant 
# (C) Calculate the total agricultural land area in NB
# (D) Calculate how much area (in km2) has to be hedgerows for Deltaplan (5%)
# (E) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# (F) calculate proportions (Sol1 scen1)
# (G) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# (H) calculate proportions (sol13 scen 1)
# (I) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# (L) calculate proportions (sol1 scen 2)
# (M) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# (N) calculate proportions (sol15 scen 2)
# (O) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# ===========================================================================

library(raster)
library(bnspatial)


# (A) Import data
# Agricultural area
agri <- raster("Data/CleanData/Hedgerows/Agri_NB.tif")
agri[!agri == 51] <- NA 
agri[agri == 51] <- 1 
plot(agri)
# Hedgerows density map 
hedges <- raster("Data/CleanData/Hedgerows/Hedge_density.tif")
# Solutions
sol1 <- raster("Outcome/Scenario1/Solution/Solution1.tif")
sol1[sol1== 0] <- NA
sol14 <- raster("Outcome/Scenario1/Solution/Solution14.tif")
sol14[sol14== 0] <- NA
sol1Scen2<- raster("Outcome/Scenario2/Solution/S2_Solution1.tif")
sol1Scen2[sol1Scen2== 0] <- NA
sol14Scen2<- raster("Outcome/Scenario2/Solution/S2_Solution14.tif")
sol14Scen2[sol14Scen2== 0] <- NA

# (B) Calculate area (in km2) of hedgerows in Noord Brabant 
# The denisty map in in km/km2
# I need to fisr calculate an adimentional map by multiplying by an average
# 2.5m width of hedges (0.0025 km)
hedges_adimentional <- hedges * 0.0025
# Then i calculate the area, in m2
hedges_surface <- hedges_adimentional*(500*500)
# And turn it into km2 (*e-6)
hedges_km2 <- hedges_surface/ 1000000
# Now i can sum up all the valeus per cells to find the area
tot_hedge_km2 <- cellStats(hedges_km2, "sum", na.rm= T)  # 41.9 km2


# (C) Calulate the total agricultural land area in NB
# I fist count the cells (at a resolution 500x500m)
agri_count_tot <- cellStats(agri, "sum", na.rm = TRUE)
# multiply by 0.25 to tun into km2
agri_area_km2<- agri_count_tot *0.25  # 3195km2


# (D) Calulate how much area (in km2) has to be hegderows for deltaplan (5%)
hederows_km2_required <- (agri_area_km2*5)/100 # 159.75 km2  with a 5% target
hederows_km2_required_fulltarget <- (agri_area_km2*10)/100  # 319.5 km2

 
# (E) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# Fisr I extract by mask the hedgerows density map wirth the solution map
sol1_density  <- extractByMask(hedges, msk=sol1, spatial = TRUE )
# Then I can apply the ame steps as described in (B)
sol1_adimentional <- sol1_density*0.0025
sol1_surface <- sol1_adimentional*(500*500)
sol1_km2 <- sol1_surface/ 1000000
tot_sol1_km2 <- cellStats(sol1_km2, "sum", na.rm= T) # 10.77 km2


# (F) calculate proportions
 #    (1) Proportion of hedgerows secured in solution vs hedgerows required
HedgeSolution_fordeltaplan <- (tot_sol1_km2*100)/hederows_km2_required          # 6.7%
 #    (2) Proportion of hedgerows secures ccompared to the extent of agric land
hedgesSol1_inagriland <- (tot_sol1_km2*100)/agri_area_km2                       # 0.33 %
 #    (3) Proportion of existing  hedgerows  ccompared to the extent of agric land
hedgesTot_inagriland <- (tot_hedge_km2*100)/agri_area_km2                       # 1.3 %
 #    (3) Proportion of hedgerows secured over the total amount of hedgerows
hegdeNetwork_conserved <- (tot_sol1_km2*100)/tot_hedge_km2                      # 25.7% 
 #    (4) Hedgerows required for deltaplan vs existing hedges 
hedges_restoration <-  (hederows_km2_required*100)/tot_hedge_km2                # 381.3 %


# (G) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# First I extract by mask the hedgerows density map wirth the solution map
sol14_density  <- extractByMask(hedges, msk=sol14, spatial = TRUE )
# Then I can apply the ame steps as described in (B)
sol14_adimentional <- sol14_density*0.0025
sol14_surface <- sol14_adimentional*(500*500)
sol14_km2 <- sol14_surface/ 1000000
tot_sol14_km2 <- cellStats(sol14_km2, "sum", na.rm= T) # 12.6 km2


# (H) calculate proportions
#    (1) Proportion of hedgerows secured in solution vs hedgerows required
HedgeSolution_fordeltaplan14 <- (tot_sol14_km2*100)/hederows_km2_required       # 17.2%
#    (2) Proportion of hedgerows secures ccompared to the extent of agric land
hedgesSol14_inagriland<- (tot_sol14_km2*100)/agri_area_km2                      # 0.4 %
#    (3) Proportion of hedgerows secured over the total amount of hedgerows
hegdeNetwork_conserved_14 <- (tot_sol14_km2*100)/tot_hedge_km2                  # 30.2% 


# (I) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# Fisr I extract by mask the hedgerows density map wirth the solution map
sol1Scen2_density  <- extractByMask(hedges, msk=sol1Scen2, spatial = TRUE )
# Then I can apply the ame steps as described in (B)
sol1Scen2_adimentional <- sol1Scen2_density*0.0025
sol1Scen2_surface <- sol1Scen2_adimentional*(500*500)
sol1Scen2_km2 <- sol1Scen2_surface/ 1000000
tot_sol1Scen2_km2 <- cellStats(sol1Scen2_km2, "sum", na.rm= T) # 8 km2 


# (L) calculate proportions
#    (1) Proportion of hedgerows secured in solution vs hedgerows required
HedgeSolution_fordeltaplan1Scen2 <- (tot_sol1Scen2_km2*100)/hederows_km2_required  # 5 %
#    (2) Proportion of hedgerows secures ccompared to the extent of agric land
hedgesSol1Scen2_inagriland<- (tot_sol1Scen2_km2*100)/agri_area_km2                # 0.25 %
#    (3) Proportion of hedgerows secured over the total amount of hedgerows
hegdeNetwork_conserved_1Scen2 <- (tot_sol1Scen2_km2*100)/tot_hedge_km2            # 19.1% 


# (M) Calculate the total area occupied by hedgerows in solution (assuming 2.5W)
# First I extract by mask the hedgerows density map wirth the solution map
sol14Scen2_density  <- extractByMask(hedges, msk=sol14Scen2, spatial = TRUE )
# Then I can apply the ame steps as described in (B)
sol14Scen2_adimentional <- sol14Scen2_density*0.0025
sol14Scen2_surface <- sol14Scen2_adimentional*(500*500)
sol14Scen2_km2 <- sol14Scen2_surface/ 1000000
tot_sol14Scen2_km2 <- cellStats(sol14Scen2_km2, "sum", na.rm= T) # 13.3 km2 


# (N) calculate proportions
#    (1) Proportion of hedgerows secured in solution vs hedgerows required
HedgeSolution_fordeltaplan14Scen2 <- (tot_sol14Scen2_km2*100)/hederows_km2_required  # 8.3 %
#    (2) Proportion of hedgerows secures ccompared to the extent of agric land
hedgesSol14Scen2_inagriland<- (tot_sol14Scen2_km2*100)/agri_area_km2            # 0.41 %
#    (3) Proportion of hedgerows secured over the total amount of hedgerows
hegdeNetwork_conserved_14Scen2 <- (tot_sol14Scen2_km2*100)/tot_hedge_km2        # 31.7% 
