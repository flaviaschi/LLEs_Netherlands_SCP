#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Sensitivity analysis -  Create function for calculating species targets for 
# scenario 2 (Minimize Costs Scenario)
#===============================================================================


#=========================================================================================================================================
# Species 35quantile
#=========================================================================================================================================

# Calculate species' specific targets 

speciestargets <- function(lb, hb) {
  
  spp_target_percentage <-
    loglinear_interpolation(
      x = species_areas,
      coordinate_one_x = lb,
      coordinate_one_y = 1,
      coordinate_two_x = hb,
      coordinate_two_y = 0.1)
  # Create data frame
  species_targets <- as.data.frame(spp_target_percentage)
  colnames(species_targets)<-"percentage_target"
  # Create a list with names 
  sp_list<-as.data.frame(sp_35quantile_names)
  colnames(sp_list) <- "Species"
  # Bind names wirth targets
  species_targets <- cbind(sp_list, species_targets)
  
  #=========================================================================================================================================
  # Species 10quantile
  #=========================================================================================================================================
  
  # Find target
  spp_target_percentage1 <-
    loglinear_interpolation(
      x = species_areas1,
      coordinate_one_x = lb,
      coordinate_one_y = 1,
      coordinate_two_x = hb,
      coordinate_two_y = 0.1) 
  
  # Create data frame
  species_targets1<-as.data.frame(spp_target_percentage1)
  colnames(species_targets1)<-"percentage_target"
  
  # Create a list of species' names
  sp_list1<-as.data.frame(sp_10quantile_names)
  colnames(sp_list1) <-"Species"
  
  # Bind targets and sp names 
  species_targets1<-cbind(sp_list1, species_targets1)
  
  # Bind the two species targets DF
  targets <- rbind(species_targets, species_targets1)
  
  # Sort alphabetically by sp names 
  targets <- targets[order(targets$Species),]
  
  # Save species names as a list 
  sp_list <- targets$Species
  
  
  #Bind together targtes and current coverage
  coverage_targets <- cbind(cover, targets)
  coverage_targets <-  subset(coverage_targets, select = -c(sp_list) )
  
  # Calculate remainig target - here I assign 0.1 target for species that end up with targets lowew than 0
  needed_sp_target <- apply(coverage_targets, 1, function(x) max(c(0.1, (as.numeric(x["percentage_target"]) - as.numeric(x["cover"]))), na.rm = TRUE))
  
  # Replace NAN with 1 (100% target)
  needed_sp_target <- tidyr::replace_na(data =  needed_sp_target, replace = 1) 
  
  #Covert into data frame
  needed_sp_target<- as.numeric(needed_sp_target) 
  needed_sp_target <- as.data.frame(needed_sp_target)
  
  #Assign species name to each value
  sp_list <- as.data.frame(sp_list)
  needed_sp_target <- cbind(needed_sp_target, sp_list)
  
  #Save csv
  write.csv(needed_sp_target, paste0("Outcomes/targets/Scenario2/", "Scen2_Target_LB", lb, "_HB", hb, ".csv"))
  needed_sp_target$needed_sp_target
  
}



