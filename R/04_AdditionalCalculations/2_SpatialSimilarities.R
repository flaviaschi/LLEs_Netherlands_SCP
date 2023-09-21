#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# The following script contains codes for calculating differences between 
# selected solutions
#===============================================================================


# Load in soludions Sceanrio 1 and 2
S1_Sol13<- raster("Outcome/Scenario1/Solution/Solution13.tif")
S1_Sol14<- raster("Outcome/Scenario1/Solution/Solution14.tif")


S2_Sol15<- raster("Outcome/Scenario2/Solution/S2_Solution15.tif")
S2_Sol14<- raster("Outcome/Scenario2/Solution/S2_Solution14.tif")


# COMPARISONS 
ss1 <- S1_Sol13 + S2_Sol14    
plot(ss1)

ss1[ss1 == 2 ] <- NA
ss1[ss1 == 0 ] <- NA
count_ss1 <- cellStats(ss1, 'sum', na.rm= TRUE) #501

plot(ss1)

# -----
ss2 <- S1_Sol13 + S2_Sol15    
plot(ss2)

ss2[ss2 == 2 ] <- NA
ss2[ss2 == 0 ] <- NA
count_ss2 <- cellStats(ss2, 'sum', na.rm= TRUE) # 566

plot(ss2)

# -----

ss3 <- S1_Sol14 + S2_Sol14    
plot(ss3)

ss3[ss3 == 2 ] <- NA
ss3[ss3 == 0 ] <- NA
count_ss3 <- cellStats(ss3, 'sum', na.rm= TRUE) # 490

plot(ss3)

# -----
ss4 <- S1_Sol14 + S2_Sol15    
plot(ss4)

ss4[ss4 == 2 ] <- NA
ss4[ss4 == 0 ] <- NA 
count_ss4 <- cellStats(ss4, 'sum', na.rm= TRUE) # 523

plot(ss4)

## In the end both Solutions with S2_Sol14 are fine (ss1 or ss3)



# Load in soludions Sceanrio 1 and 2
S1_Sol1<- raster("Outcome/Scenario1/Solution/Solution1.tif")
S1_Sol13<- raster("Outcome/Scenario1/Solution/Solution13.tif")


S2_Sol1<- raster("Outcome/Scenario2/Solution/S2_Solution1.tif")
S2_Sol14<- raster("Outcome/Scenario2/Solution/S2_Solution14.tif")

hedges<-raster("Data/CleanData/Hedgerows/Hedge_density_Agriculture_NB.tif")


#count number of cells - land sharing
S1_Sol1[S1_Sol1 == NA] <- 0
Count_S1_Sol1 <- cellStats(S1_Sol1, 'sum', na.rm= TRUE) #2427

S2_Sol1[S2_Sol1 == NA] <- 0
Count_S2_Sol1 <- cellStats(S2_Sol1, 'sum', na.rm= TRUE) #2721

2427 - 2721

# count number of cells - land sparing
S1_Sol13[S1_Sol13 == NA] <- 0
Count_S1_Sol13 <- cellStats(S1_Sol13, 'sum', na.rm= TRUE) #2778

S2_Sol14[S2_Sol14 == NA] <- 0
Count_S2_Sol14 <- cellStats(S2_Sol14, 'sum', na.rm= TRUE) #2911

2778 - 2911

# count cells hedges
hedges[hedges == NA] <- 0
count_hedges<- cellStats(hedges, "sum", na.rm= TRUE). #43.964,99
