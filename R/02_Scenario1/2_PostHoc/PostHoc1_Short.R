#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Scenario 1 (Maximize ES Scenario) : POST-HOC
# ------------------------------------------------------------------------------
# The following script contains codes for calculating :
# 1. The € saved as ESS in the solutions (1 and 13)
# 2. Th e€ saved as C. sequestration in the solutions (1 and 13)
# 3. The € saved as Pollination in the solutions (1 and 13)
# 4. the total € spent for securing the solutions 
# 5. Ratio cost/benefit: € spent vs € saved 
# 6. Map of Ratio
#===============================================================================
library(bnspatial)
library(raster)

#===============================================================================
# LOAD DATA
#===============================================================================

# A) ESS values: Carbon and Pollination monetary values - "straight"
ESS_costs<- raster("Data/CleanData/Costs/ESS/ESS.tif")
plot(ESS_costs,main = " Carbon and Pollinaton costs")

Cseq<-raster("Data/CleanData/Costs/ESS/cseqestration_straight.tif")
Poll<-raster("Data/CleanData/Costs/ESS/pollation_straight.tif")

# B) NORMAL COSTS: Opportunity costs and management costs
expenses<- raster("Data/CleanData/Costs/Normal_costs/Costs.tif")
plot(expenses, main = " Expenses based on hedge density")

# C) SOLUTIONS 
solution1 <- raster("Outcome/Scenario1/Solution/Solution1.tif")
solution13 <- raster("Outcome/Scenario1/Solution/Solution13.tif")

#===============================================================================
# 1. MONETARY VALUES OF ES SAVED
#===============================================================================

# Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
#also where the cells = 0
solution1[solution1 == 0] <- NA
solution13[solution13 == 0] <- NA

# solution1
ESS_savedSol1 <- extractByMask(layer = ESS_costs, msk = solution1, spatial = TRUE)
plot(ESS_savedSol1)
ESS_saved_sum_Sol1<-cellStats(ESS_savedSol1, 'sum', na.rm= TRUE)
a1 <- cellStats(ESS_savedSol1, stat='mean', na.rm=TRUE)
a2 <- cellStats(ESS_savedSol1, stat='sd', na.rm=TRUE)

# solution14
ESS_savedSol13<- extractByMask(layer = ESS_costs, msk = solution13, spatial = TRUE)
plot(ESS_savedSol13)
ESS_saved_sum_Sol13<-cellStats(ESS_savedSol13, 'sum', na.rm= TRUE)
q1 <- cellStats(ESS_savedSol13, stat='mean', na.rm=TRUE)
q2 <- cellStats(ESS_savedSol13, stat='sd', na.rm=TRUE)

#===============================================================================
# 2. CALCULATE EUROS SAVED WITH C. sequestration in solutions
#===============================================================================

# Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
#also where the cells = 0
solution1[solution1 == 0] <- NA
solution13[solution13 == 0] <- NA

# solution1
Cseq_savedSol1 <- extractByMask(layer = Cseq, msk = solution1, spatial = TRUE)
plot(Cseq_savedSol1)
Cseq_saved_sum_Sol1<-cellStats(Cseq_savedSol1, 'sum', na.rm= TRUE)

# solution13
Cseq_savedSol13<- extractByMask(layer = Cseq, msk = solution13, spatial = TRUE)
plot(Cseq_savedSol13)
Cseq_saved_sum_Sol13<-cellStats(Cseq_savedSol13, 'sum', na.rm= TRUE)

#===============================================================================
# 3. CALCULATE EUROS SAVED WITH Pollination in solutions
#===============================================================================
# Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
#also where the cells = 0
solution1[solution1 == 0] <- NA
solution13[solution13 == 0] <- NA

# solution1
Poll_savedSol1 <- extractByMask(layer = Poll, msk = solution1, spatial = TRUE)
plot(Poll_savedSol1)
Poll_saved_sum_Sol1<-cellStats(Poll_savedSol1, 'sum', na.rm= TRUE)

# solution14
Poll_savedSol13<- extractByMask(layer = Poll, msk = solution13, spatial = TRUE)
plot(Poll_savedSol13)
Poll_saved_sum_Sol13<-cellStats(Poll_savedSol13, 'sum', na.rm= TRUE)

#===============================================================================
# 4. MONEY SPENT
#===============================================================================

# solution1
euros_spent_Sol1 <- extractByMask(layer = expenses, msk = solution1, spatial = TRUE)
euros_spent_sum_Sol1<-cellStats(euros_spent_Sol1, 'sum')

# solution14
euros_spent_Sol13 <- extractByMask(layer = expenses, msk = solution13, spatial = TRUE)
euros_spent_sum_Sol13<-cellStats(euros_spent_Sol13, 'sum')


#===============================================================================
# 5. PROPORTION
#===============================================================================

prop1 <- ESS_saved_sum_Sol1 /euros_spent_sum_Sol1
prop13 <- ESS_saved_sum_Sol13 /euros_spent_sum_Sol13

#===============================================================================
# 6. Mapping the ratio cost/benefit
#===============================================================================

ratio1 <- ESS_savedSol1/euros_spent_Sol1
plot(ratio1)
ratio1[ratio1 != 0] <- 1
plot(ratio1)

ratio14 <- ESS_savedSol13/euros_spent_Sol13
plot(ratio13)
ratio13[ratio13 != 0] <- 1
plot(ratio14)


