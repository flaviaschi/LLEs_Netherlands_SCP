#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Scenario 2 - (Minimize Costs Scenario) : POST_HOC
# -----------------------------------------------------------------------------
# The following script contains codes for calculating :
# 1. The € saved as ESS in the solution
# 2. the total € spent for securing the solutions (all of them)
# 3. Graphs that describe the results
# (the script is dependent on the script of the conservation problem)
#===============================================================================
library(bnspatial)
library(raster)

#===============================================================================
# 1) LOAD DATA
#===============================================================================

# A) ESS values: Carbon and Pollination monetary values - "straight"
ESS_costs<- raster("Data/CleanData/Costs/ESS/ESS.tif")
plot(ESS_costs,main = " Carbon and Pollinaton costs")

# B) NORMAL COSTS: Opportunity costs and management costs
expenses<- raster("Data/CleanData/Costs/Normal_costs/Costs.tif")
plot(expenses, main = " Expenses based on hedge density")

#===============================================================================
# 2) CALCULATE EUROS SAVED WITH ESS in solutions
#===============================================================================

# Turn 0s into NA: Because otherwise later, when I use extract by mask it extract
#also where the cells = 0 
solution1[solution1 == 0] <- NA 
solution2[solution2 == 0] <- NA   
solution3[solution3 == 0] <- NA   
solution4[solution4 == 0] <- NA   
solution5[solution5 == 0] <- NA   
solution6[solution6 == 0] <- NA   
solution7[solution7 == 0] <- NA   
solution8[solution8 == 0] <- NA   
solution9[solution9 == 0] <- NA   
solution10[solution10 == 0] <- NA   
solution11[solution11 == 0] <- NA   
solution12[solution12 == 0] <- NA  
solution13[solution13 == 0] <- NA  
solution14[solution14 == 0] <- NA  
solution15[solution15 == 0] <- NA  
solution16[solution16 == 0] <- NA  

# solution1
ESS_savedSol1 <- extractByMask(layer = ESS_costs, msk = solution1, spatial = TRUE)
plot(ESS_savedSol1)
ESS_saved_sum_Sol1<-cellStats(ESS_savedSol1, 'sum', na.rm= TRUE) 
a1 <- cellStats(ESS_savedSol1, stat='mean', na.rm=TRUE)
a2 <- cellStats(ESS_savedSol1, stat='sd', na.rm=TRUE)

# solution2
ESS_savedSol2 <- extractByMask(layer = ESS_costs, msk = solution2, spatial = TRUE)
plot(ESS_savedSol2)
ESS_saved_sum_Sol2<-cellStats(ESS_savedSol2, 'sum', na.rm= TRUE) 
b1 <- cellStats(ESS_savedSol2, stat='mean', na.rm=TRUE)
b2 <- cellStats(ESS_savedSol2, stat='sd', na.rm=TRUE)

# solution3
ESS_savedSol3 <- extractByMask(layer = ESS_costs, msk = solution3, spatial = TRUE)
plot(ESS_savedSol3)
ESS_saved_sum_Sol3<-cellStats(ESS_savedSol3, 'sum', na.rm= TRUE)  
c1 <- cellStats(ESS_savedSol3, stat='mean', na.rm=TRUE)
c2 <- cellStats(ESS_savedSol3, stat='sd', na.rm=TRUE)

# solution4
ESS_savedSol4 <- extractByMask(layer = ESS_costs, msk = solution4, spatial = TRUE)
plot(ESS_savedSol4)
ESS_saved_sum_Sol4<-cellStats(ESS_savedSol4, 'sum', na.rm= TRUE)  
d1 <- cellStats(ESS_savedSol4, stat='mean', na.rm=TRUE)
d2 <- cellStats(ESS_savedSol4, stat='sd', na.rm=TRUE)

# solution5
ESS_savedSol5 <- extractByMask(layer = ESS_costs, msk = solution5, spatial = TRUE)
plot(ESS_savedSol5)
ESS_saved_sum_Sol5<-cellStats(ESS_savedSol5, 'sum', na.rm= TRUE) 
e1 <- cellStats(ESS_savedSol5, stat='mean', na.rm=TRUE)
e2 <- cellStats(ESS_savedSol5, stat='sd', na.rm=TRUE)

# solution6
ESS_savedSol6 <- extractByMask(layer = ESS_costs, msk = solution6, spatial = TRUE)
plot(ESS_savedSol6)
ESS_saved_sum_Sol6<-cellStats(ESS_savedSol6, 'sum', na.rm= TRUE)  
f1 <- cellStats(ESS_savedSol6, stat='mean', na.rm=TRUE)
f2 <- cellStats(ESS_savedSol6, stat='sd', na.rm=TRUE)

# solution7
ESS_savedSol7 <- extractByMask(layer = ESS_costs, msk = solution7, spatial = TRUE)
plot(ESS_savedSol7)
ESS_saved_sum_Sol7<-cellStats(ESS_savedSol7, 'sum', na.rm= TRUE)  
g1 <- cellStats(ESS_savedSol7, stat='mean', na.rm=TRUE)
g2 <- cellStats(ESS_savedSol7, stat='sd', na.rm=TRUE)

# solution8
ESS_savedSol8 <- extractByMask(layer = ESS_costs, msk = solution8, spatial = TRUE)
plot(ESS_savedSol8)
ESS_saved_sum_Sol8<-cellStats(ESS_savedSol8, 'sum', na.rm= TRUE)  
h1 <- cellStats(ESS_savedSol8, stat='mean', na.rm=TRUE)
h2 <- cellStats(ESS_savedSol8, stat='sd', na.rm=TRUE)

# Solution 9 
ESS_savedSol9 <- extractByMask(layer = ESS_costs, msk = solution9, spatial = TRUE)
plot(ESS_savedSol9)
ESS_saved_sum_Sol9<-cellStats(ESS_savedSol9, 'sum', na.rm= TRUE)  
i1 <- cellStats(ESS_savedSol9, stat='mean', na.rm=TRUE)
i2 <- cellStats(ESS_savedSol9, stat='sd', na.rm=TRUE)

# solution 10
ESS_savedSol10 <- extractByMask(layer = ESS_costs, msk = solution10, spatial = TRUE)
plot(ESS_savedSol10)
ESS_saved_sum_Sol10<-cellStats(ESS_savedSol10, 'sum', na.rm= TRUE)  
l1 <- cellStats(ESS_savedSol10, stat='mean', na.rm=TRUE)
l2 <- cellStats(ESS_savedSol10, stat='sd', na.rm=TRUE)

# solution 11
ESS_savedSol11 <- extractByMask(layer = ESS_costs, msk = solution11, spatial = TRUE)
plot(ESS_savedSol11)
ESS_saved_sum_Sol11<-cellStats(ESS_savedSol11, 'sum', na.rm= TRUE) 
m1 <- cellStats(ESS_savedSol11, stat='mean', na.rm=TRUE)
m2 <- cellStats(ESS_savedSol11, stat='sd', na.rm=TRUE)

# solution 12
ESS_savedSol12 <- extractByMask(layer = ESS_costs, msk = solution12, spatial = TRUE)
plot(ESS_savedSol12)
ESS_saved_sum_Sol12<-cellStats(ESS_savedSol12, 'sum', na.rm= TRUE)  
o1 <- cellStats(ESS_savedSol12, stat='mean', na.rm=TRUE)
o2 <- cellStats(ESS_savedSol12, stat='sd', na.rm=TRUE)

# solution 13
ESS_savedSol13 <- extractByMask(layer = ESS_costs, msk = solution13, spatial = TRUE)
plot(ESS_savedSol13)
ESS_saved_sum_Sol13<-cellStats(ESS_savedSol13, 'sum', na.rm= TRUE)  
q1 <- cellStats(ESS_savedSol13, stat='mean', na.rm=TRUE)
q2 <- cellStats(ESS_savedSol13, stat='sd', na.rm=TRUE)

# solution 14
ESS_savedSol14<- extractByMask(layer = ESS_costs, msk = solution14, spatial = TRUE)
plot(ESS_savedSol14)
ESS_saved_sum_Sol14<-cellStats(ESS_savedSol14, 'sum', na.rm= TRUE)  
r1 <- cellStats(ESS_savedSol14, stat='mean', na.rm=TRUE)
r2 <- cellStats(ESS_savedSol14, stat='sd', na.rm=TRUE)

# solution 15
ESS_savedSol15<- extractByMask(layer = ESS_costs, msk = solution15, spatial = TRUE)
plot(ESS_savedSol15)
ESS_saved_sum_Sol15<-cellStats(ESS_savedSol15, 'sum', na.rm= TRUE)  
t1 <- cellStats(ESS_savedSol15, stat='mean', na.rm=TRUE)
t2 <- cellStats(ESS_savedSol15, stat='sd', na.rm=TRUE)

# solution 16
ESS_savedSol16<- extractByMask(layer = ESS_costs, msk = solution16, spatial = TRUE)
plot(ESS_savedSol16)
ESS_saved_sum_Sol16<-cellStats(ESS_savedSol16, 'sum', na.rm= TRUE)  
u1 <- cellStats(ESS_savedSol16, stat='mean', na.rm=TRUE)
u2 <- cellStats(ESS_savedSol16, stat='sd', na.rm=TRUE)

# Data frame - raster statistics
raster_statistic <-  data.frame(mean_cells = c(a1,b1,c1,d1,e1,f1,g1,h1, i1,l1,
                                               m1, o1, q1, r1, t1 ),
                                sd= c(a2,b2,c2,d2,e2,f2,g2,h2,i2,l2,m2, o2, q2,
                                      r2,t2))
#Import Pu_stat
pu_stat<- read.csv2("Outcome/Scenario2/pu_stat_scen2_lowpen.csv")

# Bind data frame with PU stat
raster_stat <- cbind(pu_stat, raster_statistic)


library(ggplot2)
# (a) Plot solution statistics (mean ESS per cell against the number of PU)
raster_stat %>%
  ggplot(aes(x=mean_cells, y= pu_cells)) +
  geom_point() +
  geom_point(data=raster_stat[1, ], aes(x=mean_cells, y=pu_cells), 
             colour="darkgreen", size=4) +
  geom_point(data=raster_stat[14, ], aes(x=mean_cells, y=pu_cells),
             colour="darkred", size=4) +
  geom_point(data=raster_stat[15, ], aes(x=mean_cells, y=pu_cells), 
             colour="darkblue", size=4) +
  theme_minimal()+
  labs(title = "Number of planning units VS mean ESS cell  value") 

#===============================================================================
# MONEY SPENT
#===============================================================================

# solution1
euros_spent_Sol1 <- extractByMask(layer = expenses, msk = solution1, spatial = TRUE)
euros_spent_sum_Sol1<-cellStats(euros_spent_Sol1, 'sum')  
# Alternative way for calculating costs - built- in function of prioritizr
r1 <- eval_cost_summary(p1, s1[, "solution_1"])

# Solution 2 
euros_spent_Sol2 <- extractByMask(layer = expenses, msk = solution2, spatial = TRUE)
euros_spent_sum_Sol2<-cellStats(euros_spent_Sol2, 'sum')  

r2 <- eval_cost_summary(p2, s2[, "solution_1"])

# solution3
euros_spent_Sol3 <- extractByMask(layer = expenses, msk = solution3, spatial = TRUE)
euros_spent_sum_Sol3<-cellStats(euros_spent_Sol3, 'sum')  

r3 <- eval_cost_summary(p3, s3[, "solution_1"])

# solution4
euros_spent_Sol4 <- extractByMask(layer = expenses, msk = solution4, spatial = TRUE)
euros_spent_sum_Sol4<-cellStats(euros_spent_Sol4, 'sum')  

r4 <- eval_cost_summary(p4, s4[, "solution_1"])

# solution5
euros_spent_Sol5 <- extractByMask(layer = expenses, msk = solution5, spatial = TRUE)
euros_spent_sum_Sol5<-cellStats(euros_spent_Sol5, 'sum')  

r5 <- eval_cost_summary(p5, s5[, "solution_1"])

# solution6
euros_spent_Sol6 <- extractByMask(layer = expenses, msk = solution6, spatial = TRUE)
euros_spent_sum_Sol6<-cellStats(euros_spent_Sol6, 'sum')  

r1 <- eval_cost_summary(p6, s6[, "solution_1"])

# solution7
euros_spent_Sol7 <- extractByMask(layer = expenses, msk = solution7, spatial = TRUE)
euros_spent_sum_Sol7<-cellStats(euros_spent_Sol7, 'sum') 

r7 <- eval_cost_summary(p7, s7[, "solution_1"])

# solution8
euros_spent_Sol8 <- extractByMask(layer = expenses, msk = solution8, spatial = TRUE)
euros_spent_sum_Sol8<-cellStats(euros_spent_Sol8, 'sum') 

r8 <- eval_cost_summary(p8, s8[, "solution_1"])

# solution9
euros_spent_Sol9 <- extractByMask(layer = expenses, msk = solution9, spatial = TRUE)
euros_spent_sum_Sol9<-cellStats(euros_spent_Sol9, 'sum') 

r9 <- eval_cost_summary(p9, s9[, "solution_1"])

# solution10
euros_spent_Sol10 <- extractByMask(layer = expenses, msk = solution10, spatial = TRUE)
euros_spent_sum_Sol10<-cellStats(euros_spent_Sol10, 'sum')  

r10 <- eval_cost_summary(p10, s10[, "solution_1"])

# solution11
euros_spent_Sol11 <- extractByMask(layer = expenses, msk = solution11, spatial = TRUE)
euros_spent_sum_Sol11<-cellStats(euros_spent_Sol11, 'sum')  

r11 <- eval_cost_summary(p11, s11[, "solution_1"])

# solution12
euros_spent_Sol12 <- extractByMask(layer = expenses, msk = solution12, spatial = TRUE)
euros_spent_sum_Sol12<-cellStats(euros_spent_Sol12, 'sum') 

r12<- eval_cost_summary(p12, s12[, "solution_1"])

# solution13
euros_spent_Sol13 <- extractByMask(layer = expenses, msk = solution13, spatial = TRUE)
euros_spent_sum_Sol13<-cellStats(euros_spent_Sol13, 'sum')  

r13 <- eval_cost_summary(p13, s13[, "solution_1"])

## solution14
euros_spent_Sol14 <- extractByMask(layer = expenses, msk = solution14, spatial = TRUE)
euros_spent_sum_Sol14<-cellStats(euros_spent_Sol14, 'sum')  

r14 <- eval_cost_summary(p14, s14[, "solution_1"])

# solution15
euros_spent_Sol15 <- extractByMask(layer = expenses, msk = solution15, spatial = TRUE)
euros_spent_sum_Sol15<-cellStats(euros_spent_Sol15, 'sum')  

r15 <- eval_cost_summary(p15, s15[, "solution_1"])

# solution16
euros_spent_Sol16 <- extractByMask(layer = expenses, msk = solution16, spatial = TRUE)
euros_spent_sum_Sol16<-cellStats(euros_spent_Sol16, 'sum')  

r16 <- eval_cost_summary(p16, s16[, "solution_1"])


#===============================================================================
# Plots
#===============================================================================

library(ggplot2)
library(dplyr)
library(gridExtra)

# Select 2 solution based on boundary penalty, costs and € saved
# Set up data frame ESS saved VS € Spent
df <- data.frame(ESS_saved = c(ESS_saved_sum_Sol1, ESS_saved_sum_Sol2,
                               ESS_saved_sum_Sol3, ESS_saved_sum_Sol4,
                               ESS_saved_sum_Sol5,
                               ESS_saved_sum_Sol6,  ESS_saved_sum_Sol7, 
                               ESS_saved_sum_Sol8, ESS_saved_sum_Sol9,
                               ESS_saved_sum_Sol10, ESS_saved_sum_Sol11,
                               ESS_saved_sum_Sol12,ESS_saved_sum_Sol13,
                               ESS_saved_sum_Sol14,ESS_saved_sum_Sol15
                               ),
                 expenses = c(euros_spent_sum_Sol1, euros_spent_sum_Sol2,
                              euros_spent_sum_Sol3, euros_spent_sum_Sol4,
                              euros_spent_sum_Sol5, euros_spent_sum_Sol6,
                              euros_spent_sum_Sol7, euros_spent_sum_Sol8,
                              euros_spent_sum_Sol9, euros_spent_sum_Sol10, 
                              euros_spent_sum_Sol11, euros_spent_sum_Sol12,
                              euros_spent_sum_Sol13, euros_spent_sum_Sol14,
                              euros_spent_sum_Sol15))

# (b) Make scatter plot of cosys vs ESS saved
df %>%
  ggplot (aes(x=expenses, y= ESS_saved)) +
  geom_point()+
  labs(title = " EES saved VS Expenses") +
  geom_point(data=df[4, ], aes(x=expenses, y=ESS_saved), colour="blue", size=3) +
 geom_point(data=df[1, ], aes(x=expenses, y=ESS_saved), colour="blue", size=3) +
  geom_point(data=df[15, ], aes(x=expenses, y=ESS_saved), colour="blue", size=3) 




# Scatrterplot and liune plot to find the threshold for boundary penalty
df1 <- data.frame(expenses = c(euros_spent_sum_Sol1, euros_spent_sum_Sol2,
                               euros_spent_sum_Sol3,euros_spent_sum_Sol4,
                               euros_spent_sum_Sol5, euros_spent_sum_Sol6,
                               euros_spent_sum_Sol7),
                  boundary_penalty = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1))


# (b) Plot BP vs Costs 
df1 %>%
  ggplot (aes(x=boundary_penalty, y= expenses)) +
  geom_line( color= "grey")+
  geom_point() +
  geom_point(data=df1[4, ], aes(x=boundary_penalty, y=expenses), colour="red", size=3)+
  theme_minimal() +
  labs(title = "Scenario 2 - Increase of the cost due to the increase in boundary penalty")


# DF with a subset of data
dff1 <- data.frame(expenses = c(euros_spent_sum_Sol1, euros_spent_sum_Sol2,
                                euros_spent_sum_Sol3, euros_spent_sum_Sol4,
                                euros_spent_sum_Sol5, euros_spent_sum_Sol6,
                                euros_spent_sum_Sol7, euros_spent_sum_Sol8,
                                euros_spent_sum_Sol9, euros_spent_sum_Sol10, 
                                euros_spent_sum_Sol11, euros_spent_sum_Sol12,
                                euros_spent_sum_Sol13, euros_spent_sum_Sol14,
                                euros_spent_sum_Sol15, euros_spent_sum_Sol16),
                  boundary_penalty = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2,
                                       2.5, 3, 4, 5, 10, 20, 25))


# (c) Plot BP vs Costs
dff1 %>%
  ggplot (aes(x=boundary_penalty, y= expenses)) +
  geom_line( color= "grey")+
  geom_point() +
  geom_point(data=dff1[1, ], aes(x=boundary_penalty, y=expenses), colour="darkgreen", size=1)+
  geom_point(data=dff1[4, ], aes(x=boundary_penalty, y=expenses), colour="darkred", size=1)+
  geom_point(data=dff1[15, ], aes(x=boundary_penalty, y=expenses), colour="darkblue", size=1)+
  theme_minimal() +
  labs(title = "Scenario 2 - Increase of the cost due to the increase in boundary penalty")


#  DF to find the threshold for boundary penalty
df2 <- data.frame(ESS_saved = c(ESS_saved_sum_Sol1, ESS_saved_sum_Sol2,
                                ESS_saved_sum_Sol3, ESS_saved_sum_Sol4,
                                ESS_saved_sum_Sol5,
                                ESS_saved_sum_Sol6, ESS_saved_sum_Sol7),
                  boundary_penalty = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1))


# (d) Plot BP vs ESS
df2 %>%
  ggplot (aes(x=boundary_penalty, y= ESS_saved)) +
  geom_line( color= "grey")+
  geom_point() +
  geom_point(data=df2[1, ], aes(x=boundary_penalty, y=ESS_saved), colour="darkgreen", size=3) +
  geom_point(data=df2[4, ], aes(x=boundary_penalty, y=ESS_saved), colour="darkred", size=3) +
    theme_minimal() +
  labs(title = "Sceanrio 2 - ESS saved due to the increase in boundary penalty")



# Subset DF to find the threshold for boundary penalty
dff2 <- data.frame(ESS_saved = c(ESS_saved_sum_Sol1, ESS_saved_sum_Sol2,
                                 ESS_saved_sum_Sol3, ESS_saved_sum_Sol4,
                                 ESS_saved_sum_Sol5,
                                 ESS_saved_sum_Sol6,  ESS_saved_sum_Sol7, 
                                 ESS_saved_sum_Sol8, ESS_saved_sum_Sol9,
                                 ESS_saved_sum_Sol10, ESS_saved_sum_Sol11,
                                 ESS_saved_sum_Sol12,ESS_saved_sum_Sol13,
                                 ESS_saved_sum_Sol14,ESS_saved_sum_Sol15,
                                 ESS_saved_sum_Sol16),
                  boundary_penalty = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2,
                                       2.5, 3, 4, 5, 10, 20, 25))

# (e) Plot subset of data
dff2 %>%
  ggplot (aes(x=boundary_penalty, y= ESS_saved)) +
  geom_line( color= "grey")+
  geom_point() +
  geom_point(data=dff2[1, ], aes(x=boundary_penalty, y=ESS_saved), colour="darkgreen", size=3) +
  geom_point(data=dff2[4, ], aes(x=boundary_penalty, y=ESS_saved), colour="darkred", size=3) +
  geom_point(data=dff2[15, ], aes(x=boundary_penalty, y=ESS_saved), colour="darkblue", size=3) +
   theme_minimal() +
  labs(title = "Sceanrio 2 - ESS saved due to the increase in boundary penalty")



# Dataframe solutions statistics statistics
boundary_penalty <- data.frame(boundary_penalty =
                                 c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 10, 20))
# Bind DF together
raster_stata <- cbind(raster_stat, df, boundary_penalty)
raster_stata$ratio <- (raster_stata$ESS_saved / raster_stata$expenses)


# Save as csv
write.csv2(raster_stata, "Outcome/Scenario2/raster_stata.csv")

