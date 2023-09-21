#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Sensitivity analysis -  Fitting Multiple regression analysis 
# and Generalized Additive Model for both Scenario 1 and 2
#===============================================================================
library(dplyr)
library(tidyverse)
library(tidyr)
library(performance)
library(modelbased)
library(parameters)
library(patchwork)
library(gtsummary)


# List files
Out_Scenario1 <-list.files(path= "Outcomes/tables/Scenario1", 
                           pattern='^Out_Scen1', full.names = TRUE)
# File names
file_names <-  gsub(pattern = '^Out_Scen1', replacement = "Scen1",
                    x = basename(Out_Scenario1))
# Read in the files
Out_Scenario1 <- lapply(Out_Scenario1, readRDS)
# Convert list into data frame with 6 columns and 27 rows 
Out_Scenario1 <- as.data.frame(do.call(rbind, Out_Scenario1))  

names <-  as.data.frame(file_names)

# Create new colums for DF
df<- names %>%
  mutate(cost = case_when(
    endsWith(file_names, "costa.RDS") ~ "Both",
    endsWith(file_names, "costb.RDS") ~ "Pollination",
    endsWith(file_names, "costc.RDS") ~ "C.Seq"))  %>%
  mutate (LB =case_when(
    startsWith(file_names, "Scen1_LB100") ~ "100",
    startsWith(file_names, "Scen1_LB136") ~"136",
    startsWith(file_names, "Scen1_LB186") ~ "186",
    startsWith(file_names, "Scen1_LB254") ~ "254",
    startsWith(file_names, "Scen1_LB348") ~ "348",
    startsWith(file_names, "Scen1_LB475") ~ "475",
    startsWith(file_names, "Scen1_LB649") ~ "649",
    startsWith(file_names, "Scen1_LB887") ~ "887",
    startsWith(file_names, "Scen1_LB1212") ~ "1212",
    startsWith(file_names, "Scen1_LB1656") ~ "1656",
    startsWith(file_names, "Scen1_LB2262") ~ "2262",
    startsWith(file_names, "Scen1_LB3090") ~ "3090",
    startsWith(file_names, "Scen1_LB4222") ~ "4222", 
    startsWith(file_names, "Scen1_LB5768") ~ "5768",
    startsWith(file_names, "Scen1_LB_target_HB_0.1_") ~ "0.1",
    startsWith(file_names, "Scen1_LB_target_HB_1_") ~ "1")) %>%
  mutate (UB =case_when(
    str_detect(file_names, "HB7000") ~ "7000",
    str_detect(file_names, "HB7584") ~ "7584",
    str_detect(file_names, "HB8217") ~ "8217",
    str_detect(file_names, "HB8904") ~ "8904",
    str_detect(file_names, "HB9647") ~ "9647",
    str_detect(file_names, "HB10453") ~ "10453",
    str_detect(file_names, "HB11326") ~ "11326",
    str_detect(file_names, "HB12271") ~ "12271",
    str_detect(file_names, "HB13296") ~ "13296",
    str_detect(file_names, "HB14406") ~ "14406",
    str_detect(file_names, "HB15609") ~ "15609",
    str_detect(file_names, "HB16913") ~ "16913",
    str_detect(file_names, "HB18325") ~ "18325",
    str_detect(file_names, "HB19855") ~ "19855",
    str_detect(file_names, "Scen1_LB_target_HB_0.1_") ~ "0.1",
    str_detect(file_names, "Scen1_LB_target_HB_1_") ~ "1")) %>%
  select(-c(file_names))


# Bind the dataframes together
Scenario1 <- cbind(df, Out_Scenario1)

Scenario1 <- cbind(df, Out_Scenario1)%>%
  mutate_at(colnames(Scenario1)[4:7], function(x) x / 10^6) %>%
  mutate(LB = as.numeric(LB),
         UB = as.numeric(UB))
Scenario1 <- Scenario1[-(1:6),]

Scenario1$RatioSol1 <- Scenario1$ESS_saved_sum_Sol1/Scenario1$euros_spent_sum_Sol1
Scenario1$RatioSol13 <- Scenario1$ESS_saved_sum_Sol13/Scenario1$euros_spent_sum_Sol13

# Dataframes for models (ESS)

# Only land sharing

essm1 <- Scenario1 %>%
  select (ESS_saved_sum_Sol1, cost, LB, UB)%>%
  rename(ESS_saved = ESS_saved_sum_Sol1)

costm1<- Scenario1 %>%
  select (euros_spent_sum_Sol1, cost, LB, UB)%>%
  rename(expenses = euros_spent_sum_Sol1)

ratiom1 <- Scenario1 %>%
  select (RatioSol1, cost, LB, UB)%>%
  rename(RatioCostBenefit = RatioSol1)

# Only land sparing
essm2 <- Scenario1 %>%
  select (ESS_saved_sum_Sol13, cost, LB, UB)%>%
  rename(ESS_saved = ESS_saved_sum_Sol13)

costm2 <- Scenario1 %>%
  select (euros_spent_sum_Sol13, cost, LB, UB)%>%
  rename(expenses = euros_spent_sum_Sol13)

ratiom2 <- Scenario1 %>%
  select (RatioSol13, cost, LB, UB)%>%
  rename(RatioCostBenefit = RatioSol13)

# Multiple Linear Regression Model (LM)
m1 <- lm(ESS_saved ~ LB + cost + UB, data = essm1)
m2 <- lm(ESS_saved ~ LB + cost + UB, data = essm2)
m3 <- lm(RatioCostBenefit ~ LB + cost + UB, data = ratiom1)

m4 <- lm(expenses ~ LB + cost + UB, data = costm1)
m5 <- lm(expenses ~ LB + cost + UB, data = costm2)
m6 <- lm(RatioCostBenefit ~ LB + cost + UB, data = ratiom2)


# Generalized Additive Model (GAM)
library(mgcv)

g1 <-mgcv::gam(ESS_saved ~ s(LB, k= 10) + cost + s(UB, k=10), data = essm1)
g2 <- mgcv::gam(ESS_saved ~ s(LB, k= 10) + cost + s(UB, k=10), data = essm2)
g3 <- mgcv::gam(RatioCostBenefit ~ s(LB, k= 10) + cost + s(UB, k=10), data = ratiom1)

g4 <- mgcv::gam(expenses ~ s(LB, k= 10) + cost + s(UB, k=10), data = costm1)
g5 <- mgcv::gam(expenses ~ s(LB, k= 10) + cost + s(UB, k=10), data = costm2)
g6 <- mgcv::gam(RatioCostBenefit ~ s(LB, k= 10) + cost + s(UB, k=10), data = ratiom2)


# check_model(g1)
# check_model(m1)

# ------------------------------------------------------------------------------
# ESS saved 
# ------------------------------------------------------------------------------

#  make plots with three lines
library(gridExtra)
library(modelbased)
# Land sharing 
p1 <- plot(estimate_relation(g1, data = essm1)) +
  coord_cartesian(ylim = c(0, 33)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land Sharing")+
  xlab("LB (Km2)") +
  ylab(" ES saved (euros) 10^6") +
  theme_minimal() 
theme(legend.position = "none")


# Land sparing 
p2 <- plot(estimate_relation(g2, data = essm2)) + 
  coord_cartesian(ylim = c(0, 33)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land Sparing") +
  xlab("LB (Km2)") +
  ylab("ES saved (euros) 10^6") +
  theme_minimal() +
  theme(legend.position = "none")


# ------------------------------------------------------------------------------
# Expenses 
# ------------------------------------------------------------------------------
# Land sharing 

p3<- plot(estimate_relation(g4, data = costm1)) + 
  coord_cartesian(ylim = c(0, 33)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land Sharing")+
  xlab("LB (Km2)") +
  theme_minimal() +
  ylab("expenses (euros) 10^6") +
  theme(legend.position = "none")
summary(m4)

# Land sparing 
p4 <- plot(estimate_relation(g5, data = costm2)) +
  coord_cartesian(ylim = c(0, 33)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land Sparing") +
  xlab("LB (Km2)") +
  ylab("expenses (euros) 10^6") +
  theme_minimal()

# put the plots in the same graph
a<- p1 + p3 +p2 + p4 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'I')

#Save 
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlotScen1.png", plot=a, width=10, height=8)
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlotScen1.svg", plot=a, width=10, height=8)

#-----------------------------------------------------------------------------
# Ratios Cost/Benefits
#-----------------------------------------------------------------------------
ratio_sharing<-plot(estimate_relation(g3, data = ratiom1)) +
  coord_cartesian(ylim = c(0, 4.6)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land Sharing")+
  xlab("LB (Km2)") +
  ylab("Cost/Benefit Ratio") +
  theme_minimal() 
theme(legend.position = "none")


ratio_sparing <- plot(estimate_relation(g6, data = ratiom2)) +
  coord_cartesian(ylim = c(0, 4.6)) +
  scale_color_manual(values=c("#4D430F", "#F2D52E", "#B85480"))+
  labs(title = "Land sparing") +
  xlab("LB (Km2)") +
  # ylab("Cost/Benefit Ratio") +
  theme_minimal() +
  theme(axis.title.y = element_blank())
  theme(legend.position = "none")

# Plot together
b<-ratio_sharing + ratio_sparing +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'I')

#Save 
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Ratio_Scen1.png", plot=b, width=10, height=8)
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Ratio_Scen1.svg", plot=b, width=10, height=8)

#-----------------------------------------------------------------------------
# Regression Tables
#-----------------------------------------------------------------------------

# LM
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)

tbl_regression(m1, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m3, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

tbl_regression(m4, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m5, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m6, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

# GAM
summary(g1)
summary(g2)
summary(g3)

summary(g4)
summary(g5)
summary(g6)


tbl_regression(g1, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g3, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

tbl_regression(g4, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g5, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g6, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)



# ==============================================================================
# Scenario 2
# ==============================================================================


# List files
Out_Scenario2 <-list.files(path= "Outcomes/tables/Scenario2", 
                           pattern='^Out_Scen2', full.names = TRUE)
# File names
file_names <-  gsub(pattern = '^Out_Scen2', replacement = "Scen2",
                    x = basename(Out_Scenario2))
# Read in the files
Out_Scenario2 <- lapply(Out_Scenario2, readRDS)
# Conver list into dataframe with 6 colims and 27 rows 
Out_Scenario2 <- as.data.frame(do.call(rbind, Out_Scenario2))  

names <-  as.data.frame(file_names)

# Create new colums for DF
df2<- names %>%
  mutate(cost = case_when(
    endsWith(file_names, "costa.RDS") ~ "Both",
    endsWith(file_names, "costb.RDS") ~ "Management Cost",
    endsWith(file_names, "costc.RDS") ~ "Opportunity Cost"))  %>%
  mutate (LB =case_when(
    startsWith(file_names, "Scen2_LB100") ~ "100",
    startsWith(file_names, "Scen2_LB136") ~"136",
    startsWith(file_names, "Scen2_LB186") ~ "186",
    startsWith(file_names, "Scen2_LB254") ~ "254",
    startsWith(file_names, "Scen2_LB348") ~ "348",
    startsWith(file_names, "Scen2_LB475") ~ "475",
    startsWith(file_names, "Scen2_LB649") ~ "649",
    startsWith(file_names, "Scen2_LB887") ~ "887",
    startsWith(file_names, "Scen2_LB1212") ~ "1212",
    startsWith(file_names, "Scen2_LB1656") ~ "1656",
    startsWith(file_names, "Scen2_LB2262") ~ "2262",
    startsWith(file_names, "Scen2_LB3090") ~ "3090",
    startsWith(file_names, "Scen2_LB4222") ~ "4222", 
    startsWith(file_names, "Scen2_LB5768") ~ "5768",
    startsWith(file_names, "Scen2_LB_target_HB_0.1_") ~ "0.1",
    startsWith(file_names, "Scen2_LB_target_HB_1_") ~ "1")) %>%
  mutate (UB =case_when(
    str_detect(file_names, "HB7000") ~ "7000",
    str_detect(file_names, "HB7584") ~ "7584",
    str_detect(file_names, "HB8217") ~ "8217",
    str_detect(file_names, "HB8904") ~ "8904",
    str_detect(file_names, "HB9647") ~ "9647",
    str_detect(file_names, "HB10453") ~ "10453",
    str_detect(file_names, "HB11326") ~ "11326",
    str_detect(file_names, "HB12271") ~ "12271",
    str_detect(file_names, "HB13296") ~ "13296",
    str_detect(file_names, "HB14406") ~ "14406",
    str_detect(file_names, "HB15609") ~ "15609",
    str_detect(file_names, "HB16913") ~ "16913",
    str_detect(file_names, "HB18325") ~ "18325",
    str_detect(file_names, "HB19855") ~ "19855",
    str_detect(file_names, "Scen2_LB_target_HB_0.1_") ~ "0.1",
    str_detect(file_names, "Scen2_LB_target_HB_1_") ~ "1")) %>%
  select(-c(file_names))


# Bind the dataframes together
Scenario2 <- cbind(df2, Out_Scenario2)

Scenario2 <- cbind(df2, Out_Scenario2)%>%
  mutate_at(colnames(Scenario2)[4:7], function(x) x / 10^6) %>%
  mutate(LB = as.numeric(LB),
         UB = as.numeric(UB))

Scenario2 <- Scenario2[-(1:6),]

Scenario2$RatioSol1 <- Scenario2$ESS_saved_sum_Sol1/Scenario2$euros_spent_sum_Sol1
Scenario2$RatioSol14 <- Scenario2$ESS_saved_sum_Sol14/Scenario2$euros_spent_sum_Sol14


# Dataframes for models (ESS)

# Only land sharing
ess2m1<- Scenario2 %>%
  select (ESS_saved_sum_Sol1, cost, LB, UB)%>%
  rename(ESS_saved = ESS_saved_sum_Sol1)

cost2m1<- Scenario2 %>%
  select (euros_spent_sum_Sol1, cost, LB, UB)%>%
  rename(expenses = euros_spent_sum_Sol1)

ratio2m1 <- Scenario2 %>%
  select (RatioSol1, cost, LB, UB)%>%
  rename(RatioCostBenefit = RatioSol1)

# Only land sparing
ess2m2 <- Scenario2 %>%
  select (ESS_saved_sum_Sol14, cost, LB, UB)%>%
  rename(ESS_saved = ESS_saved_sum_Sol14)

cost2m2<- Scenario2 %>%
  select (euros_spent_sum_Sol14, cost, LB, UB)%>%
  rename(expenses = euros_spent_sum_Sol14)

ratio2m2 <- Scenario2 %>%
  select (RatioSol14, cost, LB, UB)%>%
  rename(RatioCostBenefit = RatioSol14)

# Multiple linear regression model (lm)
m2.1 <- lm(ESS_saved ~ LB + cost + UB, data = ess2m1)
m2.2 <- lm(ESS_saved ~ LB + cost + UB, data = ess2m2)
m2.3 <- lm(RatioCostBenefit ~ LB + cost + UB, data = ratio2m1)
m2.4 <- lm(expenses ~ LB + cost + UB, data = cost2m1)
m2.5 <- lm(expenses ~ LB + cost + UB, data = cost2m2)
m2.6 <- lm(RatioCostBenefit ~ LB + cost + UB, data = ratio2m2)

# Generalized Additive Model (GAM)
g1.2 <- mgcv::gam(ESS_saved ~ s(LB, k= 10) + cost + s(UB, k= 10), data = ess2m1)
g2.2 <- mgcv::gam(ESS_saved ~ s(LB, k= 10) + cost + s(UB, k= 10), data = ess2m2)
g3.2 <- mgcv::gam(RatioCostBenefit ~ s(LB, k= 10) + cost + s(UB, k= 10), data = ratio2m1)

g4.2 <- mgcv::gam(expenses ~ s(LB, k= 10) + cost + s(UB, k= 10), data = cost2m1)
g5.2 <- mgcv::gam(expenses ~ s(LB, k= 10) + cost + s(UB, k= 10), data = cost2m2)
g6.2 <- mgcv::gam(RatioCostBenefit ~ s(LB, k= 10) + cost + s(UB, k= 10), data = ratio2m2)


# check_model(m1)
# ------------------------------------------------------------------------------
# ESS saved 
# ------------------------------------------------------------------------------

#  make plots with three lines
library(gridExtra)


p5 <- plot(estimate_relation(g1.2, data = ess2m1)) +
  coord_cartesian(ylim = c(0, 33)) +
  labs(title = "Land Sharing") + 
  xlab("LB (Km2)") +
  ylab("ES saved (euros) 10^6") +
  theme_minimal()


p6 <- plot(estimate_relation(g2.2, data = ess2m2)) +
  coord_cartesian(ylim = c(0, 33)) +
  labs(title = "Land Sparing") +
  xlab("LB (Km2)") +
  ylab("ES saved (euros) 10^6") +
  theme_minimal()

# ------------------------------------------------------------------------------
# Expenses 
# ------------------------------------------------------------------------------


p7 <- plot(estimate_relation(g4.2, data = cost2m1)) +
  coord_cartesian(ylim = c(0, 33)) +
  labs(title = "Land Sharing") + 
  xlab("LB (Km2)") +
  ylab("expenses (euros) 10^6") +
  theme_minimal()

p8 <- plot(estimate_relation(g5.2, data = cost2m2)) +
  coord_cartesian(ylim = c(0, 33)) +
  labs(title = "Land Sparing") +
  xlab("LB (Km2)") +
  ylab("expenses (euros) 10^6") +
  theme_minimal()


# put the plots in the same graph

c <- p5 + p7 + p6 + p8 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'I')

#Save plot
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Scen2.png", plot=c, width=10, height=8)
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Scen2.svg", plot=c, width=10, height=8)


#-----------------------------------------------------------------------------
# Ratios cost/benefits
#-----------------------------------------------------------------------------

ratio_sharing_s2<-plot(estimate_relation(g3.2, data = ratio2m1)) +
  coord_cartesian(ylim = c(0, 4.6)) +
  labs(title = "Land Sharing")+
  xlab("LB (Km2)") +
  ylab("Cost/Benefit Ratio") +
  theme_minimal() +
  theme(legend.position = "none")


ratio_sparing_s2<-plot(estimate_relation(g6.2, data = ratio2m2)) +
  coord_cartesian(ylim = c(0, 4.6)) +
  labs(title = "Land sparing")+
  xlab("LB (Km2)") +
  # ylab("Cost/Benefit Ratio") +
  theme_minimal() +
  theme(axis.title.y = element_blank())
  # theme(legend.position = "none")

# Plot together
d <- ratio_sharing_s2 + ratio_sparing_s2 +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'I')

#Save plot
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Ratio_Scen2.png", plot=d, width=10, height=8)
ggsave(file="Figures/MarginalEffectPlots/MarginalEffectPlot_Ratio_Scen2.svg", plot=d, width=10, height=8)


#-----------------------------------------------------------------------------
# Regression Tables
#-----------------------------------------------------------------------------

# LM
summary(m2.1)
summary(m2.2)
summary(m2.3)

summary(m2.4)
summary(m2.5)
summary(m2.5)

tbl_regression(m2.1, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2.3, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2.4, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2.5, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(m2.6, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

# GAM
summary(g1.2)
summary(g2.2)
summary(g3.2)
summary(g4.2)
summary(g5.2)
summary(g6.2)


tbl_regression(g1.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g2.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g3.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

tbl_regression(g4.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g5.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)
tbl_regression(g6.2, conf.level = 0.95, intercept = T, add_estimate_to_reference_rows = T)

# ==============================================================================


