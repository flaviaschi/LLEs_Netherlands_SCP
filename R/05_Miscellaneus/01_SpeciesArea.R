#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# The script contains code for checking species area
#===============================================================================

# Import data + adjust df
species_area1_df<-read.csv("DataNew/Excel/Species_Area_df1.csv")
species_area1_df<-species_area1_df[,-c(1)]
species_area_df <-read.csv("DataNew/Excel/Species_Areas_df.csv")
species_area_df<-species_area_df[,-c(1)]

# rename columns
colnames(species_area_df) <- c("Species", "Area")
colnames(species_area1_df) <- c("Species", "Area")
#Bind dfs
species_area<-rbind(species_area_df, species_area1_df)
#remove sp that do not occur in NB
species_area_NB <- species_area[-c(18,20,21,23),]

# info on area
mean_area<-mean(species_area_NB$Area)
hist(species_area_NB$Area)

max(species_area_NB$Area)
# 1000 (LB)
sum(species_area_NB$Area < 1000)
sum(species_area_NB$Area > 1000)
# 100(LB)
sum(species_area_NB$Area < 100) 


