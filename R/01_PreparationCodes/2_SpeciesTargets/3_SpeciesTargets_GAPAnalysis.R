#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# Species coverage in NNN (Natuur Netwerk Nederlands) areas - GAP ANALYSIS
# The following script contains codes for defining species targets taking into 
# consideration how much of the species' range is already included in 
# NNN areas.
#===============================================================================

library(raster)
library(bnspatial)
library(tidyr)
library(dplyr)
library(stringi)
library(ggplot2)


# Import species RasterStack 
con_features<-list.files(path= "Data/CleanData/Sp_quantile/All_conservation_features/",
                         pattern='*.tif$', full.names = TRUE)
all_confeatures <- lapply(con_features, raster)
conservation_features<- stack(all_confeatures)
plot(conservation_features)
sp_list <- names(conservation_features)

# Plot species occurrence (and remove species not present in NB)
{
# Remove species that do not occur in NB
conservation_features1 <- dropLayer(conservation_features,  12)
conservation_features1 <- dropLayer(conservation_features1, 14)
conservation_features1 <- dropLayer(conservation_features1, 16)
conservation_features1 <- dropLayer(conservation_features1, 20)

# Set up data for ggplot
coords <- xyFromCell(conservation_features1, seq_len(ncell(conservation_features1)))
conservation_features1 <- stack(as.data.frame(getValues(conservation_features1)))
names(conservation_features1) <- c('value', 'variable')

conservation_features1 <- cbind(coords, conservation_features1)

# Plot ratser stack
ggplot(conservation_features1) + 
  geom_raster(aes(x, y, fill = value)) +
  facet_wrap(~ variable, ncol = 4) +
  scale_fill_gradientn(colours = rev(terrain.colors(225)), na.value = "transparent", n.breaks = 6) +
  theme_minimal() +
  theme(axis.text = element_blank(), legend.position = "right", legend.direction = "vertical",
        axis.title = element_blank()) +
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()) +
  labs(title = "Species occurrence in Noord Brabant") +
  theme(plot.title = element_text(size = )) + 
  coord_equal()

}

# Import Protected areas raster
PA <-  raster("Data/CleanData/lock_in_areas.tif")

# Turn 0s into NAs for mask
PA[PA==0] <- NA

#Create empty list
cover <- c()

# ForLoop for extracting % coverage in existing PAs.
for (sp in conservation_features@layers) {
  sp1 <- extractByMask(sp, msk=PA, spatial = TRUE)
  # print(sp1)
  sp2<- sp1
  sp2[sp2==0] <- NA
  count <- cellStats(sp2, 'sum', na.rm= TRUE)
  count_tot <- cellStats(sp, "sum", na.rm = TRUE)
  count_cover <- (count / count_tot)
  cover<-rbind(cover, count_cover)
}

# Covert the cover numbers into data frame
cover<- as.numeric(cover) 
cover <- as.data.frame(cover)
# Assign species name to each value
sp_list <- as.data.frame(sp_list)
cover <- cbind(cover, sp_list)

# Save file
 # saveRDS(cover, "DataNew/Excel/cover")
 # write.csv2(cover, "DataNew/Excel/cover.csv")

#Remove NaN
cover1 <- cover %>%
  na.omit() 

mean(cover1$cover)

# Save file
# saveRDS(cover1, "DataNew/Excel/cover1")
# write.csv2(cover1, "DataNew/Excel/cover1.csv")

# Make barplot with percentage coverage
cover1 %>%
  ggplot(aes(x= cover*100 , y=sp_list)) +
  geom_bar(stat="identity") +
  # scale_fill_manual(values = getPalette(colourCount)) +
  xlab("Percent areal overlap with conservation areas") +
  ylab("Species names") +
  ggtitle("Gap analysis of species coverage within areas of conservation") +
  theme(legend.position="none")


# Load in sp targets
# targets35<-read.csv2("DataNew/Excel/Species_35quantile_targets.csv")
# targets10<-read.csv2("DataNew/Excel/Species_10quantile_targets.csv")
# targets<-rbind(targets35, targets10)
# targets<-select(targets, -c(X))
# write.csv2(targets, "DataNew/Excel/Species_quantile_targets.csv")
targets <- read.csv2("DataNew/Excel/Species_quantile_targets.csv")

# Bind together targets and current coverage
coverage_targets <- cbind(cover, targets)
coverage_targets <-  subset(coverage_targets, select = -c(sp_list) )
# saveRDS(coverage_targets, "DataNew/Excel/TargetsCover")

# Calculate remaining target
needed_sp_target <- (coverage_targets$percentage_target) - (coverage_targets$cover)

# Replace NAN with 1 (100% target)
needed_sp_target <- tidyr::replace_na(data =  needed_sp_target, replace = "1") 

# Covert into data frame
needed_sp_target<- as.numeric(needed_sp_target) 
needed_sp_target <- as.data.frame(needed_sp_target)

# Assign species name to each value
sp_list <- as.data.frame(sp_list)
needed_sp_target <- cbind(needed_sp_target, sp_list)

# Filter out NAs and caluclate percentage targets
# needed_sp_target <- needed_sp_target %>%
#   filter(!needed_sp_target == 1.0000000) %>%
# mutate(needed_sp_target*100)

needed_sp_target <- needed_sp_target %>%
  na.omit() %>%
  mutate(needed_sp_target*100)

# # replace negative values 
# needed_sp_target <- needed_sp_target %>%
#   filter(!needed_sp_target == 1.0000000) %>%
#   replace(needed_sp_target < 0, 0.1) %>%
#   mutate(needed_sp_target*100)

  
needed_sp_target %>%
  ggplot(aes(x= needed_sp_target * 100, y=sp_list)) +
  geom_bar(stat="identity") +
  # scale_fill_manual(values = getPalette(colourCount)) +
  xlab("Percent species targets ") +
  ylab("Species names") +
  ggtitle("Species targets after gap analysis") +
  theme(legend.position="none")

# Save csv
 # write.csv2(needed_sp_target, "DataNew/Excel/Species_targets_pa.csv")

# Plot cover and needed sp targets together
# Species needed targets
target <- needed_sp_target %>% 
  select(-c(1)) %>%
  rename("target" = "needed_sp_target * 100")
# Assign avribale with status 
target$Status <- "Conservation Target"

# Species gap analysis 
conserved <- cover1 %>%
  mutate(cover*100) %>%
  select(-c(1)) %>%
  rename ("target" = "cover * 100")
# Assign variable with status 
conserved$Status <- "Range Overlap NNN"

# Bind dataframes together 
targ <- rbind(target, conserved)

# Remove "_" form list
targ$sp_list<-gsub("_", " ", targ$sp_list, fixed=TRUE)

# save data frame in csv
 # write.csv2(targ, "DataNew/Excel/targ.csv")

#Re - Import data frame 
# (for some reasons if I plot before re-importing I miss values for 2 species)
targ <- read.csv2("DataNew/Excel/targ.csv")

# Package used fro fct_rev() - to organize alphabetically y axis
library(forcats)
library(tidyverse)


# Plot GAP ANALYSIS 
ggplot(targ, aes(x = reorder(target), fill =Status, y= fct_rev(sp_list))) + 
    geom_bar(stat="identity") + 
   # geom_col(position = "dodge") +
   # scale_fill_manual(values=c("#076D0c", "#AF1049"))+ #076D0c #107A8C
   scale_fill_manual(values=c("#107A8C", "#076D0c"))+ #076D0c #107A8C
   xlim(0, 100) +
   xlab("Percent species' targets and coverage ") +
   ggtitle("Gap Analysis and Species' Targets") +
   theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
   theme(
     axis.title.y = element_blank(),
     axis.text.y = element_text(face = "italic")) +
    theme(
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


# Plot with bars in ascending orderw
targ %>%
  group_by(sp_list) %>%
  mutate(combined_target = sum(target)) %>%
  ggplot(aes(x = target, fill = Status, y = fct_reorder(sp_list, combined_target))) +
  geom_bar(stat = "identity") 

