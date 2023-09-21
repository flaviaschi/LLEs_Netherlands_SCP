#===============================================================================
# Flavia Aschi
# "Costs and benefits of protecting Linear Landscape Elements: 
# Applying Systematic Conservation Planning on a case study in the Netherlands"
# 2023
# ------------------------------------------------------------------------------
# The script contains codes for creating a figure of the speices  distirbution
# in Noord-Brabant
#===============================================================================

library(ggplot2)
library(raster)
library(bnspatial)


# Import as RasterStack species maps quantile 10
con_features<-list.files(path= "Data/RawData/Sp_quantile/q10_NB/",
                         pattern='*.tif$', full.names = TRUE)
all_confeatures <- lapply(con_features, raster)
conservation_features<- stack(all_confeatures)
plot(conservation_features)
sp_list <- names(conservation_features)

# Import as a rasterStack species maps quantile 35
con_features1<-list.files(path= "Data/RawData/Sp_quantile/q35_NB/",
                          pattern='*.tif$', full.names = TRUE)
all_confeatures1 <- lapply(con_features1, raster)
conservation_features1<- stack(all_confeatures1)
plot(conservation_features1)
sp_list1 <- names(conservation_features1)

# STack together all the species ratser files 
all_conf <- stack(conservation_features, conservation_features1)
plot(all_conf)

# Remove species that do not occur in Noord-Brabant 
all_conf <- dropLayer(all_conf, c(5, 7, 8, 10))

# Plot species occurrence 
coords <- xyFromCell(all_conf, seq_len(ncell(all_conf)))
all_conf <- stack(as.data.frame(getValues(all_conf)))
names(all_conf) <- c('value', 'variable')
all_conf <- cbind(coords, all_conf)

# Plot ratser stack
ggplot(all_conf) + 
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
