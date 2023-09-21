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

#sum the layes together for sp richness
sp_richness<- calc(all_conf, fun = sum)
plot(sp_richness)
writeRaster(sp_richness, "Figures/NewFigures/SP_Richness.tif")


# Plot species occurrence 
coords <- xyFromCell(sp_richness, seq_len(ncell(sp_richness)))
sp_richness <- stack(as.data.frame(getValues(sp_richness)))
names(sp_richness) <- c('value', 'variable')
sp_richness <- cbind(coords, sp_richness)


# Plot ratser stack
custom_palette <- c(rev(terrain.colors(6)), "#5AAE61", "#2F7D2E", "#00441B")

plot<-ggplot(sp_richness) + 
  geom_raster(aes(x, y, fill = value)) +
  facet_wrap(~ variable, ncol = 4) +
  scale_fill_gradientn(colours = custom_palette, 
                       na.value = "transparent", 
                       limits = c(1, 9),
                       breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +  theme_minimal() +
  theme(axis.text = element_blank(), 
        legend.position = "right", 
        legend.direction = "vertical",
        axis.title = element_blank()) +
  theme(
    panel.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()) +
  # labs(title = "Species richness in Noord-Brabant") +
  theme(plot.title = element_text(size = )) + 
  coord_equal()
}

ggsave(file="Figures/NewFigures/sp_rich.svg", plot=plot, width=10, height=8)
ggsave(file="Figures/NewFigures/sp_rich.jpeg", plot=plot, width=10, height=8)
