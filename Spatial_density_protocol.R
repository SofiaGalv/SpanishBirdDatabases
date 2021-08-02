################################################################################
######################## SPATIAL DENSITY ANALYSIS ##############################
################################################################################

library(rnaturalearth)
library(devtools)
library(rnaturalearthhires)
library(sf)
library(rAvis)

##1. Obtain the map of Spain.
Spain <- ne_states(country = 'spain')
Spain <- st_as_sf(Spain)

##2. With Passer domesticus
IEET_pasdomest <- IEET[IEET$species == "Passer domesticus",]

SEO_pasdomest <- SEO[SEO$species == "Passer domesticus",]

ebird_pasdomest <- ebird[ebird$species == "Passer domesticus",]

avis_pasdomest <- AVIS[AVIS$species == "Passer domesticus",]
avis_pasdomest$utm <- str_sub(avis_pasdomest$utm, -4, nchar(avis_pasdomest$utm))
colnames(avis_pasdomest)[8] <- "utm"
avis_pasdomest <- merge(x = avis_pasdomest, y = ravisUTMLatLong ,by = "utm", all.x = T)

  #IEET
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = decimalLongitude, y = decimalLatitude, 
                      fill = ..level..), alpha = 0.3, 
                  contour_var = "count", geom = "polygon", 
                  data = IEET_pasdomest[IEET_pasdomest$decimalLatitude > 25,],
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

  #SEO
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = X, y = Y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = SEO_pasdomest,
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

  #eBird
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = X, y = Y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = ebird_pasdomest,
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

  #AVIS
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = x, y = y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = avis_pasdomest,
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

##2. With Passer hispaniolensis
IEET_pashisp <- IEET[IEET$species == "Passer hispaniolensis",]

SEO_pashisp <- SEO[SEO$species == "Passer hispaniolensis",]

ebird_pashisp <- ebird[ebird$species == "Passer hispaniolensis",]

avis_pashisp <- AVIS[AVIS$species == "Passer hispaniolensis",]
avis_pashisp$utm <- str_sub(avis_pashisp$utm, -4, nchar(avis_pashisp$utm))
colnames(avis_pashisp)[8] <- "utm"
avis_pashisp <- merge(x = avis_pashisp, y = ravisUTMLatLong ,by = "utm", all.x = T)

  #IEET
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = decimalLongitude, y = decimalLatitude, 
                      fill = ..level..), 
                  alpha = 0.3, contour_var = "count",
                  geom = "polygon", 
                  data = IEET_pashisp[IEET_pashisp$decimalLatitude > 25,],
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

#SEO
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = X, y = Y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = SEO_pashisp,
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()

#eBird
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = X, y = Y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = ebird_pashisp,
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()


#AVIS
ggplot() +
  geom_sf(data = Spain, colour = "gray48", fill = NA) +
  stat_density_2d(aes(x = X, y = Y, fill = ..level..), alpha = 0.3,
                  contour_var = "count",
                  geom = "polygon", data = AVIS_prueba[-2,],
                  show.legend= T) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  xlim(-17,10) +
  ylim(25,47) +
  theme_void()