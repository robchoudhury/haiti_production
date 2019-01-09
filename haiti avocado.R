library(tidyverse)
library(sf)
library(leaflet)
haiti <- readRDS("data/gadm36_HTI_1_sf.rds")
haiti_av_production <- read_csv("data/production_haiti_avocado.csv")
haiti_ba_production <- read_csv("data/production_haiti_banana_plantain.csv")
haiti_ma_production <- read_csv("data/production_haiti_mango.csv") %>%
  mutate(total = mangue_fran + mangue_autre)
haiti_sw_production <- read_csv("data/production_haiti_sweetpotato.csv") #patate - sweetpotato?
haiti_crops_production <- read_csv("data/production_haiti_crops.csv")
haiti_production_map_avocado <- haiti %>%
  full_join(haiti_av_production)

ggplot() +
  geom_sf(data=haiti_production_map_avocado, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Avocado Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

#ggsave(filename = "figures/haiti_avocado_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_banana <- haiti %>%
  full_join(haiti_ba_production)

ggplot() +
  geom_sf(data=haiti_production_map_banana, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Banana Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

#ggsave(filename = "figures/haiti_banana_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_mango <- haiti %>%
  full_join(haiti_ma_production)

ggplot() +
  geom_sf(data=haiti_production_map_mango, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Mango Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

#ggsave(filename = "figures/haiti_mango_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_sweetpotato <- haiti %>%
  full_join(haiti_sw_production)

ggplot() +
  geom_sf(data=haiti_production_map_sweetpotato, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Patate Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

#ggsave(filename = "figures/haiti_sweetpotato_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_crops <- haiti %>%
  full_join(haiti_crops_production)

names_haiti = haiti_crops_production %>%
  select(-NAME_1) %>%
  names(.)

pal_avocado <- colorBin("plasma", 
                domain = haiti_production_map_crops$avocado, 
                bins = 7)


m <- leaflet(haiti_production_map_crops) %>%
  setView(lng = -71.35, lat = 18.55,zoom = 7) %>%
  # Base groups
  addProviderTiles(providers$Esri.WorldTopoMap,group = "Esri.WorldTopoMap",
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.Terrain,group = "Stamen.Terrain",
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Esri.WorldTerrain,group = "Esri.WorldTerrain",
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri.NatGeoWorldMap",
                   options = providerTileOptions(opacity = 1)) %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen.TonerLite",
                   options = providerTileOptions(opacity = 1)) %>% 
#Add the datalayers
  addPolygons(
    fillColor = ~pal_avocado(avocado),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "2",
    fillOpacity = 0.5, 
    group = "Avocado",
    highlight = highlightOptions(
      weight = 2,
      color = "grey",
      dashArray = "1",
      fillOpacity = 0.8,
      bringToFront = TRUE))  %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Stamen.Terrain",
                   "Esri.WorldTopoMap", 
                   "Esri.WorldTerrain", 
                   "Stamen.TonerLite", 
                   "Esri.NatGeoWorldMap"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% 
  addLegend(pal = pal_avocado, 
            values = ~avocado, 
            opacity = 0.7, 
            title = "Avocado Production (MT)",
            position = "bottomright");m

####

haiti_crop_data_gather = haiti_production_map_crops %>%
  gather(., key = crop, value = production,
                                names_haiti)


ggplot() +
  geom_sf(data=haiti_crop_data_gather, aes(fill=production))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Avocado Production (MT)") +
  theme(legend.position = c(0.2, 0.6)) +
  coord_sf() +
  facet_wrap(~crop)

for (i in names_haiti) {
  haiti_crop_data_gather %>%
    filter(crop == i) %>%
    print(
      ggplot(.) +
    geom_sf( aes(fill=production))
    )} #+
    theme_minimal()+
    coord_sf()+
    scale_fill_viridis_c(option = "C", 
                         name=paste(tools::toTitleCase(i), " Production (MT)")) +
    theme(legend.position = c(0.2, 0.6))  %>%
    ggsave(filename = paste("figures/", i, "_production.png"), 
           width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")
}
