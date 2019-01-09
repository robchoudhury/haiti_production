library(tidyverse) #lots of useful packages inside
library(sf) #spatial analysis package
library(leaflet) #interactive map package
haiti <- readRDS("data/gadm36_HTI_1_sf.rds") #Haitian Level 1 GADM map
haiti_crops_production <- read_csv("data/production_haiti_crops.csv")
haiti_production_map_crops <- haiti %>% #take this haitian map data 
  full_join(haiti_crops_production) #combine with the haiti crop data

names_haiti = haiti_crops_production %>% #Names of the unique crops in the dataset
  select(-NAME_1) %>% #drop the names of the regions
  names(.) #get the names of the columns

haiti_crop_data_gather = haiti_production_map_crops %>% #take map/crop data
  gather(., key = crop, value = production, #gather together to get one column with all the crops
         names_haiti)

for (i in names_haiti) { #for all of the crops in the dataset
  temp.plot = ggplot(data = filter(haiti_crop_data_gather , crop == i)) + #make a ggplot using subsetted data
    geom_sf( aes(fill=production)) + #call the spatial data
    theme_minimal()+ #use a minimal theme
    coord_sf()+ #plot it like a map
    scale_fill_viridis_c(option = "C", #color the regions color-blind friendly
                         name=paste(tools::toTitleCase(i), " Production (MT)")) + #name the legend with the crop
    theme(legend.position = c(0.2, 0.6)) #place the legend in the corner out of the map
  
  ggsave(temp.plot, filename = paste("figures/", i, "_production.png"), #save the plots with unique names
         width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png") #figure options so they look good
}

#####
#####
#####


pal_avocado <- colorBin("plasma", #set the color palette for the Leaflet interactive map
                domain = haiti_production_map_crops$avocado, #where the data is
                bins = 7) #how many bins to split the data into


m <- leaflet(haiti_production_map_crops) %>% #create a leaflet map using the haitian map/crop data
  setView(lng = -71.35, lat = 18.55,zoom = 7) %>% #set the location of where the map is centered
  # Base groups for different types of maps available
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
#Add the datalayers on top of the map
  addPolygons( #in the form of polygons (based on the GADM shape file)
    fillColor = ~pal_avocado(avocado), #fill them based on the palette above and the avocado data
    weight = 1, #features of the outline
    opacity = 1, #features of the outline
    color = "white", #features of the outline
    dashArray = "2", #features of the outline
    fillOpacity = 0.5,  #features of the outline
    group = "Avocado", #features of the outline
    highlight = highlightOptions( #when you scroll over a department, it will highlight
      weight = 2,
      color = "grey",
      dashArray = "1",
      fillOpacity = 0.8,
      bringToFront = TRUE))  %>%
  # Layers control
  addLayersControl( 
    baseGroups = c("Stamen.Terrain", #the order of the base maps
                   "Esri.WorldTopoMap", 
                   "Esri.WorldTerrain", 
                   "Stamen.TonerLite", 
                   "Esri.NatGeoWorldMap"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% 
  addLegend(pal = pal_avocado, #add legend to bottom right based on avocado data
            values = ~avocado, 
            opacity = 0.7, 
            title = "Avocado Production (MT)",
            position = "bottomright");m

####



