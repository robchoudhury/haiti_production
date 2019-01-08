library(tidyverse)
library(sf)
haiti <- readRDS("data/gadm36_HTI_1_sf.rds")
haiti_av_production <- read_csv("data/production_haiti_avocado.csv")
haiti_ba_production <- read_csv("data/production_haiti_banana_plantain.csv")
haiti_ma_production <- read_csv("data/production_haiti_mango.csv") %>%
  mutate(total = mangue_fran + mangue_autre)
haiti_sw_production <- read_csv("data/production_haiti_sweetpotato.csv") #patate - sweetpotato?

haiti_production_map_avocado <- haiti %>%
  full_join(haiti_av_production)

ggplot() +
  geom_sf(data=haiti_production_map_avocado, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Avocado Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

ggsave(filename = "figures/haiti_avocado_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_banana <- haiti %>%
  full_join(haiti_ba_production)

ggplot() +
  geom_sf(data=haiti_production_map_banana, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Banana Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

ggsave(filename = "figures/haiti_banana_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_mango <- haiti %>%
  full_join(haiti_ma_production)

ggplot() +
  geom_sf(data=haiti_production_map_mango, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Mango Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

ggsave(filename = "figures/haiti_mango_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")

haiti_production_map_sweetpotato <- haiti %>%
  full_join(haiti_sw_production)

ggplot() +
  geom_sf(data=haiti_production_map_sweetpotato, aes(fill=total))+
  theme_minimal()+
  scale_fill_viridis_c(option = "C", name="Patate Production (MT)") +
  theme(legend.position = c(0.2, 0.6))

ggsave(filename = "figures/haiti_sweetpotato_map.png", width = 9, height = 5, units = "in",dpi = 300, type = "cairo-png")
