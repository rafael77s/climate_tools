#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table",
       "corrplot", "Hmisc","raster", "stars", "rgdal", "ncmeta",
       "RStoolbox", "RNetCDF", "ncdf4", "caret", "sp", "sf", 
       "rgdal","Amelia", "parallel", "doParallel", "rasterVis",
       "RColorBrewer", "metR", "mapproj", "maps")


#carregar os arquivos

world <- map_data("world")


setwd("D:/Rafael/Trabalhos/Clima/Doutorado-UFRGS/Dados-Brutos/")
GlanceNetCDF("Sea-Ice/noaa_SI_m_81-22.nc")
GlanceNetCDF("NOAA_OISST/sst_mon_81-0222.nc")

sea_ice <- ReadNetCDF("Sea-Ice/noaa_SI_m_81-22.nc", vars=c("icec"),
                      subset = list(lat = -50:-90,
                                    time = "2022-02-01"))

sea_ice$lon <- sea_ice$lon-180



sst <- ReadNetCDF("NOAA_OISST/sst_mon_81-0222.nc", vars=c("sst"),
                  subset = list(lat = -50:-90,
                                time = "2022-02-01"))


sst$lon <- sst$lon-180

#visualizar os dados
 
ggpolar(pole = "S", max.lat = -60, min.lat = -90)

SI_plot<- ggplot(sea_ice, aes(lon, lat))+ 
  geom_tile(aes(fill = icec))+
  scale_fill_distiller(palette = "Purples", direction = -1)+
  borders("world",colour = "black", fill= NA)+
  coord_sf(ylim=c(-90,-50),expand=F,clip="on",
           crs=st_crs("3031"))
SI_plot

SI_plot<- ggplot(sea_ice, aes(lon, lat))+ 
  geom_tile(aes(fill = icec))+
  scale_fill_distiller(palette = "Blues", direction = -1)+
  borders("world",colour = "black", fill= NA)+
  coord_polar(clip="on")+
  ylim(-90,-50)
SI_plot

SST_plot<- ggplot(sst, aes(lon, lat))+ 
  geom_tile(aes(fill = sst))+
  scale_fill_distiller(palette = "RdYlBu", direction = -1)+
  coord_polar()
SST_plot

# com stars


sea_ice <- stars::read_ncdf("Sea-Ice/noaa_SI_m_81-22.nc")
antartica <- rgdal::readOGR("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Brutos/World_Continents/antarctica.shp") %>% 
  st_as_sf() %>% 
  st_transform(st_crs(sea_ice))

ggplot() + 
  geom_sf(data = antartica, 
          color = 'black', 
          fill = 'grey', 
          size = .2) + 
  geom_stars(data = sea_ice) + 
  scale_fill_continuous(na.value = 0)




