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

GlanceNetCDF("ERA5/era_pnmm_50-21.nc")

MSL <- ReadNetCDF("ERA5/era_pnmm_50-21.nc", vars=c("msl"),
                  subset = list(latitude = -60:20,
                                longitude = -90:20,
                                time = c("1981-01-01","2010-12-01") ))

MSL$month <- lubridate::month(MSL$time)

#realizar conversoes nos arquivos
MSL$longitude <- MSL$longitude-360 #se não estiver na longitude -180

MSL_clima <- MSL %>% 
  group_by(latitude,longitude) %>% 
  summarise(pnmm=mean(msl))

MSL_clima %>%
  group_by(group = cut(pnmm, breaks = seq(1017,max(1019),by=.1))) %>%
  summarise(n = n())


MSL_clima$pnmm <-round(MSL_clima$pnmm, digits=0)



  
#visualizar os dados

#normal
clima_plot<- 
  ggplot(MSL_clima, aes(longitude, latitude, z=pnmm))+
  geom_contour_fill(breaks = seq(min(MSL_clima$pnmm),max(MSL_clima$pnmm))) +
  scale_fill_distiller(palette = "Spectral", direction = -1,
                       limits = c(min(MSL_clima$pnmm),max(MSL_clima$pnmm)),
                       breaks = seq(min(MSL_clima$pnmm),max(MSL_clima$pnmm),by=6),
                       name="PNMM (hPa)")+
  geom_contour2(data=MSL_clima, color = "#4B4B4B",
                breaks= seq(min(MSL_clima$pnmm),max(MSL_clima$pnmm),by=4)) +
  geom_text_contour(stroke = 0.2, size=2.5,
                    label.placer = label_placer_flattest(),
                    check_overlap = T,
                    min.size = 10) +
  borders("world",colour = "black", fill= NA)+
  coord_sf(xlim=c(-90,20),ylim=c(-60,20), clip="on", expand=F,
           crs=4326, default_crs = sf::st_crs(4326))
clima_plot

#Apenas a isobara 1018
clima_plot<- ggplot(MSL_clima, aes(longitude, latitude, z=pnmm))+
  geom_contour2(data=MSL_clima, color = "blue",
                breaks= 1018) +
  geom_text_contour(stroke = 0.2, size=2.5,
                    breaks=1018,
                    label.placer = label_placer_flattest(),
                    check_overlap = T,
                    min.size = 10) +
  borders("world",colour = "black", fill= "grey")+
  coord_sf(xlim=c(-90,20),ylim=c(-60,20), clip="on", expand=F,
           crs=4326, default_crs = sf::st_crs(4326))+
  theme_bw()
clima_plot

#Por mês
msl_feb<- MSL %>% 
  filter(month==2) %>% 
  group_by(latitude,longitude) %>% 
  summarise(pnmm=mean(msl)) %>% 


msl_feb$pnmm<- round(msl_feb$pnmm, digits=0)


feb_plot<- msl_feb %>% 
  ggplot(aes(longitude, latitude, z=pnmm))+
  geom_contour2(data=msl_feb, color = "blue",
                breaks= 1018) +
  geom_text_contour(stroke = 0.2, size=2.5,
                    breaks=1018,
                    label.placer = label_placer_flattest(),
                    check_overlap = T,
                    min.size = 10) +
  borders("world",colour = "black", fill= "grey")+
  coord_sf(xlim=c(-90,20),ylim=c(-60,20), clip="on", expand=F,
           crs=4326, default_crs = sf::st_crs(4326))
feb_plot



