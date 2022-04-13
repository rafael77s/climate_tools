#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "RNetCDF", "ncdf4", "caret", "sp", "sf", "rgdal","Amelia",
     "parallel", "doParallel", "rasterVis", "RColorBrewer", "metR", "mapproj", "maps")


#carregar os arquivos

shp <- sf::read_sf("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Brutos/World_Continents/world-administrative-boundaries.shp")
setwd("D:/Rafael/Trabalhos/Clima/Doutorado-UFRGS/Colegas/Gabriel_Hofmann/")
setwd("D:/Rafael/Trabalhos/Clima/Doutorado-UFRGS/Dados-Brutos/ERA5/")
GlanceNetCDF("era5_prec_test.nc")
GlanceNetCDF("era5_omega_79-89.nc")

omega <- ReadNetCDF("era5_omega_79-89.nc")
prec <- ReadNetCDF("era5_prec_test.nc")

prec$tp <- prec$tp*10000
prec$time <- as.Date(prec$time)

prec <- filter(prec, time >= "1990-01-01", time <="2000-12-01")


#visualizar os dados
ggplot(prec, aes(longitude, latitude))+ 
  geom_tile(aes(fill = tp))

#eof

prec[, prec_t_w := Anomaly(tp)*sqrt(cos(latitude*pi/180)), by = .(longitude, latitude, month(time))]

eof <- EOF(prec_t_w ~ time | longitude + latitude , data = prec, n = 1:10)
str(eof)

#jeito a resolver
ggplot(eof$right, aes(longitude, latitude)) +
  geom_contour_fill(aes(z = prec_t_w), binwidth = 0.001) +
  scale_fill_divergent() +
  coord_quickmap() +
  borders("world",colour="black")+
  ylim(-34,6)+
  xlim(-74,-33)+
  facet_wrap(~PC) 

#Jeito certo
ggplot(eof$right, aes(longitude, latitude)) +
  geom_contour_fill(aes(z = prec_t_w), binwidth = 0.001) +
  scale_fill_divergent() +
  labs(fill="")+
  geom_sf(data = shp, inherit.aes = F, fill = NA)+
  coord_sf(ylim=c(-34,6), xlim=c(-74,-33))+
  facet_wrap(~PC) 




ggplot(eof$left, aes(time, prec_t_w)) +
  geom_line(aes(color = PC)) +
  scale_x_date(expand = c(0, 0))
