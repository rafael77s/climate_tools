#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "sf", "raster","rnaturalearth", 
       "rnaturalearthdata","spData","tmap", "tmaptools")

#adicionar shapes

pasta <- readClipboard()
setwd(pasta)

Capitals <- st_read("capitais_BR.shp")

Meso <- st_read("mesorreg_BR.shp")
Meso
Meso <- filter(Meso, SIGLA_UF=="MG")

Brazil <- st_read("estados_BR.shp")
Brazil

Countries <- st_read("world-administrative-boundaries.shp")

#primeiro vamos fazer um mapa do brasil

mapa_Cap <- tm_shape(Countries)+
  tm_fill("white")+tm_borders()+
  tm_shape(Brazil, is.master=T)+ tm_polygons()+
  tm_shape(Capitals)+
  tm_dots(size=1, col="red")+tm_text("nome", size=2, auto.placement=T)+
  tm_compass(type = "4star", position = c("right", "top"), size = 3) +
  tm_scale_bar(breaks=c(0,250,500),text.size = 1)+
  tm_graticules(lines=F) +
  tm_layout(bg.color = "lightblue")
mapa_Cap

jpeg("map_capitals.jpeg", units="in", width=15, height=15, res=900)
mapa_Cap
dev.off()

#agora vamos tentar fazer um mapa de minas gerais, mesorregioes e uberlandia

Minas_Gerais <- (Brazil[1,])
Triangulo <- filter(Meso, NM_MESO=="Triângulo Mineiro/Alto Paranaíba")

mydf <- structure(list(cidade = "Uberlândia", 
                       longitude = -48.3337772, 
                       latitude = -18.9220586 ), 
                  .Names = c("cidade","longitude","latitude"), 
                  class = "data.frame", row.names = c("1"))
xy <- mydf[,c(2,3)]

Uberlandia <- SpatialPointsDataFrame(coords = xy, data = mydf,
                               proj4string = 
                                 CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#Mapa triangulo
mapa_triangulo <- 
  tm_shape(Brazil)+ tm_polygons(col="white")+
  tm_shape(Minas_Gerais,is.master=T)+
  tm_polygons(col="grey")+
  tm_shape(Meso)+tm_borders(col="black")+
  tm_shape(Triangulo)+tm_fill(col="red",title = "Mesorregião")+
  tm_shape(Capitals)+
  tm_dots(size=0.5, col="black")+tm_text("nome", auto.placement=T,
                                       size=1)+
  tm_shape(Uberlandia)+
  tm_dots(size=0.5, col="black")+tm_text("cidade", 
                                         size=1,auto.placement=T)+
  tm_compass(type = "4star",
             position = c("left", "top"),
             size =4) +
  tm_scale_bar(breaks=c(0,100,200),
               text.size = 1,
               position=c("right", "bottom"))+
  tm_graticules(lines=F) +
  tm_add_legend(title = 'Legenda',
                size= 2,
                type = 'fill',
                labels = c("Triângulo Mineiro/Alto Paranáiba", "Minas Gerais"),
                text = c("Triângulo Mineiro/Alto Paranáiba", "Minas Gerais"),
                col = c("red", "grey"),
                border.col="black") +
  tm_layout(bg.color = "lightblue")

mapa_triangulo

jpeg("map_triangulo.jpeg", units="in",width=10, height=10, res=900)
mapa_triangulo
dev.off()

#Agora o Mapa de Minas Gerais no Brasil

mapa_Minas <-
  tm_shape(Countries)+
  tm_fill("white")+tm_borders()+
  tm_shape(Brazil,is.master=T)+ tm_polygons(col="white")+
  tm_shape(Minas_Gerais,is.master=T)+
  tm_polygons(col="black", alpha = 0.5)+
  tm_shape(Triangulo)+tm_fill(col="red",title = "Mesorregião")+
  tm_graticules()+
  tm_layout(bg.color = "lightblue")

mapa_Minas

# E agora vamos juntar os dois mapas e fazer um mapa final
mapa_triangulo
print(mapa_Minas, vp = grid::viewport(0.88,0.835,
                                      width = 0.3, 
                                      height = 0.3))


jpeg("map_final_triangulo.jpeg", units="in",width=10, height=10, res=900)
mapa_triangulo
print(mapa_Minas, vp = grid::viewport(0.88,0.769,
                                      width = 0.3, 
                                      height = 0.3))
dev.off()
