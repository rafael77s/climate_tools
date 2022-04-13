#pacote muito util para instalar e ativar pacotes
install.packages("pacman")
library(pacman)
p_load("tidyverse", "readxl", "data.table", "forcats",
       "raster", "sf", "gdalUtils","rgdal")
#esse pequeno truque permite copiar o endereço da pasta no computador
pasta <- readClipboard()
setwd(pasta)
#agora vamos adicionar os rasters de temperatura e precipitacao
temperatura <- raster::stack("cru_t_m_81-10.nc")
precipitacao <- raster::stack("cru_p_m_81-10.nc")
#criando um ponto da nossa cidade, vamos extrair um df para cada variavel e depois juntar
#Criando o ponto
Brasilia <- structure(list(cidade = "Brasília", 
                       longitude = -48.0780006, 
                       latitude = -15.7757256 ), 
                  .Names = c("cidade","longitude","latitude"), 
                  class = "data.frame", row.names = c("1"))
xy <- Brasilia[,c(2,3)]
Brasilia <- SpatialPointsDataFrame(coords = xy, data = Brasilia,
                                     proj4string = 
                                       CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#Extraindo os df
df_temp <- raster::extract(temperatura, Brasilia, df=T) %>% transpose()
df_prec <- raster::extract(precipitacao, Brasilia, df=T) %>% transpose()

#Juntando os df
datas <- seq.Date(from=as.Date("1981/01/01"),to=as.Date("2010/12/16"),by="month")
df_final <- cbind(df_temp[2:361,],df_prec[2:361,]) %>% as.data.frame() 
df_final <- cbind(datas,df_final)
names(df_final) <- c("datas", "temp", "prec")

#summarizar a base por mes
df_final <- df_final %>%  
  mutate(month = month(datas)) %>%
  group_by(month) %>%
  summarise(
    clim_temp = mean(temp),
    clim_prec = mean(prec))

#Agora vamos plotar o climograma

climograma <- ggplot(df_final, aes(x = as.factor(month), y = clim_prec)) 

climograma + geom_bar(aes(y=clim_prec),fill="blue",stat="identity") + 
  geom_line(aes(y = scales::rescale(clim_temp, to=range(clim_prec))), color="red", size=1.5, group = 1)+
  scale_y_continuous("Precipitation mm", 
    sec.axis = sec_axis(~scales::rescale(., to=range(df_final$clim_temp)), 
                        name = "Temperature °C"))+
  labs(x = "Month",title = "Climograph Brasília - Brazil")+
  theme_gray()

ggsave(filename= "climograma_brasilia.jpeg", width = 15, height = 7, device='jpeg', dpi=300)

