
#pacote muito util para instalar e ativar pacotes
install.packages(pacman)
library(pacman)
p_load("tidyverse", "readxl", "data.table", "forcats",
       "raster", "sf", "gdalUtils","rgdal")
#esse pequeno truque permite copiar o endereço da pasta no computador
pasta <- readClipboard()
setwd(pasta)
#primeiro carregar os arquivos das amostras e um shape do brasil
samples <- st_read("capitais_BR.shp")
Brazil <- st_read("estados_BR.shp")
#agora vamos visualizar aonde esses pontos estao
ggplot() + 
  geom_sf(data = Brazil, fill = "grey", color = "grey") +
  geom_sf(data = samples, fill = "black") +
  ggtitle("Brazil Capitals") + 
  coord_sf()
#agora para adicionar os dados bioclimaticos, vamos olhar na pasta dos dados bioclim
pasta2<- readClipboard()
setwd(pasta2)
#e posteriormente construir um stack de rasters
current.list <- list.files(path=pasta2, 
                           pattern =".tif$", full.names=TRUE)
tempraster <- stack(current.list)
#precisamos corrigir o nome de cada layer desse stack
bandas <- c("bio_01", "bio_02", "bio_03", "bio_04", "bio_05", 
            "bio_06", "bio_07", "bio_08", "bio_09", "bio_10",
            "bio_11", "bio_12", "bio_13", "bio_14", "bio_15", 
            "bio_16", "bio_17", "bio_18", "bio_19", "elev")
names(tempraster@layers) <- bandas
tempraster@layers
#e agora podemos extrair um df com os valores das variaveis para cada capital
df_bioclim <- raster::extract(tempraster, samples, df=T)
#e tambem devemos colocar o nome e regiao das capitais
#se observarmos bem, as variaveis novamente perdem os nomes. Vamos corrigir
df_bioclim <- df_bioclim[,-1]
colnames(df_bioclim) <- bandas
df_bioclim <- cbind(samples[,1:4], df_bioclim)
#vamos ordenar os dados

df_bioclim <- df_bioclim %>% mutate(nome = fct_reorder(nome, Regiao)) 
#e agora por fim vamos fazer um plot simples para visualizar a precipitação em cada
#capital

p  <- ggplot(df_bioclim, aes(y=nome,x=bio_12))+
  geom_col(aes(fill = bio_12), position = position_stack(reverse = T)) +
  labs(title="", x ="Annual Precipitation (mm)", y = "Brazil Capitals") + 
  scale_fill_distiller(palette = "YlGnBu",direction=1, name="mm")+
  theme(legend.position="bottom")
p 

#e se quisermos podemos exportar os dados que geramos
setwd(pasta)
write.table(df_bioclim, file = "df_bioclim.csv", sep =";", dec=".")
