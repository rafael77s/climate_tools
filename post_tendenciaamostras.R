#pacote muito util para instalar e ativar pacotes
install.packages(pacman)
library(pacman)
p_load("tidyverse", "readxl", "data.table", "lubridate",
       "ggpubr","viridis", "RColorBrewer","forcats", 
       "mapproj", "maps", "raster", "sf", "gdalUtils",
       "rgdal", "RNetCDF", "ncdf4",
       "metR","tseries","Kendall","trend")
#esse pequeno truque permite copiar o endereço da pasta no computador
pasta <- readClipboard()
setwd(pasta)
#primeiro carregar os arquivos das amostras e um shape do brasil
samples <- st_read("capitais_BR.shp")

Brazil <- st_read("estados_BR.shp")

#agora vamos adicionar os dados da serie temporal do CRU
pasta2<- readClipboard()
setwd(pasta2)
#Olhar estes dados, ver a resolucao temporal, espacial


GlanceNetCDF("cru_prec_mon_01-20.nc")

prec <- raster::stack("cru_prec_mon_01-20.nc")

#e agora podemos extrair um df com a serie temporal para cada capital
df_prec <- raster::extract(prec, samples, df=T)

#aqui colocamos a localizacao de cada capital junto com a serie temporal
df_prec <- cbind(samples[,1:4], df_prec)
#criamos um vetor com as datas da serie temporal 
#e substituimos ele no data frame
datas <- seq(dmy("01-01-1901"), dmy("31-12-2020"), by = "months")
datas <- as.character(datas)
nomes <- c("nome", "regiao", "lat", "lon", "ID")
nomes <- c(nomes,datas)

#aqui vamos extrair um data frame so para serie temporal e exportar
names(df_prec) <- nomes
df_prec[,1446] <- NULL
df_prec <- as.data.frame(df_prec)
write.table(df_prec, file = "capitais_ts_1901-2020.csv", sep =";", dec=".")

#Agora precisamos transpor esse df, para que cada coluna seja uma cidade
nome <- df_prec$nome

df_prec2 <- transpose(df_prec)

df_prec3 <- df_prec2[6:1445,]

df_prec3 <- cbind(datas,df_prec3)
names(df_prec3) <- c("data",nome)

df_prec3[,2:28] <- as.data.frame(lapply(df_prec3[,2:28], as.numeric))

#agora podemos calcular a serie temporal para cada coluna do df

df_ts <- lapply(df_prec3[,2:28], ts, frequency = 12, start = c(1901, 1))

#e vamos criar uma lista com cada analise estatistica
MK_result <- lapply(df_ts, SeasonalMannKendall)
MK_result
SenS_result <- lapply(df_ts, sea.sens.slope)
SenS_result

#agora vamos converter essas listas em df para exportacao
MK_result <- data.frame(matrix(unlist(MK_result), nrow=27, byrow=TRUE),stringsAsFactors=FALSE)
mk_var <- c("tau", "sl", "S", "D", "varS")
names(MK_result) <- mk_var
MK_result <- cbind(nome,MK_result)

SenS_result <- data.frame(matrix(unlist(SenS_result), nrow=27, byrow=TRUE),stringsAsFactors=FALSE)
sen_var <- "sen_slope"
names(SenS_result) <- sen_var

#juntar esses df e transformar em um df final e exportar
df_estatistica <- cbind(MK_result,SenS_result)
df_estatistica

localizacao <- df_prec[,1:5]

df_estatistica <- left_join(localizacao,df_estatistica,by="nome")

write.table(df_estatistica, file = "resultados-tendencia.csv", sep =";", dec=".")

#vamos converter esse df em um shapefile novamente
shp_tend <- st_as_sf(x = df_estatistica,                         
               coords = c("lon", "lat"), crs=st_crs(4326))


#e criar plots para visualizar os resultados
plot1<- ggplot() + 
  geom_sf(data = Brazil, fill = "grey", color = "grey") +
  geom_sf(data = shp_tend, aes(colour=tau), size= 3)+
  scale_colour_distiller(palette="RdBu", direction=1, name="MK")+                         
  ggtitle("Brazil Capitals Precipitation Trends") + 
  theme_bw()+
  coord_sf()

plot2<- ggplot() + 
  geom_sf(data = Brazil, fill = "grey", color = "grey") +
  geom_sf(data = shp_tend, aes(colour=sen_slope), size= 3)+
  scale_colour_distiller(palette="BrBG", direction=1, name="SenSlope")+
  ggtitle(" ") + 
  theme_bw()+
  coord_sf()

ggarrange(plot1, plot2, ncol=2, nrow=1) 

setwd(pasta)

ggsave(filename= "tendencias_capitais.png", 
       width = 9.7, height = 10, device='png', dpi=300)
