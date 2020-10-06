#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc", "plotly", 'caret',  "FactoMineR", "factoextra",
       "Factoshiny","Rtsne", "cluster", "purrr", "PCAmixdata",
       "raster","rgdal")

#adicionar dados e transformar em fator
grid_covar <- read_excel("D:/Rafael/Clipado/grid_covar.xlsx")
cols <- c("id", "cd_fito", "textura","legenda_su", "relevo","ordem","subordem","forma","provincia","sub_provincia","era_period","nm_regiao")
grid_covar[cols] <- lapply(grid_covar[cols], factor) 


#olhar dos dados
skim(grid_covar)

PCAshiny(var_num)

#separar localização e base numérica
ids <- grid_covar %>% select(id,lat,lon)
var_num <- grid_covar %>% select(altitude, declividad, bio_01, bio_02, bio_03, bio_04, bio_05, bio_06,
                                 bio_07,bio_08,bio_09,bio_10,bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,
                                 bio_17,bio_18,bio_19)

#selecionar as variáveis que vai usar na pca
var_mix <- grid_covar %>% select(altitude, declividad, bio_01, bio_02, bio_03, bio_04, bio_05, bio_06,
                                 bio_07,bio_08,bio_09,bio_10,bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,
                                 bio_17,bio_18,bio_19,ordem,textura)

var_mix_num<- var_mix[,1:21]
var_mix_fac<- var_mix[,22:23]

#realizar processo de scaling
scaling <- preProcess(var_mix_num, method= c("scale", "center"))
var_mix_num <- predict(scaling,var_mix_num)
var_mix <- c(var_mix_num,var_mix_fac)
var_mix <-  as.data.frame(var_mix)

#separar os numéricos e categóricos
split <- splitmix(var_mix)
X1 <- split$X.quanti 
X2 <- split$X.quali
       
## PCA-MIX
res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, 
                     rename.level=TRUE, 
                     graph=T, 
                     ndim=36)

## Analisar a pca

summary(res.pcamix)
print(res.pcamix)
plot(res.pcamix,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")
res.pcamix$eig

#extrair os eixos e individuos da pca para formar data frame dos eixos
pred <- predict(res.pcamix,X1,X2)
pred <- as.data.frame(pred)
pca_score <- pred[,1:9]

pca_ind <- res.pcamix$ind
pca_ind <- as.data.frame(pca_ind)
pca_ind <- pca_ind[,1:5]

#cluster kmeans na base da pca
model_km <-  kmeans(pca_score, 5, nstart = 25)
fviz_cluster(model_km, data = pca_score, geom = 'point')

#cluster clara
model_clara <- clara(pca_score,5, metric="manhattan")
       
# plotar o cluster clara
fviz_cluster(model_clara, data = pca_score, 
             geom = "point", 
             stand = F, 
             main = "CLARA — CLUSTERING", 
             ellipse = F)

#criar a coluna cluster
clust <-  model_clara$cluster
var_final <-  mutate(pca_score, Cluster = clust)
var_final <- c(ids,var_final)
var_final <- as.data.frame(var_final)

#criar o arquivo shape

coordinates(var_final) <- ~lon+lat
proj4string(var_final)<- CRS("+proj=longlat +datum=WGS84")
plot(var_final, col=var_final$Cluster, pch=19)

raster::shapefile(var_final, "D:/Rafael/Clipado/var_final.shp")
write_excel_csv2(var_final, "D:/Rafael/Clipado/var_final.csv")
       
#teste PCA numérica

dat_s <-  var_num %>% na.omit() %>% sample_n(5000)

pca_num <- prcomp(var_num, scale=TRUE, center=TRUE)
print(pca_num)
plot(pca_num)
pca_clim <- pca_num$x
pca_clim <- as.data.frame(pca_clim)
pca_clim <- pca_clim[,1:4]

autoplot(pca_num)

model_km <-  kmeans(pca_clim, 5, nstart = 25)
fviz_cluster(model_km, data = pca_clim, geom = 'point')

