#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc", "plotly", 'caret',  "FactoMineR", "factoextra",
       "Factoshiny","Rtsne", "cluster", "h2o", "purrr", "PCAmixdata")
#adicionar dados e transformar em fator
grid_covar <- read_excel("D:/Rafael/Clipado/grid_covar.xlsx")
grid_covar$id <- as.factor(grid_covar$id)
grid_covar$cd_fito <- as.factor(grid_covar$cd_fito)

#olhar dos dados
skim(grid_covar)

PCAshiny(var_num)

#separar localização e base numérica
ids <- grid_covar %>% select(id,lat,lon)
var_num <- grid_covar %>% select(altitude, declividad, bio_01, bio_02, bio_03, bio_04, bio_05, bio_06,
                                 bio_07,bio_08,bio_09,bio_10,bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,
                                 bio_17,bio_18,bio_19)
#criar base de dados mistos
var_mix <- grid_covar
var_mix$lat <- NULL
var_mix$lon <- NULL
var_mix$id <- NULL

#selecionar as variáveis que vai usar na pca
var_mix <- var_mix %>% select(altitude, declividad, bio_01, bio_02, bio_03, bio_04, bio_05, bio_06,
                                 bio_07,bio_08,bio_09,bio_10,bio_11,bio_12,bio_13,bio_14,bio_15,bio_16,
                                 bio_17,bio_18,bio_19,cd_fito,provincia,forma,ordem,relevo,legenda_su,textura)

#transformar em fator
var_mix$textura<- as.factor(var_mix$textura)
var_mix$legenda_su<- as.factor(var_mix$legenda_su)
var_mix$relevo<- as.factor(var_mix$relevo)
var_mix$ordem<- as.factor(var_mix$ordem)
var_mix$forma<- as.factor(var_mix$forma)
var_mix$provincia <- as.factor(var_mix$provincia)


#separar os numéricos e categóricos
split <- splitmix(var_mix)
## PCA-MIX
res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, 
                     rename.level=TRUE, 
                     graph=T, 
                     ndim=25)

## Analisar a pca

summary(res.pcamix)
print(res.pcamix)
plot(res.pcamix,choice="sqload",coloring.var=T, leg=TRUE,
     posleg="topright", main="All variables")

#extrair os individuos da pca para formar data frame dos eixos
pca_score <- as.data.frame(res.pcamix$ind)
pca_score <- pca_score[,1:4]

#cluster na base da pca
model_km <-  kmeans(pca_score, 6, nstart = 25)
fviz_cluster(model_km, data = pca_score, geom = 'point')

#criar a coluna cluster
clust <-  model_km$cluster
var_final <-  mutate(grid_covar, Cluster = clust) 

#criar o arquivo shape
write_excel_csv2(var_final, "D:/Rafael/Clipado/var_final.csv")
