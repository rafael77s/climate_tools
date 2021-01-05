#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal","Amelia",
       "parallel", "doParallel", "rasterVis", "RColorBrewer")

#adicionar o raster de vegetação separado
sab_veg <- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_quali/sab_veg2.tif")

# Transformar a classe de vegetacao em um layer de raster categorico para classificacao
classe <- sab_veg[[1]]
classe <- ratify(classe)


set.seed(99)
# Criar uma amostragem equilibrada baseada nas classes e no componente espacial
amostras <- sampleStratified(classe, size = 10000, na.rm = TRUE, sp = TRUE)
amostras

# Extrair os valores dos PCs para as amostras e remover a coluna ID
#Aqui entra os eixos da PCA mas voce pode ignorar essa parte
val_amostras <- raster::extract(PCS, amostras, df = TRUE)

val_amostras <- val_amostras[,-1]

# combinar as classes com os valores extraidos dos PCs
#Aqui removemos os NAs das classes subrepresentadas, isso é opcional devido a natureza dos dados
#Deixei aqui os numeros referentes as fitofisionomias atualizadas, a descrição está na chave em formato excel
data_amostras <- data.frame(classvalue = amostras$veg_sab2, val_amostras)
data_amostras <- data_amostras %>%  
  filter(data_amostras$classvalue %in% c("2","3","4","5","6","7","8","9","10","11"))

data_amostras$classvalue <- as.factor(data_amostras$classvalue)

data_amostras <- drop_na(data_amostras)