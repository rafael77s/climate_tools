#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal", "rpart","Amelia")

#carregar os arquivos
#adicionar o raster de vegetação separado
sab_veg <- raster("D:/Rafael/mestrado/finais_modelo/sab_veg.tif")

#gerar uma lista de arquivos raster da pasta e remover o raster de vegetação
grids <- list.files("D:/Rafael/mestrado/finais_modelo/" , pattern = "*.tif$")
raster_pred <- grids[-44]

#criar o stack de raster de preditoras com a lista
preditoras <- raster::stack(paste0("D:/Rafael/mestrado/finais_modelo/", raster_pred))

#calcular a pca com o rasterStack; 
#atenção para normalização; Também é permitido adicionar o componente espacial
#se quiser reduzir o tempo de proc utilizar samples;
#utilizar o numero de variaveis para gerar os PC, pode se reduzir caso já conheça os dados
PCA_raster <- rasterPCA(preditoras, nSamples = NULL, nComp = nlayers(preditoras), 
                        spca = T,maskCheck = TRUE)
beep(sound=8)

#extrair resultados da PCA
summary(PCA_raster$model)
loadings(PCA_raster$model)
#criar objeto com os layers dos PCs
PCS <- (PCA_raster$map)
PCS <- stack(PCS)
#selecionar apenas os PCs desejados
PCS <- subset(PCS,1:18)
#visualizar 
plot(PCS,1:18)

#criar um objeto com a vegetacao e os PCs
data1 <- stack(sab_veg,PCS)
plot(data1,1:19)

# Transformar a classe de vegetação em um layer de raster categórico para classificação 
classe <- data1[[1]]
classe <- ratify(classe)

set.seed(99)
# Criar uma amostragem equilibrada baseada nas classes e no componente espacial
amostras <- sampleStratified(classe, size = 1000, na.rm = TRUE, sp = TRUE)
amostras

# Extrair os valores dos PCs para as amostras e remover a coluna ID
val_amostras <- raster::extract(PCS, amostras, df = TRUE)

val_amostras <- val_amostras[, -1]

# combinar as classes com os valores extraidos dos PCs
#Aqui removemos os NAs das classes subrepresentadas, isso é opcional devido a natureza dos dados
data_amostras <- data.frame(classvalue = amostras$sab_veg, val_amostras)
data_amostras$classvalue <- as.factor(data_amostras$classvalue)
data_amostras <- drop_na(data_amostras)

# Treinar o modelo
model_rf <- train(classvalue~., data=data_amostras, method = 'rf')

#Gerar a predição
predicao <- predict(PCS, model_rf, type='class')
plot(predicao)
