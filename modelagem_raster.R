#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal","Amelia",
       "parallel", "doParallel", "rasterVis", "RColorBrewer")

#carregar os arquivos
#adicionar o raster de vegetação separado
sab_veg <- raster("D:/Rafael/mestrado/finais_modelo/veg/c-vege_sab_wgs.tif")

#gerar uma lista de arquivos raster da pasta e remover o raster de vegetação
grids <- list.files("D:/Rafael/mestrado/finais_modelo/teste/" , pattern = "*.tif$")
raster_pred <- grids

#criar o stack de raster de preditoras com a lista
preditoras <- raster::stack(paste0("D:/Rafael/mestrado/finais_modelo/teste/", grids))

#calcular a pca com o rasterStack; 
#atenção para normalização; também é permitido adicionar o componente espacial
#se quiser reduzir o tempo de proc utilizar samples;
#utilizar o numero de variaveis para gerar os PC, pode se reduzir caso já conheça dos dados
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
writeRaster(PCS, 'D:/Rafael/mestrado/prontos/pca_present.tif')
#criar um objeto com a vegetacao e os PCs
data1 <- stack(sab_veg,PCS)
plot(data1,1:19)

# Transformar a classe de vegetacao em um layer de raster categorico para classificacao
classe <- sab_veg[[1]]
classe <- ratify(classe)


set.seed(99)
# Criar uma amostragem equilibrada baseada nas classes e no componente espacial
amostras <- sampleStratified(classe, size = 10000, na.rm = TRUE, sp = TRUE)
amostras

# Extrair os valores dos PCs para as amostras e remover a coluna ID
val_amostras <- raster::extract(PCS, amostras, df = TRUE)

val_amostras <- val_amostras[, -1]

# combinar as classes com os valores extraidos dos PCs
#Aqui removemos os NAs das classes subrepresentadas, isso Ã© opcional devido a natureza dos dados
data_amostras <- data.frame(classvalue = amostras$c.vege_sab_wgs, val_amostras)
data_amostras <- data_amostras %>%  
  filter(data_amostras$classvalue == c("1","2","3","4","6","7","11"))

data_amostras$classvalue <- as.factor(data_amostras$classvalue)

data_amostras <- drop_na(data_amostras)

# Treinar o modelo
set.seed(77)
index <- createDataPartition(data_amostras$classvalue, p = 0.7, list = FALSE)
train_data <- data_amostras[index, ]
test_data  <-data_amostras[-index, ]
train_data <- droplevels(train_data)
test_data <- droplevels(test_data)


fitControl <- trainControl(method = "LOOCV")

cl <- makeCluster(2)
registerDoParallel(cl)
getDoParWorkers()


model_rf <- train(classvalue~., data=train_data, 
                  method = 'rf')
                  
model_rf
beep(sound=8)

stopCluster(cl)


pred_rf <- predict(model_rf, newdata = test_data)
confusionMatrix(pred_rf, reference=test_data$classvalue,mode="everything")

#Gerar a predição
predicao <- predict(PCS, model_rf)

levelplot(predicao,att="value")
beep(sound=8)

writeRaster(predicao, 'D:/Rafael/mestrado/prontos/predicao_presente.tif',overwrite=T)

#Paleoclimas
nomes <- c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18")

LGM_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_LGM.tif")
names(LGM_pca) <- nomes

LGM_veg <- predict(LGM_pca, model_rf)

levelplot(LGM_veg,att="value")

writeRaster(LGM_veg, 'D:/Rafael/mestrado/prontos/predicao_LGM.tif',overwrite=T)


HS1_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_HS1.tif")
names(HS1_pca) <- nomes

HS1_veg <- predict(HS1_pca, model_rf)

levelplot(HS1_veg,att="value")

writeRaster(HS1_veg, 'D:/Rafael/mestrado/prontos/predicao_HS1.tif',overwrite=T)

BA_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_BA.tif")
names(BA_pca) <- nomes

BA_veg <- predict(BA_pca, model_rf)

levelplot(BA_veg,att="value")

writeRaster(BA_veg, 'D:/Rafael/mestrado/prontos/predicao_BA.tif',overwrite=T)

YD_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_YD.tif")
names(YD_pca) <- nomes

YD_veg <- predict(YD_pca, model_rf)

levelplot(YD_veg,att="value")

writeRaster(YD_veg, 'D:/Rafael/mestrado/prontos/predicao_YD.tif',overwrite=T)

EH_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_EH.tif")
names(EH_pca) <- nomes

EH_veg <- predict(EH_pca, model_rf)

levelplot(EH_veg,att="value")

writeRaster(EH_veg, 'D:/Rafael/mestrado/prontos/predicao_EH.tif',overwrite=T)

MH_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_MH.tif")
names(MH_pca) <- nomes

MH_veg <- predict(MH_pca, model_rf)

levelplot(MH_veg,att="value")

writeRaster(MH_veg, "D:/Rafael/mestrado/prontos/predicao_MH.tif",overwrite=T)

LH_pca <- raster::stack("D:/Rafael/mestrado/prontos/pca_LH.tif")
names(LH_pca) <- nomes

LH_veg <- predict(LH_pca, model_rf)

levelplot(LH_veg,att="value")

writeRaster(LH_veg, "D:/Rafael/mestrado/prontos/predicao_LH.tif",overwrite=T)
