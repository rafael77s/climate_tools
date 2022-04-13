#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", "cluster",
       "Hmisc", "plotly", "FactoMineR", "factoextra","Factoshiny","cluster", "PCAmixdata",
       "purrr", "tseries", "Kendall", "trend", "lubridate", "zoo", "greenbrown","openxlsx")

p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal","Amelia",
       "parallel", "doParallel", "rasterVis", "RColorBrewer")

setwd("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/")

pre_RF<- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/Bio_total_final_novas/Total/mapas_finais/LH/Mapas_rf_LH_moda.tif")
pre_SVM<- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/Bio_total_final_novas/Total/mapas_finais/LH/Mapas_svmRadialSigma_LH_moda.tif")
pre_kknn<- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/Bio_total_final_novas/Total/mapas_finais/LH/Mapas_kknn_LH_moda.tif")
pre_nnet <- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/Bio_total_final_novas/Total/mapas_finais/LH/Mapas_avNNet_LH_moda.tif")

RF_df<- freq(pre_RF)
RF_df <- as.data.frame(RF_df)
SVM_df<- freq(pre_SVM)
SVM_df <- as.data.frame(SVM_df)
KKNN_df<- freq(pre_kknn)
KKNN_df <- as.data.frame(KKNN_df)
NNET_df<- freq(pre_nnet)
NNET_df <- as.data.frame(NNET_df)

pre_df <- cbind(RF_df,SVM_df$count,KKNN_df$count,NNET_df$count)
names(pre_df) <- c("veg","rf","svm","kknn","nnet")
pre_df<- pre_df[-9,]
soma_rf <- colSums(pre_df)
pre_df <- rbind(pre_df,soma_rf)

porc_df <- (pre_df*100)/1037577
porc_df$veg <- pre_df$veg

write.xlsx(porc_df, file= "D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/tabelas_porc/total_LH_porc.xlsx")


ibge_pre<- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_quali/fito.tif")
ibge_df<- freq(ibge_pre)
ibge_df <- as.data.frame(ibge_df)

ibge_df<- ibge_df[-15,]
soma_ibge <- colSums(ibge_df)
ibge_df <- rbind(ibge_df,soma_ibge)



write.xlsx(ibge_df, file= "D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_def/tabelas_porc/total_IBGE_porc.xlsx")


