#adicionar os pacotes de sempre
install.packages(pacman)
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal","Amelia",
       "parallel", "doParallel", "rasterVis", "RColorBrewer")

#carregar os arquivos
#adicionar o raster de vegetacao separado
cerrado2019 <- raster("D:/Rafael/Trabalhos/Clima/Dados_MapBiomas/triangulomineiro_1989.tif")
freq(cerrado2019)
# Transformar a classe de vegetacao em um layer de raster categorico
classe <- cerrado2019[[1]]
classe <- ratify(classe)
#criar uma mascara com os dados da classe de interesse
tmpfilter <- classe == 3
classe2 <- tmpfilter==1

set.seed(99)
# Criar uma amostragem equilibrada baseada nas classes
amostras <- sampleStratified(classe2, size = 400, na.rm = T, sp = T)
amostras

writeOGR(amostras, dsn = "D:/Rafael/Trabalhos/Clima/Dados_MapBiomas/amostras2.shp", 
         layer="point",  driver = "ESRI Shapefile")


