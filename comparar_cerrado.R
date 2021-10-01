#adicionar os pacotes de sempre
install.packages(pacman)
library(pacman)
p_load("devtools","tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", "cluster", 
       "Hmisc","raster", "caret", "sp", "sf", "rgdal", "parallel", "doParallel", "vegan", 
       "lattice", "maptools",  "broom",  "FactoMineR", "factoextra","Factoshiny","cluster", "ggdendro")

#adicionar os dados, converter e conferir os dados
df1 <- read_excel("D:/Rafael/Trabalhos/Clima/Doutorado-UFRGS/SR-GPG00087/resultados iniciais2.xlsx")
df1$Tipo <- as.factor(df1$Tipo)
var1 <- df1[,12:14]

skim(df1)

#teste de normalidade
lshap <- lapply(var1, shapiro.test)
lres <- sapply(lshap, `[`, c("statistic","p.value"))
lres

hist(df1$difSOIL)

#como não temos normalidade em todas variaveis, realizar Mann-Whitney

wilcox.test(difAET~Tipo,data=df1, conf.int=T)
wilcox.test(difPR~Tipo,data=df1, conf.int=T)
wilcox.test(difSOIL~Tipo, data= df1, conf.int=T)

#boxplot para visualizar os dados

boxplot(difAET~Tipo, data= df1)
boxplot(difPR~Tipo, data= df1)
boxplot(difSOIL~Tipo, data= df1)
