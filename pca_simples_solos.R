#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc", "plotly", "FactoMineR", "factoextra","Factoshiny","cluster", "PCAmixdata")
#adicionar dados e transformar em fator
perfis <- read_excel("D:/Rafael/aulas6-8_minicursoR/perfis_rafael.xlsx")
compostas <- read_excel("D:/Rafael/aulas6-8_minicursoR/compostas_rafael.xlsx")

cols <- c("ID", "Perfil", "Horizonte","Ambiente", "Class_Solo")
perfis[cols] <- lapply(perfis[cols], factor) 

cols2 <- c("ID","Perfil", "Composta", "Ambiente", "Parcela")
compostas[cols2] <- lapply(compostas[cols2], factor) 

#calcular a pca
df_perfil <- perfis[,9:35]
pca_perfil <- PCA(df_perfil, scale.unit=T)

#gráficos de váriaveis, contribuição em eixos e total
fviz_pca_var(pca_perfil, alpha.var = "cos2")
fviz_contrib(pca_perfil, choice = "var", axes = 2, top = 27)
fviz_pca_var(pca_perfil, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#gráfico de indivíduos
fviz_pca_ind(pca_perfil, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_ind(pca_perfil,
             geom.ind = "point", 
             col.ind = perfis$Ambiente, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Groups")
#Biplot
fviz_pca_biplot(pca_perfil, repel = T,
                col.var = "grey",
                habillage ="none",
                alpha.var="contrib",
                col.ind = perfis$Perfil)

#Agora repetir para compostas

#calcular a pca
df_compo <- compostas[,6:32]
pca_compo <- PCA(df_compo, scale.unit=T)

#gráficos de váriaveis, contribuição em eixos e total
fviz_pca_var(pca_compo, alpha.var = "cos2")
fviz_contrib(pca_compo, choice = "var", axes = 1, top = 27)
fviz_pca_var(pca_compo, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#gráfico de indivíduos
fviz_pca_ind(pca_compo, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_ind(pca_compo,
             geom.ind = "point", 
             col.ind = compostas$Ambiente, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Groups")
#Biplot
fviz_pca_biplot(pca_compo, repel = T,
                col.var = "grey",
                habillage ="none",
                alpha.var="contrib",
                col.ind = compostas$Perfil)
