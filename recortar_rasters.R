#adicionar os pacotes de sempre
library(pacman)
p_load("tidyverse", "beepr", "readxl", 'skimr', "data.table", "corrplot", 
       "Hmisc","raster", "RStoolbox", "caret", "sp", "sf", "rgdal","Amelia",
       "parallel", "doParallel", "rasterVis", "RColorBrewer")

#carregar os arquivos
#adicionar o raster de vegetação separado
sab_veg <- raster("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/resultados/pos_quali/sab_veg2.tif")


# read files
setwd("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Paleoclima/paleoclim/PRE/")

e <- extent(sab_veg)

outpath <- "D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Paleoclima/paleoclim/PRE/PRE/"
dir.create(outpath)

files <- list.files(pattern=".tif$")   

# add output directory
outfiles <- paste0(outpath, files)


for(i in 1:length(files)) {
  r <-raster(files[i])
  rc <- crop(r, e)
  rc <- resample(rc,sab_veg)
  rc <- writeRaster(rc, outfiles[i])
}

#Criar as listas de stack
BA_clim<- list.files("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Paleoclima/paleoclim/BA/BA/" , pattern = "*.tif$")
solos <- list.files("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Solos/rasters/clipados/", pattern=".tif$")

#Stackear preditoras
BA_rast <- raster::stack(paste0("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Paleoclima/paleoclim/BA/BA/", BA_clim))
solos_rast <- raster::stack(paste0("D:/Rafael/Trabalhos/Clima/Rafael-Mestrado/Solos/rasters/clipados/", solos))
#Stack final
teste_stack <- raster::stack(sab_veg,solos_rast,BA_rast)
