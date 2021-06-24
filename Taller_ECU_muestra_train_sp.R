# Documentar

#cargar librerias
library(aqp)

library(GSIF)

library(raster)

library(elevatr)

library(mapview)

library(landmap)

library(rgdal)

library(geoR)

library(plotKML)

library(raster)

library(glmnet)

library(xgboost)

library(kernlab)

library(deepnet)

library(forestError)

library(mlr)


bd_taller_unida <- read.csv("Downloads/bd_taller_unida.csv") #https://drive.google.com/file/d/1xNm3CgaAUhO9v47BaZaJfZurMudap3cE/view?usp=sharing

cos <- bd_taller_unida[c("ID_PER", "CORX.x", "CORY.x", "LIMSUP.x" , "LIMINF.x", "CO.x")]



cos <- na.omit(cos)

cos$IDPROF <- paste0("IDPROF_", cos$CORX.x, "_", cos$CORY.x)

cos$ID_DEPTH <- paste0("IDPROF_", cos$LIMSUP.x, "_", cos$LIMINF.x)

coordinates(cos) <- ~ CORX.x  +  CORY.x

proj4string(cos) <-  CRS(" +init=epsg:32717")

lim <- raster::getData('GADM', country='ECU', level=1)

lim <- spTransform(lim, CRS(projection(cos)))

ex <- extent(555809 , 579484.7 , 9848637, 9877033 )

plot(ex)

cos_crop <- crop(cos, ex)

mapview(cos_crop)

sp4 <- as.data.frame(cos_crop)

depths(sp4) <- ID_PER  ~ LIMSUP.x + LIMINF.x 

plot(sp4[1:10], color='CO.x')

site(sp4) <- ~ CORX.x + CORY.x

coordinates(sp4) <- ~ CORX.x + CORY.x

train <- data.frame()

#ver error en as.data.frame(cos[cos$ID_PER == '1092',])

for (i in 1:length(sp4)){
  try(cos_sp <- mpspline(sp4[i], 'CO.x', d = t(c(0,100))))
  cos_sp <- data.frame(
    x = sp4[i]@sp@coords[,1],
    y = sp4[i]@sp@coords[,2],
    cossp_0100 = cos_sp$var.std[,1])
  train <- rbind(train, cos_sp)
  }

for (i in 3:length(sp4)){
  try(cos_sp <- mpspline(sp4[i], 'CO.x', d = t(c(0,100))))
  cos_sp <- data.frame(
    x = sp4[i]@sp@coords[,1],
    y = sp4[i]@sp@coords[,2],
    cossp_0100 = cos_sp$var.std[,1])
  train <- rbind(train, cos_sp)
}

for (i in 47:length(sp4)){
  try(cos_sp <- mpspline(sp4[i], 'CO.x', d = t(c(0,100))))
  cos_sp <- data.frame(
    x = sp4[i]@sp@coords[,1],
    y = sp4[i]@sp@coords[,2],
    cossp_0100 = cos_sp$var.std[,1])
  train <- rbind(train, cos_sp)
}

coordinates(train) <- ~ x+y

proj4string(train) <- CRS(projection(cos_crop))

elev <- get_elev_raster(cos_crop, prj = projection(cos_crop), z = 10, clip = "bbox")

plot(elev)

plot(cos_crop, add=TRUE)

x <- terrain(elev, opt=c('slope', 'aspect'), unit='degrees')

plot(x)


# TPI for different neighborhood size:
tpiw <- function(x, w=15) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}

tpi5 <- tpiw(elev, w=15)

ndvi <- raster('Downloads/ndvi.tif')

ndvi <- projectRaster(ndvi, elev)

covar <- as(stack(elev, tpi5, x, ndvi), 'SpatialPixelsDataFrame')

names(covar) <- c('elev', 'tpi', 'slope', 'aspect')

plot(elev)

plot(train, add=TRUE)

mapview(train['cossp_0100'])

train@data <- cbind(train@data, over(train, covar))

train$log_cos0100 <- log1p(train$cossp_0100)

m <- train.spLearner(train["log_cos0100"], covariates=covar, lambda = 1)

summary(m@spModel$learner.model$super.model$learner.model)

mp <- predict(m)



