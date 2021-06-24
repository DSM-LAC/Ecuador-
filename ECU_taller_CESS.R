# Documentar

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

ces <- bd_taller_unida[c("ID_PER", "CORX.x", "CORY.x", "LIMSUP.x" , "LIMINF.x", "CESS.x")]



ces <- na.omit(ces)

ces$IDPROF <- paste0("IDPROF_", ces$CORX.x, "_", ces$CORY.x)

ces$ID_DEPTH <- paste0("IDPROF_", ces$LIMSUP.x, "_", ces$LIMINF.x)

coordinates(ces) <- ~ CORX.x  +  CORY.x

proj4string(ces) <-  CRS(" +init=epsg:32717")

lim <- raster::getData('GADM', country='ECU', level=1)

lim <- spTransform(lim, CRS(projection(ces)))

ex <- extent(491069.9 , 641706.1, 9790439 , 9941075 )

#ex <- extent(555809 , 579484.7 , 9848637, 9877033 )

plot(ex)

ces_crop <- crop(ces, ex)

mapview(ces_crop)

sp4 <- as.data.frame(ces_crop)

depths(sp4) <- ID_PER  ~ LIMSUP.x + LIMINF.x 

plot(sp4[1:10], color='CESS.x')

site(sp4) <- ~ CORX.x + CORY.x

coordinates(sp4) <- ~ CORX.x + CORY.x

train <- data.frame()

#ver error en as.data.frame(ces[ces$ID_PER == '1092',])

for (i in 1:length(sp4)){
  try(ces_sp <- mpspline(sp4[i], 'CESS.x', d = t(c(0,100))))
  ces_sp <- data.frame(
    x = sp4[i]@sp@coords[,1],
    y = sp4[i]@sp@coords[,2],
    cessp_0100 = ces_sp$var.std[,1])
  train <- rbind(train, ces_sp)
  }


coordinates(train) <- ~ x+y

proj4string(train) <- CRS(projection(ces_crop))

elev <- get_elev_raster(ces_crop, prj = projection(ces_crop), z = 10, clip = "bbox")

plot(elev)

plot(ces_crop, add=TRUE)

x <- raster::terrain(elev, opt=c('slope', 'aspect'), unit='degrees')

plot(x)


# TPI for different neighborhood size:
tpiw <- function(x, w=15) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}

tpi5 <- tpiw(elev, w=15)

ndvi <- raster('Downloads/ndvi_2.tif')

ndvi <- projectRaster(ndvi, elev)

stk <- stack(ndvi, mask(stack(elev, tpi5, x), ndvi))

covar <- as(stk, 'SpatialPixelsDataFrame')

names(covar) <- c('ndvi','elev', 'tpi', 'slope', 'aspect')

plot(elev)

plot(train, add=TRUE)

mapview(train['cessp_0100'])

train@data <- cbind(train@data, over(train, covar))

train$log_ces0100 <- log1p(train$cessp_0100)

m <- train.spLearner(train["log_ces0100"], covariates=covar, oblique.coords=FALSE)

summary(m@spModel$learner.model$super.model$learner.model)

mp <- predict(m)

prediction <- expm1(stack(mp$pred))

writeRaster(prediction, file='band1RESPONSE_band2ERROR_band3LOWER, band4UPPER.tif')
