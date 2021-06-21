library(aqp)
library(rgdal)
library(GSIF)
library(raster)
library(elevatr)


bd_taller_unida <- read.csv("Downloads/bd_taller_unida.csv")

arcilla <- bd_taller_unida[c("ID_PER", "CORX.x", "CORY.x", "LIMSUP.x" , "LIMINF.x", "ARCILLA.x")]

arcilla <- na.omit(arcilla)

arcilla$IDPROF <- paste0("IDPROF_", arcilla$CORX.x, "_", arcilla$CORY.x)

coordinates(arcilla) <- ~ CORX.x  +  CORY.x

proj4string(arcilla) <-  CRS(" +init=epsg:32717")

lim <- getData('GADM', country='ECU', level=2)

lim <- spTransform(lim, CRS(projection(arcilla)))

ex <- extent(555809 , 579484.7 , 9848637, 9877033 )

plot(ex)

arcilla_crop <- crop(arcilla, ex)

elev <- get_elev_raster(arcilla_crop, prj = projection(arcilla_crop), z = 10, clip = "bbox")

plot(elev)

plot(arcilla_crop, add=TRUE)

x <- terrain(elev, opt=c('slope', 'aspect'), unit='degrees')

plot(x)


# TPI for different neighborhood size:
tpiw <- function(x, w=5) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}

tpi5 <- tpiw(elev, w=5)

covar <- stack(elev, tpi5, x)

sp4 <- as.data.frame(arcilla_crop)

depths(sp4) <- ID_PER  ~ LIMSUP.x + LIMINF.x 

#define the coordinates for sampling sites
site(sp4) <- ~ CORX.x + CORY.x
#define the same coordinates
coordinates(sp4) <- ~ CORX.x + CORY.x

train <- data.frame()

for (i in 50:length(sp4)){#2 #49 #problemas en
#performa a mass preservative spline to estimate ECe at 0-100 cm
try(arcilla_sp <- mpspline(sp4[i], 'ARCILLA.x', d = t(c(0,5))))

arcilla_sp <- data.frame(
  #we include coordinates
  x = sp4@sp@coords[,1],
  y = sp4@sp@coords[,2],
  #the ID for the site
  site=sp4@site,
  # and the values of ECe
  ARCILLAsp_0100 = arcilla_sp$var.std[,1])
   
train <- rbind(train, arcilla_sp)

#rm(arcilla_sp)

}


coordinates(arcilla) <- ~ CORX.x + CORY.x
