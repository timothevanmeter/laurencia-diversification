# In order to create the 2D binned kernel density map I first loaded 
# the KernSmooth and raster package.

library("KernSmooth")
library("raster")
library(rgdal)
library(gtools)
library(MASS)
library(rworldmap)

# Then we read the input file with the coordinates of the points 
# that we want to generate the kernel density estimate off. This input
# file is an export from the Ocean Biographic Information System (OBIS) 
# and represents distribution records from the seaweed Alaria esculenta.

setwd(dir = "//home//user//Bureau")

r <- raster("//home//user//Documents//archives//stage_laurencia//travail//data_MARSPEC//biogeo08_17_30s//biogeo16_30s")

# get the coordinates
coordinates <- read.csv("C://Users//USER//Desktop//coordinates.csv"
                        , header=FALSE,sep=",")
coordinates <- coordinates[,5:6]
coordinates <- coordinates[,c(2,1)]

res(r) <-0


wmap <- getMap(resolution = "coarse")  # differentes resolutions disponibles
plot(wmap)
points(coordinates, col="red")

val <- rep(1, 744)
spg <- coordinates
coordinates(spg) <- coordinates[,1:2]
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
rasterDF <- raster(spg)
rasterDF



# Next we compute the 2D binned kernel density estimate. In order to 
# limit the size of the generated output files I set all values
# smaller then 0.00001 to 0.

# compute the 2D binned kernel density estimate
est <- kde2d(coordinates[,1], coordinates[,2]) 
              bandwidth=c(7,7), 
              gridsize=c(4320,2160),
              range.x=list(c(-180,180),c(-90,90)))
# est$fhat[est$fhat<0.00001] <- 0 ## ignore very small values


# Finally we create a raster file from the output of bkde2D,
inspect it visually and export it as an ascii file.

# create raster
est.raster = raster(list(x=est$x1,y=est$x2,z=est$fhat))
projection(est.raster) <- CRS("+proj=longlat +datum=WGS84")
xmin(est.raster) <- -180
xmax(est.raster) <- 180
ymin(est.raster) <- -90
ymax(est.raster) <- 90

# Then we add the world map layer to the data projections
FDC <- readOGR(dsn=path.expand("//home//user//Documents//archives//stage_laurencia
                //travail//georef//LME66")
               ,layer = "LME66")

list.ras <- mixedsort(list.files(paste(est, FDC, sep = "")
                                 ,full.names = T))

plot(FDC)


# visually inspect the raster output
cols <- sample(c("red","green","pink"),75,TRUE)
# plot(rnorm(100),rnorm(100),col=cols,pch=16,cex=4)
plot(est.raster, add = T, col=cols)
# write an ascii file
writeRaster(est.raster,output,"ascii")

