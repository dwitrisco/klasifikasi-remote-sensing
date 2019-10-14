library(sp)
library(rgdal)
library(raster)
setwd('D:/2. bahan thesis/6. data excel/buat shp segmen')

centroids <- read.csv("shp.csv", sep=';', header=TRUE)
str(centroids)

x2<-centroids$X
x3<-centroids$Y
xy<-cbind(x2,x3)
xy1<-project(xy[,1:2], "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84
+towgs84=0,0,0") 
xy1<-as.data.frame(xy1)
xy1$idsegmen<-centroids$idsegmen
colnames(xy1)[colnames (xy1) == "x2"] <- "X"
colnames(xy1)[colnames (xy1) == "x3"] <- "Y"
xy2<-centroids[,-c(5,12,13)]
xy1<-cbind(xy1,xy2)

write.csv(xy1, 'D:/2. bahan thesis/6. data excel/buat shp segmen/sampelUTM.csv')

#lihat projection
cord.dec = SpatialPoints(cbind(xy1$X,xy1$Y), proj4string = CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84
+towgs84=0,0,0 "))
plot(cord.dec, cex=0.2,
     col=ifelse(xy1$Subseg=="A1",'blue', ifelse(xy1$Subseg=="B2",'red',
         ifelse(xy1$Subseg=="C3",'green','brown'))))
View(cord.dec)


# set the radius for the plots
radius <- 0.0002 # radius in meters
# define the plot edges based upon the plot radius. 
yPlus <- xy1$Y-radius
xPlus <- xy1$X+radius
yMinus <- xy1$Y+radius
xMinus <- xy1$X-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

# Extract the plot ID information
ID=xy1$ID

# create spatial polygons from coordinates
bwi_sampel<- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string=CRS(as.character("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84
+towgs84=0,0,0 ")))

# plot the new polygons
plot(bwi_sampel, cex=2)


#buat shp
# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
bwi_sampel.df <- SpatialPolygonsDataFrame(bwi_sampel, data.frame(id=ID, row.names=ID))
plot(bwi_sampel.df, col=rainbow(50, alpha=0.5))

#simpan
writeOGR(bwi_sampel.df, '.', 'bwi_sampel', 'ESRI Shapefile')

#gabung data
bwi_sampel<-readOGR(dsn='.', layer = 'bwi_sampel')
plot(bwi_sampel, border='red')
colnames(xy1)[colnames (xy1) == "ID"] <- "id"

bwi_sampel <- merge(bwi_sampel, xy1, by='id')


View(bwi_sampel) 




