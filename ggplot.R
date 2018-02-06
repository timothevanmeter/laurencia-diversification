library(ggplot2)
library(ggmap)

setwd("C://Users//USER//Desktop//georef")
data  <- read.csv("geo_ref.csv", header=FALSE,sep=",")


names(data)=c("id","lat","long")
data$long <- as.numeric(data$long))
data$lat <- as.numeric(data$lat))

head(data)

# determine a reasonable center for map, 
# this could fail in some places (near poles, 180th meridian)
# also google appears to shift things slightly
# center = c(mean(data$long), mean(data$lat))
center = c(0,0)


# get map image from google
map <- get_map(location = center, zoom = 1, maptype = "terrain", 
               source = "google")

# start a ggplot. it won't plot til we type p
p <- ggmap(map)

# add text labels, these will overlap
p <- p + geom_text(data=data,aes(x = long, y = lat, 
                                 label = id),
                   colour="red",size=2,hjust=0, vjust=0)+
  theme(legend.position = "none") 

# add points last so they are on top
p <- p + geom_point(data=data,aes(x=long, y=lat),colour="red",size=2)

# display plot
p 

