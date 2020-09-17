# map

library(maptools)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(viridis)
library(magrittr)
library(gpclib)


# as stated in the other answer, this is the same as your shapefile
arg_adm <- raster::getData('GADM', country='ARG', level=1)
port_adm <- raster::getData('GADM', country='PRT', level=1)


# make the polygons a bit less verbose
gSimplify(arg_adm, 
          0.01, 
          topologyPreserve=TRUE) %>%
  SpatialPolygonsDataFrame(dat=arg_adm@data) -> arg_adm

gSimplify(port_adm, 
          0.01, 
          topologyPreserve=TRUE) %>%
  SpatialPolygonsDataFrame(dat=port_adm@data) -> port_adm

# Union together all polygons that make up a region
try_require(c("gpclib", "maptools"))
unioned <- unionSpatialPolygons(cp, invert(polys))

library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# turn them into a data frame
arg_map <- fortify(arg_adm, region="NAME_1")
port_map <- fortify(port_adm, region="NAME_1")
port_map2 <- fortify(port_adm, region="CC_1")

# use a gd projection for this region
arg_proj <- "+proj=aeqd +lat_0=-37.869859624840764 +lon_0=-66.533203125"

# reproducibly simulate some data
set.seed(1492)

unique(arg_map$id)
unique(port_map$id)
unique(port_map$group)

unique(port_map2$id)

puntos <- data.frame(id=c("Aveiro", "Azores", "Beja", "Braga", "Bragança", "Castelo Branco", 
                          "Coimbra", "Évora", "Faro", "Guarda", "Leiria", "Lisboa",
                          "Madeira", "Portalegre", "Porto", "Santarém", "Setúbal", "Viana do Castelo",
                          "Vila Real", "Viseu"),
                     value = sample(100,20))

puntos2 <- data.frame(id=c("01", "02", "03", "04", "05", "06", "07", "08", "09"),
                     value = sample(100,9))



# plot it
gg <- ggplot() 

# necessary in the new world of ggplot2 mapping O_o
gg <- gg + geom_blank(data=port_map2, 
                      aes(long, 
                          lat)
)

# draw the base polygon layer
gg <- gg + geom_map(data=port_map2, 
                    map=port_map2, 
                    aes(map_id = id),
                    color="#b2b2b2", 
                    size=0.15, 
                    fill=NA)

# fill in the polygons
gg <- gg + geom_map(data=puntos2, 
                    map=port_map2,
                    aes(fill=value, 
                        map_id=id),
                    color="#b2b2b2", 
                    size=0.15)

gg <- gg + scale_fill_viridis(name="Scale Title")
#gg <- gg + coord_proj(arg_proj)
gg <- gg + theme_map()
gg <- gg + theme(legend.position=c(0.8, 0.1))
gg
