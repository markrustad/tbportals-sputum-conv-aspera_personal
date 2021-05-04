# Map plotting----

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
op <- options(gvis.plot.tag='chart')  # set gViz options
# install libraries
install.packages(c("OpenStreetMap", "DT", "RColorBrewer", "mapproj", "sf", "RgoogleMaps", 
                   "scales", "rworldmap", "maps", "tidyverse", "rnaturalearth", 
                   "rnaturalearthdata", "rgeos", "ggspatial", "maptools", "leaflet", "sf", 
                   "tmap", "here", "rgdal", "scales"))

# install package from github
devtools::install_github("dkahle/ggmap")
# load packages
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
# load data
world <- ne_countries(scale = "medium", returnclass = "sf")
# gene world map
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))