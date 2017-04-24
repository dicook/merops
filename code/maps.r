#library(maptools)
#shapeFile <- "AUST_postcodes/POA_2011_AUST.shp"
#sF <- readShapeSpatial(shapeFile)
#class(sF)

# Download the ESRI format data 
# (Postal Areas ASGS Non ABS Structures Ed 2011 Digital Boundaries in ESRI Shapefile Format ) 
# from http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202011?OpenDocument
# Unzip and rename the folder AUST_postcodes
library(rgdal)
sF <- readOGR(dsn="AUST_postcodes", layer="POA_2011_AUST")

library(rmapshaper)
sFsmall <- ms_simplify(sF, keep=0.1) # use instead of thinnedSpatialPoly

map_data <- sF@data
head(map_data)
map_data$id <- rownames(map_data)
map_data$POA_CODE <- as.character(map_data$POA_CODE)
map_data$POA_NAME <- as.character(map_data$POA_NAME)

library(ggplot2)
library(ggthemes)
library(dplyr)
#nat_map <- ggplot2::fortify(sFsmall)
nat_map <- ggplot2::fortify(sFsmall)
head(nat_map)
nat_map$group <- paste("g",nat_map$group,sep=".")
nat_map$piece <- paste("p",nat_map$piece,sep=".")
nat_map <- left_join(nat_map, map_data)
nat_map %>% select(POA_CODE) %>% distinct()
ggplot(data=map_data) +
  geom_map(map=nat_map, aes(map_id=id)) +
  expand_limits(x=nat_map$long, y=nat_map$lat) 

# Read in populations
library(readr)
pop <- read_csv("postcode_population.csv", 
                col_types="cddddddddd")
nat_map_pop <- left_join(nat_map, pop)

ggplot(data=nat_map_pop) +
  geom_map(map=nat_map_pop, aes(map_id=id, fill=Pop)) +
  expand_limits(x=nat_map_pop$long, y=nat_map_pop$lat) + 
  theme_map()
