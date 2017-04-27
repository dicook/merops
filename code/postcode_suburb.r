
# Download the ESRI format data
# from http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202011?OpenDocument
# (Postal Areas ASGS Non ABS Structures Ed 2011 Digital Boundaries in ESRI Shapefile Format )
#   Unzip and rename the folder AUST_postcodes
# (State Suburbs ASGS Non ABS Structures Ed 2011 Digital Boundaries in ESRI Shapefile Format )
#   Unzip and rename AUST_suburbs

library(tidyverse)
library(rgdal)
library(rgeos)

postcode_shapes <- readOGR(dsn="AUST_postcodes", layer="POA_2011_AUST")
suburb_shapes <- readOGR(dsn="AUST_suburbs", layer="SSC_2011_AUST")

# Shrink suburb shapes slightly, so we don't count small overlaps
# angle of 200m [at the equator]
0.2 / (40075/360)
shrunk <- gBuffer(suburb_shapes, byid=TRUE, width=-0.0018)

result <- gIntersects(postcode_shapes, shrunk, byid=TRUE, returnDense=FALSE)

links <- data_frame(postcode_row=seq_along(result), suburb_row=result) %>% 
    unnest() %>%
    mutate(
        postcode = postcode_shapes@data$POA_CODE[postcode_row],
        suburb = shrunk@data$SSC_NAME[suburb_row]) %>%
    select(postcode, suburb)

write_csv(links, "postcode_suburb.csv")

