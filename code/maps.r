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
nat_map <- ggplot2::fortify(sFsmall)
#nat_map <- ggplot2::fortify(sF)
head(nat_map)
nat_map$group <- paste("g",nat_map$group,sep=".")
nat_map$piece <- paste("p",nat_map$piece,sep=".")
nat_map <- left_join(nat_map, map_data)
#nat_map %>% select(POA_CODE) %>% distinct()
ggplot(data=map_data) +
  geom_map(map=nat_map, aes(map_id=id)) +
  expand_limits(x=nat_map$long, y=nat_map$lat)

# Read in populations
# Data from http://www.abs.gov.au/ausstats/abs@.nsf/DetailsPage/2033.0.55.0012011?OpenDocument
library(readr)
pop <- read_csv("postcode_population.csv",
                col_types="cddddddddd")
nat_map_pop <- left_join(nat_map, pop)

ggplot(data=nat_map_pop) +
  geom_map(map=nat_map_pop, aes(map_id=id, fill=Pop)) +
  expand_limits(x=nat_map_pop$long, y=nat_map_pop$lat) +
  theme_map()

# Save map, so it doesn't need re-creating each time
write_csv(nat_map_pop, path="nat_map_pop.csv")
#install.packages('tilegramsR')
#library(tilegramsR)
# None for Oz, and have to create

# We need the polygon centers to do a cartogram
polys <- as(sFsmall, "SpatialPolygons")
class(polys) # should be SpatialPolygons
length(polys)
slotNames(polys)
Polygon(polys[1])
centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  data.frame(long_c=ctr[1], lat_c=ctr[2])
}
centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)
head(centroids)
map_data <- data.frame(map_data, centroids)
write_csv(map_data, path="map_data.csv")

ggplot(data=nat_map_pop) +
  geom_map(map=nat_map_pop, aes(map_id=id), fill="grey90", colour="white") +
  geom_point(data=map_data,
             aes(x=long_c, y=lat_c), size=2, alpha=0.4,
             colour="#f0027f") +
  # xlim(c(136, 142)) + ylim(-36, -33) +
  theme_map()

library(eechidna)
library(purrr)
# Check for Adelaide
adelaide <- aec_extract_f(map_data, ctr=c(138.6, -34.9), expand=c(3,4))
ggplot(data=nat_map_pop) +
  geom_map(map=nat_map_pop, aes(map_id=id), fill="grey90", colour="white") +
  geom_point(data=adelaide, aes(x=long_c, y=lat_c), size=2, alpha=0.4,
             colour="#f0027f") +
 # xlim(c(136, 142)) + ylim(-36, -33) +
  theme_map()

# Now do all the cities
cities <- list(c(151.2, -33.8), # Sydney
               c(153.0, -27.5), # Brisbane
               c(145.0, -37.8), # Melbourne
               c(138.6, -34.9), # Adelaide,
               c(115.9, -32.0)) # Perth
expand <- list(c(2,3.8), c(2,3), c(2.6,4.1), c(4,3), c(12,6))
nat_carto <- purrr::map2(.x=cities, .y=expand,
                         .f=aec_extract_f, aec_data=map_data) %>%
  purrr::map_df(aec_carto_f) %>%
  mutate(region=as.integer(as.character(region))) %>%
  rename(id=region)
nat_data_cart <- aec_carto_join_f(map_data, nat_carto)

nat_data_cart2 <- aec_carto_f(map_data) %>% rename(id=region)

ggplot(data=nat_map_pop) +
  geom_map(map=nat_map_pop, aes(map_id=id), fill="grey90", colour="white") +
  geom_point(data=nat_data_cart2, aes(x=x, y=y), size=1, alpha=0.4,
             colour="#f0027f", inherit.aes=FALSE) +
  theme_map() + coord_equal()

# Group transactions by store postcode
library(sqldf)
md <- src_sqlite('MelbDatathon2017.sqlite', create=FALSE)
ts <- tbl(md, 'Transactions')
stores <- tbl(md, 'Stores') %>% collect()
colnames(stores) = c('Store_ID', 'StateCode', 'postcode', 'IsBannerGroup')
stores <- mutate(stores, Store_ID = as.numeric(Store_ID))
tstore <- ts %>% group_by(Store_ID) %>% summarise(purchases=n(), totalSpend=sum(PatientPrice_Amt)) %>% collect()
store_postcode <- left_join(stores, tstore) %>% group_by(postcode) %>% summarise(numPurchases=sum(purchases), totalSpend=sum(totalSpend), numStores=n())
store_pop <- left_join(pop, store_postcode, 
                       by=c('POA_CODE' = 'postcode')) %>% 
  mutate(totalSpend = ifelse(is.na(totalSpend), 0, totalSpend),
  numPurchases = ifelse(is.na(numPurchases), 0, numPurchases),
  numStores = ifelse(is.na(numStores), 0, numStores),
  SpendPer1000 = totalSpend/Pop*1000)
nat_map_store <- left_join(nat_map, store_pop)

ggplot(data=nat_map_store) +
  geom_map(map=nat_map_store, aes(map_id=id, fill=SpendPer1000)) +
  expand_limits(x=nat_map_pop$long, y=nat_map_pop$lat) +
  theme_map()

# Top store suburb is a commerical district in West Newcastle
head(arrange(store_pop, desc(SpendPer1000))) %>% 
  select(POA_CODE, numPurchases, totalSpend, numStores, SpendPer1000)

# Expenditure per 1000 people decreases as suburb wealth proxy increases
ggplot(data=filter(store_pop, numStores>0)) + 
  geom_point(aes(x=SES_Adv_Score, y=SpendPer1000))

# Find post code location
library(ggmap)
get_lonlat <- function(state, pc) {
  loc <- geocode(paste0(state, ", ", pc))
  return(loc)
}
l <- get_lonlat("NSW", "2302")

library(RCurl)
library(jsonlite)
rev_geoCode <- function(lat, lon, verbose=FALSE) {
  if (verbose) cat(address,"\n")
  u <- url(lat, lon)
  doc <- getURL(u)
  x <- jsonlite::fromJSON(doc, flatten = FALSE)
  if (x$status == "OK") {
    n <- length(x$results$address_components[[1]]$short_name)
    postcode <- x$results$address_components[[1]]$short_name[n]
    return(postcode)
  } else {
    return(NA)
  }
}
url <- function(lat, lon, return.call = "json") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?latlng=", lat, ",", lon, sep = "")
  return(URLencode(u))
}
getURL(url(l$lat, l$lon))
rev_geoCode(l[1], l[2])
