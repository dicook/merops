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
nat_map <- ggplot2::fortify(sF)
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


# Group transactions by store postcode
library(sqldf)
md <- src_sqlite('MelbDatathon2017.sqlite', create=FALSE)
ts <- tbl(md, 'Transactions')
stores <- tbl(md, 'Stores') %>% collect()
colnames(stores) = c('Store_ID', 'StateCode', 'postcode', 'IsBannerGroup')
stores <- mutate(stores, Store_ID = as.numeric(Store_ID))
tstore <- ts %>% group_by(Store_ID) %>% summarise(purchases=n(), totalSpend=sum(PatientPrice_Amt)) %>% collect()
store_postcode <- left_join(stores, tstore) %>% group_by(postcode) %>% summarise(numPurchases=sum(purchases), totalSpend=sum(totalSpend), numStores=n())
store_pop <- left_join(pop, store_postcode, by=c('POA_CODE' = 'postcode')) %>% mutate(totalSpend = ifelse(is.na(totalSpend), 0, totalSpend),
                                                                                      numPurchases = ifelse(is.na(numPurchases), 0, numPurchases),
                                                                                      numStores = ifelse(is.na(numStores), 0, numStores),
                                                                                      SpendPer1000 = totalSpend/Pop*1000)

nat_map_store <- left_join(nat_map, store_pop)

ggplot(data=nat_map_store) + 
  geom_map(map=nat_map_store, aes(map_id=id, fill=SpendPer1000)) + 
  expand_limits(x=nat_map_pop$long, y=nat_map_pop$lat) + 
  theme_map()

# Top store suburb is a commerical district in West Newcastle
head(arrange(store_pop, desc(SpendPer1000)))

# Expenditure per 1000 people decreases as suburb wealth proxy increases
ggplot(data=filter(store_pop, numStores>0)) + geom_point(aes(x=SES_Adv_Score, y=SpendPer1000, colour=factor(numStores)))
