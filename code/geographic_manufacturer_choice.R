library(sqldf)
library(ggplot2)
library(dplyr)
library(readr)

# Often there are several different manufacturers offering medication with the same generic ingredient and same strength.
# I investigate whether doctors in certain postcodes are more likely to prescribe a particular manufacturer's version than the national average.
# For each drug ingredient and strength combination I found the proportion of sales belonging to a particular manufacturer.
# This is compared to the proportion of sales in each postcode
# For each postcode and manufacturer, the ratio between Actual sales : Expected sales is found

md <- src_sqlite('MelbDatathon2017.sqlite', create=FALSE)
ts <- tbl(md, 'Transactions')
pt <- tbl(md, 'Patients') %>% collect()
colnames(pt) <- c('Patient_ID', 'gender', 'year_of_birth', 'postcode')
pt <- mutate(pt, Patient_ID = as.numeric(Patient_ID))
chronic <- tbl(md, 'ChronicIllness') %>% collect()
drugs <- tbl(md, 'Drugs') %>% collect() %>% mutate(MasterProductID = as.numeric(MasterProductID))

# This string of operations results in a dataframe with number of purchases of each drug by postcode, with some useful info merged on at the end
GeoDrug <- ts %>% group_by(Patient_ID, Drug_ID) %>% summarise(purchases=sum(Dispensed_Qty)) %>% collect() %>%
  ungroup() %>% left_join(pt) %>% group_by(postcode, Drug_ID) %>% summarise(purchases=sum(purchases)) %>% 
  ungroup() %>% left_join(drugs, by=c('Drug_ID' = 'MasterProductID')) %>% select(postcode, Drug_ID, purchases, GenericIngredientName) %>%
  left_join(select(drugs, MasterProductID, ManufacturerName, StrengthCode, PackSizeNumber),
            by=c('Drug_ID' = 'MasterProductID')) %>% 
  left_join(select(chronic, ChronicIllness, MasterProductID), by=c('Drug_ID' = 'MasterProductID'))

# This groups up all of the drugs by Generic Ingredient and Strength, and calculations the times a particular drug was purchased
# It then finds the ratio of sales of each drug : total sales of drugs in that ingredient/strength combination 
Generics <- GeoDrug %>% group_by(GenericIngredientName, Drug_ID, StrengthCode, ChronicIllness, ManufacturerName) %>% summarise(purchases=sum(purchases)) %>% ungroup()
Generics <- Generics %>% group_by(GenericIngredientName, StrengthCode) %>% summarise(total=sum(purchases)) %>% ungroup() %>%
  right_join(Generics) %>% mutate(ProportionPop = purchases/total)

# This does a similar thing to the above, except at the postcode level
# It results in a dataframe of each postcode/drug, how often that drug was selected in the postcode, and how often that drug was selected in the population
GeoDrugProp <- GeoDrug %>% group_by(postcode, GenericIngredientName, StrengthCode) %>% summarise(total=sum(purchases)) %>% ungroup() %>%
  right_join(GeoDrug) %>% mutate(ProportionPostcode = purchases/total) %>% left_join(select(Generics, -total, -purchases))

# The total drug sales per manufacturer/postcode and how much you would expect to sell if that postcode sold in the population proportions
FavouredManuf <- GeoDrugProp %>% group_by(postcode, ManufacturerName) %>% 
  summarise(purchases=sum(purchases), expected=sum(total*ProportionPop)) %>%
  mutate(difference = purchases-expected, ratio = purchases/expected) %>% ungroup()

# Each manufacturer ordered by total sales (wholesale pharmacy price)
BigManuf <- ts %>% left_join(select(drugs, MasterProductID, ManufacturerName), by=c('Drug_ID' = 'MasterProductID'), copy=TRUE) %>%
  group_by(ManufacturerName) %>% summarise(sales = sum(WholeSalePrice_Amt*Dispensed_Qty)) %>% collect() %>% ungroup()  %>% 
  mutate(sales = ifelse(is.na(sales), 0, sales), 
         prop = sales / sum(BigManuf$sales, na.rm=TRUE)) %>%
  arrange(desc(sales))
BigManuf$cumul = cumsum(BigManuf$prop)
# The top 10 account for 67% of Aus Sales
top10 = BigManuf$ManufacturerName[1:10]

SubsetFavoured <- FavouredManuf %>% filter(ManufacturerName %in% top10) %>% mutate(postcode = as.numeric(postcode))
# Apotex Pty Ltd appears to be heavily overprescribed in some postcodes
ggplot(SubsetFavoured) + geom_point(aes(postcode, ratio, colour=ManufacturerName)) + labs(y='Ratio of Total : Expected Revenue')

