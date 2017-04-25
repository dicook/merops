library(zoo)
library(RSQLite)
library(forcats)
library(tidyverse)
library(lubridate)

db <- src_sqlite("MelbDatathon2017.sqlite", create = FALSE)
trans <- tbl(db, "Transactions")
drugs <- tbl(db, "Drugs")
illness_db <- tbl(db, "ChronicIllness")

## Look at purchase behaviour over time
dispense <- trans %>% 
  group_by(Dispense_Week) %>% 
  summarise(n = count()) %>% 
  collect(n = Inf)

dispense <- dispense %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE)
  )

dispense_month <- dispense %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016) %>% 
  group_by(Dispense_Year, Dispense_Month) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrMon = as.yearmon(
    paste(Dispense_Year, Dispense_Month, sep = "-"), "%Y-%b")
  )

# 2014-11, 2015-05 are unusally high, the end of 2014 & 2015 seems changed.
dispense_month %>% 
  ggplot(aes(x = Dispense_YrMon, y = trans_count)) +
  geom_line() +
  geom_point() +
  scale_x_yearmon(n = 10)

# By chronic illness or not
illness <- illness_db %>% collect(n = Inf)
illness <- illness %>% mutate(MasterProductID = as.integer(MasterProductID))
chronic_drugs <- illness$MasterProductID
dispense_bychronic <- trans %>% 
  mutate(Chronic = if_else(Drug_ID %in% chronic_drugs, "Y", "N")) %>% 
  group_by(Chronic, Dispense_Week) %>% 
  tally() %>% 
  collect(n = Inf)

dispense_bychronic <- dispense_bychronic %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE)
  )

dispense_bychronic_month <- dispense_bychronic %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016) %>% 
  group_by(Chronic, Dispense_Year, Dispense_Month) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrMon = as.yearmon(
    paste(Dispense_Year, Dispense_Month, sep = "-"), "%Y-%b")
  )

dispense_bychronic_month %>% 
  # filter(Chronic == "Y") %>% 
  ggplot(aes(x = Dispense_YrMon, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(Chronic ~ .) +
  scale_x_yearmon(n = 10)

dispense_chronic <- trans %>% 
  filter(Drug_ID %in% chronic_drugs) %>% 
  collect(n = Inf)

dispense_chronic <- dispense_chronic %>% 
  left_join(illness, by = c("Drug_ID" = "MasterProductID")) %>% 
  group_by(ChronicIllness, Dispense_Week) %>% 
  tally()

dispense_chronic <- dispense_chronic %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE)
  )

dispense_chronic_month <- dispense_chronic %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016) %>% 
  group_by(ChronicIllness, Dispense_Year, Dispense_Month) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrMon = as.yearmon(
    paste(Dispense_Year, Dispense_Month, sep = "-"), "%Y-%b")
  )

dispense_chronic_month %>% 
  ggplot(aes(x = Dispense_YrMon, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ ., scales = "free_y") +
  scale_x_yearmon(n = 10)

dispense_chronic_month %>% 
  ggplot(aes(x = Dispense_YrMon, y = trans_count, fill = ChronicIllness)) +
  geom_bar(stat = "identity", position = "fill")

## Which drug is consumed the most, by quantities? by amount?
drug_count <- trans %>% 
  group_by(Drug_ID) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  collect()

drug_100 <- drug_count %>% 
  mutate(Drug_ID = as.character(Drug_ID)) %>% 
  top_n(100)

drug_100_id <- drug_100$Drug_ID
drugs_100 <- drugs %>% 
  filter(MasterProductID %in% drug_100_id) %>% 
  select(MasterProductID, MasterProductFullName) %>% 
  collect()
illness_100 <- illness %>% 
  filter(MasterProductID %in% drug_100_id) %>% 
  collect()

drug_100 %>% 
  left_join(drugs_100, by = c("Drug_ID" = "MasterProductID")) %>% 
  ggplot(aes(x = reorder(MasterProductFullName, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

drug_100 %>% 
  left_join(illness_100, by = c("Drug_ID" = "MasterProductID")) %>% 
  ggplot(aes(x = reorder(ChronicIllness, -n), y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
