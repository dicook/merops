library(zoo)
library(RSQLite)
library(forcats)
library(tidyverse)
library(lubridate)

db <- src_sqlite("MelbDatathon2017.sqlite", create = FALSE)
trans <- tbl(db, "Transactions")
drugs <- tbl(db, "Drugs")
illness_db <- tbl(db, "ChronicIllness")
patients_db <- tbl(db, "Patients")

## Look at purchase behaviour over time
dispense <- trans %>% 
  group_by(Dispense_Week) %>% 
  summarise(n = count()) %>% 
  collect(n = Inf)

dispense <- dispense %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

# Look at weekly series
dispense %>% 
  group_by(Dispense_Week) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  ggplot(aes(x = Dispense_Week, y = trans_count)) +
  geom_line() +
  geom_point()

# Look at montly series
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

# Quarterly series
dispense_qtr <- dispense %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016) %>% 
  group_by(Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

dispense_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

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
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

dispense_bychronic_qtr <- dispense_bychronic %>% 
  group_by(Chronic, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

dispense_bychronic_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(Chronic ~ .) +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

# By different chronic illness
dispense_chronic_dat <- trans %>% 
  filter(Drug_ID %in% chronic_drugs) %>% 
  collect(n = Inf)

dispense_chronic <- dispense_chronic_dat %>% 
  left_join(illness, by = c("Drug_ID" = "MasterProductID")) %>% 
  group_by(ChronicIllness, Dispense_Week) %>% 
  tally() %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

dispense_chronic_qtr <- dispense_chronic %>% 
  group_by(ChronicIllness, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

dispense_chronic_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ ., scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

# Look at chronic illness by gender
patients <- patients_db %>% collect(n = Inf) %>% 
  mutate(Patient_ID = as.integer(Patient_ID))
dispense_chronic_full <- dispense_chronic_dat %>% 
  left_join(illness, by = c("Drug_ID" = "MasterProductID")) %>% 
  left_join(patients, by = "Patient_ID")

dispense_chronic_bygender <- dispense_chronic_full %>% 
  group_by(gender, ChronicIllness, Dispense_Week) %>% 
  tally() %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

dispense_chronic_bygender_qtr <- dispense_chronic_bygender %>% 
  group_by(gender, ChronicIllness, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

dispense_chronic_bygender_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ gender, scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

dispense_chronic_bypostcode <- dispense_chronic_full %>% 
  group_by(postcode, ChronicIllness, Dispense_Week) %>% 
  tally() %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

dispense_chronic_bypostcode_qtr <- dispense_chronic_bypostcode %>% 
  group_by(postcode, ChronicIllness, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_count = sum(n, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

postcode_illness <- dispense_chronic_bypostcode_qtr %>% 
  ungroup() %>% 
  select(postcode, ChronicIllness, Dispense_YrQtr, trans_count) %>%  
  split(.$ChronicIllness) %>% 
  map(spread, postcode, trans_count, fill = 0)

postcode_illness_ts <- postcode_illness %>% 
  map(select, -c(ChronicIllness, Dispense_YrQtr)) %>% 
  map(ts, start = c(2011, 1), frequency = 4)

# devtools::install_github("earowang/tscognostics")
library(tscognostics)
library(plotly)
cogs_ts <- postcode_illness_ts %>% 
  map(tsmeasures) %>% 
  map(as.data.frame) %>% 
  map(select, -c(relativeNA, fixedNA))
illness_names <- names(cogs_ts)
postcode_2 <- cogs_ts %>% 
  map(row.names)

cogs_df <- cogs_ts %>% 
  map2(illness_names, ~ mutate(.x, ChronicIllness = .y)) %>% 
  map2(postcode_2, ~ mutate(.x, Postcode = .y)) %>% 
  bind_rows() %>% 
  gather(Cognostics, Value, ACF1:trough) %>% 
  unnest(Value) %>% 
  as_tibble()

cogs_df %>% 
  # filter(ChronicIllness == "Diabetes") %>% 
  ggplot(aes(x = "1", y = Value, group = Postcode)) +
  geom_jitter() +
  facet_grid(ChronicIllness ~ Cognostics, scales = "free_y")

cogs_df %>% 
  filter(Cognostics == "spikiness") %>% 
  ggplot(aes(x = "1", y = Value, group = Postcode)) +
  geom_jitter() +
  facet_wrap(~ ChronicIllness, scales = "free_y")
ggplotly()

dispense_chronic_bypostcode_qtr %>% 
  filter(postcode == "4105", ChronicIllness == "Diabetes") %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)