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
# patients_db$Patient_ID <- patients_db$`Patient_ID`
# colnames(patients)[1] <- "Patient_ID"
patients <- patients_db %>% collect(n = Inf) #%>% 
  #mutate(Patient_ID = as.integer(Patient_ID))
patients$Patient_ID <- as.integer(patients$Patient_ID)
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
  map(select, -c(ChronicIllness, Dispense_YrQtr)) 
postcode_illness_ts <- postcode_illness_ts %>% 
  lapply(function(x) ts(x, start = c(2011, 1), frequency = 4))

# devtools::install_github("earowang/tscognostics")
library(tscognostics)
library(plotly)

range01 <- function(x) { # normalise cognostics b/t 0 and 1
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

cogs_ts <- postcode_illness_ts %>% 
  map(tsmeasures)
cogs_ts_scaled <- cogs_ts %>% 
  map(~ apply(., 2, range01)) %>% # normalised with mean of 0, var of 1
  map(as.data.frame)
illness_names <- names(cogs_ts)
postcode_2 <- cogs_ts %>% 
  map(row.names)

cogs_df <- cogs_ts_scaled %>% 
  map2(illness_names, ~ mutate(.x, ChronicIllness = .y)) %>% 
  map2(postcode_2, ~ mutate(.x, Postcode = .y)) %>% 
  bind_rows() %>% 
  mutate(
    ChronicIllness = if_else(
      ChronicIllness == "Chronic Obstructive Pulmonary Disease (COPD)",
      "COPD", ChronicIllness)
  ) %>% 
  gather(Cognostics, Value, ACF1:trough) %>% 
  as_tibble()

cogs_df %>% 
  ggplot(aes(x = Cognostics, y = Value, group = Postcode)) +
  geom_jitter(size = 0.2) +
  facet_grid(ChronicIllness ~ .)
ggplotly()

top1_trend <- cogs_df %>%
  filter(Cognostics == "trend") %>%
  group_by(ChronicIllness) %>% 
  filter(row_number(Value) == n())

dispense_chronic_bypostcode_qtr %>% 
  filter(postcode %in% top1_trend$Postcode) %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ postcode, scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

dispense_chronic_bypostcode_qtr %>% 
  filter(postcode == "4350") %>% # highest in depression
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ ., scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

top1_entropy <- cogs_df %>%
  filter(Cognostics == "entropy") %>%
  group_by(ChronicIllness) %>% 
  filter(row_number(Value) == 1)

dispense_chronic_bypostcode_qtr %>% 
  filter(postcode %in% top1_entropy$Postcode) %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ postcode, scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

dispense_chronic_bypostcode_qtr %>% 
  filter(postcode == "3630") %>% # lowest in Urology
  ggplot(aes(x = Dispense_YrQtr, y = trans_count)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ ., scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

library(ggfortify)
library(gridExtra)
cogs_pca <- cogs_ts_scaled %>% 
  map(~ prcomp(., center = FALSE))
p_cogs_pca <- cogs_pca %>% 
  map(~ autoplot(., loadings = TRUE, loadings.label = TRUE))
grid.arrange(
  p_cogs_pca[[1]], p_cogs_pca[[2]], p_cogs_pca[[3]], p_cogs_pca[[4]],
  p_cogs_pca[[5]], p_cogs_pca[[6]], p_cogs_pca[[7]], p_cogs_pca[[8]],
  p_cogs_pca[[9]], p_cogs_pca[[10]], p_cogs_pca[[11]],
  nrow = 3, ncol = 4
)

## Look at purchase amout over time
purchase <- trans %>% 
  group_by(Dispense_Week) %>% 
  summarise(Amt = sum(PatientPrice_Amt)) %>% 
  collect(n = Inf)

purchase <- purchase %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

# Quarterly series
purchase_qtr <- purchase %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016) %>% 
  group_by(Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_amt = sum(Amt, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

purchase_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_amt)) +
  geom_line() +
  geom_point() +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

# By chronic illness or not
illness <- illness_db %>% collect(n = Inf)
illness <- illness %>% mutate(MasterProductID = as.integer(MasterProductID))
chronic_drugs <- illness$MasterProductID
purchase_bychronic <- trans %>% 
  mutate(Chronic = if_else(Drug_ID %in% chronic_drugs, "Y", "N")) %>% 
  group_by(Chronic, Dispense_Week) %>% 
  summarise(Amt = sum(PatientPrice_Amt)) %>% 
  collect(n = Inf)

purchase_bychronic <- purchase_bychronic %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

purchase_bychronic_qtr <- purchase_bychronic %>% 
  group_by(Chronic, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_amt = sum(Amt, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

purchase_bychronic_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_amt)) +
  geom_line() +
  geom_point() +
  facet_grid(Chronic ~ .) +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

# By different chronic illness
purchase_chronic_dat <- trans %>% 
  filter(Drug_ID %in% chronic_drugs) %>% 
  collect(n = Inf)

purchase_chronic <- purchase_chronic_dat %>% 
  left_join(illness, by = c("Drug_ID" = "MasterProductID")) %>% 
  group_by(ChronicIllness, Dispense_Week) %>% 
  summarise(Amt = sum(PatientPrice_Amt)) %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

purchase_chronic_qtr <- purchase_chronic %>% 
  group_by(ChronicIllness, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_amt = sum(Amt, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

purchase_chronic_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_amt)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ ., scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)

# Look at chronic illness by gender
patients <- patients_db %>% collect(n = Inf) %>% 
  mutate(Patient_ID = as.integer(Patient_ID))
purchase_chronic_full <- purchase_chronic_dat %>% 
  left_join(illness, by = c("Drug_ID" = "MasterProductID")) %>% 
  left_join(patients, by = "Patient_ID")

purchase_chronic_bygender <- purchase_chronic_full %>% 
  group_by(gender, ChronicIllness, Dispense_Week) %>% 
  summarise(Amt = sum(PatientPrice_Amt)) %>% 
  mutate(
    Dispense_Week = as_date(Dispense_Week),
    Dispense_Year = year(Dispense_Week),
    Dispense_Month = month(Dispense_Week, label = TRUE),
    Dispense_Qtr = quarter(Dispense_Week)
  ) %>% 
  filter(Dispense_Year >= 2011, Dispense_Year < 2016)

purchase_chronic_bygender_qtr <- purchase_chronic_bygender %>% 
  group_by(gender, ChronicIllness, Dispense_Year, Dispense_Qtr) %>% 
  summarise(trans_amt = sum(Amt, na.rm = TRUE)) %>% 
  mutate(Dispense_YrQtr = as.yearqtr(
    paste(Dispense_Year, Dispense_Qtr, sep = " Q"), "%Y Q%q")
  )

purchase_chronic_bygender_qtr %>% 
  ggplot(aes(x = Dispense_YrQtr, y = trans_amt)) +
  geom_line() +
  geom_point() +
  facet_grid(ChronicIllness ~ gender, scales = "free_y") +
  scale_x_yearqtr(format = "%Y Q%q", n = 5)
