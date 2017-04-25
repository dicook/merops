# This will create an sqlite database called MelbDatathon2017.sqlite
# in the your current project directory
library(sqldf)
#library(RMySQL)
library(DBI)
library(readr)
library(dplyr)
db <- dbConnect(SQLite(), dbname="MelbDatathon2017.sqlite")
p1 <- read_tsv("Transactions/patients_1.txt", 
               col_types="iiiiccccciiiidddiii")
dbWriteTable(conn=db, name="Transactions", value=p1)
for (i in 2:50) {
  nm <- paste0("Transactions/patients_",i,".txt")
  p1 <- read_tsv(nm, col_types="iiiiccccciiiidddiii")
  dbWriteTable(conn=db, name="Transactions", value=p1, append=TRUE)
  cat(i,"\n")
}

# This data was distributed in a second .zip file
for (i in 1:50) {
  nm <- paste0("MISSING_TRANSACTIONS/missing_patients_",i,".txt")
  #Slightly different format: IsDeferredScript given as True/False
  p1 <- read_tsv(nm, col_types="iiiicccccliiidddiii") %>%
      mutate(IsDeferredScript = as.integer(IsDeferredScript))
  dbWriteTable(conn=db, name="Transactions", value=p1, append=TRUE)
  cat(i,"\n")
}

dbListTables(db)
dbListFields(db, "Transactions")
#dbRemoveTable(db, "Transactions")

# Create patient, stores, drug, ChronicIllness, ATC tables
patients <- read_tsv("Lookups/patients.txt", 
                     col_types="ccic")
dbWriteTable(conn=db, name="Patients", value=patients)
dbListTables(db)
dbListFields(db, "Patients")

stores <- read_tsv("Lookups/stores.txt", 
                   col_types="ccci")
dbWriteTable(conn=db, name="Stores", value=stores)
dbListTables(db)
dbListFields(db, "Stores")

drugs <- read_tsv("Lookups/Drug_LookUp.txt", 
                  col_types = "ccccccicccccccdccccc")
dbWriteTable(conn=db, name="Drugs", value=drugs)
dbListTables(db)
dbListFields(db, "Drugs")

ills <- read_tsv("Lookups/ChronicIllness_LookUp.txt", 
                 col_types="ccc")
dbWriteTable(conn=db, name="ChronicIllness", value=ills)
dbListTables(db)
dbListFields(db, "ChronicIllness")

ATC <- read_tsv("Lookups/ATC_LookUp.txt", 
                col_types="cccccccccc")
dbWriteTable(conn=db, name="ATC", value=ATC)
dbListTables(db)
dbListFields(db, "ATC")

library(dplyr)
library(ggplot2)
library(lubridate)
md <- src_sqlite("MelbDatathon2017.sqlite", create=FALSE)
ts <- tbl(md, "Transactions")
colnames(ts)
d <- ts %>% group_by(Prescription_Week) %>% tally() %>% collect()
d <- d %>% mutate(Prescription_Week=ymd(Prescription_Week))
ggplot(d) + geom_line(aes(x=Prescription_Week, y=n)) 
