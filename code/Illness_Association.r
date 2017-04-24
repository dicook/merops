library(sqldf)
library(tidyverse)
# run these lines from chronic_illness.Rmd to get data needed
md <- src_sqlite("MelbDatathon2017.sqlite", create=FALSE)
src_tbls(md)

drug_illness <- tbl(md,"ChronicIllness") %>%
  transmute(illness=ChronicIllness, Drug_ID=as.integer(MasterProductID)) %>%
  collect(n=Inf) %>%
  mutate(illness=ifelse(illness=="Chronic Obstructive Pulmonary Disease (COPD)","COPD",illness))

patient_drug <- tbl(md,"Transactions") %>%
  count(Patient_ID, Drug_ID) %>%
  collect(n=Inf) 

 patient_illness <- inner_join(ungroup(patient_drug), drug_illness, by="Drug_ID") %>%
  distinct(Patient_ID, illness)

co_illness <- rename(patient_illness, illness1=illness) %>%
  inner_join(rename(patient_illness, illness2=illness), by="Patient_ID") %>%
  count(illness1,illness2) %>% ungroup()

# For each illness estimate the marginal probability of a patient having the illness
# estimate = number of patients with illness / number of patients
illnesses = data.frame(type=unique(drug_illness$illness))
for(i in 1:nrow(illnesses)){
  illnesses$proportion[i] = (patient_illness %>% filter(illness == illnesses$type[i]) %>% nrow())/558352
}

# Assuming independence between illnesses, the joint probability of a patient with two illnesses is the product of marginals.
expected_proportions = illnesses$proportion %*% t(illnesses$proportion)
# Replace the diagonal with the marginals instead of marginals squared.
diag(expected_proportions) = illnesses$proportion
colnames(expected_proportions) = rownames(expected_proportions) = illnesses$type
# Calculate expected number of each pair = probability * number of patients
expected_cases = expected_proportions * 558352

# Add the expected number of patients for each pair of illnesses
co_illness$expected = 0
for(i in 1:nrow(co_illness)){
  co_illness$expected[i] = expected_cases[co_illness$illness1[i], co_illness$illness2[i]]
}

# See which pairs were more likely to co-exist than expected
co_illness$proportion = co_illness$n / co_illness$expected

ggplot(co_illness, aes(x=reorder(illness1,-n), y=reorder(illness2,-n), fill=proportion-1)) +    
  geom_tile(color="black") + coord_fixed() +
  scale_fill_viridis(name="Association") +
  theme_minimal() + theme( axis.text.x=element_text(angle=90,vjust=0.5,hjust=1) ) +
  labs(x="",y="")







