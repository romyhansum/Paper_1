#Overview of planned expenditure in operational programmes, aggregated on regional/national level.
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)

op_react <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/fundsvar/data_ESI-funds.xlsx", sheet="EU-Countryoverview_10_22")

##bereits in Excel-Datei alle Kommas in Finanzspalten (O,P,Q) entfernen.

op_react <- op_react %>% 
  mutate(country_name=str_to_lower(country_name),
         total_expenditure_selected=str_replace_all(`total_eligible_cost_decided_(selected)`, ",",""),
         total_expenditure_selected=as.numeric(total_expenditure_selected),
         total_expenditure_spent=str_replace_all(total_eligible_spending, ",",""),
         total_expenditure_spent=as.numeric(total_expenditure_spent)) %>% 
  rename(total_EU_expenditure=EU_amount_planned,
         total_national_expenditure=National_Amount_planned,
         total_expenditure=Total_Amount_planned) %>% 
  select(-ms, -`total_eligible_cost_decided_(selected)`, -total_eligible_spending)

op_react <- op_react %>% 
  mutate(regionalisation=case_when(country_name=="italy" & political_level=="national" ~"No", #see notes: decision to not treat the distribution as regionalised one.
                                   country_name=="poland" & political_level=="national" ~"No", #see notes: decision to not treat the distribution as regionalised one.
                                   TRUE~regionalisation))

op_react_aggregated <- op_react %>% 
  group_by(political_level, country_id, country_name, regionalisation) %>% 
  summarise(eu_cofinancing_rate=sum(total_EU_expenditure)/sum(total_expenditure),
            total_EU_expenditure=sum(total_EU_expenditure),
            total_national_expenditure=sum(total_national_expenditure),
            total_expenditure=sum(total_EU_expenditure),
            total_expenditure_selected=sum(total_expenditure_selected),
            total_expenditure_spent=sum(total_expenditure_spent))

op_react_aggregated <- op_react_aggregated %>% 
  mutate(counted_twice="no")

#Austria has allocated certain budget shares of a national operational programme to regions.
#This distribution can be studied like regional operational programmes.
#However, when studying the total budget, these regional allocations have to be neglected (otherwise they are counted twice).

op_react_austria <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/fundsvar/data_ESI-funds.xlsx", sheet="Countryoverview_Austria_11_22")

op_react_austria <- op_react_austria %>% 
  mutate(counted_twice="yes") %>% 
  select(-ms, -region, -nuts_equivalent)

op_react_aggregated <- op_react_aggregated %>% 
  bind_rows(op_react_austria)

write_csv(op_react_aggregated, file.path("./data/processed/op_react_aggregated.csv"))
