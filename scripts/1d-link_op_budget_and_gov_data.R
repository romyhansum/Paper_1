#Link aggregated REACT-EU budget from operational programmes to gov data.

library(tidyverse)

reg_and_nat_governments_2122  <- read_csv("./data/processed/reg_and_nat_governments_2122.csv")
op_react_aggregated  <- read_csv("./data/processed/op_react_aggregated.csv")

op_react_aggregated <- op_react_aggregated %>% 
  rename(region=political_level)

gov_and_op_budget <- reg_and_nat_governments_2122 %>% 
  full_join(op_react_aggregated, by=c("region", "country_id", "country_name"))

gov_and_op_budget <- gov_and_op_budget %>% 
  mutate(regionalisation=case_when(country_name=="belgium" & is.na(regionalisation)~"not applicable",
                                   country_name=="netherlands" & is.na(regionalisation)~"not applicable",
                                   country_name=="italy"~"no",
                                   is.na(regionalisation) ~"No",
                                   TRUE~regionalisation),
         country_name_short=case_when(country_name=="france" & is.na(country_name_short)~"FRA",
                                      country_name=="netherlands" & is.na(country_name_short)~"NLD",
                                      TRUE~country_name_short))

write_csv(gov_and_op_budget, file.path("./data/processed/gov_and_op_budget.csv"))
