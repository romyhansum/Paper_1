#Link the different data sets on funds, operational programmes, votes and government composition into one dataset.

library(tidyverse)

# 1. Link aggregated REACT-EU budget from operational programmes to gov data.####
reg_and_nat_governments_2021  <- read_csv("./data/processed/reg_and_nat_governments_2021.csv")
op_react_aggregated  <- read_csv("./data/processed/op_react_aggregated.csv")

op_react_aggregated <- op_react_aggregated %>% 
  rename(region=political_level)

gov_and_op_budget <- reg_and_nat_governments_2021 %>% 
  full_join(op_react_aggregated, by=c("region", "country_id", "country_name"))

gov_and_op_budget <- gov_and_op_budget %>% 
  mutate(regionalisation=case_when(country_name=="belgium" & is.na(regionalisation)~"not applicable", 
                                   country_name=="netherlands" & is.na(regionalisation)~"not applicable",
                                   country_name=="italy"~"no",
                                   is.na(regionalisation) ~"no",
                                   country_name=="austria" ~"yes",
                                   regionalisation=="Yes" ~"yes",
                                   regionalisation=="No" ~"no",
                                   TRUE~regionalisation),
         country_name_short=case_when(country_name=="france" & is.na(country_name_short)~"FRA",
                                      country_name=="netherlands" & is.na(country_name_short)~"NLD",
                                      TRUE~country_name_short)) %>% 
  mutate(total_EU_expenditure = case_when(regionalisation=="no" ~ 0,
                                          regionalisation=="not applicable" ~ 0,
                                          TRUE~ total_EU_expenditure)) %>% 
  select(-election_id_regional, -start_date_regional, -cabinet_name_regional, -cabinet_id_regional, -previous_cabinet_id_regional, -party_prime_min_english_regional, -left_right_prime_min_regional)

write_csv(gov_and_op_budget, file.path("./data/processed/gov_and_op_budget.csv"))


# 2. Link vote data (EU-NED database) with gov data.#### 

gov_and_op_budget <- read.csv("./data/processed/gov_and_op_budget.csv")
vote_nuts_2021 <- read.csv("./data/processed/vote_nuts_2021.csv")

gov_vote_op_budget <- vote_nuts_2021 %>% 
  filter(regionname2!="Abroad votes") %>%  #not interesting for this research purpose, as abroad regions do not receive funds.
  filter(nuts2!="BGZZ") %>% #Abroad votes
  filter(nuts2!="HRZZ") %>% #Abroad votes
  filter(nuts2!="LTZZ") %>% #Abroad votes
  filter(nuts2!="LVZZ") %>% #Abroad votes
    mutate(political_region_nuts_level=as.character(political_region_nuts_level)) %>% 
  left_join(gov_and_op_budget, by=c("political_region_nuts", "reference_year", "political_region_nuts_level"))

gov_vote_op_budget <- gov_vote_op_budget %>% 
  select(-country_name, - country_name_short, -year, -regionname2, -type, -election_date, -party_voteshare) %>% 
  rename(political_region=region) %>% 
  select(country_code, country, country_id, nuts2, reference_year, election_date_national, election_id_national, everything())

gov_vote_op_budget <- gov_vote_op_budget %>% 
  mutate(govparty_national=case_when(party_id==govparty1_national ~1,
                                     party_id==govparty2_national ~1,
                                     party_id==govparty3_national ~1,
                                     party_id==govparty4_national ~1,
                                     party_id==govparty5_national ~1,
                                     party_id==govparty6_national ~1,
                                     party_id==govparty7_national ~1,
                                     TRUE~0),
         pm_national=case_when(party_id==prime_min_party_national~1,
                               TRUE~0))

gov_vote_op_budget_aland <- gov_vote_op_budget %>% #in aland, no votes for the government coalition parties.
  filter(nuts2=="FI20") %>% 
  group_by(nuts2, reference_year, country_code, country, country_id, election_date_national, election_id_national, election_date_regional,
           electorate, total_vote, valid_vote, turnout, political_region_nuts_level, political_region_nuts, political_region,
           form_of_government, start_date_national, cabinet_name_national, cabinet_id_national, previous_cabinet_id_national,
           caretaker_regional, caretaker_national, govparty1_regional, govparty1_national, govparty2_regional, govparty2_national,
           govparty3_regional, govparty3_national, govparty4_regional, govparty4_national, govparty5_regional, govparty5_national,
           govparty6_regional, govparty6_national, govparty7_regional, govparty7_national, prime_min_party_regional, prime_min_party_national,
           party_prime_min_english_national, party_prime_min_short_national, party_prime_min_short_regional, left_right_prime_min_national,
           firstyear_incumbent_regional, firstyear_incumbent_national, alignment1, alignment2, alignment3, regionalisation, eu_cofinancing_rate,
           total_EU_expenditure, total_national_expenditure, total_expenditure, total_expenditure_selected, total_expenditure_spent) %>% 
  summarize(gov_votes=0,
            pm_votes=0)

gov_vote_op_budget1 <- gov_vote_op_budget %>% 
  filter(govparty_national==1) %>% 
  group_by(nuts2, reference_year, country_code, country, country_id, election_date_national, election_id_national, election_date_regional,
           electorate, total_vote, valid_vote, turnout, political_region_nuts_level, political_region_nuts, political_region,
           form_of_government, start_date_national, cabinet_name_national, cabinet_id_national, previous_cabinet_id_national,
           caretaker_regional, caretaker_national, govparty1_regional, govparty1_national, govparty2_regional, govparty2_national,
           govparty3_regional, govparty3_national, govparty4_regional, govparty4_national, govparty5_regional, govparty5_national,
           govparty6_regional, govparty6_national, govparty7_regional, govparty7_national, prime_min_party_regional, prime_min_party_national,
           party_prime_min_english_national, party_prime_min_short_national, party_prime_min_short_regional, left_right_prime_min_national,
           firstyear_incumbent_regional, firstyear_incumbent_national, alignment1, alignment2, alignment3, regionalisation, eu_cofinancing_rate, 
           total_EU_expenditure, total_national_expenditure,total_expenditure, total_expenditure_selected, total_expenditure_spent) %>% 
  summarise(gov_votes=sum(party_vote)) 


gov_vote_op_budget2 <- gov_vote_op_budget %>% 
  filter(pm_national==1) %>% 
  group_by(nuts2, reference_year) %>% 
  summarise(pm_votes=sum(party_vote))

gov_vote_op_budget <- gov_vote_op_budget1 %>% 
  left_join(gov_vote_op_budget2, by=c("nuts2", "reference_year")) %>% 
  bind_rows(gov_vote_op_budget_aland) %>% 
  mutate(gov_voteshare=gov_votes/valid_vote,
         pm_voteshare=pm_votes/valid_vote) %>% 
  mutate(gov_voteshare=case_when(is.na(gov_voteshare)~0,
                                 TRUE~gov_voteshare),
         pm_voteshare=case_when(is.na(pm_voteshare)~0,
                                TRUE~pm_voteshare))

write_csv(gov_vote_op_budget, file.path("./data/processed/gov_vote_op_budget.csv"))

# 3. Link funds data to gov data.####
funds_aggregated  <- read_csv("./data/processed/REACT-EU-funds_aggregated.csv")
gov_vote_op_budget  <- read_csv("./data/processed/gov_vote_op_budget.csv")

funds_aggregated <- funds_aggregated %>% 
  rename(country=country_name,
         nuts2=nuts_2) %>% 
  filter(nuts2!="higher NUTS") %>% 
  filter(nuts2!="multiple NUTS")

gov_vote_op_funds <- gov_vote_op_budget %>% 
  left_join(funds_aggregated, by=c("nuts2", "country", "country_id")) %>% 
  select(-total_expenditure_selected, -total_expenditure_spent, -total_expenditure, -total_national_expenditure, -eu_cofinancing_rate) %>% 
  rename(nuts=nuts2) %>%
  rename(regional_react_budget=total_EU_expenditure,
         react_funds=sum_EU_means,
         eu_cofinancing_rate=mean_cofinancing_rate) %>% 
  mutate(number_of_projects=case_when(is.na(number_of_projects) ~ 0,
                                      TRUE ~ number_of_projects))

write_csv(gov_vote_op_funds, file.path("./data/processed/gov_vote_budget_funds.csv"))

# 4. Link socioeconomic data to gov-vote-budgetfunds-data####
gov_vote_budget_funds  <- read_csv("./data/processed/gov_vote_budget_funds.csv")
population  <- read_csv("./data/processed/population.csv")

population <- population %>% 
  select(-region) %>% 
  filter(nuts!="SI04") %>% #treated as one NUTS-region in the EU-NED data set.
  mutate(population=case_when(nuts=="SI03"~1003931+1105046,
         TRUE~population),
         nuts=case_when(nuts=="SI03"~"SI00",
                        TRUE~nuts))

gov_vote_budget_funds <- gov_vote_budget_funds %>% 
  full_join(population, by="nuts") %>% 
  mutate(funds_pc=react_funds/population)

write_csv(gov_vote_budget_funds, file.path("./data/processed/gov_vote_budget_funds.csv"))


#5. Link political data to gov-vote-budgetfunds-data
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)
