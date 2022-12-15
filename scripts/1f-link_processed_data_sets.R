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
           party_prime_min_english_national, party_prime_min_short_national, party_prime_min_short_regional, left_right_prime_min_national, eu_anti_pro_prime_min_national,
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
           party_prime_min_english_national, party_prime_min_short_national, party_prime_min_short_regional, left_right_prime_min_national, eu_anti_pro_prime_min_national,
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
##Falls doch nicht nur 2020 socioeconomic variables, dann hier jeweils filter auf reference_year=2020 löschen, und bei left-Join, ref_year ergänzen.
gov_vote_budget_funds  <- read_csv("./data/processed/gov_vote_budget_funds.csv")
population  <- read_csv("./data/processed/population.csv")
population_pooled  <- read_csv("./data/processed/population_pooled.csv")
ue <- read_csv("./data/processed/ue.csv")
ue_pooled <- read_csv("./data/processed/ue_pooled.csv")
gdppc <- read_csv("./data/processed/gdppc.csv")
covid_cases <- read_csv("./data/processed/covid_cases.csv")


population <- population %>% 
  filter(nuts!="SI04") %>% #treated as one NUTS-region in the EU-NED data set.
  filter(nuts!="SI03") %>% 
  mutate(nuts=case_when(nuts=="SI0"~"SI00",
                        TRUE~nuts),
         reference_year=as.integer(reference_year))

gov_vote_budget_funds <- gov_vote_budget_funds %>% 
  full_join(population, c("nuts", "reference_year")) %>% 
  full_join(population_pooled, c("political_region_nuts", "reference_year")) %>% 
  mutate(react_funds_pc=react_funds/population,
         regional_react_budget_pc=regional_react_budget/population_political_region)

ue <- ue %>% 
  filter(nuts!="SI04") %>% #treated as one NUTS-region in the EU-NED data set.
  filter(nuts!="SI03") %>% 
  filter(reference_year=="2020") %>% 
  mutate(nuts=case_when(nuts=="SI0"~"SI00",
                        TRUE~nuts)) %>% 
  select(-reference_year)

ue_pooled <- ue_pooled %>% 
  filter(reference_year=="2020") %>% 
  select(-reference_year)

gdppc <- gdppc %>% 
  filter(nuts!="SI04") %>% #treated as one NUTS-region in the EU-NED data set.
  filter(nuts!="SI03")

covid_cases <- covid_cases %>% 
  filter(reference_year=="2020")%>% 
  select(-reference_year)

gov_vote_budget_funds <- gov_vote_budget_funds %>% 
  left_join(ue, by=c("nuts")) %>% 
  left_join(ue_pooled, by="political_region_nuts") %>% 
  left_join(gdppc, by=c("nuts")) %>% 
  left_join(covid_cases, by=c("nuts"))


#5. Link political data to gov-vote-budgetfunds-data####
qog_national  <- read_csv("./data/processed/qog_national.csv")
rai <- read_csv("./data/processed/rai.csv")

gov_vote_budget_funds <- gov_vote_budget_funds %>% 
  left_join(qog_national, by=c("country_code")) %>% 
  left_join(rai, by="country") %>% 
  mutate(electoral_system=case_when(country %in% c("austria", "belgium", "bulgaria", "croatia", "cyprus", "czech republic",
                                                   "denmark", "estonia", "finland", "greece", "ireland", "italy", "latvia",
                                                   "luxembourg", "malta", "netherlands", "poland", "portugal", "romania", 
                                                   "slovakia", "slovenia", "spain", "sweden")~"prv", 
                                    country=="france" ~ "mv",
                                    country %in% c("germany", "hungary", "lithuania")  ~ "mixed")) %>% 
  rename(covid_rate_per_100k=rate_per_100k) %>% 
  select(nuts, reference_year, country_code, country, country_id, react_funds, react_funds_pc, eu_cofinancing_rate, number_of_projects, regionalisation, regional_react_budget, 
         regional_react_budget_pc, electoral_system, election_date_national, election_id_national,electorate, total_vote, valid_vote, turnout, start_date_national, firstyear_incumbent_national, 
         cabinet_name_national, cabinet_id_national, previous_cabinet_id_national, caretaker_national, govparty1_national, govparty2_national, govparty3_national, govparty4_national,
         govparty5_national, govparty6_national, govparty7_national, prime_min_party_national, gov_votes, pm_votes, gov_voteshare, pm_voteshare,party_prime_min_english_national, 
         party_prime_min_short_national,left_right_prime_min_national, eu_anti_pro_prime_min_national, election_date_regional, firstyear_incumbent_regional, political_region_nuts_level, 
         political_region_nuts, political_region, caretaker_regional, govparty1_regional, govparty2_regional, govparty3_regional, govparty4_regional, govparty5_regional,
         govparty6_regional, govparty7_regional, prime_min_party_regional, party_prime_min_short_regional, alignment1, alignment2, alignment3, population, area,
         pop_density, capital_region, population_political_region, area_political_region, pop_density_political_region, capital_political_region, ue_rate, ue_change_1920, 
         ue_rate_political_region, ue_change_1920_political_region, gdppc, gdp_change_1920, gdppc_political_region, gdp_change_1920_political_region, covid_rate_per_100k, 
         rate_per_100k_political_region, eqi_score_national, rai)

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

write_csv(gov_vote_budget_funds, file.path("./data/processed/gov_vote_budget_funds.csv"))