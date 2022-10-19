#Data analysis - Paper 1.
#Objective: reshape ParlGov data and link it with own collected regional data set.
library(tidyverse)
library(lubridate)
cabinet <- read_csv("./data/raw/view_cabinet.csv")

#Make date set smaller.
(table(cabinet$country_name))

cabinet <- cabinet%>%
  mutate(election_date=ymd(election_date)) %>%
  filter(election_date>="2013-01-01") %>%
  filter(!(country_name=="Australia" | country_name=="Canada" | country_name=="Iceland" | country_name=="Switzerland" | country_name=="United Kingdom" | country_name=="Japan" | country_name=="Norway" | country_name=="New Zealand" | country_name=="Turkey" | country_name=="Israel"))


##Reshape data set in order to have one row per cabinet.

cabinet1 <- cabinet %>%
  filter(cabinet_party==1) %>%
  group_by(cabinet_id)%>%
  group_size()

#Maximum 7 parties in government coalition.

cabinet1 <- cabinet %>%
  filter(cabinet_party==1) %>%
  mutate(govparty1=NA,
         govparty2=NA,
         govparty3=NA,
         govparty4=NA,
         govparty5=NA,
         govparty6=NA,
         govparty7=NA,
         number=1)

cabinet1 <- cabinet1 %>%
  group_by(cabinet_id) %>%
  mutate(counter=cumsum(number)) %>%
  mutate(govparty1=if_else(counter==1, party_id, NULL),
         govparty2=if_else(counter==2, party_id, NULL),
         govparty3=if_else(counter==3, party_id, NULL),
         govparty4=if_else(counter==4, party_id, NULL),
         govparty5=if_else(counter==5, party_id, NULL),
         govparty6=if_else(counter==6, party_id, NULL),
         govparty7=if_else(counter==7, party_id, NULL)) %>%
  fill(govparty1,.direction="downup") %>%
  fill(govparty2,.direction="downup") %>%
  fill(govparty3,.direction="downup") %>%
  fill(govparty4,.direction="downup") %>%
  fill(govparty5,.direction="downup") %>%
  fill(govparty6,.direction="downup") %>%
  fill(govparty7,.direction="downup") %>%
  filter(counter==1) %>%
  select(-counter, -election_seats_total, -cabinet_party, -number, -prime_minister, -seats, -party_id, -party_name_short, -party_name, -party_name_english, -left_right)
  
cabinet2 <- cabinet %>%
  filter(prime_minister==1) %>%
  mutate(prime_min_party=party_id) %>%
  rename(left_right_prime_min="left_right",
         party_prime_min_short="party_name_short",
         party_prime_min_english="party_name_english") %>%
  select(-cabinet_party, -prime_minister, -seats, -party_id, -party_name, -country_name_short, -country_name, -country_id, -election_date, -start_date, -cabinet_name, -caretaker, -election_seats_total, -election_id, -previous_cabinet_id)

cabinet1 <- cabinet1 %>%
  full_join(cabinet2, by="cabinet_id") %>%
  mutate(region="national")

#### final Absatz zu regional_gov löschen bis auf letzte read_csv Zeile ####
##parties Datei wurde aktualisiert mit ParlGov 2022 Version.
library(readxl)
regional_gov <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/politicalvar/data_politicalvariables.xlsx")
regional_gov <- regional_gov %>%
  select(-`in EU-NED`, -comments) %>%
  filter(election_level=="regional elections")

library(haven)
parties <- read_dta("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/politicalvar/parlgov/parties.dta")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_primeminister"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(prime_min_party="id")%>%
  rename(party_prime_min_short="party_primeminister")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov1"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty1="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov2"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty2="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov3"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty3="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov4"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty4="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov5"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty5="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov6"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty6="id")

regional_gov <- regional_gov %>%
  left_join(parties, by=c("party_gov7"="name_short","country_id"="country_id")) %>%
  select(-family_id, -name_english, -name,  -name_ascii, -name_nonlatin,  -wikipedia, -data_source, -description, -comment, -cmp, -euprofiler, -ees, -morgan, -castles_mair, -huber_inglehart, -ray, -benoit_laver, -chess, -old_countryid, -old_partyid) %>%
  rename(govparty7="id")

regional_gov <- regional_gov %>%
  mutate(election_date=ymd(election_date),
         country_name=str_to_lower(country_name)) %>%
  select(-country_abbrev, -party_gov1, -party_gov2, -party_gov3, -party_gov4, -party_gov5, -party_gov6, -party_gov7)
  
write_csv(regional_gov, file.path("./data/raw/","regional_governments.csv"))
#### bis hierhin löschen ####
regional_gov <- read_csv("./data/raw/regional_governments.csv")

#Objective: Add own data collection on regional governments to the reshaped ParlGov data.
#Party names and IDs in party_gov and party_primeminister are the same as used in ParlGov Version 2022.
#If the regional parties did not appear in the ParlGov data, an additional, not yet attributed ID (starting from ID=3000) was added.

#Some information are added to the ParlGov data. General elections are more precisely "national lower house elections" (ParlGov dataset).

cabinet1 <- cabinet1 %>%
  mutate(country_name=str_to_lower(country_name),
         election_level="general elections")

cabinet_nat_and_reg <- cabinet1 %>%
  bind_rows(regional_gov) %>%
  group_by(country_id) %>%
  fill(form_of_government,.direction="downup") %>%
  fill(country_name_short,.direction="downup") %>%
  mutate(form_of_government=case_when(
    country_name=="bulgaria" | country_name=="croatia"| country_name=="czech republic" | country_name=="estonia" | country_name=="finland" | country_name=="greece" | 
      country_name=="hungary" | country_name=="ireland" |country_name=="latvia" | country_name=="malta" | country_name=="slovakia" | country_name=="slovenia"  ~"parliamentary republic",
    country_name=="luxembourg" | country_name=="sweden" ~"parliamentary monarchy",
    country_name=="cyprus" ~ "presidential republic",
    country_name=="lithuania" | country_name=="portugal" | country_name=="romania" ~ "semipresidential",
  TRUE ~ form_of_government)) %>%
  ungroup()

#Cabinet change within one election term is specified with cabinet_change==1.
#Cabinet change within the same year is specified with cabinet_change_withinyear==1.
#If several within-term cabinet changes occur during one year, only the last cabinet is taken into account and only for the last the relevant_cabinet==1.
#If a government is formed in an election year and (without new election), another gov in the subsequent year, the latter gov is used.
#Summarized, firstyear_incumbent reflects the de facto first year of incumbency (e.g., one year after election year if gov was created in it).
#firstyear_incumbent==0 if cabinet is not relevant (e.g., first cabinet of two within one year without election)
cabinet_nat_and_reg <- cabinet_nat_and_reg %>%
  mutate(number=1) %>%
  group_by(election_id, election_date, country_name, region) %>%
  mutate(cabinet_change= case_when(n()>1~1, TRUE~0)) %>%
  arrange(start_date)%>%
  mutate(cabinetinelectionyear=case_when(year(start_date)==year(election_date)~TRUE,
                                         TRUE~FALSE)) %>%
  mutate(cabinet_change_withinyear = case_when((year(start_date) - lag(year(start_date)))==0~1, 
                                               (year(start_date) - lead(year(start_date)))==0~1,
                                               ((year(start_date)+1) - lead(year(start_date))==0)&(cabinetinelectionyear==TRUE)~1, #government formed in election year competes with government of the subsequent year, is counted here as a de facto within year cabinet change.
                                               ((year(start_date) - (lag(year(start_date))+1))==0)&(cabinetinelectionyear==TRUE)~1, #same as line above.
                                               TRUE~0), 
         counter=cumsum(number)) %>%
  ungroup() %>%
  group_by(election_id, election_date, country_name, region, cabinet_change_withinyear) %>%
  arrange(counter) %>%
  mutate(relevant_cabinet= case_when(cabinet_change==0 ~1, 
                                     (cabinet_change==1 & cabinet_change_withinyear==0) ~1,
                                     (cabinet_change==1 & cabinet_change_withinyear==1 & counter==max(counter))~1,
                                      TRUE~0)) %>%
  ungroup() 

cabinet_nat_and_reg <- cabinet_nat_and_reg %>%
  mutate(election_date=ymd(election_date)) %>%
  mutate(electionyearandone=(year(election_date)+1),
         start_year=year(start_date)) %>%
  mutate(firstyear_incumbent=case_when(cabinet_change==0 ~ electionyearandone,
                                       cabinet_change==1 & relevant_cabinet==1 & cabinet_change_withinyear==0 & cabinetinelectionyear==TRUE ~ electionyearandone,
                                       cabinet_change==1 & relevant_cabinet==1 & cabinet_change_withinyear==0 & cabinetinelectionyear==FALSE ~ start_year,
                                       cabinet_change==1 & relevant_cabinet==1 & cabinet_change_withinyear==1 & (electionyearandone>start_year) ~ electionyearandone,
                                       cabinet_change==1 & relevant_cabinet==1 & cabinet_change_withinyear==1 & (electionyearandone<=start_year) ~ start_year,
                                       cabinet_change==1 & relevant_cabinet==0 ~ 0)) %>%
  select(-relevant_cabinet, -counter, -number, -start_year, -cabinetinelectionyear, -cabinet_change_withinyear, -electionyearandone, -cabinet_change)


#Period for which each government is the relevant incumbent.
#Period 1900-01-01 - 1900-12-31 as placeholder for NA.
cabinet_nat_and_reg <- cabinet_nat_and_reg %>%
  mutate(firstyear_incumbent=make_date(firstyear_incumbent, month="01", day="01")) %>%
  group_by(country_name, region) %>%
  arrange(firstyear_incumbent) %>%
  mutate(number=1)%>%
  mutate(counter=cumsum(number)) %>%
  mutate(maxcount=max(counter)) %>%
  mutate(incumbency_period=case_when(firstyear_incumbent==0~interval(ymd("1900-01-01"), ymd("1900-12-31")),
                                   firstyear_incumbent>0 & counter<maxcount ~ interval(firstyear_incumbent, ((lead(firstyear_incumbent))-1)),
                                   firstyear_incumbent>0 & counter==maxcount  ~ (interval(firstyear_incumbent, ymd("2023-01-01"))),
                                   TRUE~interval(ymd("1900-01-01"), ymd("1900-12-31")))) %>%
  mutate(firstyear_incumbent=year(firstyear_incumbent)) %>%
  select(-counter, -number, -maxcount)

#Is government relevant for 2021?
cabinet_nat_and_reg <- cabinet_nat_and_reg %>%
  mutate(included_2021=(ymd("2021-01-01") %within% incumbency_period))


#mit wide reshapen, sodass national und regional nebeneinander aufgeführt werden.
#alignment erstellen.



write_csv(regional_gov, file.path("./data/processed/","reg_and_nat_governments.csv"))
