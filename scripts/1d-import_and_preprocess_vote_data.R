#Import and preprocess vote data.

##falls umgestellt werden soll auf Wahlen im 1. Halbjahr müssen folgende Aspekte geändert werden:
##-Zeile 82: is.na löschen. Hiermit wurden die manuell hinzugefügten Wahlen in 2021 entfernt.
##-Zeile 83ff.: Wahldaten für die betroffenen Länder so ersetzen, dass wieder max. 2 Wahltermine pro Land genannt sind. Achtung auf TRUE-Argument, ggf. verändern.
##-Zeile 120ff.: betroffene Länder hier falls vorhanden entfernen (da diese nun für die beiden Referenzjahre verschiedene Wahltermine haben).
##-Zeile 150ff.: betroffene Länder hier falls vorhanden entfernen (da diese nun für die beiden Referenzjahre verschiedene Wahltermine haben).
##-Zeile 193ff.: betroffene Länder hier hinzufügen/verändern mit Referenzjahr pro Wahltermin.

library(tidyverse)
library(readxl)
library(lubridate)

#1. Import EU-NED database, complement it with own data collection for not covered years and preprocess it.####

vote_nuts2 <- read_csv("./data/raw/eu_ned_national.csv")
vote_nuts2_complement <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/politicalvar/data_politicalvariables.xlsx", sheet="elections_notinEU-NED")

vote_nuts2_all <- bind_rows(vote_nuts2, vote_nuts2_complement)

vote_nuts2_all <- vote_nuts2_all %>% 
  filter(country!="Norway" & country!="Switzerland" & country!="Turkey" & country!="United Kingdom") %>% 
  mutate(nuts2=case_when(nutslevel==2 & country!= "Spain" ~nuts2016, #For Spain some regions are indicated as nuts 2 level, however their code is actually nuts 3.
                         nutslevel==2 & country=="Spain" ~ str_extract(nuts2016, "\\w{2}[:alnum:]{2}"),
                         nutslevel==3~str_extract(nuts2016, "\\w{2}[:alnum:]{2}"),
                         TRUE~"NA"),
         regionname2=case_when(nutslevel==2~regionname,
                               nutslevel==3~"",
                               TRUE~""),
         partyfacts_id=case_when(partyfacts_id==2338 ~633, #für diese IDs existiert kein equivalent im ParlGov Datensatz. Zumeist Parteizusammenschlüsse - für diese wird stattdessen die stärkste Partei gewählt.
                                 partyfacts_id==6115 ~2057,
                                 partyfacts_id==2522 ~1475,
                                 partyfacts_id==4094 ~110,
                                 partyfacts_id==1044 ~1096,
                                 partyfacts_id==1898 ~3942,
                                 partyfacts_id==5953 ~8657,
                                 partyfacts_id==8168 ~1595,
                                 partyfacts_id==7909 ~1468,
                                 partyfacts_id==8249 ~859,
                                 partyfacts_id==8241 ~3954,
                                 partyfacts_id==8058 ~1626,
                                 partyfacts_id==7790 ~1365,
                                 partyfacts_id==5657 ~75,
                                 partyfacts_id==7619 ~1704,
                                 partyfacts_id==7421 ~4795,
                                 TRUE~partyfacts_id))

#2. Get ParlGov Party ID through Partyfacts ID.####

partyfacts <- read_csv("./data/raw/partyfacts-external-parties.csv")

partyfacts <- partyfacts %>% 
  filter(dataset_key=="parlgov") %>% 
  select(dataset_party_id, partyfacts_id)

vote_nuts2_all <- vote_nuts2_all %>% 
  left_join(partyfacts, by="partyfacts_id") %>% 
  rename(party_id=dataset_party_id) %>% 
  select(-partyfacts_id)

# 3. Preprocess data - create sound variables: turnout and percentage vote share for each party + variables that are needed to join this data set to other data sets (election_date + reference_year).####

vote_nuts2_all <-  vote_nuts2_all %>% 
  mutate(regionname2=case_when(regionname2=="Las Palmas"~"",  #Despite having the nuts2 code in EU-NED, the name actually identifies the nuts3 level. This hinders the aggregation and thus is deleted.
                               regionname2=="Santa Cruz de Tenerife"~"",
                               TRUE~regionname2)) %>% 
  group_by(country, country_code, nuts2, regionname2, type, year, party_id, election_date) %>% 
  summarize(party_vote=sum(partyvote),
            electorate=sum(electorate),
            total_vote=sum(totalvote),
            valid_vote=sum(validvote)) %>% 
  ungroup()


vote_nuts2_all <- vote_nuts2_all %>% 
  mutate(turnout=total_vote/electorate,
         party_voteshare=party_vote/valid_vote,
         country=str_to_lower(country))

vote_nuts2_all <- vote_nuts2_all %>% 
  filter(is.na(election_date)) %>% 
  mutate(election_date=case_when(country=="austria" & year=="2019"~ymd("2019-09-29"),
                                 country=="belgium" & year=="2019"~ymd("2019-05-26"),
                                 country=="bulgaria" & year=="2017"~ymd("2017-03-26"),
                                 country=="croatia" & year=="2016"~ymd("2016-09-11"),
                                 country=="croatia" & year=="2020"~ymd("2020-07-05"),
                                 country=="cyprus" & year=="2016"~ymd("2016-05-22"),
                                 country=="czechia" & year=="2017"~ymd("2017-10-21"),
                                 country=="denmark" & year=="2019"~ymd("2019-06-05"),
                                 country=="estonia" & year=="2019"~ymd("2019-03-03"),
                                 country=="finland" & year=="2019"~ymd("2019-04-14"),
                                 country=="france" & year=="2017"~ymd("2017-06-18"),
                                 country=="germany" & year=="2017"~ymd("2017-09-24"),
                                 country=="greece" & year=="2019"~ymd("2019-07-07"),
                                 country=="hungary" & year=="2018"~ymd("2018-04-08"),
                                 country=="ireland" & year=="2016"~ymd("2016-02-26"),
                                 country=="ireland" & year=="2020"~ymd("2020-02-08"),
                                 country=="italy" & year=="2018"~ymd("2018-03-04"),
                                 country=="latvia" & year=="2018"~ymd("2018-10-06"),
                                 country=="lithuania" & year=="2016"~ymd("2016-10-09"),
                                 country=="lithuania" & year=="2020"~ymd("2020-10-11"),
                                 country=="luxembourg" & year=="2018"~ymd("2018-10-14"),
                                 country=="malta" & year=="2017"~ymd("2017-06-03"),
                                 country=="netherlands" & year=="2017"~ymd("2017-03-15"),
                                 country=="poland" & year=="2019"~ymd("2019-10-13"),
                                 country=="portugal" & year=="2019"~ymd("2019-10-06"),
                                 country=="romania" & year=="2016"~ymd("2016-12-11"),
                                 country=="romania" & year=="2020"~ymd("2020-12-06"),
                                 country=="slovakia" & year=="2016"~ymd("2016-03-06"),
                                 country=="slovakia" & year=="2020"~ymd("2020-02-29"),
                                 country=="slovenia" & year=="2018"~ymd("2018-06-03"),
                                 country=="spain" & year=="2019"~ymd("2019-11-10"),
                                 country=="sweden" & year=="2018"~ymd("2018-09-09"),
                                 TRUE~ymd(election_date)))


vote_nuts2_all <- 
  bind_rows(vote_nuts2_all, vote_nuts2_all %>% 
              filter((election_date=="2019-09-29" & country=="austria") |
                       (election_date=="2019-05-26" & country=="belgium")|
                       (election_date=="2017-03-26" & country=="bulgaria") |
                       (election_date=="2016-05-22" & country=="cyprus") |
                       (election_date=="2017-10-21" & country=="czechia") |
                       (election_date=="2019-06-05" & country=="denmark") |
                       (election_date=="2019-03-03" & country=="estonia") |
                       (election_date=="2019-04-14" & country=="finland") |
                       (election_date=="2017-06-18" & country=="france") |
                       (election_date=="2017-09-24" & country=="germany") |
                       (election_date=="2019-07-07" & country=="greece") |
                       (election_date=="2018-04-08" & country=="hungary") |
                       (election_date=="2018-03-04" & country=="italy") |
                       (election_date=="2018-10-06" & country=="latvia") |
                       (election_date=="2018-10-14" & country=="luxembourg") |
                       (election_date=="2017-06-03" & country=="malta") |
                       (election_date=="2017-03-15" & country=="netherlands") |
                       (election_date=="2019-10-13" & country=="poland") |
                       (election_date=="2019-10-06" & country=="portugal") |
                       (election_date=="2018-06-03" & country=="slovenia") |
                       (election_date=="2019-11-10" & country=="spain") |
                       (election_date=="2018-09-09" & country=="sweden") 
                       ))

vote_nuts2_2021 <- vote_nuts2_all %>% 
  filter(!is.na(election_date)) %>% 
  arrange(election_date) %>% 
  group_by(party_id, nuts2) %>% 
  mutate(reference_year=row_number()) %>% 
  ungroup() %>% 
  mutate(reference_year=case_when(country=="austria" & reference_year==1 ~"2020",
                                  country=="austria" & reference_year==2~"2021",
                                  country=="belgium" & reference_year==1 ~"2020",
                                  country=="belgium" & reference_year==2 ~"2021",
                                  country=="bulgaria" & reference_year==1 ~"2020",
                                  country=="bulgaria" & reference_year==2 ~"2021",
                                  country=="cyprus" & reference_year==1 ~"2020",
                                  country=="cyprus" & reference_year==2 ~"2021",
                                  country=="czechia" & reference_year==1 ~"2020",
                                  country=="czechia" & reference_year==2 ~"2021",
                                  country=="denmark" & reference_year==1 ~"2020",
                                  country=="denmark" & reference_year==2 ~"2021",
                                  country=="estonia" & reference_year==1 ~"2020",
                                  country=="estonia" & reference_year==2 ~"2021",
                                  country=="finland" & reference_year==1 ~"2020",
                                  country=="finland" & reference_year==2 ~"2021",
                                  country=="france" & reference_year==1 ~"2020",
                                  country=="france" & reference_year==2 ~"2021",
                                  country=="germany" & reference_year==1 ~"2020",
                                  country=="germany" & reference_year==2 ~"2021",
                                  country=="greece" & reference_year==1 ~"2020",
                                  country=="greece" & reference_year==2 ~"2021",
                                  country=="hungary" & reference_year==1 ~"2020",
                                  country=="hungary" & reference_year==2 ~"2021",
                                  country=="italy" & reference_year==1 ~"2020",
                                  country=="italy" & reference_year==2 ~"2021",
                                  country=="latvia" & reference_year==1 ~"2020",
                                  country=="latvia" & reference_year==2 ~"2021",
                                  country=="luxembourg" & reference_year==1 ~"2020",
                                  country=="luxembourg" & reference_year==2 ~"2021",
                                  country=="malta" & reference_year==1 ~"2020",
                                  country=="malta" & reference_year==2 ~"2021",
                                  country=="netherlands" & reference_year==1 ~"2020",
                                  country=="netherlands" & reference_year==2 ~"2021",
                                  country=="poland" & reference_year==1 ~"2020",
                                  country=="poland" & reference_year==2 ~"2021",
                                  country=="portugal" & reference_year==1 ~"2020",
                                  country=="portugal" & reference_year==2 ~"2021",
                                  country=="slovenia" & reference_year==1 ~"2020",
                                  country=="slovenia" & reference_year==2 ~"2021",
                                  country=="spain" & reference_year==1 ~"2020",
                                  country=="spain" & reference_year==2 ~"2021",
                                  country=="sweden" & reference_year==1 ~"2020",
                                  country=="sweden" & reference_year==2 ~"2021",
                                  country=="croatia" & election_date==ymd("2016-09-11") ~"2020",
                                  country=="croatia" & election_date==ymd("2020-07-05") ~"2021",
                                  country=="ireland" & election_date==ymd("2016-02-26") ~"2020",
                                  country=="ireland" & election_date==ymd("2020-02-08") ~"2021",
                                  country=="lithuania" & election_date==ymd("2016-10-09") ~"2020",
                                  country=="lithuania" & election_date==ymd("2020-10-11") ~"2021",
                                  country=="romania" & election_date==ymd("2016-12-11") ~"2020",
                                  country=="romania" & election_date==ymd("2020-12-06") ~"2021",
                                  country=="slovakia" & election_date==ymd("2016-03-06") ~"2020",
                                  country=="slovakia" & election_date==ymd("2020-02-29") ~"2021",
                                  ))

#in order to join it with regional gov data, the regional level unit must be defined also in this dataset.
vote_nuts2_2021<- vote_nuts2_2021 %>% 
  mutate(country=case_when(country=="czechia"~"czech republic",
                           TRUE~country)) %>% 
  mutate(political_region_nuts_level=case_when(country=="austria" | country=="denmark" | country=="italy" | country=="netherlands" | country=="poland" | country=="spain" ~"2",
                                               country=="belgium" | country=="germany" | country=="france" ~"1",
                                               TRUE~"0")) %>% 
  mutate(political_region_nuts_level=case_when(country=="france" & nuts2=="FRY1"~"2",  #exceptions, where the political level is not the same for all regions in a country.
                                               country=="france" & nuts2=="FRY2"~"2",
                                               country=="france" & nuts2=="FRY3"~"2",
                                               country=="france" & nuts2=="FRY4" ~"2",
                                               country=="france" & nuts2=="FRY5"~"2",
                                               country=="italy" & nuts2=="ITH1"~"irregular",
                                               country=="italy" & nuts2=="ITH2"~"irregular",
                                               country=="poland" & nuts2=="PL91"~"1",
                                               country=="poland" & nuts2=="PL92"~"1",
                                               TRUE~political_region_nuts_level))


vote_nuts2_2021 <- vote_nuts2_2021 %>% 
  mutate(political_region_nuts=case_when(political_region_nuts_level=="2"~nuts2,
                                         political_region_nuts_level=="1"~str_extract(nuts2, "\\w{2}[:alnum:]{1}"),
                                         political_region_nuts_level=="0" & country!="slovenia"~str_extract(nuts2, "\\w{2}"),
                                         political_region_nuts_level=="irregular" ~ "ITH1/ITH2",
                                         country=="slovenia"~"SI",
                                         TRUE~"0"))

write_csv(vote_nuts2_2021, file.path("./data/processed/","vote_nuts_2021.csv"))
