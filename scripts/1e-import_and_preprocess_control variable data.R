#Import and preprocess data on socioeconomic and political factors.

library(tidyverse)
library(readxl)
library(lubridate)

#1. Population statistics (population size, population density, area, capital region) ####
population <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_population_06122022.csv")
area <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_area_07122022.csv")

population <- population %>% 
  filter(geo!="FRXX") %>% 
  filter(geo!="HUXX") %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(population=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE)

hr02 <- c("HR02", "2020", 1060209) #Croatian 2020 observations in NUTS-2021 units are added manually.
hr05 <- c("HR05", "2020", 809235)
hr06 <- c("HR06", "2020", 814919)

population <- population %>% 
  rbind(hr02) %>% 
  rbind(hr05) %>% 
  rbind(hr06) %>% 
  filter(nuts!="HR04")

area <- area %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(area=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE)

area1 <- area %>% 
  filter(str_detect(nuts, "HR04.+")) %>% 
  filter(reference_year==2020)

hr02 <- c("HR02", "2020", 23220) #Croatian 2020 observations in NUTS-2021 units are added manually.
hr05 <- c("HR05", "2020", 641)
hr06 <- c("HR06", "2020", 8028)

area <- area %>% 
  filter(!str_detect(nuts, "HR04.+")) %>% 
  filter(!(nuts=="HR02" & reference_year==2020)) %>% 
  filter(!(nuts=="HR05" & reference_year==2020)) %>% 
  filter(!(nuts=="HR06" & reference_year==2020)) %>% 
  filter(nuts!="HR04") %>% 
  rbind(hr02) %>% 
  rbind(hr05) %>% 
  rbind(hr06)

population <- population %>% 
  full_join(area, by=c("reference_year", "nuts")) %>% 
  mutate(population=as.numeric(population),
         area=as.numeric(area),
         pop_density=population/area) %>% 
  mutate(capital_region=case_when(nuts=="BE10"~1,
                                  nuts=="BG41"~1,
                                  nuts=="DK01"~1,
                                  nuts=="DE30"~1,
                                  nuts=="EE00"~1,
                                  nuts=="FI1B"~1,
                                  nuts=="FR1"~1,
                                  nuts=="EL30"~1,
                                  nuts=="IE06"~1,
                                  nuts=="ITI4"~1,
                                  nuts=="HR05"~1,
                                  nuts=="LT01"~1,
                                  nuts=="LU00"~1,
                                  nuts=="LV00"~1,
                                  nuts=="MT00"~1,
                                  nuts=="NL32"~1,
                                  nuts=="AT13"~1,
                                  nuts=="PL91"~1,
                                  nuts=="PT17"~1,
                                  nuts=="RO32"~1,
                                  nuts=="SE11"~1,
                                  nuts=="SI04"~1,
                                  nuts=="SI0"~1,
                                  nuts=="SK01"~1,
                                  nuts=="ES30"~1,
                                  nuts=="CZ01"~1,
                                  nuts=="HU11"~1,
                                  nuts=="CY00"~1,
                                  TRUE~0))

write_csv(population, file.path("./data/processed/population.csv"))

#2. Unemployment statistics ####
#ggf. checken, ob bislang fehlende und manuell ergänzte Werte nun in Eurostat verfügbar sind.
#Falls ja, auch in Codebook und Data Dictionary anpassen.
ue <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_ue_06122022.csv")

ue <- ue %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(ue_rate=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE)


hr02 <- c("HR02", "2019", 16.1) #Croatian 2019 and 2020 observations in NUTS-2021 version units are added manually.
hr05 <- c("HR05", "2019", 3.7)
hr06 <- c("HR06", "2019", 5.8)
hr021 <- c("HR02", "2020", 13.6) 
hr051 <- c("HR05", "2020", 5.2)
hr061 <- c("HR06", "2020", 5)

fi200 <- c("FI20", "2019", 3.5) #Åland observations are added manually.
fi201 <- c("FI20", "2020", 9.5) 
fi202 <- c("FI20", "2021", 6.3)

pl43201 <- c("PL43", "2020", 6.6) #Lubuskie observations are added manually.
pl43202 <- c("PL43", "2021", 5.1)

deb201 <- c("DEB2", "2020", 3.5) #Some German observations are added manually.
deb202 <- c("DEB2", "2021", 3.8)
ded402 <- c("DED4", "2020", 5.4)
dec002 <- c("DEC0", "2020", 7.2)
deb102 <- c("DEB1", "2020", 4.7)
de7302 <- c("DE73", "2020", 4.8)
de5002 <- c("DE50", "2020", 11.2)
de2602 <- c("DE26", "2020", 3.4)
de2402 <- c("DE24", "2020", 3.9)
de2302 <- c("DE23", "2020", 3.3)
de2202 <- c("DE22", "2020", 3.6)

fry5 <- c("FRY5", "2021", 30) #Mayotte observation is added manually

ue <- ue %>% 
  rbind(hr02) %>% 
  rbind(hr05) %>% 
  rbind(hr06) %>% 
  rbind(hr021) %>% 
  rbind(hr051) %>% 
  rbind(hr061) %>% 
  filter(nuts!="HR04") %>% 
  filter(nuts!="FI20") %>% 
  filter(!(nuts=="PL43" & reference_year=="2020")) %>% 
  filter(!(nuts=="PL43" & reference_year=="2021")) %>% 
  filter(!(nuts=="DEB2" & reference_year=="2020")) %>% 
  filter(!(nuts=="DEB2" & reference_year=="2021")) %>% 
  filter(!(nuts=="DED4" & reference_year=="2020")) %>% 
  filter(!(nuts=="DEC0" & reference_year=="2020")) %>% 
  filter(!(nuts=="DEB1" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE73" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE50" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE26" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE24" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE23" & reference_year=="2020")) %>% 
  filter(!(nuts=="DE22" & reference_year=="2020")) %>% 
  rbind(fi200) %>% 
  rbind(fi201) %>% 
  rbind(fi202) %>% 
  rbind(pl43201) %>% 
  rbind(pl43202) %>% 
  rbind(dec002) %>% 
  rbind(deb201) %>% 
  rbind(deb202) %>% 
  rbind(ded402) %>% 
  rbind(deb102) %>% 
  rbind(de7302) %>% 
  rbind(de5002) %>% 
  rbind(de2602) %>% 
  rbind(de2402) %>% 
  rbind(de2302) %>% 
  rbind(de2202) %>% 
  rbind(fry5) 

ue <- ue %>% 
  pivot_wider(names_from=reference_year, values_from = ue_rate) %>% 
  mutate(ue_change_1920=as.double(`2020`)-as.double(`2019`)) %>% 
  pivot_longer(cols=c(`2019`, `2020`, `2021`), names_to = "reference_year", values_to="ue_rate") %>% 
  filter(reference_year!="2019") %>% 
  mutate(ue_change_1920=case_when(reference_year=="2021"~99,
                                  TRUE~ue_change_1920))
write_csv(ue, file.path("./data/processed/ue.csv"))

#3. GDP per capita and GVA change ####
#checken ob bei GVA noch 2020 bei Ungarn und Polen vervollständigt wurde.
gdppc <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_gdppercapita_06122022.csv")
gvagrowth <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_GVAgrowth_07122022.csv")
gdp <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_gdp_07122022.csv")


gdppc <- gdppc %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(gdppc=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE)

si00 <- c("SI00", "2020", 26628)
fr1 <- c("FR1", "2020", 52700)
frb <- c("FRB", "2020", 25500)
frc <- c("FRC", "2020", 24841)
frd <- c("FRD", "2020", 24983)
fre <- c("FRE", "2020", 24800)
frf <- c("FRF", "2020", 25357)
frg <- c("FRG", "2020", 27700)
frh <- c("FRH", "2020", 27000)
fri <- c("FRI", "2020", 26273)
frj <- c("FRJ", "2020", 26047)
frk <- c("FRK", "2020", 30633)
frl <- c("FRL", "2020", 28900)
frm <- c("FRM", "2020", 23400)

gdppc <- gdppc %>% 
  rbind(fr1) %>% 
  rbind(frb) %>% 
  rbind(frc) %>% 
  rbind(frd) %>% 
  rbind(fre) %>% 
  rbind(frf) %>% 
  rbind(frg) %>% 
  rbind(frh) %>% 
  rbind(fri) %>% 
  rbind(frj) %>% 
  rbind(frk) %>% 
  rbind(frl) %>% 
  rbind(frm) %>% 
  rbind(si00) %>% 
  mutate(reference_year=as.numeric(reference_year))
  


gvagrowth <- gvagrowth %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(gvagrowth=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE) %>% 
  filter(reference_year!="2019")



gdp <- gdp %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(nuts=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(gdp=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE) 

si00a <- c("SI00", "2019", 57781.94)
fr1a <- c("FR1", "2019", 698588.4)
frba <- c("FRB", "2019", 69429.93)
frca <- c("FRC", "2019", 73186.54)
frda <- c("FRD", "2019", 88658.35)
frea <- c("FRE", "2019", 159332.6)
frfa <- c("FRF", "2019", 149687.5)
frga <- c("FRG", "2019", 111784.83)
frha <- c("FRH", "2019", 94040.33)
fria <- c("FRI", "2019", 167121.4)
frja <- c("FRJ", "2019", 163998.2)
frka <- c("FRK", "2019", 260278)
frla <- c("FRL", "2019", 157412.52)
frma <- c("FRM", "2019", 8664.96)

si00 <- c("SI00", "2020", 55809.1)
fr1 <- c("FR1", "2020", 652399.02)
frb <- c("FRB", "2020", 65757.82)
frc <- c("FRC", "2020", 69418.42)
frd <- c("FRD", "2020", 82778.59)
fre <- c("FRE", "2020", 148498.2)
frf <- c("FRF", "2020", 140377)
frg <- c("FRG", "2020", 106715.96)
frh <- c("FRH", "2020", 91557.77)
fri <- c("FRI", "2020", 158119.2)
frj <- c("FRJ", "2020", 155026.2)
frk <- c("FRK", "2020", 247032.4)
frl <- c("FRL", "2020", 148186.16)
frm <- c("FRM", "2020", 8107.56)

gdp <- gdp %>%
  rbind(fr1a) %>% 
  rbind(frba) %>% 
  rbind(frca) %>% 
  rbind(frda) %>% 
  rbind(frea) %>% 
  rbind(frfa) %>% 
  rbind(frga) %>% 
  rbind(frha) %>% 
  rbind(fria) %>% 
  rbind(frja) %>% 
  rbind(frka) %>% 
  rbind(frla) %>% 
  rbind(frma) %>% 
  rbind(si00a) %>% 
  rbind(fr1) %>% 
  rbind(frb) %>% 
  rbind(frc) %>% 
  rbind(frd) %>% 
  rbind(fre) %>% 
  rbind(frf) %>% 
  rbind(frg) %>% 
  rbind(frh) %>% 
  rbind(fri) %>% 
  rbind(frj) %>% 
  rbind(frk) %>% 
  rbind(frl) %>% 
  rbind(frm) %>% 
  rbind(si00) %>% 
  pivot_wider(names_from=reference_year, values_from = gdp) %>% 
  mutate(gdp_change_1920=(as.double(`2020`)/as.double(`2019`))-1) %>% 
  pivot_longer(cols=c(`2019`, `2020`), 
               names_to = "reference_year", 
               values_to="gdp") %>% 
  filter(reference_year!="2019") %>% 
  mutate(reference_year=as.numeric(reference_year))

gdppc <- gdppc %>% 
  left_join(gvagrowth) %>% 
  left_join(gdp)

write_csv(gdppc, file.path("./data/processed/gdppc.csv"))

gdp_national <- read_csv("C:/Users/RomyH/Downloads/tec00115__custom_4085930_linear.csv")

gdp_national <- gdp_national %>% 
  select(geo, OBS_VALUE, TIME_PERIOD) %>% 
  rename(country=geo,
         reference_year=TIME_PERIOD) %>% 
  mutate(gdp_growth=unlist(OBS_VALUE)) %>% 
  select(-OBS_VALUE) %>% 
  filter(reference_year!="2019")


gdp1 <- gdp %>% 
  mutate(gdp=as.integer(gdp)) %>% 
  mutate(country=str_extract(nuts, "\\w{2}")) %>% 
  group_by(country, reference_year) %>%
  summarise(gdpsum=sum(gdp)) %>% 
  pivot_wider(names_from=reference_year, values_from = gdpsum) %>% 
  mutate(gdp_growth_pooled=(as.double(`2020`)/as.double(`2019`))-1) %>% 
  pivot_longer(cols=c(`2019`, `2020`), 
               names_to = "reference_year", 
               values_to="gdp") %>% 
  filter(reference_year!="2019") %>% 
  mutate(reference_year=as.numeric(reference_year)) %>% 
  left_join(gdp_national, by=c("country", "reference_year")) %>% 
  mutate(gdp_growth_pooled=gdp_growth_pooled*100,
         gdp_growth_difference=gdp_growth-gdp_growth_pooled) %>% 
  filter(country!="FR") %>% 
  filter(country!="SI") %>% 
  filter(!duplicated(country))

gdp1 %>% 
  ggplot(aes(x = gdp_growth_pooled, y=gdp_growth)) +
  geom_point() +
  geom_abline(xintercept = 0, slope=1, linetype="dashed") +
  labs(title="Comparison pooled and national Eurostat data") +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_x_continuous(labels = function(x) paste0(x, '%'))

#4. COVID-19 related statistics #### 

covid <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/ecdc_covid.csv")
covid_national <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/ecdc_covid_national.csv")


covid_aggregate <- covid %>% 
  filter(country!="Iceland") %>% 
  filter(country!="Liechtenstein") %>% 
  filter(country!="Norway") %>% 
  filter(!is.na(weekly_count)) %>% 
  filter(!(country=="Denmark" & nuts_code=="GL")) %>% 
  mutate(year=str_extract(year_week, "\\d{4}")) %>% 
  mutate(nuts=str_extract(nuts_code, "\\w{2}[:alnum:]{1,2}")) %>% 
  mutate(nuts=case_when(nuts=="SI03" ~"SI00",
                        nuts=="SI04"~"SI00",
                        TRUE~nuts)) %>% 
  filter(year!="2022") %>% 
  group_by(nuts, year, country) %>% 
  summarise(cases_total=sum(weekly_count)) %>% 
  mutate(nuts=case_when(nuts=="CY0"~"CY00",
                        nuts=="FI2"~"FI20",
                        nuts=="LU0"~"LU00",
                        nuts=="MT0"~"MT00",
                        nuts=="EEG1"~"EE00",
                        nuts=="DE3"~"DE30",
                        nuts=="DE4"~"DE40",
                        nuts=="DE5"~"DE50",
                        nuts=="DE6"~"DE60",
                        nuts=="DE8"~"DE80",
                        nuts=="DEC"~"DEC0",
                        nuts=="DEE"~"DEE0",
                        nuts=="DEF"~"DEF0",
                        nuts=="DEG"~"DEG0",
                        nuts=="DE1"~"DE11",
                        nuts=="DE2"~"DE21",
                        nuts=="DE7"~"DE71",
                        nuts=="DE9"~"DE91",
                        nuts=="DEA"~"DEA1",
                        nuts=="DEB"~"DEB1",
                        nuts=="DED"~"DED2",
                        nuts=="BE1"~"BE10",
                        nuts=="BE2"~"BE21",
                        nuts=="BE3"~"BE31",
                        nuts=="PTCS"~"PT11",
                          TRUE~nuts)) %>% 
  mutate(reference_year=case_when(year=="2020" ~ 2020,
                                   year=="2021" ~ 2021)) %>% 
  ungroup()

covid_national <- covid_national %>% 
  filter(country=="Netherlands") %>% 
  filter(indicator=="cases") %>% 
  filter(year_week=="2020-53" | year_week=="2021-52")
  
covid_NL <- covid_aggregate %>%  #For Netherlands there is no information for 2020 in the data. To reflect differences between regions values from 2021 are imputed for 2020 and then to reflect the overall values, they are adapted to 2020/2021 nationwide cases ratio.
  filter(country=="Netherlands") %>% 
  mutate(reference_year=2020,
         cases_total=cases_total*0.259)

covid_aggregate <- covid_aggregate %>% 
  bind_rows(covid_NL) %>% 
  select(-year)

covid_pop <- covid %>% 
  filter(country!="Iceland") %>% 
  filter(country!="Liechtenstein") %>% 
  filter(country!="Norway") %>% 
  filter(!(country=="Denmark" & nuts_code=="GL")) %>% 
  mutate(year=str_extract(year_week, "\\d{4}")) %>% 
  mutate(nuts=str_extract(nuts_code, "\\w{2}[:alnum:]{1,2}")) %>% 
  mutate(nuts=case_when(nuts=="SI03" ~"SI00",
                        nuts=="SI04"~"SI00",
                        TRUE~nuts)) %>% 
  filter(year_week=="2020-52" | year_week=="2021-51") %>% 
  group_by(nuts, year, country) %>% 
  summarise(population=sum(population)) %>% 
  mutate(nuts=case_when(nuts=="CY0"~"CY00",
                        nuts=="FI2"~"FI20",
                        nuts=="LU0"~"LU00",
                        nuts=="MT0"~"MT00",
                        nuts=="EEG1"~"EE00",
                        nuts=="DE3"~"DE30",
                        nuts=="DE4"~"DE40",
                        nuts=="DE5"~"DE50",
                        nuts=="DE6"~"DE60",
                        nuts=="DE8"~"DE80",
                        nuts=="DEC"~"DEC0",
                        nuts=="DEE"~"DEE0",
                        nuts=="DEF"~"DEF0",
                        nuts=="DEG"~"DEG0",
                        nuts=="DE1"~"DE11",
                        nuts=="DE2"~"DE21",
                        nuts=="DE7"~"DE71",
                        nuts=="DE9"~"DE91",
                        nuts=="DEA"~"DEA1",
                        nuts=="DEB"~"DEB1",
                        nuts=="DED"~"DED2",
                        nuts=="BE1"~"BE10",
                        nuts=="BE2"~"BE21",
                        nuts=="BE3"~"BE31",
                        nuts=="PTCS"~"PT11",
                        TRUE~nuts)) %>% 
  mutate(reference_year=case_when(year=="2020" ~ 2020,
                                  year=="2021" ~ 2021)) %>% 
  ungroup() %>% 
  select(-year)

covid_NL_pop <- covid_pop %>%  
  filter(country=="Netherlands") %>% 
  mutate(reference_year=2020)


covid_pop <- covid_pop %>% 
  bind_rows(covid_NL_pop)

covid_cases <- covid_aggregate %>% 
  full_join(covid_pop, by=c("nuts", "reference_year", "country")) %>% 
  mutate(rate_per_100k=cases_total/(population/100000)) 


covid_cases_DE1 <- covid_cases %>%  #Germany is included only in NUTS-1 level in the data. To make it join with the NUTS-2 level, the NUTS-1 value is repeated.
  filter(country=="Germany") %>%
  filter(!nuts%in% c("DE30", "DE40", "DE50", "DE60", "DE80", "DEC0", "DEE0", "DEF0", "DEG0")) %>% 
  mutate(nuts=case_when(nuts=="DE11"~"DE12",
                        nuts=="DE21"~"DE22",
                        nuts=="DE71"~"DE72",
                        nuts=="DE91"~"DE92",
                        nuts=="DEA1"~"DEA2",
                        nuts=="DEB1"~"DEB2",
                        nuts=="DED2"~"DED4"))
covid_cases_DE2 <- covid_cases_DE1 %>% 
  filter(!nuts%in% c("DE30", "DE40", "DE50", "DE60", "DE80", "DEC0", "DEE0", "DEF0", "DEG0")) %>% 
  mutate(nuts=case_when(nuts=="DE12"~"DE13",
                        nuts=="DE22"~"DE23",
                        nuts=="DE72"~"DE73",
                        nuts=="DE92"~"DE93",
                        nuts=="DEA2"~"DEA3",
                        nuts=="DEB2"~"DEB3",
                        nuts=="DED4"~"DED5"))
covid_cases_DE3 <- covid_cases_DE1 %>% 
  filter(!nuts%in% c("DE30", "DE40", "DE50", "DE60", "DE80", "DEC0", "DEE0", "DEF0", "DEG0", "DE71", "B1", "D2")) %>% 
  mutate(nuts=case_when(nuts=="DE12"~"DE14",
                        nuts=="DE22"~"DE24",
                        nuts=="DE92"~"DE94",
                        nuts=="DEA2"~"DEA4"))
covid_cases_DE4 <- covid_cases_DE1 %>% 
  filter(!nuts%in% c("DE11", "DE30", "DE40", "DE50", "DE60", "DE80", "DE91", "DEC0", "DEE0", "DEF0", "DEG0", "DE71", "B1", "D2")) %>% 
  mutate(nuts=case_when(nuts=="DE22"~"DE25",
                        nuts=="DEA2"~"DEA5"))
covid_cases_DE5 <- covid_cases_DE1 %>% 
  filter(!nuts%in% c("DE11", "DE30", "DE40", "DE50", "DE60", "DE80", "DE91", "DEC0", "DEE0", "DEF0", "DEG0", "DE71", "DEB1", "DED2", "DEA1")) %>% 
  mutate(nuts=case_when(nuts=="DE22"~"DE26"))
covid_cases_DE6 <- covid_cases_DE1 %>% 
  filter(!nuts%in% c("DE11", "DE30", "DE40", "DE50", "DE60", "DE80", "DE91", "DEC0", "DEE0", "DEF0", "DEG0", "DE71", "DEB1", "DED2", "DEA1")) %>% 
  mutate(nuts=case_when(nuts=="DE22"~"DE27"))

covid_cases <- covid_cases %>% 
  bind_rows(covid_cases_DE1, covid_cases_DE2, covid_cases_DE3, covid_cases_DE4, covid_cases_DE5, covid_cases_DE6)

covid_cases_BE1 <- covid_cases %>%  #Belgium is included only in NUTS-1 level in the data. To make it join with the NUTS-2 level, the NUTS-1 value is repeated.
  filter(country=="Belgium") %>%
  filter(!nuts%in% c("BE10")) %>% 
  mutate(nuts=case_when(nuts=="BE21"~"BE22",
                        nuts=="BE31"~"BE32"))
covid_cases_BE2 <- covid_cases %>%
  filter(country=="Belgium") %>%
  filter(!nuts%in% c("BE10")) %>% 
  mutate(nuts=case_when(nuts=="BE21"~"BE23",
                        nuts=="BE31"~"BE33"))
covid_cases_BE3 <- covid_cases %>%
  filter(country=="Belgium") %>%
  filter(!nuts%in% c("BE10")) %>% 
  mutate(nuts=case_when(nuts=="BE21"~"BE24",
                        nuts=="BE31"~"BE34"))
covid_cases_BE4 <- covid_cases %>%
  filter(country=="Belgium") %>%
  filter(!nuts%in% c("BE10")) %>% 
  mutate(nuts=case_when(nuts=="BE21"~"BE25",
                        nuts=="BE31"~"BE35"))
covid_cases_PL1 <- covid_cases %>%
  filter(nuts=="PL92") %>%
  mutate(nuts=case_when(nuts=="PL92"~"PL91"))

covid_cases <- covid_cases %>% 
  bind_rows(covid_cases_BE1, covid_cases_BE2, covid_cases_BE3, covid_cases_BE4, covid_cases_PL1)


covid_cases_PT1 <- covid_cases %>%  #Portugal is included only in NUTS-0 level in the data. To make it join with the NUTS-2 level, the NUTS-0 value is repeated.
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT15"))
covid_cases_PT2 <- covid_cases %>%
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT16"))
covid_cases_PT3 <- covid_cases %>%
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT17"))
covid_cases_PT4 <- covid_cases %>%
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT18"))
covid_cases_PT5 <- covid_cases %>%
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT20"))
covid_cases_PT6 <- covid_cases %>%
  filter(country=="Portugal") %>%
  mutate(nuts=case_when(nuts=="PT11"~"PT30"))


covid_cases <- covid_cases %>% 
  bind_rows(covid_cases_PT1, covid_cases_PT2, covid_cases_PT3, covid_cases_PT4, covid_cases_PT5, covid_cases_PT6) %>% 
  select(-country, -population, -cases_total)

write_csv(covid_cases, file.path("./data/processed/covid_cases.csv"))

#5. Political control variables: Quality of Governance Index ####

qog1 <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/Quality of Governance/qog_eureg_wide1_nov20.csv")
qog2 <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/Quality of Governance/qog_eureg_wide2_nov20.csv")

qog1 <- qog1 %>% 
  filter(! (nuts0 %in% c("AL", "IS", "LI", "ME", "MK", "NO", "RS", "CH", "TR", "UK", "AT", "DK", "IT", "NL", "ES"))) %>% 
  filter(!(nuts0=="BG" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="CY" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="CZ" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="EE" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="FI" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="EL" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="HU" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="IE" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="LV" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="LT" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="LU" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="MT" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="PT" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="RO" & !is.na(nuts1))) %>%
  filter(!(nuts0=="SK" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="SI" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="SE" & !is.na(nuts1))) %>% 
  filter(!(nuts0=="HR" & !is.na(nuts1))) %>% 
  filter(year=="2017" | year=="2016" | year=="2018"|year=="2019" |year=="2020") %>% 
  mutate(nuts0=case_when(nuts0=="EL"~"GR",
                         TRUE~nuts0))

qog1 <- qog1 %>% 
  group_by(nuts0, year) %>% 
  fill(eqi_score_nuts0,.direction="down") %>%  #eventuell noch weitere Variablen hinzufügen.
  ungroup()

qog1 <- qog1 %>%  
  filter(!(nuts0=="BE" & is.na(nuts1))) %>% 
  filter(!(nuts0=="FR" & is.na(nuts1))) %>% 
  filter(!(nuts0=="DE" & is.na(nuts1))) %>% 
  filter(!(nuts0=="PL" & is.na(nuts1))) %>% 
  filter(!(nuts0=="FR" & nuts1=="FRY")) %>% 
  filter(!(nuts0=="PL" & (nuts1 %in% c("PL1","PL2", "PL3", "PL4", "PL5", "PL6", "PL7", "PL8")))) 

qog1 <- qog1 %>% 
  mutate(political_region_nuts=case_when(is.na(nuts1)~nuts0,
                                         TRUE~nuts1)) %>% 
  select(region_code, region_name, year, political_region_nuts, everything())


qog2 <- qog2 %>% 
  filter(nuts0=="AT" | nuts0=="DK" | nuts0=="IT" | nuts0=="NL" | nuts0=="ES" | nuts0=="FR" | nuts0=="PL") %>% 
  filter(year=="2017")

qog2 <- qog2 %>% 
  group_by(nuts0, year) %>% 
  fill(eqi_score_nuts0,.direction="down") %>%  #eventuell noch weitere Variablen hinzufügen.
  ungroup()

qog2 <- qog2 %>% 
  filter(!(nuts0=="AT" & is.na(nuts2))) %>% 
  filter(!(nuts0=="DK" & is.na(nuts2))) %>% 
  filter(!(nuts0=="IT" & is.na(nuts2))) %>% 
  filter(!(nuts0=="NL" & is.na(nuts2))) %>% 
  filter(!(nuts0=="ES" & is.na(nuts2))) %>% 
  filter(!(nuts0=="FR" & is.na(nuts2))) %>% 
  filter(!(nuts0=="PL" & is.na(nuts2))) %>% 
  filter(!(nuts0=="FR" & (nuts2 %in% c("FR10","FR91", "FRB0", "FRC1", "FRC2", "FRD1", "FRD2", "FRE1", "FRE2", "FRF1", "FRF2", "FRF3", "FRG0", 
                                       "FRH0", "FRI1", "FRI2", "FRI3", "FRJ1", "FRJ2", "FR2", "FRK1", "FRK2", "FRL0", "FRM0")))) %>% 
  filter(!(nuts0=="PL" & nuts2=="PL91")) %>% 
  filter(!(nuts0=="PL" & nuts2=="PL92"))

qog2 <- qog2 %>% 
  mutate(political_region_nuts=nuts2) %>% 
  select(region_code, region_name, year, political_region_nuts, everything())

qog <- qog1 %>% 
  bind_rows(qog2) %>% 
  select(political_region_nuts, nuts0, eqi_score_nuts0, eqi_score_nuts1, eqi_score_nuts2) %>% 
  rename(country_code=nuts0) %>% 
  mutate(eqi_score_political_region=case_when(!is.na(eqi_score_nuts2)~eqi_score_nuts2,
                                              !is.na(eqi_score_nuts1) & is.na(eqi_score_nuts2)~eqi_score_nuts1,
                                              TRUE~eqi_score_nuts0)) %>% 
  rename(eqi_score_national=eqi_score_nuts0) %>% 
  select(-eqi_score_nuts1, -eqi_score_nuts2)

qog_nuts <- qog %>% 
  select(country_code, eqi_score_political_region, political_region_nuts) %>% 
  filter(!is.na(eqi_score_political_region))
  
qog_national <- qog %>% 
  select(country_code, eqi_score_national) %>% 
  filter(!is.na(eqi_score_national)) %>% 
  filter(!duplicated(country_code))


write_csv(qog_nuts, file.path("./data/processed/qog_nuts.csv"))
write_csv(qog_national, file.path("./data/processed/qog_national.csv"))


#6. Political control variables: strength of regional level ####
rai <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/RAI_country-2021.xlsx")

rai <- rai %>% 
  mutate(country_name=str_to_lower(country_name)) %>% 
  rename(rai=n_RAI) %>% 
  filter(year=="2018") %>% 
  select(country_name, rai) %>% 
  rename(country=country_name)

write_csv(rai, file.path("./data/processed/rai.csv"))
