#Import and preprocess data on socioeconomic and political factors.

library(tidyverse)
library(readxl)

#1. Population statistics ####
population <- read_csv("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_population_06122022.csv")

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

#3. GDP per capita ####

write_csv(ue, file.path("./data/processed/ue.csv"))
