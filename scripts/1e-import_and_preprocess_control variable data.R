#Import and preprocess data on socioeconomic and political factors.

#1. Population statistics ####

library(tidyverse)
library(readxl)

population1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/eurostat_population_02122022.xlsx", sheet="Sheet 1", skip=10)

colnames(population1) <- c("nuts", "region", "population")

population <- population1 %>% 
  filter(str_detect(nuts, "\\w{2}[:alnum:]{2}")) %>% 
  filter(nuts!="DE_TOT") %>% 
  filter(nuts!="EU27_2020") %>% 
  filter(nuts!="Special value") %>% 
  filter(region!="Not regionalised/Unknown level 2") %>% 
  filter(region!="Not regionalised/Unknown NUTS 2") %>% 
  filter(region!="Not regionalised/Unknown NUTS 2") %>% 
  filter(!is.na(region)) %>% 
  filter(region!="not available") %>% 
  filter(!str_detect(nuts, "FR[1,B,C, D, E, F, G, H, I, J, K, L, M]\\d")) %>% 
  filter(!str_detect(nuts, "UK.+")) %>% 
  filter(!str_detect(nuts, "MK.+")) %>%
  filter(!str_detect(nuts, "AL.+")) %>%
  filter(!str_detect(nuts, "RS.+")) %>%
  filter(!str_detect(nuts, "TR.+")) %>%
  filter(!str_detect(nuts, "NO.+")) %>%
  filter(!str_detect(nuts, "CH.+")) %>%
  filter(!str_detect(nuts, "ME.+")) %>%
  filter(!str_detect(nuts, "LI.+")) %>%
  filter(!str_detect(nuts, "IS.+")) %>%
  filter(nuts!="HR04") %>% #old NUTS2016 code.
  filter(nuts!="EFTA") %>%
  mutate(population=as.integer(population)) 


population2 <- population1 %>% 
  filter(str_detect(nuts, "\\w{2}[:alnum:]{2}")) %>% 
    filter(str_detect(nuts, "\\FR[1,B,C, D, E, F, G, H, I, J, K, L, M]\\d")) %>% 
  mutate(nuts=str_extract(nuts, "\\w{2}[:alnum:]{1}")) %>% 
  mutate(population=as.integer(population)) %>% 
  group_by(nuts) %>% 
  summarise(population=sum(population))

population <- population %>% 
  bind_rows(population2)

write_csv(population, file.path("./data/processed/population.csv"))

#2. unemployment statistics ####
