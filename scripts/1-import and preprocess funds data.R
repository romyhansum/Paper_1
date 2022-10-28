#Import and preprocess funds data
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(data.table)



reg_and_nat_governments_2122  <- read_csv("./data/processed/reg_and_nat_governments_2122.csv")

#1. step: Import Excel/csv list of observations
#2. step: Reshape the respective list of projects: Unify list of projects, so that every list contains: operation_name, beneficiary, operation_summary, start_date, end_date, 
#total_EU_expenditure (in €), eu_cofinancing_rate, location_indicator, country_name, category_of_intervention, priority_axis and a NUTS-II indicator.
#3. step: Add respective list to one larger df.

#Austria####
##checken: NUTS-II bzw. NUTS-III Einheiten, die direkt angegeben worden sind (3-digits), müssen separat in case_when aufgeführt werden - alle vorhanden?
austria_efrd <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/austria_2022-09-30-EFRD.xlsx")
austria_esf <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/austria_2022-08-22-ESF.xlsx", skip=4)


colnames(austria_efrd) <- c("operation_name", "beneficiary", "operation_summary", "start_date", "end_date", "total_EU_expenditure", "eu_cofinancing_rate",
                            "location_indicator", "country_name", "category_of_intervention", "priority_axis")

table(austria_efrd$location_indicator)

austria_efrd <- austria_efrd %>%
  filter(priority_axis=='P7 - REACT-EU') %>% 
  mutate(country_name="austria",
         country_id=59,
         fund="efrd",
         total_EU_expenditure=total_EU_expenditure*eu_cofinancing_rate,
         location1=str_extract(location_indicator, "\\d")) %>% 
  mutate(nuts_2=case_when(location1=="1" ~"AT11",
                          location1=="1" ~"AT11",
                          location1=="2"~"AT21",
                          location1=="3"~"AT12",
                          location1=="4"~"AT31",
                          location1=="5"~"AT32",
                          location1=="6"~"AT22",
                          location1=="7"~"AT33",
                          location1=="8"~"AT34",
                          location1=="9"~"AT13")) %>% 
  mutate(nuts_2=case_when(location_indicator=="130 - WIEN (NUTS)"~"AT13",
                          location_indicator=="131 - Wien (NUTS)"~"AT13",
                          TRUE~nuts_2)) %>% 
  select(-location1)


funds <- austria_efrd

austria_esf <- austria_esf %>% 
  select(-`ProjektCode / \r\ncode of operation`) 

colnames(austria_esf) <- c("beneficiary", "operation_name", "operation_summary", "start_date", "end_date", "total_EU_expenditure", "eu_cofinancing_rate",
  "location_indicator", "country_name", "category_of_intervention")

table(austria_esf$location_indicator)

austria_esf <- austria_esf %>% 
  filter(eu_cofinancing_rate==1) %>% 
  mutate(country_name="austria",
         country_id=59,
         fund="esf",
         total_EU_expenditure=total_EU_expenditure*eu_cofinancing_rate,
         priority_axis=NA,
         location1=str_extract(location_indicator, "\\d")) %>% 
  mutate(nuts_2=case_when(location1=="1" ~"AT11",
                          location1=="1" ~"AT11",
                          location1=="2"~"AT21",
                          location1=="3"~"AT12",
                          location1=="4"~"AT31",
                          location1=="5"~"AT32",
                          location1=="6"~"AT22",
                          location1=="7"~"AT33",
                          location1=="8"~"AT34",
                          location1=="9"~"AT13")) %>% 
  mutate(nuts_2=case_when(location_indicator=="110 - BURGENLAND (NUTS)" ~ "AT11",
                          location_indicator=="111 - Mittelburgenland (NUTS)" ~ "AT11",
                          location_indicator=="112 - Nordburgenland (NUTS)" ~ "AT11",
                          location_indicator=="113 - Südburgenland (NUTS)" ~ "AT11",
                          location_indicator=="120 - NIEDERÖSTERREICH (NUTS)" ~ "AT12",
                          location_indicator=="121 - Mostviertel-Eisenwurzen (NUTS)" ~ "AT12",
                          location_indicator=="122 - Niederösterreich-Süd (NUTS)" ~ "AT12",
                          location_indicator=="123 - Sankt Pölten (NUTS)" ~ "AT12",
                          location_indicator=="124 - Waldviertel (NUTS)" ~ "AT12",
                          location_indicator=="125 - Weinviertel (NUTS)" ~ "AT12",
                          location_indicator=="126 - Wiener Umland Südteil (NUTS)" ~ "AT12",
                          location_indicator=="127 - Wiener Umland Nordteil (NUTS)" ~ "AT12",
                          location_indicator=="130 - WIEN (NUTS)" ~ "AT13",
                          location_indicator=="131 - Wien (NUTS)" ~ "AT13",
                          location_indicator=="210 - KÄRNTEN (NUTS)" ~ "AT21",
                          location_indicator=="211 - Klagenfurt-Villach (NUTS)" ~ "AT21",
                          location_indicator=="212 - Oberkärnten (NUTS)" ~"AT21",
                          location_indicator=="213 - Unterkärnten (NUTS)" ~"AT21",
                          location_indicator=="220 - STEIERMARK (NUTS) "~"AT22",
                          location_indicator=="222 - Liezen (NUTS)" ~"AT22",
                          location_indicator=="223 - Östliche Obersteiermark (NUTS)" ~"AT22",
                          location_indicator=="224 - Oststeiermark (NUTS)" ~"AT22",
                          location_indicator=="225 - West- und Südsteiermark (NUTS)" ~"AT22",
                          location_indicator=="226 - Westliche Obersteiermark (NUTS)" ~"AT22",
                          location_indicator=="310 - OBERÖSTERREICH (NUTS)" ~"AT31",
                          location_indicator=="311 - Innviertel (NUTS)" ~"AT31",
                          location_indicator=="312 - Linz-Wels (NUTS) "~"AT31",
                          location_indicator=="313 - Mühlviertel (NUTS)"~"AT31",
                          location_indicator=="314 - Steyr-Kirchdorf (NUTS)"~"AT31",
                          location_indicator=="315 - Traunviertel (NUTS)"~"AT31",
                          location_indicator=="320 - SALZBURG (NUTS)"~"AT32",
                          location_indicator=="321 - Lungau (NUTS)"~"AT32",
                          location_indicator=="322 - Pinzgau-Pongau (NUTS)"~"AT32",
                          location_indicator=="323 - Salzburg-Umgebung (NUTS)"~"AT32",
                          location_indicator=="331 - Außerfern (NUTS)"~"AT33",
                          location_indicator=="332 - Innsbruck (NUTS)"~"AT33",
                          location_indicator=="333 - Osttirol (NUTS)"~"AT33",
                          location_indicator=="334 - Tiroler Oberland (NUTS)"~"AT33",
                          location_indicator=="335 - Tiroler Unterland (NUTS)"~"AT33",
                          location_indicator=="340 - VORARLBERG (NUTS)"~"AT34",
                          location_indicator=="341 - Bludenz-Bregenzer Wald (NUTS)"~"AT34",
                          location_indicator=="342 - Rheintal-Bodenseegebiet (NUTS)"~"AT34",
                          location_indicator=="0 - ÖSTERREICH (NUTS)" ~ "higher NUTS",
                          location_indicator=="100 - OSTÖSTERREICH (NUTS)" ~ "higher NUTS",
                          TRUE~nuts_2)) %>%  
  select(-location1)

funds <- funds %>% 
  bind_rows(austria_esf)
  

#Bulgaria####
##Für OP regions in growth (efrd1) and OP science and education (esf2) Nuts 2 manuell hinzugefügt. Überprüfen, ob weitere Begünstigte hinzugekommen sind.
##Für diese kann ggf. die manuelle Zuordnung von efrd2 schon Abhilfe schaffen.
##Auch bei efrd2 viele manuell hinzugefügt. Überprüfen, ob weitere Begünstigte hinzugekommen sind.
#OPs erdf1, esf1 and esf2 are already filtered for REACT-EU projects. 

bulgaria_efrd1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/bulgaria/bg_OP regions in growth_2022_10_19.xlsx",col_types = c("text","text","text","date","date","numeric","numeric", "text", "text", "skip"), skip=3)
bulgaria_efrd2 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/bulgaria/bg_OP innovations and competitiveness_2022_10_19_REACT.xlsx",col_types = c("text","skip","text", "skip", "text","numeric", "numeric", "skip", "skip", "skip", "skip"), skip=3)
bulgaria_esf1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/bulgaria/bg_OP Human Ressources projects_2022_10_21_REACT.xlsx",col_types = c("text","skip","text", "skip", "text","numeric", "numeric", "skip", "skip", "skip", "skip"), skip=3)
bulgaria_esf2 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/bulgaria/bg_OP science and education_2022_10_19_REACT.xlsx",col_types = c("text","skip","text", "skip", "text","numeric", "numeric", "skip", "skip", "skip", "skip"), skip=3)

LAU_bulgaria <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/EU-27-LAU-2021-NUTS-2021.xlsx", sheet="BG")
LAU_bulgaria <- LAU_bulgaria %>% 
  mutate(nuts_2=str_extract(`NUTS 3 CODE`, "BG\\d{2}"),
         location_indicator=`LAU NAME NATIONAL`) %>% 
  select(nuts_2, location_indicator)

colnames(bulgaria_efrd1) <- c("beneficiary","operation_name", "operation_summary", "start_date", "end_date", "total_EU_expenditure", "eu_cofinancing_rate",
                            "location_indicator", "category_of_intervention")
colnames(bulgaria_efrd2) <- c("beneficiary","location_indicator","operation_name",  "total_budget", "total_EU_expenditure")
colnames(bulgaria_esf1) <- c("beneficiary","location_indicator","operation_name",  "total_budget", "total_EU_expenditure")
colnames(bulgaria_esf2) <- c("beneficiary","location_indicator","operation_name",  "total_budget", "total_EU_expenditure")

bulgaria_efrd1 <- bulgaria_efrd1 %>% 
  mutate(react=str_detect(category_of_intervention, "13 Преодоляване на последиците от извънредното положение, предизвикано от пандемията от COVID-19, и подготовка за екологично, цифрово и устойчиво възстановяване на икономиката")) %>% #filters the REACT-EU thematic objective:  13 Overcoming the consequences of the state of emergency caused by the COVID-19 pandemic and preparing for a green, digital and sustainable recovery of the economy.
  filter(react==TRUE) %>%
  select(-react) %>%
  mutate(total_EU_expenditure=total_EU_expenditure/1.955, #amount is indicated in BGN (exchange rate as specified in list of project:1/1,955)
         eu_cofinancing_rate=eu_cofinancing_rate/100,
         country_name="bulgaria",
         country_id=10,
         fund="efrd")

bulgaria_efrd1 <- bulgaria_efrd1 %>% 
  mutate(nuts_2=case_when(beneficiary=="000695317 Министерство на здравеопазването" ~ "higher NUTS",
                          beneficiary=="000695317 МИНИСТЕРСТВО НА ЗДРАВЕОПАЗВАНЕТО" ~ "higher NUTS"))

funds <- funds %>% 
  bind_rows(bulgaria_efrd1)

bulgaria_efrd2 <- bulgaria_efrd2 %>% 
  filter(!is.na(total_EU_expenditure)) %>% 
  mutate(eu_cofinancing_rate=total_EU_expenditure/total_budget,
         total_EU_expenditure=total_EU_expenditure/1.955, #amount is indicated in BGN (exchange rate as specified in list of project:1/1,955)
         country_name="bulgaria",
         country_id=10,
         fund="efrd") %>% 
  select(-total_budget)

bulgaria_efrd2 <- bulgaria_efrd2 %>% 
  left_join(LAU_bulgaria, by="location_indicator") %>% 
  mutate(municipality1=str_replace(location_indicator, "гр.", "")) %>% #"гр."=city
  mutate(location_indicator=case_when(is.na(nuts_2)==TRUE~municipality1,
                                      TRUE~location_indicator)) %>% 
  left_join(LAU_bulgaria, by="location_indicator") %>% 
  mutate(nuts_2=nuts_2.y) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE~nuts_2.y,
                          TRUE~nuts_2)) %>% 
  select(-municipality1, -nuts_2.x, -nuts_2.y)

bulgaria_efrd2 <- bulgaria_efrd2 %>% 
  mutate(municipality1=str_extract(location_indicator, "[(]BG\\d+[)]")) %>%
  mutate(municipality1=str_extract(municipality1, "BG\\d{2}")) %>% 
  mutate(municipality2=str_extract(location_indicator, "[(].+[)]")) %>%
  mutate(municipality2=gsub('^.{5}', "", municipality2)) %>%
  mutate(municipality2=gsub('.${1}', "", municipality2)) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE ~ municipality1,
                          TRUE~nuts_2)) %>% 
  left_join(LAU_bulgaria, by=c(municipality2="location_indicator")) %>%
  mutate(nuts_2=nuts_2.x) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE~nuts_2.y,
                          TRUE~nuts_2)) %>% 
  select(-municipality1, -municipality2, -nuts_2.y, -nuts_2.x) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE & location_indicator=="София"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="България"~"higher NUTS",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Войводиново"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Свети Влас"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Браниполе"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Труд"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Тополи"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Шемшево"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Игнатиево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Нови Искър"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Драгор"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Царацово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Добринище"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ведраре"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Вълкосел"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Главиница"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Граф Игнатиево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Иваняне"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Казичене"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Пролеша"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Струмяни"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Стряма"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Хрищени"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла (общ.Бяла, обл.Русе)"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Ветрен"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Мартен"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Брезница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бусманци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добрич (общ.Димитровд)"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долна махала"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дъбница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кочан"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кранево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Нови хан"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Рогош"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Скутаре"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Слокощица"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кулата"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Горна Брезница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Комунига"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дъбене"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Разлив"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Асеновд"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла (обл.Русе)"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Добрич-д"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Добричка"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Братя Даскалови"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Самуил"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Банкя"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Баня"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла черква"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Каблешково"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Китен"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Айдемир"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Балканец"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Белащица"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бели Осъм"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Белиш"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Боянци"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Вакарел"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Владая"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Габарево"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Гавраилово"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Голяновци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Гурмазово"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Девенци"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дерманци"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дибич"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Доброславци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Друган"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дълбок дол"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Еленино"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Елховец"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Енина"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Замфирово"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Звъничево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Избеглии"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Йоаким Груево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Калековец"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Камена"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кирково"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Китка"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Костенец"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Крайници"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кърналово"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Локорско"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Марикостиново"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Марчево"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Патрешко"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Петърч"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Равно поле"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Стоб"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Трудовец"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Хераково"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Цалапица"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Чернево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Черноморец"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Ахелой"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Ахтопол"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Берковица,гр.Костинброд"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Брацигово,гр.Пловдив"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Българово"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла (общ.Бяла, обл.Варна)"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Враца,гр.София"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Дебелец"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Долна Оряховица"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Казанлък,гр.Павел баня"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Кермен"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Койнаре,гр.Петрич"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Меричлери"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Обзор"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Перник,гр.София"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Плиска"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Пловдив,с.Голям чардак"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Рилски манастир"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Рудозем,с.Оряховец"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Алеко Константиново"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Антон"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Баланово"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Баните"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Беглеж"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Беласица"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бели извор"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Боголин"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Борима"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Боян Ботево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Братаница"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Брестник"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бутан"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бучино"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Винище"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Войсил"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Враниловци"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Врачеш"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Вресово"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Въгларово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Галиче"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Генерал Кантарджиево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Герман"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Гецово"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Геша"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Голям чардак"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Горна Василица"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Горна Малина"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Горна Мелна"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Граница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Гривица"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Гърмен"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дебелт"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дедово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дивотино"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добродан"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добродан,с.Тумбалово"~"multiple NUTS",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добромир"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добромирци"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Доброплодно"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долна Вереница"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долно Дряново"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долно Камарци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долно Сахране"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Драгижево"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Драгиново"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дядово"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дянково"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Единаковци"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Езеро"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Елин Пелин"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Желю войвода"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Жълтеш"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Зверино"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Злати войвода"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Златитрап"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Змеица"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Зърнево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Игнатица"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Калейца"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Калоян"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кичево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кленовик"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Клисурица"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Козарево"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Козарско"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Копривец"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Копривлен"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Костино"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Крепост"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Крибул"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Крислово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Крушевец"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кубратово"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кукорево"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Куртово Конаре"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Къкрина"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кътина"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Леново"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Лесидрен"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Лесичарка"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Лесичово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Лиляк"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Лъжница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Маноле"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Мещица"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Микрево"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Милево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="c.Мирково"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Мировяне"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Михнево"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Младежко"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Мърчаево"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Негован"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Николаевка"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Осина"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Острово"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Панчарево"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Петревене"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Пищигово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Подем"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Подкрепа"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Покровник"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Присово32"~"BG",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Първомай"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Равадиново"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Равда"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Радиево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Радилово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Радко Димитриево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Резбарци"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Розовец"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Рударци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Руец"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ряхово"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ряховците"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Сандрово"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Сатовча"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Световрачене"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Севар"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Синеморец"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Склаве"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Слишовци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Слънчево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Средногорци"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Стражец (общ.Разд)"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Тулово"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Туховища"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Търновлаг"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Хитрино"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Царева ливада"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Цветкова бара"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Чакаларово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Червена вода"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Черневци"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Черни Осъм"~"BG31",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Черноочене"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Чешнегирово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Шкорпиловци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ябълково (общ.Димитровд)"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Яворец"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ягодина"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Яздач"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Яна"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ясенково"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Сливен,с.Новачево"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="София,с.Копривлен"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Шивачево"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Мирково"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Присово"~"BG32",
                          TRUE~nuts_2))
                          

##Test, ob Begünstigte hinzugekommen sind
test <- bulgaria_efrd2 %>% 
  filter(is.na(nuts_2)) %>%
  group_by(location_indicator) %>% 
  summarize(freq=n())

funds <- funds %>% 
  bind_rows(bulgaria_efrd2)

bulgaria_esf1 <- bulgaria_esf1 %>% 
  filter(!is.na(total_EU_expenditure)) %>% 
  mutate(eu_cofinancing_rate=total_EU_expenditure/total_budget,
         total_EU_expenditure=total_EU_expenditure/1.955, #amount is indicated in BGN (exchange rate as specified in list of project:1/1,955)
         country_name="bulgaria",
         country_id=10,
         fund="esf") %>% 
  select(-total_budget)

bulgaria_esf1 <- bulgaria_esf1 %>% 
  left_join(LAU_bulgaria, by="location_indicator") %>% 
  mutate(municipality1=str_replace(location_indicator, "гр.", "")) %>% #"гр."=city
  mutate(location_indicator=case_when(is.na(nuts_2)~municipality1,
                                      TRUE~location_indicator)) %>% 
  left_join(LAU_bulgaria, by="location_indicator") %>% 
  mutate(nuts_2=nuts_2.y) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE~nuts_2.y,
         TRUE~nuts_2)) %>% 
  select(-municipality1, -nuts_2.x, -nuts_2.y) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2)==TRUE & location_indicator=="София"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="България"~"higher NUTS",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Войводиново"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Свети Влас"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Браниполе"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Труд"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Тополи"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Шемшево"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Игнатиево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Нови Искър"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Драгор"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Царацово"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Добринище"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Ведраре"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Вълкосел"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Главиница"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Граф Игнатиево"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Иваняне"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Казичене"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Пролеша"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Струмяни"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Стряма"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Хрищени"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла (общ.Бяла, обл.Русе)"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Ветрен"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="Мартен"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Брезница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Бусманци"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Добрич (общ.Димитровд)"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Долна махала"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дъбница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кочан"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кранево"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Нови хан"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Рогош"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Скутаре"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Слокощица"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Кулата"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Горна Брезница"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Комунига"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Дъбене"~"BG42",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Разлив"~"BG41",
                          is.na(nuts_2)==TRUE & location_indicator=="Асеновд"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Бяла (обл.Русе)"~"BG32",
                          is.na(nuts_2)==TRUE & location_indicator=="Добрич-д"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="Добричка"~"BG33",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Братя Даскалови"~"BG34",
                          is.na(nuts_2)==TRUE & location_indicator=="с.Самуил"~"BG32",
                          TRUE~nuts_2))

funds <- funds %>% 
  bind_rows(bulgaria_esf1)


bulgaria_esf2 <- bulgaria_esf2 %>% 
  filter(!is.na(total_EU_expenditure)) %>% 
  mutate(eu_cofinancing_rate=total_EU_expenditure/total_budget,
         total_EU_expenditure=total_EU_expenditure/1.955, #amount is indicated in BGN (exchange rate as specified in list of project:1/1,955)
         country_name="bulgaria",
         country_id=10,
         fund="esf") %>% 
  select(-total_budget) %>% 
  mutate(nuts_2="higher NUTS")

funds <- funds %>% 
  bind_rows(bulgaria_esf2)


#Croatia####
##ESF noch 0 Vorhaben in Prio-Achse 6 REACT-EU. Name von Prio-Achse muss noch ergänzt werden.

croatia_efrd <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/Croatia_2022-05-31-EFRD.xlsx", sheet="CUT OFF 31.05.2022", col_types = c("skip","text","text","text","text","date","date", "skip", "numeric", "guess", "skip", "numeric", "skip", "skip", "skip", "skip", "text", "skip"), skip=3)
croatia_esf <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/croatia_2022-06-30-ESF.xlsx", skip=2)

postal_code_croatia  <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/Nuts-Postal code/pc2020_HR_NUTS-2021_v2.0.csv"))
postal_code_croatia  <- postal_code_croatia %>% 
  mutate(nuts_2=str_extract(NUTS3, "HR\\d{2}"),
         postal_code=as.numeric(str_extract(CODE, "\\d+")))


croatia_efrd <- croatia_efrd %>% 
  filter(`Beneficiary name`!="c")

colnames(croatia_efrd) <- c("fund", "beneficiary", "operation_name", "operation_summary", "start_date", "end_date", "total_EU_expenditure", "eu_cofinancing_rate",
                            "location_indicator", "category_of_intervention")


croatia_efrd <- croatia_efrd %>% 
  filter(fund=="EFRR_REACT EU") %>% 
  mutate(country_name="croatia",
         country_id=62,
         fund="efrd",
         priority_axis="11",
         postal_code=location_indicator,
         eu_cofinancing_rate=as.numeric(eu_cofinancing_rate), 
         total_EU_expenditure=total_EU_expenditure/7.53) %>% #amount is indicated in kn (exchange rate:1/7,53)
  left_join(postal_code_croatia, by="postal_code") %>% 
  select(-NUTS3, -postal_code, -CODE) %>%
  mutate(location_indicator=as.character(location_indicator))

funds <- funds %>% 
  bind_rows(croatia_efrd)
  


croatia_esf <- croatia_esf %>% 
  select(-`ŠIFRA UGOVORA\r\nCONTRACT CODE`,-`SPECIFIČNI CILJSPECIFIC OBJECTIVE`,-`NAZIV POSTUPKA DODJELECALL FOR PROPOSALS`,-`VRSTA POSTUPKA DODJELE\r\nTYPE OF AWARD PROCEDURE`, -`STATUS PROVEDBE PROJEKTNIH AKTIVNOSTI\r\nPROJECT ACTIVITIES IMPLEMENTATION STATUS`, -`LOKACIJA NOSITELJA PROJEKTA (županija)PROJECT BENEFICIARY LOCATION (County)`,
         -`STOPA NACIONALNOG SUFINANCIRANJA %`, -`Bespovratna sredstva - Nacionalni dio - HRK`,-`Bespovratna sredstva - Ukupno (EU+Nac) HRK\r\n= Ukupna ugovorena vrijednost bespovratnih sredstava`,
         -`Javni doprinos korisnika - HRK`,-`Privatni doprinos korisnika - HRK`,-`UKUPNI PRIHVATLJIVI IZDACI\r\nTOTAL ELIGIBLE EXPENDITURE\r\n= Bespovratna sredstva ukupno + doprinos korisnika\r\n= Ukupni prihvatljivi troškovi HRK\r\n= Ukupna ugovorena vrijednost projekta`,
         -`POSREDNIČKO TIJELO RAZINE 1INTERMEDIATE BODY LEVEL 1`,-`POSREDNIČKO TIJELO RAZINE 2INTERMEDIATE BODY LEVEL 2` )

colnames(croatia_esf) <- c("priority_axis", "operation_name", "beneficiary", "start_date", "end_date", "location_indicator","eu_cofinancing_rate",
                           "total_EU_expenditure", "operation_summary", "category_of_intervention")
                          
croatia_esf <- croatia_esf %>% 
  filter(priority_axis=="6.") %>% 
  mutate(country_name="croatia",
         country_id=62,
         fund="esf",
         total_EU_expenditure=total_EU_expenditure/7.53) #amount is indicated in kn (exchange rate:1/7,53))


#Czechia####
##überprüfen mit table, ob unter Prioritätsachse 6 weiterhin nur 6.1 existiert.
##Projekte wurden händisch zugeordnet, ggf. noch weitere Begünstigte hinzugekommen?
##beneficiary=="Dům s pečovatelskou službou "Penzion" Polička" in Excel-Datei abwandeln zu: "Dům s pečovatelskou službou Penzion Polička"
##überprüfen, ob weitere hinzugekommene Projekte in supported_projects, die (!) nicht schon in approved projects inkludiert sind.
czechia_efrd1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/CZ_2022-10-10-EFRD-approvedprojects.xlsx", sheet="Schváleno", skip=1)
czechia_efrd2 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/CZ_2022-10-10-EFRD-supported_projects.xlsx")


czechia_efrd1 <- czechia_efrd1 %>% 
  select(-`Datum schválení`, -Výzva, -`Reg. Číslo`, -`Požadováno ze SR (CZK)`)

colnames(czechia_efrd1) <- c("priority_axis",  "beneficiary", "operation_name", "total_EU_expenditure", "total")

table(czechia_efrd1$priority_axis)

czechia_efrd1 <- czechia_efrd1 %>% 
  filter(priority_axis=="6.1") %>% 
  mutate(country_name="czech republic",
         country_id=68,
         fund="efrd",
         eu_cofinancing_rate=total_EU_expenditure/total,
         total_EU_expenditure=total_EU_expenditure/25) %>% #amount indicated is in czk (exchange rate: 1/25)
  select(-total)

czechia_efrd1 <- czechia_efrd1 %>% 
  mutate(nuts_2=case_when(beneficiary=="Fakultní Thomayerova nemocnice"~"CZ01",
                          beneficiary=="Fakultní nemocnice u sv. Anny v Brně"~"CZ06",
                          beneficiary=="Masarykova nemocnice Rakovník s.r.o."~"CZ02",
                          beneficiary=="Mulačova nemocnice s.r.o."~"CZ03",
                          beneficiary=="Nemocnice Nymburk s.r.o."~"CZ02",
                          beneficiary=="NH Hospital a.s."~"CZ02",
                          beneficiary=="PRIVAMED a.s."~"CZ03",
                          beneficiary=="Vojenská nemocnice Brno"~"CZ06",
                          beneficiary=="Nemocnice AGEL Nový Jičín a.s."~"CZ08",
                          beneficiary=="Nemocnice TGM Hodonín, příspěvková organizace"~"CZ06",
                          beneficiary=="Nemocnice Znojmo, příspěvková organizace"~"CZ06",
                          beneficiary=="Nemocnice Na Františku"~"CZ01",
                          beneficiary=="Nemocnice AGEL Ostrava-Vítkovice a.s."~"CZ08",
                          beneficiary=="Kroměřížská nemocnice a.s."~"CZ07",
                          beneficiary=="Vsetínská nemocnice a.s."~"CZ07",
                          beneficiary=="AGEL Středomoravská nemocniční a.s."~"CZ07",
                          beneficiary=="Domažlická nemocnice, a.s."~"CZ03",
                          beneficiary=="Nemocnice Hranice a.s."~"CZ07",
                          beneficiary=="Krajské ředitelství policie Zlínského kraje"~"CZ07",
                          beneficiary=="Nemocnice Ivančice, příspěvková organizace"~"CZ06",
                          beneficiary=="Ústav hematologie a krevní transfuze Praha"~"CZ01",
                          beneficiary=="Nemocnice Žatec, o.p.s."~"CZ04",
                          beneficiary=="Hasičský záchranný sbor Ústeckého kraje"~"CZ04",
                          beneficiary=="Záchranný útvar Hasičského záchranného sboru České republiky"~"higher NUTS",
                          beneficiary=="Zdravotnická záchranná služba Jihočeského kraje"~"CZ03",
                          beneficiary=="Zdravotnická záchranná služba Kraje Vysočina, příspěvková organizace"~"CZ06",
                          beneficiary=="Zdravotnická záchranná služba Královéhradeckého kraje"~"CZ05",
                          beneficiary=="Zdravotnická záchranná služba hlavního města Prahy"~"CZ01",
                          beneficiary=="Zdravotnická záchranná služba Středočeského kraje, příspěvková organizace"~"CZ02",
                          beneficiary=="Fakultní nemocnice Bulovka"~"CZ05",
                          beneficiary=="Institut klinické a experimentální medicíny"~"CZ01",
                          beneficiary=="Nemocnice AGEL Třinec-Podlesí a.s."~"CZ08",
                          beneficiary=="Ústřední vojenská nemocnice - Vojenská fakultní nemocnice Praha"~"CZ01",
                          beneficiary=="Centrum kardiovaskulární a transplantační chirurgie Brno"~"CZ06",
                          beneficiary=="Hasičský záchranný sbor hlavního města Prahy"~"CZ01",
                          beneficiary=="Hasičský záchranný sbor Karlovarského kraje"~"CZ04",
                          beneficiary=="Zdravotnická záchranná služba Karlovarského kraje, příspěvková organizace"~"CZ04",
                          beneficiary=="Centrum Paraple, o.p.s."~"CZ01",
                          beneficiary=="Člověk zpět k člověku, z.s."~"CZ01",
                          beneficiary=="Diakonie ČCE - středisko v Krabčicích"~"CZ04",
                          beneficiary=="Městys Čestice"~"CZ03",
                          beneficiary=="Charita Starý Knín"~"CZ02",
                          beneficiary=="Nemocnice AGEL Jeseník a.s."~"CZ07",
                          beneficiary=="Nemocnice Břeclav, příspěvková organizace"~"CZ06",
                          beneficiary=="Charita Česká Kamenice"~"CZ04",
                          beneficiary=="Město Osečná"~"CZ05",
                          beneficiary=="Nemocnice s poliklinikou Havířov, příspěvková organizace"~"CZ08",
                          beneficiary=="Mělnická zdravotní, a.s."~"CZ02",
                          beneficiary=="Charita Uherský Brod"~"CZ07",
                          beneficiary=="DH Liberec, o.p.s."~"CZ05",
                          beneficiary=="Krajské ředitelství policie hlavního města Prahy"~"CZ01",
                          beneficiary=="Česká abilympijská asociace, z.s."~"higher NUTS",
                          beneficiary=="Krajská hygienická stanice Jihomoravského kraje se sídlem v Brně"~"CZ06",
                          beneficiary=="Krajská hygienická stanice Zlínského kraje se sídlem ve Zlíně"~"CZ07",
                          beneficiary=="Krajské ředitelství policie Středočeského kraje"~"CZ02",
                          beneficiary=="Zdravotnická záchranná služba Libereckého kraje, příspěvková organizace"~"CZ05",
                          beneficiary=="Charita České Budějovice"~"CZ03",
                          beneficiary=="Krajské ředitelství policie kraje Vysočina"~"CZ06",
                          beneficiary=="Krajská hygienická stanice Plzeňského kraje se sídlem v Plzni"~"CZ03",
                          beneficiary=="Hasičský záchranný sbor Libereckého kraje"~"CZ05",
                          beneficiary=="Krajské ředitelství policie Jihomoravského kraje"~"CZ06",
                          beneficiary=="Sociální služby města Pardubic"~"CZ05",
                          beneficiary=="Krajská hygienická stanice Královéhradeckého kraje se sídlem v Hradci Králové"~"CZ05",
                          beneficiary=="Sjednocená organizace nevidomých a slabozrakých České republiky, zapsaný spolek"~"higher NUTS",
                          beneficiary=="Život Plus, z. ú."~"CZ02",
                          beneficiary=="Krajské ředitelství policie Libereckého kraje"~"CZ05",
                          beneficiary=="Vzdělávací, sociální a kulturní středisko při Nadaci Jana Pivečky, o.p.s."~"CZ07",
                          beneficiary=="Oblastní charita Pardubice"~"CZ05",
                          beneficiary=="Krajské ředitelství policie Olomouckého kraje"~"CZ07",
                          beneficiary=="Oblastní charita Sušice"~"CZ03",
                          beneficiary=="GALAXIE CENTRUM POMOCI z.ú."~"CZ08",
                          beneficiary=="BONA, o.p.s."~"higher NUTS",
                          beneficiary=="Charita Uherské Hradiště"~"CZ07",
                          beneficiary=="Komunitní centrum Petrklíč, z.s."~"CZ01",
                          beneficiary=="HEWER, z.s."~"higher NUTS",
                          beneficiary=="Charita Svitavy"~"CZ05",
                          beneficiary=="SOCIÁLNĚ PSYCHIATRICKÉ CENTRUM SLUNÍČKO z.ú."~"CZ04",
                          beneficiary=="Krajská hygienická stanice Karlovarského kraje se sídlem v Karlových Varech"~"CZ04",
                          beneficiary=="Charita Most"~"CZ04",
                          beneficiary=="Domov Alzheimer Darkov z.ú."~"CZ08",
                          beneficiary=="NADĚJE"~"CZ05",
                          beneficiary=="Krajská hygienická stanice Olomouckého kraje se sídlem v Olomouci"~"CZ07",
                          beneficiary=="Handicap centrum Srdce, o.p.s."~"CZ02",
                          beneficiary=="Zdravotně sociální služby Turnov, příspěvková organizace"~"CZ05",
                          beneficiary=="Krajská hygienická stanice Středočeského kraje se sídlem v Praze"~"CZ02",
                          beneficiary=="ESTER z. s."~"CZ07",
                          beneficiary=="Svaz neslyšících a nedoslýchavých osob v ČR, z.s.,  Krajská organizace Ústeckého kraje, p.s."~"CZ04",
                          beneficiary=="Sociální služby Praha 9, z.ú."~"CZ01",
                          beneficiary=="Charita Litoměřice"~"CZ04",
                          beneficiary=="Krajská hygienická stanice Pardubického kraje se sídlem v Pardubicích"~"CZ05",
                          beneficiary=="Centrum denních služeb Čtyřlístek, z.ú."~"CZ04",
                          beneficiary=="Charita Písek"~"CZ03",
                          beneficiary=="CENTRUM HÁJEK z.ú."~"CZ03",
                          beneficiary=="Jihoměstská sociální a.s."~"higher NUTS",
                          beneficiary=="Pečovatelské centrum Praha 7"~"CZ01",
                          beneficiary=="Městská správa sociálních služeb v Mostě - příspěvková organizace"~"CZ04",
                          beneficiary=="Liga vozíčkářů, z. ú."~"higher NUTS",
                          beneficiary=="Kamarád - LORM"~"CZ04",
                          beneficiary=="ČMELÁČEK z. s."~"CZ08",
                          beneficiary=="Zámek Břežany, příspěvková organizace"~"CZ06",
                          beneficiary=="Sociální služby Města Milevska"~"CZ03",
                          beneficiary=="Anděl Strážný, z.ú."~"higher NUTS",
                          beneficiary=="Charita Frýdek - Místek"~"CZ08",
                          beneficiary=="Spirála pomoci, o.p.s."~"higher NUTS",
                          beneficiary=="K srdci klíč, o.p.s."~"higher NUTS",
                          beneficiary=="Mobilní hospic Ondrášek, o.p.s."~"CZ08",
                          beneficiary=="Charita Hlučín"~"CZ08",
                          beneficiary=="Národní ústav pro autismus, z.ú."~"CZ01",
                          beneficiary=="Ústav sociálních služeb města Nové Paky"~"CZ05",
                          beneficiary=="Středisko sociálních služeb města Frýdlant nad Ostravicí"~"CZ08",
                          beneficiary=="CENTROM z. s."~"CZ08",
                          beneficiary=="Život Hradec Králové, o. p. s."~"CZ05",
                          beneficiary=="Hospic sv. Zdislavy, o.p.s."~"CZ05",
                          beneficiary=="Centrum pro integraci osob se zdravotním postižením Královehradeckého kraje, o.p.s."~"CZ05",
                          beneficiary=="Krajská hygienická stanice Libereckého kraje se sídlem v Liberci"~"CZ05",
                          beneficiary=="OPORA"~"higher NUTS",
                          beneficiary=="Kvalitní podzim života, z.ú."~"CZ02",
                          beneficiary=="Magdaléna, o.p.s."~"CZ02",
                          beneficiary=="Dohled na dosah, z.s."~"CZ07",
                          beneficiary=="Charita Česká Lípa"~"CZ05",
                          beneficiary=="SEMIRAMIS z. ú."~"higher NUTS",
                          beneficiary=="Za sklem o.s."~"CZ07",
                          beneficiary=="Háta, o.p.s."~"CZ06",
                          beneficiary=="Centrum sociálních služeb Český Těšín, příspěvková organizace"~"CZ08",
                          beneficiary=="Helias Ústí nad Labem, o.p.s."~"CZ04",
                          beneficiary=="Městská charita Plzeň"~"CZ03",
                          beneficiary=="Harmonie, příspěvková organizace"~"CZ08",
                          beneficiary=="Domov Na Hrádku, poskytovatel sociálních služeb"~"CZ04",
                          beneficiary=="Domov seniorů Hranice, příspěvková organizace"~"CZ07",
                          beneficiary=="Vila Vančurova o.p.s."~"CZ08",
                          beneficiary=="Bonanza Vendolí, z.ú."~"CZ05",
                          beneficiary=="Kostka Krásná Lípa, p.o."~"CZ04",
                          beneficiary=="Farní charita Karlovy Vary"~"CZ04",
                          beneficiary=="Polovina nebe, o.p.s."~"CZ01",
                          beneficiary=="Krystal Help, z.ú."~"CZ08",
                          beneficiary=="Nezávislý život, z.ú."~"CZ02",
                          beneficiary=="Pečovatelská služba Trutnov"~"CZ05",
                          beneficiary=="Sociální služby města Třince, příspěvková organizace"~"CZ08",
                          beneficiary=="Centrum sociální a ošetřovatelské pomoci Praha 15"~"CZ01",
                          beneficiary=="Dobrovolnické centrum, z.s."~"CZ04",
                          beneficiary=="Socius, z. ú."~"CZ02",
                          beneficiary=="Domov Laguna Psáry, poskytovatel sociálních služeb"~"CZ02",
                          beneficiary=="Adámkova vila, Osobní asistence, z.ú."~"CZ08",
                          beneficiary=="Pečovatelská služba okresu Benešov"~"CZ02",
                          beneficiary=="Charita Strážnice"~"CZ06",
                          beneficiary=="Oblastní charita Pelhřimov"~"CZ06",
                          beneficiary=="Centrum sociální a ošetřovatelské pomoci v Praze 10, příspěvková organizace"~"CZ01",
                          beneficiary=="Charita Přerov"~"CZ07",
                          beneficiary=="Nalžovický zámek, poskytovatel sociálních služeb"~"CZ02",
                          beneficiary=="Domov pro seniory Podbořany, příspěvková organizace"~"CZ04",
                          beneficiary=="Hospicová péče sv. Kleofáše, o.p.s."~"CZ03",
                          beneficiary=="PROSAZ, z. ú."~"higher NUTS",
                          beneficiary=="FOKUS České Budějovice, z.ú."~"CZ03",
                          beneficiary=="Centrum sociálních služeb Kojetín, příspěvková organizace"~"CZ07",
                          beneficiary=="SKP-CENTRUM, o.p.s."~"CZ05",
                          beneficiary=="Proxima Sociale o.p.s."~"CZ01",
                          beneficiary=="Krajské ředitelství policie Královéhradeckého kraje"~"CZ05",
                          beneficiary=="Generace Care z.ú."~"CZ06",
                          beneficiary=="Domovy sociálních služeb Litvínov, příspěvková organizace"~"CZ04",
                          beneficiary=="Humanitární sdružení PERSPEKTIVA, z.s."~"CZ04",
                          beneficiary=="REMEDIA PLUS z.ú."~"CZ03",
                          beneficiary=="Laxus z. ú."~"CZ02",
                          beneficiary=="Charita Kroměříž"~"CZ07",
                          beneficiary=="Pracoviště pečovatelské péče, o.p.s."~"CZ05",
                          beneficiary=="AlFi, z.s."~"CZ08",
                          beneficiary=="Charita Lovosice"~"CZ04",
                          beneficiary=="Domácí hospic Vysočina, o.p.s."~"CZ06",
                          beneficiary=="Domov seniorů Jenštejn, poskytovatel sociálních služeb"~"CZ02",
                          beneficiary=="CEDR Pardubice o.p.s."~"CZ05",
                          beneficiary=="CENTRUM SOCIÁLNÍCH A ZDRAVOTNÍCH SLUŽEB MĚSTA PŘÍBRAM"~"CZ02",
                          beneficiary=="Domov důchodců Roudnice nad Labem, příspěvková organizace"~"CZ04",
                          beneficiary=="Asociace pracovní rehabilitace ČR z.s."~"CZ04",
                          beneficiary=="Centrum sociálních služeb Jeseník"~"CZ07",
                          beneficiary=="Neposeda, z.ú."~"CZ01",
                          beneficiary=="Pečovatelská služba Hrádek nad Nisou,příspěvková organizace"~"CZ05",
                          beneficiary=="Unie neslyšících Brno, z.s."~"CZ06",
                          beneficiary=="Město Cvikov"~"CZ05",
                          beneficiary=="Městská pečovatelská služba s denním stacionářem Louny, příspěvková organizace"~"CZ04",
                          beneficiary=="Domov seniorů Mšeno"~"CZ02",
                          beneficiary=="Digitus Mise, z.ú."~"CZ02",
                          beneficiary=="Domov důchodců"~"CZ05",
                          beneficiary=="Komunitní centrum Motýlek, z.ú."~"CZ01",
                          beneficiary=="Villa Vallila, z.ú."~"CZ02",
                          beneficiary=="Diakonie ČCE - Středisko křesťanské pomoci v Litoměřicích"~"CZ04",
                          beneficiary=="Kormidlo Šluknov o.p.s."~"CZ04",
                          beneficiary=="Oblastní spolek Českého červeného kříže Brno"~"CZ06",
                          beneficiary=="Dům s pečovatelskou službou Penzion Polička"~"CZ05",
                          beneficiary=="Sociální služby Česká Třebová"~"CZ05",
                          beneficiary=="TyfloCentrum Brno, o.p.s."~"CZ06",
                          beneficiary=="Centrum MARTIN o.p.s."~"CZ01",
                          beneficiary=="DomA - domácí asistence, z.s."~"CZ08",
                          beneficiary=="Darmoděj z.ú."~"CZ07",
                          beneficiary=="Pečovatelská služba Praha 3"~"CZ01",
                          beneficiary=="DPS Buštěhrad"~"CZ02",
                          beneficiary=="DŮM ROMSKÉ KULTURY o.p.s."~"CZ04",
                          beneficiary=="FOKUS Vysočina, z.ú."~"CZ06",
                          beneficiary=="Občanské sdružení Melius, z.s."~"CZ01",
                          beneficiary=="LECCOS, z.s."~"CZ02",
                          beneficiary=="NAŠE ULITA z.s."~"CZ05",
                          beneficiary=="Sociální služby Sokolov, příspěvková organizace"~"CZ04",
                          beneficiary=="DOTYK II, o.p.s."~"CZ06",
                          beneficiary=="Centrum sociálních služeb Naděje Broumov"~"CZ05",
                          beneficiary=="Společnost Mana, o.p.s."~"CZ07",
                          beneficiary=="Cesta do světa, pobočný spolek Slunečnice, z.s."~"CZ04",
                          beneficiary=="Městské středisko sociálních služeb OÁZA Nové Město nad Metují"~"CZ05",
                          beneficiary=="Centrum sociálních služeb Sokolov, o.p.s."~"CZ04",
                          beneficiary=="Agentura Pondělí, z.s."~"CZ04",
                          beneficiary=="Zet-My, z.s."~"CZ07",
                          beneficiary=="Domov Kytín, poskytovatel sociálních služeb"~"CZ02",
                          beneficiary=="Domov seniorů Jankov, poskytovatel sociálních služeb"~"CZ02",
                          beneficiary=="Sociální služby Chomutov, příspěvková organizace"~"CZ04",
                          beneficiary=="Charita Kralupy nad Vltavou"~"CZ02",
                          beneficiary=="Krajská hygienická stanice kraje Vysočina se sídlem v Jihlavě"~"CZ06",
                          beneficiary=="Centrum zdravotní a sociální péče Liberec, příspěvková organizace"~"CZ05",
                          beneficiary=="Charita Bystřice pod Hostýnem"~"CZ07",
                          beneficiary=="Domov pro seniory a Pečovatelská služba v Žatci"~"CZ04",
                          beneficiary=="Rytmus Střední Čechy, o.p.s."~"CZ02",
                          beneficiary=="Sociální služby SOVY obecně prospěšná společnost"~"CZ03",
                          TRUE~"missing"))

missing_beneficiaries <- czechia_efrd1 %>%
  filter(nuts_2=="missing")

czechia_efrd2 <- czechia_efrd2 %>% 
  select(-Téma, -`Registrační číslo projektu`, -`Název stavu`, -`Název výzvy`)

colnames(czechia_efrd2) <- c("priority_axis",  "operation_name", "beneficiary", "total_EU_expenditure")


czechia_efrd2 <- czechia_efrd2 %>% 
  filter(beneficiary!="Nemocnice AGEL Třinec-Podlesí a.s.") %>%  #Project is already included in efrd1.
  filter(priority_axis=="6.1 - REACT-EU") %>% 
  mutate(country_name="czech republic",
         country_id=68,
         fund="efrd") %>% 
  mutate(total_EU_expenditure=total_EU_expenditure/25, #amount indicated is in czk (exchange rate: 1/25).
         nuts_2=case_when(beneficiary=="Krajské ředitelství policie Jihomoravského kraje"~"CZ06",
                          beneficiary=="Moravskoslezský kraj"~"CZ08",
                          beneficiary=="Fakultní nemocnice Plzeň"~"CZ03",
                          beneficiary=="Nemocnice Pardubického kraje, a.s."~"CZ05",
                          beneficiary=="Diecézní charita Brno"~"CZ06",
                          beneficiary=="Město Chlumec nad Cidlinou"~"CZ05",
                          beneficiary=="Ministerstvo vnitra"~"higher NUTS",
                          TRUE~"missing"))

missing_beneficiaries <- czechia_efrd2 %>%
  filter(nuts_2=="missing")

funds <- funds %>% 
  bind_rows(czechia_efrd1)

funds <- funds %>% 
  bind_rows(czechia_efrd2)


#Denmark####
##EFRD noch 0 Vorhaben in Prio-Achse 6 REACT-EU, aber REACT-EU Vorhaben unter 8. Name von Prio-Achse 6 muss ggf. noch zusätzlich/anstelle ergänzt werden.

denmark_efrd <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/dk_2022-10-19-EFRD.csv"))
denmark_esf  <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/dk_2022-10-19-ESF.csv"))

postal_code_denmark  <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/Nuts-Postal code/pc2020_DK_NUTS-2021_v1.0.csv"))
postal_code_denmark  <- postal_code_denmark %>% 
  mutate(nuts_2=str_extract(NUTS3, "DK\\d{2}"),
         postal_code=as.numeric(str_extract(CODE, "\\d+")))


denmark_efrd <- denmark_efrd %>% 
  select(-`Budget / Budget`, -`Land / Country`, -`Vejnavn / Street name`, -`Bynavn / City name`,-`Interventionskategori4 / Category of intervention4`,-`Kontaktperson / Contacts`,-`Partere / Project partners`)

colnames(denmark_efrd) <- c("operation_name", "start_date", "end_date", "priority_axis", "total_EU_expenditure", "eu_cofinancing_rate", "operation_summary", "fund", "region", "beneficiary", "location_indicator", "category_of_intervention", "category_of_intervention2", "category_of_intervention3", "unklar")

denmark_efrd <- denmark_efrd %>% 
  filter(total_EU_expenditure!="0") %>%
  filter(priority_axis=="8 Gr\xf8n digital genopretning af SMV.\nGr\xf8n, digital genstart af SMVer.\nLokale Erhvervsstyrker" |
           priority_axis=="8 Gr\xf8n digital genopretning af SMV.\nGr\xf8n, digital genstart af SMVer.\nNye gr\xf8nne innovative teknologier - nye potentialer" |
           priority_axis=="8 Gr\xf8n digital genopretning af SMV.\nGr\xf8n, digital genstart af SMVer.\nVirksomhedsprogram") %>%
  select(-unklar) %>%
  mutate(country_name="denmark",
         country_id=21,
         fund="efrd",
         start_date=dmy(start_date),
         end_date=dmy(end_date),
         eu_cofinancing_rate=str_replace(eu_cofinancing_rate, "[:punct:]", ""),
         eu_cofinancing_rate=as.numeric(eu_cofinancing_rate)/100,
         total_EU_expenditure=str_replace(total_EU_expenditure,"\\.", "")) %>%
  mutate(total_EU_expenditure=str_replace(total_EU_expenditure,"\\.", "")) %>%
  mutate(total_EU_expenditure=as.numeric(total_EU_expenditure)/7.44) #indicated in DKK (exchange rate:1/7,44)

denmark_efrd <- denmark_efrd %>% 
  mutate(postal_code=location_indicator) %>%
  left_join(postal_code_denmark, by="postal_code") %>% 
  select(-NUTS3, -postal_code, -CODE, -region)%>%
  mutate(location_indicator=as.character(location_indicator))

funds <- funds %>% 
  bind_rows(denmark_efrd)


denmark_esf <- denmark_esf %>% 
  select(-`Budget / Budget`, -`Land / Country`, -`Vejnavn / Street name`, -`Bynavn / City name`,-`Interventionskategori4 / Category of intervention4`,-`Kontaktperson / Contacts`,-`Partere / Project partners`)

colnames(denmark_esf) <- c("operation_name", "start_date", "end_date", "priority_axis", "total_EU_expenditure", "eu_cofinancing_rate", "operation_summary", "fund", "region", "beneficiary", "location_indicator", "category_of_intervention", "category_of_intervention2", "category_of_intervention3", "unklar")

denmark_esf <- denmark_esf %>% 
  filter(total_EU_expenditure!="0") %>%
  filter(priority_axis=="7 Iv\xe6rks\xe6tteri kompetencer og lokale erh.\nIv\xe6rks\xe6tteri kompetencer og lokale erh.\nIv\xe6rks\xe6tteri" |
           priority_axis=="7 Iv\xe6rks\xe6tteri kompetencer og lokale erh.\nIv\xe6rks\xe6tteri kompetencer og lokale erh.\nLokale erhvervsstyrker (V\xe6kstteams)" |
           priority_axis=="7 Iv\xe6rks\xe6tteri kompetencer og lokale erh.\nIv\xe6rks\xe6tteri kompetencer og lokale erh.\nKompetenceudvikling") %>%
  select(-unklar) %>%
  mutate(country_name="denmark",
         country_id=21,
         fund="esf",
         start_date=dmy(start_date),
         end_date=dmy(end_date),
         eu_cofinancing_rate=str_replace(eu_cofinancing_rate, "[:punct:]", ""),
         eu_cofinancing_rate=as.numeric(eu_cofinancing_rate)/100,
         total_EU_expenditure=str_replace(total_EU_expenditure,"\\.", "")) %>%
  mutate(total_EU_expenditure=str_replace(total_EU_expenditure,"\\.", "")) %>%
  mutate(total_EU_expenditure=as.numeric(total_EU_expenditure)/7.44) #indicated in DKK (exchange rate:1/7,44)

denmark_esf <- denmark_esf %>% 
  mutate(postal_code=location_indicator) %>%
  left_join(postal_code_denmark, by="postal_code") %>% 
  select(-NUTS3, -postal_code, -CODE, -region)%>%
  mutate(location_indicator=as.character(location_indicator))

funds <- funds %>% 
  bind_rows(denmark_esf)
#Finland####
#import as xlsx file as xls and csv are not possible (csv because commas wihtin one cell and xls due to technical issues).
##bei Aland OP noch TO 13...voller Name in (filter()) ergänzen. Aktuell noch 0 Vorhaben.
finland_efrd1 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Etelä-Karjala_EFRE.xlsx")
finland_efrd2 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Etelä-Pohjanmaa_EFRE.xlsx")
finland_efrd3 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Etelä-Savo_EFRE.xlsx")
finland_efrd4 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Kainuu_EFRE.xlsx")
finland_efrd5 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Kanta-Haeme.xlsx")
finland_efrd6 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Keski-Pohjanmaa_EFRE.xlsx")
finland_efrd7 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Keski-Suomi_EFRE.xlsx")
finland_efrd8 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Kymenlaakso_EFRE.xlsx")
finland_efrd9 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Lappi_EFRE.xlsx")
finland_efrd10 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/nationwide_EFRE.xlsx")
finland_efrd11 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Päijät-Häme_EFRE.xlsx")
finland_efrd12 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Pirkanmaa_EFRE.xlsx")
finland_efrd13 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Pohjanmaa_EFRE.xlsx")
finland_efrd14 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Pohjois-Karjala_EFRE.xlsx")
finland_efrd15 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Pohjois-Savo_EFRE.xlsx")
finland_efrd16 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Satakunta_EFRE.xlsx")
finland_efrd17 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Uusimaa_EFRE.xlsx")
finland_efrd18 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Varsinais-Suomi_EFRE.xlsx")
finland_efrd19 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/EFRE/Pohjois-Pohjanmaa_EFRE.xlsx")


finland_efrd1 <- finland_efrd1 %>% 
  mutate(nuts_2="FI1C")
finland_efrd2 <- finland_efrd2 %>% 
  mutate(nuts_2="FI19")
finland_efrd3 <- finland_efrd3 %>% 
  mutate(nuts_2="FI1D")
finland_efrd4 <- finland_efrd4 %>% 
  mutate(nuts_2="FI1D")
finland_efrd5 <- finland_efrd5 %>% 
  mutate(nuts_2="FI1C")
finland_efrd6 <- finland_efrd6 %>% 
  mutate(nuts_2="FI1D")
finland_efrd7 <- finland_efrd7 %>% 
  mutate(nuts_2="FI19")
finland_efrd8 <- finland_efrd8 %>% 
  mutate(nuts_2="FI1C")
finland_efrd9 <- finland_efrd9 %>% 
  mutate(nuts_2="FI1D")
finland_efrd10 <- finland_efrd10 %>% 
  mutate(nuts_2="higher NUTS",
         'Toteutunut EU- ja valtion rahoitus'=as.character('Toteutunut EU- ja valtion rahoitus'),
         'Toteutunut julkinen rahoitus yhteensä'=as.character('Toteutunut julkinen rahoitus yhteensä'))
finland_efrd11 <- finland_efrd11 %>% 
  mutate(nuts_2="FI1C")
finland_efrd12 <- finland_efrd12 %>% 
  mutate(nuts_2="FI19")
finland_efrd13 <- finland_efrd13 %>% 
  mutate(nuts_2="FI19")
finland_efrd14 <- finland_efrd14 %>% 
  mutate(nuts_2="FI1D")
finland_efrd15 <- finland_efrd15 %>% 
  mutate(nuts_2="FI1D")
finland_efrd16 <- finland_efrd16 %>% 
  mutate(nuts_2="FI19")
finland_efrd17 <- finland_efrd17 %>% 
  mutate(nuts_2="FI1B")
finland_efrd18 <- finland_efrd18 %>% 
  mutate(nuts_2="FI1C")
finland_efrd19 <- finland_efrd18 %>% 
  mutate(nuts_2="FI1D")

finland_efrd <- finland_efrd1 %>% 
  bind_rows(finland_efrd2, finland_efrd3, finland_efrd4, finland_efrd5, finland_efrd6, finland_efrd7, finland_efrd8, finland_efrd9, finland_efrd10,
            finland_efrd11, finland_efrd12, finland_efrd13, finland_efrd14, finland_efrd15, finland_efrd16, finland_efrd17, finland_efrd18, finland_efrd19)

finland_efrd <- finland_efrd %>%
  select(-Koodi, -Viranomainen, -Tila, -`Toteutunut EU- ja valtion rahoitus`, -`Toteutunut julkinen rahoitus yhteensä`)

colnames(finland_efrd) <- c("fund", "operation_name", "priority_axis",  "start_date", "end_date", "beneficiary", "total_EU_expenditure", "total_budget","nuts_2")

finland_efrd <- finland_efrd %>% 
  mutate(country_name="finland",
         country_id=67,
         fund="efrd",
         total_EU_expenditure=str_replace_all(total_EU_expenditure, " ", ""),
         total_EU_expenditure=as.numeric(total_EU_expenditure),
         total_budget=str_replace_all(total_budget, " ", ""),
         total_budget=as.numeric(total_budget),
         eu_cofinancing_rate=total_EU_expenditure/total_budget,
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         priority_axis=as.character(priority_axis)) %>% 
  select(-total_budget)

funds <- funds %>% 
  bind_rows(finland_efrd)


finland_esf1 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Etelä-Karjala_ESF.xlsx")
finland_esf2 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Etelä-Pohjanmaa_ESF.xlsx")
finland_esf3 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Etelä-Savo_ESF.xlsx")
finland_esf4 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Kainuu_ESF.xlsx")
finland_esf5 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Kanta-Häme_ESF.xlsx")
finland_esf6 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Keski-Pohjanmaa_ESF.xlsx")
finland_esf7 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Keski-Suomi_ESF.xlsx")
finland_esf8 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Kymenlaakso_ESF.xlsx")
finland_esf9 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Lappi_ESF.xlsx")
finland_esf10 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Pohjois-Pohjanmaa_ESF.xlsx")
finland_esf11 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Päijät-Häme_ESF.xlsx")
finland_esf12 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Pirkanmaa_ESF.xlsx")
finland_esf13 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Pohjanmaa_ESF.xlsx")
finland_esf14 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Pohjois-Karjala_ESF.xlsx")
finland_esf15 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Pohjois-Savo_ESF.xlsx")
finland_esf16 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Satakunta_ESF.xlsx")
finland_esf17 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Uusimaa_ESF.xlsx")
finland_esf18 <- read_xlsx("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/nach Region gefiltert/ESF/Varsinais-Suomi_ESF.xlsx")


finland_esf1 <- finland_esf1 %>% 
  mutate(nuts_2="FI1C")
finland_esf2 <- finland_esf2 %>% 
  mutate(nuts_2="FI19")
finland_esf3 <- finland_esf3 %>% 
  mutate(nuts_2="FI1D")
finland_esf4 <- finland_esf4 %>% 
  mutate(nuts_2="FI1D")
finland_esf5 <- finland_esf5 %>% 
  mutate(nuts_2="FI1C")
finland_esf6 <- finland_esf6 %>% 
  mutate(nuts_2="FI1D")
finland_esf7 <- finland_esf7 %>% 
  mutate(nuts_2="FI19")
finland_esf8 <- finland_esf8 %>% 
  mutate(nuts_2="FI1C")
finland_esf9 <- finland_esf9 %>% 
  mutate(nuts_2="FI1D")
finland_esf10 <- finland_esf10 %>% 
  mutate(nuts_2="FI1D")
finland_esf11 <- finland_esf11 %>% 
  mutate(nuts_2="FI1C")
finland_esf12 <- finland_esf12 %>% 
  mutate(nuts_2="FI19")
finland_esf13 <- finland_esf13 %>% 
  mutate(nuts_2="FI19")
finland_esf14 <- finland_esf14 %>% 
  mutate(nuts_2="FI1D")
finland_esf15 <- finland_esf15 %>% 
  mutate(nuts_2="FI1D")
finland_efrd16 <- finland_esf16 %>% 
  mutate(nuts_2="FI19")
finland_esf17 <- finland_esf17 %>% 
  mutate(nuts_2="FI1B")
finland_esf18 <- finland_esf18 %>% 
  mutate(nuts_2="FI1C")

finland_esf <- finland_esf1 %>% 
  bind_rows(finland_esf2, finland_esf3, finland_esf4, finland_esf5, finland_esf6, finland_esf7, finland_esf8, finland_esf9, finland_esf10,
            finland_esf11, finland_esf12, finland_esf13, finland_esf14, finland_esf15, finland_esf16, finland_esf17, finland_esf18)

finland_esf <- finland_esf %>%
  select(-Koodi, -Viranomainen, -Tila, -`Toteutunut EU- ja valtion rahoitus`, -`Toteutunut julkinen rahoitus yhteensä`)

colnames(finland_esf) <- c("fund", "operation_name", "priority_axis",  "start_date", "end_date", "beneficiary", "total_EU_expenditure", "total_budget","nuts_2")

finland_esf <- finland_esf %>% 
  mutate(country_name="finland",
         country_id=67,
         fund="esf",
         total_EU_expenditure=str_replace_all(total_EU_expenditure, " ", ""),
         total_EU_expenditure=as.numeric(total_EU_expenditure),
         total_budget=str_replace_all(total_budget, " ", ""),
         total_budget=as.numeric(total_budget),
         eu_cofinancing_rate=total_EU_expenditure/total_budget,
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         priority_axis=as.character(priority_axis)) %>% 
  select(-total_budget)

funds <- funds %>% 
  bind_rows(finland_esf)

finland_aland <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/finland/fi_2021-Åland.csv"))
finland_aland <- finland_aland %>% 
  select(-`Diarienummer / registration number`,-`Postnummer / Operation postcode`,-`Land / Country`)

colnames(finland_aland) <- c("beneficiary", "operation_name", "start_date", "end_date", "eu_cofinancing_rate", "total", "operation_summary", "category_of_intervention", "thematic_objective")

finland_aland <- finland_aland %>%
  filter(beneficiary!="") %>% 
  filter(thematic_objective=="13...") %>% 
  mutate(country_name="finland",
         country_id=67,
         fund="esf/efrd",
         eu_cofinancing_rate=str_replace(eu_cofinancing_rate, "[:punct:]", ""),
         eu_cofinancing_rate=as.numeric(eu_cofinancing_rate)/100,
         total=str_replace(total,"€", ""),
         total=str_replace_all(total," ", ""),
         total=str_replace_all(total,",", "."),
         total=as.numeric(total),
         total_EU_expenditure=eu_cofinancing_rate*total,
         start_date=dmy(start_date),
         end_date=dmy(end_date)) %>% 
  select(-thematic_objective, -total)


funds <- funds %>% 
  bind_rows(finland_aland)

#France####
##checken: noch weitere administrative regions bzw. bei volet nation weitere location_indicator dazugekommen?
##OP Mayotte: ergänzen Achsenname Achse 16 (über code codification filterbar), aktuell noch 0 Vorhaben.
##weitere administrative units für outre mer regions hinzugekommen? Nur nationale herausfiltern (siehe notes Dokument)
#in France, nuts1 level is used as equivalent to nuts_2 levels in other countries.
france_esf <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/france/FR-2022-09-national-ESF.xlsx")
france_esf <- france_esf %>% 
  filter(`libellé du PO`=="Programme Opérationnel National FSE") %>% 
  select(-`numéro d'opération`, -`libellé du PO`,-`service gestionnaire`,-`code codification`, -`Coût total retenu sur l'opération`, -`Taux d'intervention FSE réalisé sur l'ensemble de l'opération`)

colnames(france_esf) <- c("administrative_region", "beneficiary","operation_name","operation_summary", "start_date", "end_date", "total", "priority_axis",
                          "eu_cofinancing_rate", "location_indicator", "category_of_intervention")

france_esf <- france_esf %>% 
  filter(priority_axis=="Mise en œuvre des crédits REACT") %>% 
  mutate(eu_cofinancing_rate=eu_cofinancing_rate/100,
         total_EU_expenditure=total*eu_cofinancing_rate,
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         country_name="france",
         country_id=43,
         fund="esf") %>% 
  select(-total)

table(france_esf$administrative_region)
france_esf <- france_esf %>% 
  mutate(nuts_2=case_when(administrative_region=="Aquitaine"~"FRI",
                          administrative_region=="Auvergne"~"FRK",
                          administrative_region=="Bourgogne"~"FRC",
                          administrative_region=="Bretagne"~"FRH",
                          administrative_region=="Centre"~"FRB",
                          administrative_region=="Champagne-Ardenne"~"FR2",
                          administrative_region=="Franche-Comté"~"FRC",
                          administrative_region=="Ile-de-France"~"FR1",
                          administrative_region=="Limousin"~"FRI",
                          administrative_region=="Lorraine"~"FRF",
                          administrative_region=="Poitou-Charentes"~"FRI",
                          administrative_region=="Provence-Alpes-Côte d'Azur"~"FRL",
                          administrative_region=="Rhône-Alpes"~"FRK",
                          TRUE~"multiple NUTS")) %>% 
  mutate(nuts_2=case_when(nuts_2=="multiple NUTS" & location_indicator=="13 départements de l’Occitanie : Ariège (09), Aude (11) Aveyron (12), Gard (30), Haute Garonne (31), Gers (32), Hérault (34), Lot (46), Lozère (48), Hautes Pyrénées (65),  Pyrénées Orientales (66), Tarn (81), Tarn et Garonne (82)"~"FRJ",
                          nuts_2=="multiple NUTS" & location_indicator=="Auvergne Rhône-Alpes"~"FRK",
                          nuts_2=="multiple NUTS" & location_indicator=="BFC"~"FRC",
                          nuts_2=="multiple NUTS" & location_indicator=="Communes de la Région Martinique"~"FRY2",
                          nuts_2=="multiple NUTS" & location_indicator=="DEPARTEMENTS CALVADOS &#8211; MANCHE &#8211; ORNE &#8211; EURE - SEINE MARITIME"~"FRD",
                          nuts_2=="multiple NUTS" & location_indicator=="Départements des Côtes-d'Armor, du Finistère, d'Ille & Vilaine et du Morbihan"~"FRH",
                          nuts_2=="multiple NUTS" & location_indicator=="Ensemble des départements de la Région Corse"~"FRM",
                          nuts_2=="multiple NUTS" & location_indicator=="Guadeloupe & Saint-Martin"~"FRY1",
                          nuts_2=="multiple NUTS" & location_indicator=="Guadeloupe ET Saint-Martin"~"FRY1",
                          nuts_2=="multiple NUTS" & location_indicator=="GUYANE"~"FRY3",
                          nuts_2=="multiple NUTS" & location_indicator=="Hauts-de-France"~"FRE",
                          nuts_2=="multiple NUTS" & location_indicator=="Ile de France"~"FR1",
                          nuts_2=="multiple NUTS" & location_indicator=="Ile-de-France"~"FR1",
                          nuts_2=="multiple NUTS" & location_indicator=="L’ensemble des départements constituant la région Nouvelle Aquitaine."~"FRI",
                          nuts_2=="multiple NUTS" & location_indicator=="La région Bretagne"~"FRH",
                          nuts_2=="multiple NUTS" & location_indicator=="Le projet est réalisé sur les 13 départements de la Région Occitanie : ARIEGE 09,AUDE 11,AVEYRON 12,GARD 30,HAUTE GARONNE 31,GERS 32,LOT 46, LOZERE 48,HAUTES PYRENEES 65,PYRENEES ORIENTALES 66,TARN 81,TARN&GARONNE 82, HERAULT 34"~"FRJ",
                          nuts_2=="multiple NUTS" & location_indicator=="L'ensemble de la région Corse"~"FRM",
                          nuts_2=="multiple NUTS" & location_indicator=="L'ensemble des 10 départements de la région Grand Est (Ardennes, Aube, Marne, Haute-Marne, Meurthe et Moselle, Meuse, Moselle, Vosges, Bas-Rhin et Haut-Rhin)"~"FRF",
                          nuts_2=="multiple NUTS" & location_indicator=="Les cinq départements normands : Calvados- Eure-Manche-Orne-Seine Maritime"~"FRD",
                          nuts_2=="multiple NUTS" & location_indicator=="MARTINIQUE"~"FRY2",
                          nuts_2=="multiple NUTS" & location_indicator=="MAYOTTE"~"FRY5",
                          nuts_2=="multiple NUTS" & location_indicator=="Normandie"~"FRD",
                          nuts_2=="multiple NUTS" & location_indicator=="NOUVELLE-AQUITAINE"~"FRI",
                          nuts_2=="multiple NUTS" & location_indicator=="Nouvelle-Aquitaine"~"FRI",
                          nuts_2=="multiple NUTS" & location_indicator=="Région"~"FRM",
                          nuts_2=="multiple NUTS" & location_indicator=="REGION AUVERGNE RHONE-ALPES"~"FRK",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Auvergne-Rhône-Alpes"~"FRK",
                          nuts_2=="multiple NUTS" & location_indicator=="REGION BOURGOGNE FRANCHE COMTE"~"FRC",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Centre-Val de Loire"~"FRB",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Centre-Val de Loire : départements du Cher, de l'Eure-et-Loir, de l'Indre, de l'Indre-et-Loire, du Loir-et-Cher et du Loiret."~"FRB",
                          nuts_2=="multiple NUTS" & location_indicator=="REGION GRAND EST"~"FRF",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Grand Est dans sa globalité (Départements Ardennes, Marne, Haute-Marne, Aube, Meurthe et Moselle, Moselle, Meuse, Vosges, Bas-Rhin et Haut-Rhin)"~"FRF",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Hauts-de-France"~"FRE",
                          nuts_2=="multiple NUTS" & location_indicator=="REGION OCCITANIE"~"FRJ",
                          nuts_2=="multiple NUTS" & location_indicator=="Région PACA"~"FRL",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Pays de la Loire"~"FRG",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Pays de la Loire : départements de Loire atlantique, Maine et Loire, Mayenne, Sarthe et Vendée."~"FRG",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Provence Alpes Côte d Azur"~"FRL",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Provence-Alpes-Côte d'Azur"~"FRL",
                          nuts_2=="multiple NUTS" & location_indicator=="Région REUNION"~"FRY4",
                          nuts_2=="multiple NUTS" & location_indicator=="Région Réunion"~"FRY4",
                          TRUE~nuts_2)) %>% 
  select(-administrative_region)

funds <- funds %>% 
  bind_rows(france_esf)


france_esf1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/france/FR-2022-09-national-ESF.xlsx")
france_esf1 <- france_esf1 %>% 
  filter(`libellé du PO`=="PO Mayotte" |`libellé du PO`=="PO réunion" |`libellé du PO`=="PO Guadeloupe" |`libellé du PO`=="PO Guyane" |`libellé du PO`=="PO Martinique") %>% 
  select(-`numéro d'opération`, -`libellé du PO`,-`code codification`, -`Coût total retenu sur l'opération`, -`Taux d'intervention FSE réalisé sur l'ensemble de l'opération`)

colnames(france_esf1) <- c("administrative_region", "administering_unit", "beneficiary","operation_name","operation_summary", "start_date", "end_date", "total", "priority_axis",
                          "eu_cofinancing_rate", "location_indicator", "category_of_intervention")


france_esf1 <- france_esf1 %>% 
  filter(priority_axis=="Favoriser la réparation des dommages à la suite de la crise engendrée par la pandémie de COVID-19 et préparer une reprise écologique, numérique et résiliente de l’économie" | 
           priority_axis=="Réparer les dommages à la suite de la crise engendrée par la pandémie de COVID-19 et préparer une reprise écologique, numérique et résiliente de l’économie (REACT EU)" |
           priority_axis=="REACT-EU" |
           priority_axis=="RESSOURCES SUPPLÉMENTAIRES DE REACT-EU POUR FACILITER LA REPRISE")

table(france_esf1$administering_unit)

france_esf1 <- france_esf1 %>%  
  filter(administering_unit!="REGION REUNION - OI") %>% 
  mutate(eu_cofinancing_rate=eu_cofinancing_rate/100,
         total_EU_expenditure=total*eu_cofinancing_rate,
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         country_name="france",
         country_id=43,
         fund="esf") %>% 
  mutate(nuts_2=case_when(administrative_region=="Guadeloupe"~"FRY1",
                            administrative_region=="Guyane"~"FRY3",
                            administrative_region=="Martinique"~"FRY2",
                            administrative_region=="Réunion"~"FRY4",
                            administrative_region=="Mayotte"~"FRY5",
                            TRUE~"multiple NUTS")) %>% 
  select(-total, -administrative_region, -administering_unit)

funds <- funds %>% 
  bind_rows(france_esf1)

#Germany####
germany_esf <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/germany/DE_2022-10-19_Bund_ESF.csv"))

germany_esf <- germany_esf %>% 
  select(-Land, -Finanzierungsform, -`Art des Gebietes`, -Postleitzahl, -Bundesland, -`Territoriale Umsetzungsmechanismen`, -`Sekundäres ESF-Thema`, -`Thematisches Ziel`, -Wirtschaftstätigkeit, -Aktualisierungsdatum)

colnames(germany_esf) <- c("operation_name", "beneficiary",  "operation_summary", "start_date", "end_date", "total", "eu_cofinancing_rate",
                           "priority_axis", "category_of_intervention", "location_indicator")

germany_esf <- germany_esf %>% 
  filter(priority_axis=="Prioritätsachse E: Unterstützung der Krisenbewältigung im Zusammenhang mit der COVID-19-Pandemie") %>% 
  mutate(country_name="germany",
         country_id=54,
         fund="esf",
         eu_cofinancing_rate=str_replace_all(eu_cofinancing_rate, ",", "."),        
         eu_cofinancing_rate=(as.numeric(eu_cofinancing_rate))/100,
         total=str_replace_all(total, "\\.",""),
         start_date=dmy(start_date),
         end_date=dmy(end_date)) %>% 
  mutate(total=str_replace_all(total, ",", ".")) %>% 
  mutate(total=as.numeric(total)) %>% 
  mutate(total_EU_expenditure=total*eu_cofinancing_rate,
         nuts_2=str_extract(location_indicator, "DE[:alnum:]{2}")) %>% 
  select(-total)

funds <- funds %>% 
  bind_rows(germany_esf)

#Greece####
##checken, ob bei ESF noch regionale Projekte dazugekommen sind - dann entsprechend Listen von Website herunterladen.
greece_efrd1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_attiki-2022-10-19-EFRD.xlsx")
greece_efrd2 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece-anatoliki makedonia, thraki_2022-10-19-EFRD.xlsx")
greece_efrd3 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_dytiki ellada-2022-10-19-EFRD.xlsx")
greece_efrd4 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_dytiki makedonia-2022-10-19-EFRD.xlsx")
greece_efrd5 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_ionia nissia-2022-10-19-EFRD.xlsx")
greece_efrd6 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_ipeiros-2022-10-19-EFRD.xlsx")
greece_efrd7 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_kentriki makedonia-2022-10-19-EFRD.xlsx")
greece_efrd8 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_kriti-2022-10-19-EFRD.xlsx")
greece_efrd9 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_notio aigaio-2022-10-19-EFRD.xlsx")
greece_efrd10 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_peloponnisos-2022-10-19-EFRD.xlsx")
greece_efrd11 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_sterea ellada-2022-10-19-EFRD.xlsx")
greece_efrd12 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_voreio aigaio-2022-10-19-EFRD.xlsx")
greece_efrd13 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_thessalia-2022-10-19-EFRD.xlsx")
greece_efrd14 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_multiregional-2022-10-19-EFRD.xlsx")


greece_efrd1 <- greece_efrd1 %>% 
  mutate(nuts_2="EL30")
greece_efrd2 <- greece_efrd2 %>% 
  mutate(nuts_2="EL51")
greece_efrd3 <- greece_efrd3 %>% 
  mutate(nuts_2="EL63")
greece_efrd4 <- greece_efrd4 %>% 
  mutate(nuts_2="EL53")
greece_efrd5 <- greece_efrd5 %>% 
  mutate(nuts_2="EL62")
greece_efrd6 <- greece_efrd6 %>% 
  mutate(nuts_2="EL54")
greece_efrd7 <- greece_efrd7 %>% 
  mutate(nuts_2="EL52")
greece_efrd8 <- greece_efrd8 %>% 
  mutate(nuts_2="EL43")
greece_efrd9 <- greece_efrd9 %>% 
  mutate(nuts_2="EL42")
greece_efrd10 <- greece_efrd10 %>% 
  mutate(nuts_2="EL65")
greece_efrd11 <- greece_efrd11 %>% 
  mutate(nuts_2="EL64")
greece_efrd12 <- greece_efrd12 %>% 
  mutate(nuts_2="EL41")
greece_efrd13 <- greece_efrd13 %>% 
  mutate(nuts_2="EL61")
greece_efrd14 <- greece_efrd14 %>% 
  mutate(nuts_2="higher NUTS")

greece_efrd <- greece_efrd1 %>% 
  bind_rows(greece_efrd2, greece_efrd3, greece_efrd4, greece_efrd5, greece_efrd6, greece_efrd7, greece_efrd8, greece_efrd9, greece_efrd10,
            greece_efrd11, greece_efrd12, greece_efrd13, greece_efrd14)

greece_efrd <- greece_efrd %>%
  select(-`MIS code`,-`Call code`,-`Call title`,-`Spatial development`, -Horizontal, -`Major project`,-`State aid`,-`Has expropriation`, -`Funding Instruments` , -Contracts, -Payments)

colnames(greece_efrd) <- c("operation_name", "beneficiary", "start_date", "end_date", "operational_programme", "fund", "thematic_objective",
"total_budget", "total_EU_expenditure", "nuts_2")

greece_efrd <- greece_efrd %>% 
  filter(thematic_objective=="React EU") %>% 
  mutate(country_name="greece",
         country_id=41,
         fund="efrd",
         eu_cofinancing_rate=total_EU_expenditure/total_budget,
         start_date=dmy(start_date),
         end_date=dmy(end_date)) %>% 
  select(-total_budget, -operational_programme, -thematic_objective)

funds <- funds %>% 
  bind_rows(greece_efrd)

greece_esf <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/greece/greece_2022-10-19-ESF-REACT.xlsx")

greece_esf <- greece_esf %>%
  select(-`MIS code`,-`Call code`,-`Call title`,-`Spatial development`, -Horizontal, -`Major project`,-`State aid`,-`Has expropriation`, -`Funding Instruments` , -Contracts, -Payments)

colnames(greece_esf) <- c("operation_name", "beneficiary", "start_date", "end_date", "operational_programme", "fund", "thematic_objective",
                           "total_budget", "total_EU_expenditure")

greece_esf <- greece_esf %>% 
  filter(!is.na(beneficiary)) %>% 
  mutate(country_name="greece",
         country_id=41,
         fund="esf",
         eu_cofinancing_rate=total_EU_expenditure/total_budget,
         start_date=dmy(start_date),
         end_date=dmy(end_date),
         nuts_2="higher NUTS") %>% 
  select(-total_budget, -operational_programme, -thematic_objective)

funds <- funds %>% 
  bind_rows(greece_esf)


#Hungary####
#Ireland####
##noch weitere Begünstige hinzugekommen? Ggf. location_indicator und Daten ändern.
ireland_esf <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/ireland_2021-12-03-ESF.xlsx", skip=4)

ireland_esf <- ireland_esf %>% 
  select(-`Total YEI Allocated`, -`Total Eligible Expenditure Allocated`, -`Total Public Funding Allocated`, -Postcode, -Website)

colnames(ireland_esf) <- c("operation_name", "beneficiary", "operation_summary", "start_date", "end_date", "total_EU_expenditure","eu_cofinancing_rate",
                           "location_indicator","category_of_intervention")

ireland_esf <- ireland_esf %>% 
  filter(str_detect(operation_name, "REACT-EU")) %>% 
  mutate(country_name="ireland",
         country_id=37,
         fund="esf",
         nuts_2="higher NUTS") %>% 
  select(-location_indicator, -start_date, -end_date)

funds <- funds %>% 
  bind_rows(ireland_esf)


#Italy####
##weitere Begünstigte in Trentino-Alto-Adige hinzugekommen? diese müssen manuell auf die zwei NUTs-2-Einheiten aufteilt werden.
italy_esfefrd <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/italy/it_2022-10-26_alle OPs.csv"))

italy_esfefrd1 <- italy_esfefrd %>% 
  filter(OC_DESCRIZIONE_PROGRAMMA=="PON FESR FSE CITTA' METROPOLITANE" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FESR FSE GOVERNANCE E CAPACITA' ISTITUZIONALE" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FESR FSE PER LA SCUOLA - COMPETENZE E AMBIENTI PER L'APPRENDIMENTO" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FESR FSE RICERCA E INNOVAZIONE" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FESR IMPRESE E COMPETITIVITA'" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FESR INFRASTRUTTURE E RETI" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FSE INCLUSIONE" |
           OC_DESCRIZIONE_PROGRAMMA=="PON FSE SISTEMI DI POLITICHE ATTIVE PER L'OCCUPAZIONE")

remove(italy_esfefrd)

italy_esfefrd1 <- italy_esfefrd1 %>% 
  select(OC_TITOLO_PROGETTO, OC_SINTESI_PROGETTO, FONDO_COMUNITARIO, OC_DESCRIZIONE_PROGRAMMA, COD_OB_TEMATICO, DESCR_OB_TEMATICO, OC_COD_ARTICOLAZ_PROGRAMMA,
         DEN_REGIONE, DEN_PROVINCIA, FINANZ_UE, FINANZ_TOTALE_PUBBLICO, OC_DATA_INIZIO_PROGETTO, OC_DATA_FINE_PROGETTO_PREVISTA, OC_DENOM_BENEFICIARIO)

colnames(italy_esfefrd1) <- c("operation_name", "operation_summary","fund", "operational_programme", "thematic_ojc", "thematic_ojc_long",
                             "priority_axis", "region", "location_indicator", "total_EU_expenditure", "total_public_cofiance", "start_date", "end_date", "beneficiary")

italy_esfefrd <- italy_esfefrd1
remove(italy_esfefrd1)

italy_esfefrd <- italy_esfefrd %>% 
  filter(thematic_ojc=="13") %>% 
  mutate(country_name="italy",
         country_id=26,
         fund=case_when(fund=="FESR"~"efrd",
                        fund=="FSE"~"esf"),
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         total_EU_expenditure=str_replace(total_EU_expenditure, ",", "."),
         total_public_cofiance=str_replace(total_public_cofiance, ",", "."),
         total_EU_expenditure=as.numeric(total_EU_expenditure),
         total_public_cofiance=as.numeric(total_public_cofiance),
         eu_cofinancing_rate=total_EU_expenditure/total_public_cofiance,
         nuts_2=case_when(region=="ABRUZZO" ~"ITF1",
                          region=="BASILICATA" ~"ITF5",
                          region=="CALABRIA" ~"ITF6",
                          region=="CAMPANIA" ~"ITF3",
                          region=="EMILIA-ROMAGNA" ~"ITH5",
                          region=="FRIULI-VENEZIA GIULIA" ~"ITH4",
                          region=="LAZIO" ~"ITI4",
                          region=="LIGURIA" ~"ITC3",
                          region=="LOMBARDIA" ~"ITC4",
                          region=="MARCHE" ~"ITI3",
                          region=="MOLISE" ~"ITF2",
                          region=="PIEMONTE" ~"ITC1",
                          region=="PUGLIA" ~"ITF4",
                          region=="SARDEGNA" ~"ITG2",
                          region=="SICILIA" ~"ITG1",
                          region=="TOSCANA" ~"ITI1",
                          region=="TRENTINO-ALTO ADIGE" ~"ITH",
                          region=="UMBRIA" ~"ITI2",
                          region=="VENETO" ~"ITH3",
                          TRUE~"higher NUTS"),
         nuts_2=case_when(nuts_2=="ITH" & beneficiary=="UNIVERSITA' DEGLI STUDI DI TRENTO"~"ITH2",
                          nuts_2=="ITH" & beneficiary=="LIBERA UNIVERSITA' DI BOLZANO"~"ITH1",
                          TRUE~nuts_2)) %>% 
  select(-thematic_ojc, -thematic_ojc_long, -total_public_cofiance, -region, -operational_programme)


trentino_alto_adige <- italy_esfefrd %>% 
  filter(nuts_2=="ITH")
table(trentino_alto_adige$beneficiary)

funds <- funds %>% 
  bind_rows(italy_esfefrd)


#Lithuania####
lithuania_efrd <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/lithuania_2022-10-19_EFRD-Prio13.xlsx")

lithuania_efrd <- lithuania_efrd %>% 
  select(-Kodas, -`Paraiškos būsena`, -`Projekto išlaidų suma`, -Finansavimas, -`Išmokėta finansavimo suma`, -`Sutarties pasirašymo data`, -`Sutarties galiojimo pabaiga`)

colnames(lithuania_efrd) <- c("operation_name", "beneficiary", "total", "total_EU_expenditure", "end_date", "start_date")

lithuania_efrd <- lithuania_efrd %>% 
  mutate(country_name="lithuania",
         country_id=15,
         fund="efrd",
         eu_cofinancing_rate=total_EU_expenditure/total,
         nuts_2="higher NUTS") %>% 
  select(-total)

lithuania_efrd <- lithuania_efrd %>% 
  mutate(beneficiary=str_replace_all(beneficiary, '"', "")) %>% 
  mutate(nuts_2=case_when(beneficiary=="2L Architects, UAB"~"LT01",
                          TRUE~nuts_2))



#Netherlands####
##überprüfen, ob noch NA bei nuts_2 bestehen - dann ggf. händisch ergänzen (bei ESF sind die PLZ häufig Postbox-Nummern und daher nicht bei PLZ-Liste enthalten.)

netherlands_esf <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/nl_2022-03-05_ESF.csv", skip=4))
netherlands_efrd <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/nl_2022-10-27_EFRD.xlsx", skip=1)

postal_code_netherlands  <- as_tibble(fread("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/other var/Nuts-Postal code/pc2020_NL_NUTS-2021_v1.0.csv"))
postal_code_netherlands <- postal_code_netherlands %>% 
  mutate(nuts_2=str_extract(NUTS3, "NL\\d{2}"),
         postal_code=str_extract(CODE, "\\w+\\s\\w+")) %>% 
  mutate(location_indicator=str_replace(postal_code, " ", ""))


netherlands_efrd <- netherlands_efrd %>% 
  select(-`Project ID`, -`Titel van het project in het Engels`,-Autoriteit, -`Website project`,-`Overige publieke financiering`,-`Overige private financiering`,-`Totale kosten in Euro's`, -`Categorie Steunverlening`,
         -`Straatnaam`, -`Huisnummer`, -Land, -`Datum van laatste bijwerking`, -`Medebegunstigde 1`, -`Medebegunstigde 2`, -`Medebegunstigde 3`, -`Medebegunstigde 4`, -`Medebegunstigde 5`, -`Medebegunstigde 6`,
         -`Medebegunstigde 7`, -`Medebegunstigde 8`, -`Medebegunstigde 9`, -`Medebegunstigde 10`)


colnames(netherlands_efrd) <- c("beneficiary", "operation_name", "fund", "operation_summary","total_EU_expenditure", "eu_cofinancing_rate","postal_code", "region", "start_date", "end_date", "thematic_ojc")

  
netherlands_efrd <- netherlands_efrd %>% 
  filter(thematic_ojc=="13") %>% 
  mutate(country_name="netherlands",
         country_id=8,
         fund="efrd",
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         total_EU_expenditure=str_replace(total_EU_expenditure, "€", ""),
         total_EU_expenditure=str_replace_all(total_EU_expenditure, "\\.", ""),
         total_EU_expenditure=as.numeric(total_EU_expenditure),
         eu_cofinancing_rate=eu_cofinancing_rate/100) %>% 
  left_join(postal_code_netherlands, by="postal_code") %>% 
  select(-NUTS3, -CODE) %>%
  mutate(nuts_2=case_when(is.na(nuts_2) & region=="Drenthe"~"NL13",
                          is.na(nuts_2) & region=="Flevoland"~"NL23",
                          is.na(nuts_2) & region=="Friesland"~"NL12",
                          is.na(nuts_2) &  region=="Gelderland"~"NL22",
                          is.na(nuts_2) &  region=="Groningen"~"NL11",
                          is.na(nuts_2) & region=="Noord-Brabant"~"NL41",
                          is.na(nuts_2) &  region=="Noord-Holland"~"NL32",
                     is.na(nuts_2) &  region=="Overijssel"~"NL21",
                     is.na(nuts_2) &  region=="Utrecht"~"NL31",
                     is.na(nuts_2) &  region=="Zuid-Holland"~"NL33",
                     is.na(nuts_2) &   region=="Zeeland"~"NL34",
                     is.na(nuts_2) &  region=="Limburg"~"NL42",
                     TRUE~nuts_2)) %>% 
  mutate(nuts_2=case_when(is.na(nuts_2) & beneficiary=="Andela Techniek & Innovatie BV"~"NL23",
                          is.na(nuts_2) & beneficiary=="Griphingo Pharmaceuticals"~"NL11",
                          is.na(nuts_2) & beneficiary=="Stichting The Circular Transformers"~"NL32",
                          is.na(nuts_2) & beneficiary=="ChainCraft Development BV"~"NL32",
                          is.na(nuts_2) & beneficiary=="Aletta Jacobs School of Public Health"~"NL11",
                          TRUE~nuts_2),
         location_indicator=postal_code) %>% 
  select(-thematic_ojc, -region, -postal_code)

funds <- funds %>% 
  bind_rows(netherlands_efrd)

netherlands_esf <- netherlands_esf %>% 
  select(-fieldcodes, -prog, -cat_inv_prio, -V5, -oper_id, -oper_seq, -"is_sub-oper_of", -"oper_major_proj\n_id\n", -oper_type, -oper_loc_geo, -oper_loc_country, -cat_them_obj, -fin_cost_total_elig, -fin_fund,  
         -benef_nb, -benef_id, -benef_role, -benef_legal_status)

colnames(netherlands_esf) <- c("priority_axis", "operation_name", "operation_summary", "start_date", "end_date", "location_indicator", "category_of_intervention", "total_EU_expenditure", 
                               "eu_cofinancing_rate", "react", "beneficiary")


netherlands_esf<- netherlands_esf %>% 
  filter(react=="yes") %>% 
  mutate(country_name="netherlands",
         country_id=8,
         fund="esf",
         start_date=dmy(start_date),
         end_date=dmy(end_date),
         total_EU_expenditure=str_replace_all(total_EU_expenditure, "\\.", ""),
         total_EU_expenditure=as.numeric(total_EU_expenditure),
         eu_cofinancing_rate=str_replace(eu_cofinancing_rate, "%", ""),
         eu_cofinancing_rate=as.numeric(eu_cofinancing_rate),
         eu_cofinancing_rate=eu_cofinancing_rate/100,
         category_of_intervention=as.character(category_of_intervention)) %>% 
  left_join(postal_code_netherlands, by="location_indicator") %>% 
  mutate(nuts_2=case_when(is.na(nuts_2) & location_indicator=="1201GM"~"NL32",
                          is.na(nuts_2) & location_indicator=="1500GA"~"NL32",
                          is.na(nuts_2) & location_indicator=="1800BC"~"NL32",
                          is.na(nuts_2) & location_indicator=="2003PB"~"NL32",
                          is.na(nuts_2) & location_indicator=="2300PC"~"NL33",
                          is.na(nuts_2) & location_indicator=="2390AB"~"NL33",
                          is.na(nuts_2) & location_indicator=="2500DJ"~"NL33",
                          is.na(nuts_2) & location_indicator=="2700AA"~"NL33",
                          is.na(nuts_2) & location_indicator=="2800BB"~"NL33",
                          is.na(nuts_2) & location_indicator=="3000KS"~"NL33",
                          is.na(nuts_2) & location_indicator=="3300AA"~"NL33",
                          is.na(nuts_2) & location_indicator=="3503SB"~"NL31",
                          is.na(nuts_2) & location_indicator=="3800EA"~"NL31",
                          is.na(nuts_2) & location_indicator=="4000HH"~"NL22",
                          is.na(nuts_2) & location_indicator=="4200AC"~"NL33",
                          is.na(nuts_2) & location_indicator=="4460MC"~"NL34",
                          is.na(nuts_2) & location_indicator=="4800RH"~"NL41",
                          is.na(nuts_2) & location_indicator=="5000LH"~"NL41",
                          is.na(nuts_2) & location_indicator=="5200GZ"~"NL41",
                          is.na(nuts_2) & location_indicator=="5600RB"~"NL41",
                          is.na(nuts_2) & location_indicator=="5700AZ"~"NL41",
                          is.na(nuts_2) & location_indicator=="5902RK"~"NL42",
                          is.na(nuts_2) & location_indicator=="6040AX"~"NL42",
                          is.na(nuts_2) & location_indicator=="6400AA"~"NL42",
                          is.na(nuts_2) & location_indicator=="6500HG"~"NL22",
                          is.na(nuts_2) & location_indicator=="6710HK"~"NL22",
                          is.na(nuts_2) & location_indicator=="6800EL"~"NL22",
                          is.na(nuts_2) & location_indicator=="7000HA"~"NL22",
                          is.na(nuts_2) & location_indicator=="7300ES"~"NL22",
                          is.na(nuts_2) & location_indicator=="7500AA"~"NL21",
                          is.na(nuts_2) & location_indicator=="7800RA"~"NL13",
                          is.na(nuts_2) & location_indicator=="8000GA"~"NL21",
                          is.na(nuts_2) & location_indicator=="8900JA"~"NL12",
                          is.na(nuts_2) & location_indicator=="9700AN"~"NL11",
                          is.na(nuts_2) & location_indicator=="9701BC"~"N11L",
                          TRUE~nuts_2)) %>% 
  select(-react, -NUTS3, -CODE, -postal_code)

funds <- funds %>% 
  bind_rows(netherlands_esf)


#Poland####
##Achtung bei efrd2: leere Zeilen löschen vor dem Import in R. Aktuell noch 0 Vorhaben in thematischem Objective 13.
##Falls in efrd2 noch Vorhaben hinzukommen - gleiches Vorgehen wie bei EFRD1.
##checken, ob bei efrd3 doch noch Vorhaben in Achse XI hinzugekommen sind.
##checken, ob bei efrd4 doch noch weitere Vorhaben hinzugekommen sind, die REACT-EU sein und nicht higher NUTS sein könnten.
##checken, ob bei efrd5 doch noch Vorhaben in Prioritet 6 hinzugekommen sind.
#all operations that could in theory be part of REACT-EU given their start/end date and budget are higher NUTS operations. Due to their unclear thematic objective, they are not included.

poland_efrd1 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/poland/pl_2022-10-03.xlsx", skip=2)
poland_efrd2 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/poland/pl_2021-02-02_OP knowledge education development_non-competitive projects-ERDF.xlsx", skip=2)
poland_efrd3 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/poland/pl_2021-02-18_OP infrastructure and environment_non-competitive projects-ERDF.xlsx", skip=3, col_types = c("skip", "text", "text", "text", "skip",  "numeric", "numeric", "skip", "skip", "date", "date"))
##einzelne Daten (diejenigen in Textform) fehlen.
poland_efrd4 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/poland/pl_2021-03-21_OP digital poland_non-competitive projects-ERDF.xlsx", skip=3)
poland_efrd5 <- read_excel("C:/Users/RomyH/OneDrive - Hertie School/PhD/PhD project/data/List of projects/poland/pl_2022-02-18_OP smart growth_non-competitive projects-ERDF.xlsx", skip=3, col_types = c("skip", "text", "text", "text", "numeric", "numeric", "skip", "skip", "date", "date"))
##einzelne Daten (diejenigen in Textform) fehlen.

poland_efrd1 <- poland_efrd1 %>% 
  select(c(1, 2, 4, 6, 7, 12, 13, 15, 17, 18, 21, 22))
colnames(poland_efrd1) <- c("operation_name", "operation_summary", "beneficiary", "programme", "priority_axis", "total_EU_expenditure", "eu_cofinancing_rate", "location_indicator",
                                "start_date", "end_date", "category_of_intervention", "thematic_ojc")

poland_efrd1 <- poland_efrd1 %>% 
  filter(thematic_ojc=="13 Wspieranie kryzysowych działań naprawczych w kontekście pandemii COVID-19 i jej skutków społecznych oraz przygotowanie do ekologicznej i cyfrowej odbudowy gospodarki zwiększającej jej odporność") %>% 
  filter(programme=="Program Operacyjny Infrastruktura i Środowisko 2014-2020" | programme=="Program Operacyjny Polska Cyfrowa" |
           programme=="Program Operacyjny Inteligentny Rozwój" | programme=="Program Operacyjny Wiedza Edukacja Rozwój") %>% 
  mutate(country_name="poland",
         country_id=74,
         fund="efrd",
         start_date=ymd(start_date),
         end_date=ymd(end_date),
         eu_cofinancing_rate=eu_cofinancing_rate/100,
         total_EU_expenditure=total_EU_expenditure/4.725) #amount is indicated in PLN (exchange rate:1/4,725,)

poland_efrd1 <- poland_efrd1 %>% 
  mutate(location_indicator1=location_indicator) %>% 
  separate(col=location_indicator1,
           into=c("regions", "subregions"),
           sep=",") %>% 
  mutate(nuts_2=case_when(str_detect(subregions, "WOJ")~"higher NUTS",
                          regions=="Cały Kraj"~"higher NUTS",
                          TRUE~"NA")) %>% 
  mutate(nuts_2=case_when(nuts_2=="NA" & regions=="WOJ.: DOLNOŚLĄSKIE"~"PL51",
                          nuts_2=="NA" & regions=="WOJ.: KUJAWSKO-POMORSKIE"~"PL61",
                          nuts_2=="NA" & regions=="WOJ.: ŁÓDZKIE"~"PL71",
                          nuts_2=="NA" & regions=="WOJ.: LUBELSKIE"~"PL81",
                          nuts_2=="NA" & regions=="WOJ.: LUBUSKIE"~"PL43",
                          nuts_2=="NA" & regions=="WOJ.: MAŁOPOLSKIE"~"PL21",
                          nuts_2=="NA" & regions=="WOJ.: MAZOWIECKIE"~"PL92",
                          nuts_2=="NA" & regions=="WOJ.: OPOLSKIE"~"PL52",
                          nuts_2=="NA" & regions=="WOJ.: PODKARPACKIE"~"PL82",
                          nuts_2=="NA" & regions=="WOJ.: PODLASKIE"~"PL84",
                          nuts_2=="NA" & regions=="WOJ.: POMORSKIE"~"PL63",
                          nuts_2=="NA" & regions=="WOJ.: ŚLĄSKIE"~"PL22",
                          nuts_2=="NA" & regions=="WOJ.: ŚWIĘTOKRZYSKIE"~"PL72",
                          nuts_2=="NA" & regions=="WOJ.: WARMIŃSKO-MAZURSKIE"~"PL62",
                          nuts_2=="NA" & regions=="WOJ.: WIELKOPOLSKIE"~"PL41",
                          nuts_2=="NA" & regions=="WOJ.: ZACHODNIOPOMORSKIE"~"PL42",
                          nuts_2=="NA" & regions=="WOJ.: WARSZAWSKI"~"PL92",
                          TRUE~nuts_2)) %>% 
  select(-programme, -thematic_ojc, -regions, -subregions)

funds <- funds %>% 
  bind_rows(poland_efrd1)

poland_efrd2 <- poland_efrd2 %>% 
  select(c(1, 2, 4, 6, 7, 12, 13, 15, 17, 18, 21, 22))
colnames(poland_efrd2) <- c("operation_name", "operation_summary", "beneficiary", "programme", "priority_axis", "total_EU_expenditure", "eu_cofinancing_rate", "location_indicator",
                            "start_date", "end_date", "category_of_intervention", "thematic_ojc")
poland_efrd2 <- poland_efrd2 %>% 
  filter(thematic_ojc=="13 Wspieranie kryzysowych działań naprawczych w kontekście pandemii COVID-19 i jej skutków społecznych oraz przygotowanie do ekologicznej i cyfrowej odbudowy gospodarki zwiększającej jej odporność")

colnames(poland_efrd3) <- c("operation_name","beneficiary", "priority_axis", "total", "total_EU_expenditure","start_date", "end_date")
table(poland_efrd3$priority_axis)
poland_efrd3 <- poland_efrd3 %>% 
  filter(priority_axis=="XI")

poland_efrd4 <- poland_efrd4 %>% 
  select(-"Numer umowy/ decyzji/ aneksu", -"Działanie - nazwa")
colnames(poland_efrd4) <- c("operation_name","beneficiary","total", "total_EU_expenditure","start_date", "end_date")

colnames(poland_efrd5) <- c("operation_name","beneficiary", "priority_axis", "total", "total_EU_expenditure","start_date", "end_date")
table(poland_efrd5$priority_axis)

#Portugal####


#4. step: Based on unified df: one observation per NUTS-II-level in each MS with EU booked expenditure across funds (and possibly total amount/EU cofinancing rate).#### 


