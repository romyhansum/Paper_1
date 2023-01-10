##Try out and evaluate various models
library(tidyverse)
library(corrplot)
library(fixest)
library(lmerTest)
library(broom)
library(lmtest)
library(sandwich)
library(miceadds)

#1. Transforming IV and DV ####
gov_vote_budget_funds <- read_csv("./data/processed/gov_vote_budget_funds.csv")

gov_vote_budget_funds <- gov_vote_budget_funds %>%  #Replace NA with 0 for countries that are covered in general (Italy, Greece)
  mutate(react_funds_pc=case_when(country=="greece" & is.na(react_funds_pc) ~ 0,
                                  country=="italy" & is.na(react_funds_pc) ~ 0,
                                  TRUE~react_funds_pc),
         react_funds=case_when(country=="greece" & is.na(react_funds) ~ 0,
                               country=="italy" & is.na(react_funds) ~ 0,
                               TRUE~react_funds))

gov_vote_budget_funds_20 <- gov_vote_budget_funds %>% 
  filter(!is.na(react_funds_pc)) %>% 
  filter(reference_year==2020) %>%
  filter(nuts!="FRY1") %>% #potentially it should be removed for option 1c - extremly high values.
  filter(nuts!="FRY2") %>% 
  filter(nuts!="FRY3") %>% 
  filter(nuts!="FRY4") %>% 
  filter(nuts!="FRY5") %>% 
  filter(country!="germany")


#1a Log DV
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  mutate(react_funds_pc_log=case_when(react_funds_pc==0~log(0.01),
                                  TRUE~log(react_funds_pc))) #add some very small amount in case of value 0.

gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  mutate(react_funds_pc_log=case_when(react_funds_pc_s==0~log(0.01),
                                      TRUE~log(react_funds_pc_s))) #add some very small amount in case of value 0.


#1b z-transformation
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  filter(!is.na(react_funds_pc)) %>% 
  group_by(country, reference_year) %>% 
  mutate(react_funds_pc_s=scale(react_funds_pc, center=TRUE, scale=TRUE),
         gov_voteshare_s=scale(gov_voteshare, center=TRUE, scale=TRUE),
         pm_voteshare_s=scale(pm_voteshare, center=TRUE, scale=TRUE),
         diff_voteshare_s=scale(diff_voteshare, center=TRUE, scale=TRUE),
         turnout_s=scale(turnout, center=TRUE, scale=TRUE),
         ue_rate_s=scale(ue_rate, center=TRUE, scale=TRUE),
         gdppc_s=scale(gdppc, center=TRUE, scale=TRUE),
         covid_rate_per_100k_s=scale(covid_rate_per_100k, center=TRUE, scale=TRUE),
         gdp_change_1920_s=scale(gdp_change_1920, center=TRUE, scale=TRUE),
         ue_change_1920_s=scale(ue_change_1920, center=TRUE, scale=TRUE),
         pop_density_s=scale(pop_density, center=TRUE, scale=TRUE),        
         area_s=scale(area, center=TRUE, scale=TRUE)) %>% 
  ungroup()

#1c centered on cluster mean
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  filter(!is.na(react_funds_pc)) %>% 
  group_by(country, reference_year) %>% 
  mutate(react_funds_pc_s=(react_funds_pc-mean(react_funds_pc)),
         gov_voteshare_s=(gov_voteshare-mean(gov_voteshare)),
         pm_voteshare_s=(pm_voteshare-mean(pm_voteshare)),
         diff_voteshare_s=(diff_voteshare-mean(diff_voteshare)),
         turnout_s=(turnout-mean(turnout)),
         ue_rate_s=(ue_rate-mean(ue_rate)),
         gdppc_s=(gdppc-mean(gdppc)),
         covid_rate_per_100k_s=(covid_rate_per_100k-mean(covid_rate_per_100k)),
         gdp_change_1920_s=(gdp_change_1920-mean(gdp_change_1920)),
         ue_change_1920_s=(ue_change_1920-mean(ue_change_1920)),
         pop_density_s=(pop_density-mean(pop_density)),
         area_s=(area-mean(area))) %>% 
  ungroup()

#1d centered IV and DV is proportional to state level
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  filter(!is.na(react_funds_pc)) %>% 
  group_by(country, reference_year) %>% 
  mutate(react_funds_pc_s=(react_funds_pc/(sum(react_funds)/sum(population))),
         gov_voteshare_s=(gov_voteshare-mean(gov_voteshare)),
         pm_voteshare_s=(pm_voteshare-mean(pm_voteshare)),
         diff_voteshare_s=(diff_voteshare-mean(diff_voteshare)),
         turnout_s=(turnout-mean(turnout)),
         ue_rate_s=(ue_rate-mean(ue_rate)),
         gdppc_s=(gdppc-mean(gdppc)),
         covid_rate_per_100k_s=(covid_rate_per_100k-mean(covid_rate_per_100k)),
         gdp_change_1920_s=(gdp_change_1920-mean(gdp_change_1920)),
         ue_change_1920_s=(ue_change_1920-mean(ue_change_1920)),
         pop_density_s=(pop_density-mean(pop_density)),
         area_s=(area-mean(area))) %>% 
  ungroup()


#2. Data overview ####

ggplot(gov_vote_budget_funds_20, aes(react_funds_pc_s)) +
  geom_histogram()

ggplot(gov_vote_budget_funds_20, aes(react_funds_pc)) +
  geom_histogram()

ggplot(gov_vote_budget_funds_20, aes(react_funds_pc_log)) +
  geom_histogram()

ggplot(gov_vote_budget_funds_20, aes(gov_voteshare_s, react_funds_pc_s))+
  geom_point()+
  stat_smooth(method="lm", formula = 'y ~ x') 

ggplot(gov_vote_budget_funds_20, aes(gov_voteshare_s, react_funds_pc_log))+
  geom_point()+
  stat_smooth(method="lm", formula = 'y ~ x') 
  
gov_vote_budget_funds_20 %>% 
ggplot(aes(gov_voteshare_s, react_funds_pc_s, colour=country)) +
  geom_point(show.legend=FALSE) 

gov_vote_budget_funds_20 %>% 
  ggplot(aes(ue_rate_s, react_funds_pc_s, colour=country)) +
  geom_point(show.legend=FALSE) 

ggplot(gov_vote_budget_funds_20, aes(gov_voteshare, react_funds_pc)) + 
  geom_point() +
  stat_smooth(method="lm", formula = 'y ~ x' , se=F, fullrange = T) +
  facet_wrap(~country)

ggplot(gov_vote_budget_funds_20, aes(diff_voteshare, react_funds_pc)) + 
  geom_point() +
  stat_smooth(method="lm", formula = 'y ~ x' , se=F, fullrange = T) +
  facet_wrap(~country)

boxplot(react_funds_pc_s~country, data=gov_vote_budget_funds_20)

col2 <- colorRampPalette(c("#053061", "#2166AC", "#4393C3","#92C5DE", "#D1E5F0", "#FFFFFF","#FDDBC7", "#F4A582", "#D6604D","#B2182B", "#67001F"))

contvar <- gov_vote_budget_funds_20[c("gov_voteshare_s", "diff_voteshare_s", "pm_voteshare_s", "turnout_s", "left_right_prime_min_national", "eu_anti_pro_prime_min_national", "ue_rate_s", "gdppc_s", "ue_change_1920_s", "gdp_change_1920_s", "covid_rate_per_100k_s", "area_s" , "pop_density_s", "eqi_score_national", "rai")]
corMatrix <- round(cor(contvar, use="pairwise.complete.obs"),2) #Only mediate to low correlations, diff_voteshare and gov_voteshare relatively highly correlated.
corMatrix

corrplot(corMatrix, diag=FALSE, type="lower", col="col2"(200))

car::vif(vote_ols) #Variance Inflation Factor for Multicollinearity doesn't show a high level of multicollinearity.
#GDPpc or alternatively pop_density and capital_region could be considered to be removed due to VIF.


#3. Core/Swing voter model - OLS ####

#3a OLS with assumption checks

vote_ols <- lm(react_funds_pc_s ~ gov_voteshare_s+diff_voteshare_s+ ue_rate_s+gdppc_s+ue_change_1920_s +  gdp_change_1920_s + covid_rate_per_100k_s + capital_region, gov_vote_budget_funds_20)
summary(vote_ols)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0)) #Observations 31, 88 and 149 are outliers. However, they do not seem to change regression results.
plot(vote_ols, las=1) 
par(opar)
plot(vote_ols, 4) 

vote_ols <- lm(react_funds_pc_s ~ gov_voteshare_s*eqi_score_national+diff_voteshare_s*eqi_score_national+ ue_rate_s+gdppc_s+ue_change_1920_s +  gdp_change_1920_s + covid_rate_per_100k_s + capital_region, gov_vote_budget_funds_20)


#3b OLS with clustered SEs for countries

vote_olsse <- lm(react_funds_pc_s ~ gov_voteshare_s+diff_voteshare_s+ ue_rate_s+gdppc_s+ue_change_1920_s + gdp_change_1920_s + covid_rate_per_100k_s + capital_region, data = gov_vote_budget_funds_20)
vote_olsse <- coeftest(vote_olsse, vcov = vcovCL, cluster=~country)
tidy(vote_olsse)

vote_olsse <- lm(react_funds_pc_log ~ gov_voteshare_s+diff_voteshare_s+ ue_rate_s+gdppc_s+ue_change_1920_s + gdp_change_1920_s + covid_rate_per_100k_s + capital_region, data = gov_vote_budget_funds_20)
vote_olsse <- coeftest(vote_olsse, vcov = vcovCL, cluster=~country)
tidy(vote_olsse)

#3c weighted OLS (without and with clustered SEs)

vote_olsw <- lm(react_funds_pc_s ~ gov_voteshare_s+diff_voteshare_s+ ue_rate_s+gdppc_s+ue_change_1920_s +  gdp_change_1920_s + covid_rate_per_100k_s + capital_region, weights = share, data=gov_vote_budget_funds_20)
summary(vote_olsw)

vote_olswse <- lm(react_funds_pc_s ~ gov_voteshare_s+diff_voteshare_s+ ue_rate_s+gdppc_s+ue_change_1920_s +  gdp_change_1920_s + covid_rate_per_100k_s + capital_region, weights = share, data=gov_vote_budget_funds_20)
vote_olswse <- coeftest(vote_olswse, vcov = vcovCL, cluster=~country)
tidy(vote_olswse)


#4 fixed effects model with assumption test
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  mutate(react_funds_pc_log=case_when(react_funds_pc==0~log(0.000001),
                                      TRUE~log(react_funds_pc))) #add some very small amount in case of value 0.

gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  filter(react_funds_pc!=0) %>% 
  mutate(react_funds_pc_log=log(react_funds_pc)) #add some very small amount in case of value 0.


vote_fe <- feols(react_funds_pc_log ~ gov_voteshare + diff_voteshare+ ue_rate+gdppc+ue_change_1920 + gdp_change_1920 + covid_rate_per_100k + capital_region | country, gov_vote_budget_funds_20)
summary(vote_fe)

plot(resid(vote_fe), gov_vote_budget_funds_20$react_funds_pc_log) # problem: heteroscedasticity, also confirmed by Levene test

gov_vote_budget_funds_20$vote_fe_res <- residuals(vote_fe)
gov_vote_budget_funds_20$vote_fe_absol_res <- abs(gov_vote_budget_funds_20$vote_fe_res)
gov_vote_budget_funds_20$vote_fe_absol_res2 <- gov_vote_budget_funds_20$vote_fe_absol_res^2
Levene.vote_fe <- lm(vote_fe_absol_res2 ~ country, data=gov_vote_budget_funds_20)
anova(Levene.vote_fe)

gov_vote_budget_funds_20$vote_fe_res_s <- scale(residuals(vote_fe))

ggplot(gov_vote_budget_funds_20, aes(sample=vote_fe_res_s)) + #the residuals of the model seem fairly normally distributed.
  stat_qq() +
  stat_qq_line() +
   labs(x="standard normal quantiles", y="standardized residuals")

gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  mutate(react_funds_log=case_when(react_funds==0~log(0.000001),#add some very small amount in case of value 0.
                                   TRUE~log(react_funds)),
         gdp=gdppc*population) 

gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  filter(react_funds!=0) %>% 
  mutate(react_funds_log=log(react_funds),
         gdp=gdppc*population) 

vote_fe <- feols(react_funds_log ~ gov_voteshare + diff_voteshare+ ue_rate+gdp+ue_change_1920 + gdp_change_1920 + covid_rate_per_100k + capital_region + offset(1/population) | country, gov_vote_budget_funds_20)
summary(vote_fe, cluster=~country) #almost same results with offset of population than with per capita variables

#5. Core/Swing voter model - Mixed models ####

#5a random intercepts for countries, fixed slopes

vote_ri <- lmer(react_funds_pc_log ~ gov_voteshare+diff_voteshare+ue_rate+gdppc+ ue_change_1920 + gdp_change_1920 + covid_rate_per_100k + capital_region + (1|country), data=gov_vote_budget_funds_20)
summary(vote_ri)

MuMIn::r.squaredGLMM(vote_ri) #very low R2 explained by the fixed effects part.
 
plot(vote_ri) #no sign for heteroscedasticity
plot(vote_ri, resid(.) ~ fitted(.) | country)

gov_vote_budget_funds_20$vote_ri_res_s <- scale(residuals(vote_ri))
gov_vote_budget_funds_20$vote_ri_res <- residuals(vote_ri)

ggplot(gov_vote_budget_funds_20, aes(sample=vote_ri_res_s)) + #the residuals of the model seem fairly normally distributed.
  stat_qq() +
  stat_qq_line() +
  labs(x="standard normal quantiles", y="standardized residuals")

ggplot(gov_vote_budget_funds_20, aes(residuals(vote_ri))) +
  geom_histogram()

ggplot(gov_vote_budget_funds_20, aes(x=gov_voteshare, y=vote_ri_res)) + #the plot does not raise concerns about non-linear relationships between IV and DV
  geom_point()

ggplot(gov_vote_budget_funds_20, aes(x=diff_voteshare, y=vote_ri_res)) + #the plot does not raise concerns about non-linear relationships between IV and DV
  geom_point()


gov_vote_budget_funds_20$vote_ri_hat <- hatvalues(vote_ri)  #Observation ITH1 has a high leverage.
ggplot(gov_vote_budget_funds_20, aes(x=vote_ri_hat, y=vote_ri_res)) +
  geom_point()

r_int <- ranef(vote_ri)$country$`(Intercept)` #random factor normally distributed?
qqnorm(r_int)
qqline(r_int)

vote_ri <- lmer(react_funds_log ~ gov_voteshare+diff_voteshare+ue_rate+gdp+ue_change_1920 + gdp_change_1920 + covid_rate_per_100k + capital_region + offset(1/population) + (1|country), data=gov_vote_budget_funds_20)
summary(vote_ri) #different results if population is included as offset variable. 


#5b uncorrelated random slopes for certain IVs and random intercept for countries.
vote_rirs <- lmer(react_funds_pc_s ~ ue_rate_s+gdppc_s+ue_change_1920_s + (1|country)+(-1+diff_voteshare_s|country)+(-1+gov_voteshare_s|country), data=gov_vote_budget_funds_20)
summary(vote_rirs)

vote_rirs <- lmer(react_funds_pc ~ ue_rate+gdppc+ue_change_1920 + (1|country)+(-1+diff_voteshare|country)+(-1+gov_voteshare|country), data=gov_vote_budget_funds_20)
summary(vote_rirs)

vote_rirs2 <- lmer(react_funds_pc_s ~ gov_voteshare_s+ diff_voteshare_s +(-1+ue_rate_s|country)+(-1+ue_change_1920_s|country)+(-1+gdppc_s|country), data=gov_vote_budget_funds_20)
summary(vote_rirs2)

confint(vote_ri, method=c("profile"))

#5c fixed intercept, random slopes with assumption checks
vote_firs <- lmer(react_funds_pc_s ~ (-1+gov_voteshare_s|country)+(-1+diff_voteshare_s|country)+ (-1+ue_rate_s|country)+(-1+gdppc_s|country)+(-1+ue_change_1920_s|country)+(-1+gdp_change_1920_s|country)+(-1+covid_rate_per_100k_s|country) +(-1+capital_region|country), data=gov_vote_budget_funds_20)
summary(vote_firs)
anova(vote_firs)

#test if including UE improves the model (yes)
vote_firs1 <- lmer(react_funds_pc_s ~ (-1+ue_rate_s|country)+(-1+gdppc_s|country)+(-1+ue_change_1920_s|country) +(-1+diff_voteshare_s|country)+(-1+gov_voteshare_s|country), data=gov_vote_budget_funds_20, REML=FALSE)
vote_firs1_reduced <- lmer(react_funds_pc_s ~ (-1+diff_voteshare_s|country)+(-1+gdppc_s|country)+(-1+ue_change_1920_s|country)+(-1+gov_voteshare_s|country), data=gov_vote_budget_funds_20, REML=FALSE)
anova(vote_firs1_reduced, vote_firs1, test="Chi")

#testing level 1 residuals
plot(vote_firs)

gov_vote_budget_funds_20$l1resid <- resid(vote_firs)


gov_vote_budget_funds_20 %>% #checking normality of level-1 residuals
  ggplot(aes(sample=l1resid)) +
  stat_qq()

#testing level 2 residuals

l2_data <- gov_vote_budget_funds_20 %>% 
  group_by(country) %>% 
  summarise(react_mean=mean(react_funds_pc_s),
            gov_voteshare_s=mean(I(gov_voteshare_s)^2),
            diff_voteshare_s=mean(I(diff_voteshare_s)^2),
            ue_rate_s=mean(I(ue_rate_s)^2),
            gdppc_s=mean(I(gdppc_s)^2),
            ue_change_1920_s=mean(I(ue_change_1920_s)^2),
            gdp_change_1920_s=mean(I(gdp_change_1920_s)^2),
            covid_rate_per_100k_s=mean(I(covid_rate_per_100k_s)^2),
            capital_region=0) %>% 
  select(country, gov_voteshare_s, diff_voteshare_s, ue_rate_s, gdppc_s, ue_change_1920_s, gdp_change_1920_s, covid_rate_per_100k_s, capital_region, react_mean) %>% 
  unique()

l2_data$slope_resid=ranef(vote_firs)$country[,2]

l2_data %>% 
  ggplot(aes(x=slope_resid, y=ue_rate_s))+ #slope residuals seem rather independent from the predictor UE_rate
  geom_point()

l2_data %>%  #checking normality of level-2 residuals
  ggplot(aes(sample=slope_resid)) +
  stat_qq()

#checking that level-1 and level-2 residuals are independent
n_per_country <- gov_vote_budget_funds_20 %>% 
  group_by(country) %>% 
  select(country) %>% 
  count() %>% 
  ungroup() %>% 
  select(n) %>% 
  unlist()

gov_vote_budget_funds_20$slope_resid <- rep(l2_data$slope_resid, times=n_per_country)

gov_vote_budget_funds_20 %>% 
  ggplot(aes(x=l1resid, y=slope_resid)) +
  geom_point()

#5d within-between model
gov_vote_budget_funds_20 <- gov_vote_budget_funds_20 %>% 
  mutate(react_funds_pc_log=case_when(react_funds_pc==0~log(0.000001),
                                      TRUE~log(react_funds_pc))) #add some very small amount in case of value 0.

#The wbm package is only for panel data.

#6. Alignment - tested with allocation across regional operational programmes. ####

gov_vote_budget_funds <- gov_vote_budget_funds %>% 
  filter(regionalisation!="no") %>% 
  filter(regionalisation!="not applicable") 

align_simple <- lm(regional_react_budget_pc_s ~ alignment1, gov_vote_budget_funds_20)
summary(align_simple)


