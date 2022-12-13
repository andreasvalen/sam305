

## Rens "Environment"
rm(list = ls())
####Sam script####

library(tidyverse)
library(lme4)
library(forcats)
library(countrycode)

getwd()
#setwd("~/R")

ESS9 <- read.csv("ESS9e03_1.csv") #Les ESS datasettet 
#View(ESS9)

OECD_datasett <- read_csv("OECD.csv") #Lastet inn OECD datasettet 
OECD_u <- OECD_datasett %>%
  filter(TIME == 2019) %>%
  select(TIME, LOCATION, Value)

ESS9_02 <- ESS9 %>%
  select(vote,uemp3m, uemp12m, uemp5yr, gndr, agea, hinctnta, edulvlb, stfeco, 
         stfgov, stfdem, cntry) %>%
  filter(cntry %in% c("AT","BE","CZ","DE","ES","EE","FI","FR","HU","IE","IT",
                      "LT","NL","PL","PT","SE","SI","SK","NO","IS","GB","CH",
                      "DK","LV"))

ESS9_02 %>%
  count(cntry) #sjekke at alle land er med

sum(is.na(ESS9_02)) #ingen NA, men sikkert fordi variabler mC% omkodes 

d <- ESS9_02 %>%
  mutate(vote = replace(vote, vote > 3, NA),
         uemp3m = replace(uemp3m, uemp3m > 2, NA),
         uemp12m = replace(uemp12m, uemp12m > 6, NA),
         uemp5yr = replace(uemp5yr, uemp5yr > 6, NA),
         hinctnta = replace(hinctnta, hinctnta > 10, NA),
         edulvlb = replace(edulvlb, edulvlb > 800, NA),
         age = replace(agea, agea == 999, NA),
         stfeco = replace(stfeco, stfeco > 10, NA),
         stfgov = replace(stfgov, stfgov > 10, NA),
         stfdem = replace(stfdem, stfdem > 10, NA)) %>%
  mutate(vote = ifelse(vote == 1, 1, 0),
         gndr = ifelse(gndr == 1, 1, 0),
         uemp3m = ifelse(uemp3m == 1, 1, 0),
         uemp12m = ifelse(uemp12m == 1, 1, 0),
         uemp5yr = ifelse(uemp5yr == 1, 1, 0),
         edu_cat = case_when(
           edulvlb >= 0 & edulvlb <= 129 ~ 1, 
           edulvlb > 129 & edulvlb <= 520 ~ 2,
           edulvlb > 520 ~ 3
         ),
         age_cat = case_when(
           age < 30 ~ 1,
           age >= 30 & age < 45 ~ 2,
           age >= 45 & age < 60 ~ 3,
           age >= 60 ~ 4
           )) %>%
  mutate(vote = as_factor(vote),
         gndr = as_factor(gndr),
         uemp3m = as_factor(uemp3m),
         uemp12m = as_factor(uemp12m),
         uemp5yr = as_factor(uemp5yr),
         edu_cat = as_factor(edu_cat),
         age_cat = as_factor(age_cat))

sum(is.na(d)) #nC% er det missing values i datasettet

####Merger####

d1 <- OECD_u %>% 
  mutate(
    LOCATION=countrycode::countrycode(LOCATION, origin = "iso3c",destination = "iso2c"),
  ) %>% 
  right_join(d, by = c("LOCATION" = "cntry"))

d1 <- d1 %>%
  rename(year = TIME,
         cntry = LOCATION,
         cntry_uemp = Value,
         ) %>%
  mutate(vote1 = as.numeric(vote)-1,
         uemp3m1 = as.numeric(uemp3m)-1,
         uemp12m1 = as.numeric(uemp12m)-1,
         uemp5yr1 = as.numeric(uemp5yr)-1,
         )



#Viser alle dataene
glimpse(d1)

#Ferdig datarens

####Analyse####

#Gjenomsnitt for hvert lad
d1 %>%
  group_by(cntry) %>%
  summarise(mean_vote = mean(as.numeric(vote1), na.rm = TRUE),
            mean_unemp3 = mean(as.numeric(uemp3m1), na.rm = TRUE),
            mean_unemp12 = mean(as.numeric(uemp12m1), na.rm = TRUE),
            mean_unemp5y = mean(as.numeric(uemp5yr1), na.rm = TRUE))
##Gjenomsnitt for hvert land /end


ggplot(na.omit(d1), aes(x = vote)) +
  geom_dotplot()


ggplot(na.omit(d1), aes(x= as.numeric(uemp3m)-1, y=as.numeric(vote)-1)) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method="glm", se=FALSE, method.args = list(family= binomial))

#Facet wrap: plot for hvert land
ggplot(na.omit(d1), aes(x= uemp3m, y= vote)) + 
  geom_jitter(width = 0.1, height = 0.1) +
  geom_smooth(method="glm", 
              se=FALSE, 
              method.args = list(family=binomial)) +
  facet_wrap(vars(cntry)) +
  labs(x = "Unemployment", 
       y = "Likelihood to vote", 
       title = "Effect of unemployment on voting")


#####



countrylevel <- ess_oecd_comb %>%
  group_by(cntry) %>%
  summarize(cntrylevelvote = mean(as.numeric(vote)-1, na.rm = TRUE))

ggplot(countrylevel, aes(x = cntry, y = cntrylevelvote)) +
  geom_point() +
  geom_smooth(method = "lm")


#Univariate analyser
head(ess_oecd_comb)

ggplot(ess_oecd_comb_2, aes(x = cntry)) +
  geom_bar()
ggplot(ess_oecd_comb_2, aes(x = cntry_uemp, y = cntry)) +
  geom_point()


####logistisk regresjonsanalyse####

#Tom modell
library(lme4)
tom <- glmer(vote ~ 1 + (1 | cntry), data= d1, family = binomial)
summary(tom)

# ICC:
library(sjstats)
performance::icc(tom) #lav


############ steg 1 stoper her intil videre ############################


#icc <- tom@theta[1]^2/ (tom@theta[1]^2 + (3.14159^2/3))
#icc

p <- ggplot(tom, aes(x=cntry, y=vote)) + 
  geom_dotplot(binaxis='y', stackdir='center')

#med ett konstantledd
mod_uten <- glm(vote~1, family=binomial(link="logit"), data=d1)
summary(mod_uten)

#med konstantledd for hvert land
mod_med <- glm(vote ~ factor(cntry), family=binomial(link="logit"), data=d1)
summary(mod_med)

# Modellsammenligning:
anova(mod_uten, mod_med)


#STEP 1 ferdig - resultat: lav IIC, kan bruke fixed effects

mod_1 <- glm (vote ~ uemp12m + as.numeric(edu_cat) + as.numeric(age_cat) + stfgov, data=d1, family = binomial)
summary(mod_1)


mod_1 <- glm (vote ~ uemp12m + as.numeric(edu_cat) + as.numeric(age_cat) + stfgov, data=d1, family = binomial)
summary(mod_1)
library(broom)

#glm_out <- glm(vote ~ uemp3m, data=d1, family = binomial)
#tidy(glm_out, exponentiate = TRUE, conf.int= TRUE)