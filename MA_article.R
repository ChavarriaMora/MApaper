#Who protests?
#Article based partially on my MA paper
#Elias Chavarria-Mora

library (rio) #for importing
library (car)  #for recoding
library (tidyverse) #dplyer, tidyr, ggplot2
library (fastDummies)
library (gridExtra) 
library (SDMTools)
library (Amelia) #Gary King et al package for value imputation, some crazy bootstrapping bayesian shit
library (lme4) #For mixed effects models
library (lmerTest) #expands lme4 to include p-values in mixed effest glm
library (marginaleffects) #for plots of marginal effects
library (sampleSelection) #for Heckman sample selection model
options(max.print=1000000) #increase max.print

#2004-2018 Grand Merge (As of 2022, this is the most complete version, there are newer waves but not on the grand merge)

setwd("C:/Elias/1 Serious/Academia/Datasets/LAPOP")
LAPOP <- import ("2004-2018 LAPOP.dta")

#Drop all variables that are not of interest, and filter such that I only have latinamerican countries
LAPOP_Full_LatAm <-LAPOP%>%
  select (pais, fecha, wt, l1, ros1, ros4, d6, cp6, cp7, cp8, cp13, b2, 
          q2, it1, q1, ed, vb10, prot3, e5, e15, q10d,
          q10e,year, prot2) %>%
  filter (pais<22)

#Make sure that pais and year are categorical
str(LAPOP_Full_LatAm$pais) #numeric
str(LAPOP_Full_LatAm$year) #numeric

#tunr into categorical/factor
LAPOP_Full_LatAm$pais<-as.factor(LAPOP_Full_LatAm$pais)
LAPOP_Full_LatAm$year<-as.factor(LAPOP_Full_LatAm$year)
 #####
#Rename countries 
#OJO, dplyr y car chocan pq los dos usan recode pero diferente
#hay que especificar que es car::recode
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'1'='Mexico'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'2'='Guatemala'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'3'='El Salvador'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'4'='Honduras'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'5'='Nicaragua'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'6'='Costa Rica'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'7'='Panama'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'8'='Colombia'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'9'='Ecuador'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'10'='Bolivia'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'11'='Peru'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'12'='Paraguay'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'13'='Chile'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'14'='Uruguay'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'15'='Brazil'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'16'='Venezuela'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'17'='Argentina'")
LAPOP_Full_LatAm$pais <- car::recode (LAPOP_Full_LatAm$pais, "'21'='Dominican Republic'")
#####

table (LAPOP_Full_LatAm$pais, LAPOP_Full_LatAm$year) #Here you can see every single country and year in the study

#Create a variable based on country and year, if I need a key, this is it, it's all the country-year dyads in the study
LAPOP_Full_LatAm$CountryYear <-paste(LAPOP_Full_LatAm$pais, LAPOP_Full_LatAm$year, sep=" ", collapse=NULL)
table (LAPOP_Full_LatAm$CountryYear)

names(LAPOP_Full_LatAm) #all the variable in dataset

#create index of group participation/associational life as a control. 
#Each moves between 1=highest to 4=never
#I normalized it and then multiplied by 10
#Therefore it moves from 0 to 1

LAPOP_Full_LatAm$Asoc<- (1-((LAPOP_Full_LatAm$cp6+
                              LAPOP_Full_LatAm$cp7+
                               LAPOP_Full_LatAm$cp8+
                               LAPOP_Full_LatAm$cp13-4)/12))*10

#partyid, recoded so 0 means 2, use this
LAPOP_Full_LatAm<-mutate(LAPOP_Full_LatAm, PartyId = ifelse(vb10==1, 1, 0))
table (LAPOP_Full_LatAm$PartyId)
       


#create dummy for both wave/year, country and country-year dyad
LAPOP_Full_LatAm<-dummy_cols(LAPOP_Full_LatAm, select_columns='year')
LAPOP_Full_LatAm<-dummy_cols(LAPOP_Full_LatAm, select_columns='pais')
LAPOP_Full_LatAm<-dummy_cols(LAPOP_Full_LatAm, select_columns='CountryYear')



#check for missings?? like, get a table crossing the variables and include missings, decide if I should drop more
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$ros1, useNA="always")
#drop the missings
LAPOP_Full_LatAm <- subset (LAPOP_Full_LatAm, CountryYear!="Argentina 2019" & CountryYear!="Bolivia 2004" &
                               CountryYear!="Bolivia 2006" & CountryYear!="Bolivia 2019" & CountryYear!="Brazil 2007" &
                               CountryYear!="Brazil 2019" & CountryYear!="Chile 2006" & CountryYear!="Chile 2019" &
                               CountryYear!="Colombia 2004" & CountryYear!="Colombia 2006" & CountryYear!="Costa Rica 2004" & 
                               CountryYear!="Costa Rica 2006" & CountryYear!="Costa Rica 2018" & CountryYear!="Dominican Republic 2004" &
                               CountryYear!="Dominican Republic 2006" & CountryYear!="Dominican Republic 2019" & CountryYear!="Ecuador 2004" &
                               CountryYear!="Ecuador 2006" & CountryYear!="Ecuador 2019" & CountryYear!="El Salvador 2004" &
                               CountryYear!="El Salvador 2006" & CountryYear!="El Salvador 2018" & CountryYear!="Guatemala 2004" &
                               CountryYear!="Guatemala 2006" & CountryYear!="Guatemala 2014" & CountryYear!="Guatemala 2019" &
                               CountryYear!="Honduras 2004" & CountryYear!="Honduras 2006" & CountryYear!="Honduras 2018" &
                               CountryYear!="Mexico 2004" & CountryYear!="Mexico 2006" & CountryYear!="Mexico 2019" &
                               CountryYear!="Nicaragua 2004" & CountryYear!="Nicaragua 2006" & CountryYear!="Nicaragua 2019" &
                               CountryYear!="Panama 2004" & CountryYear!="Panama 2006" & CountryYear!="Panama 2014" &
                               CountryYear!="Panama 2018" & CountryYear!="Paraguay 2006" & CountryYear!="Peru 2006" &
                               CountryYear!="Peru 2019" & CountryYear!="Uruguay 2007" & CountryYear!="Uruguay 2019" &
                               CountryYear!="Venezuela 2007")
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$ros1, useNA="always") #make sure i didn't miss any, it looks fine

table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$d6, useNA="always")
LAPOP_Full_LatAm<-subset (LAPOP_Full_LatAm, CountryYear!="Argentina 2008" & CountryYear!="Bolivia 2008" &
                            CountryYear!="Brazil 2008" & CountryYear!="Chile 2008" & CountryYear!="Colombia 2008" &
                            CountryYear!="Costa Rica 2008" & CountryYear!="Dominican Republic 2008" & CountryYear!="Ecuador 2008" &
                            CountryYear!="El Salvador 2008" & CountryYear!="Guatemala 2008" & CountryYear!="Honduras 2008" &
                            CountryYear!="Mexico 2008" & CountryYear!="Nicaragua 2008" & CountryYear!="Panama 2008" &
                            CountryYear!="Paraguay 2008" & CountryYear!="Peru 2008" & CountryYear!="Uruguay 2008" &
                            CountryYear!="Venezuela 2008")

table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$prot3, useNA="always")
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$l1, useNA="always")
table (LAPOP_Full_LatAm$CountryYear, useNA="always")



#rename variables
LAPOP_Full_LatAm<- LAPOP_Full_LatAm %>%
  rename (id_i = l1 ) %>%
  rename ( ros1a = ros1 ) %>%
  rename ( soc_i = d6 ) %>%
  rename (trstinst = b2 ) %>%
  rename ( age = q2) %>%
  rename ( gentrst = it1) %>%
  rename ( sex = q1 ) %>%
  rename ( educ = ed ) %>%
  rename ( partisan = PartyId ) %>%
  rename ( income1 = q10d) %>%
  rename ( income2 = q10e) %>%
  rename ( asoca = Asoc ) %>%
  rename ( protesta = prot3)


#Reverse the direction of the policy preferences questions. 
#I'm using first words to make sure I ma not accidentaly recoding twice


LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'1'='one'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'2'='two'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'3'='three'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'5'='five'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'6'='six'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'7'='seven'")

LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'one'='7'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'two'='6'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'three'='5'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'five'='3'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'six'='2'")
LAPOP_Full_LatAm$ros1a <- car::recode (LAPOP_Full_LatAm$ros1a, "'seven'='1'")

LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'1'='one'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'2'='two'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'3'='three'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'4'='four'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'5'='five'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'6'='six'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'7'='seven'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'8'='eight'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'9'='nine'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'10'='ten'")

LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'one'='10'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'two'='9'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'three'='8'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'four'='7'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'five'='6'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'six'='5'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'seven'='4'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'eight'='3'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'nine'='2'")
LAPOP_Full_LatAm$soc_i <- car::recode(LAPOP_Full_LatAm$soc_i, "'ten'='1'")

#change the economic ideology variables from 7 to 10 point scales
LAPOP_Full_LatAm$econ_i <-((3*LAPOP_Full_LatAm$ros1a)-1)/2 

#set the gender, partisanship and protest dummies properly
LAPOP_Full_LatAm$sex <- car::recode(LAPOP_Full_LatAm$sex, "'2'='0'")
LAPOP_Full_LatAm$partisan <- car::recode(LAPOP_Full_LatAm$partisan, "'2'='0'")
LAPOP_Full_LatAm$protesta <- car::recode(LAPOP_Full_LatAm$protesta, "'2'='0'")

#get distribution of main questions, tengo que revisarlo!!!
table (LAPOP_Full_LatAm$protesta, useNA="always") #NA: 596
table (LAPOP_Full_LatAm$econ_i, useNA="always") #NA: 5105
table (LAPOP_Full_LatAm$soc_i, useNA="always") #NA: 16976
table (LAPOP_Full_LatAm$id_i, useNA="always")#NA: 19649

table (LAPOP_Full_LatAm$asoca , useNA="always")
table (LAPOP_Full_LatAm$trstinst , useNA="always")
table (LAPOP_Full_LatAm$gentrst , useNA="always")
table (LAPOP_Full_LatAm$age , useNA="always")
table (LAPOP_Full_LatAm$sex , useNA="always")
table (LAPOP_Full_LatAm$educ , useNA="always")
table (LAPOP_Full_LatAm$income1, useNA="always")
table (LAPOP_Full_LatAm$income2, useNA="always")
table (LAPOP_Full_LatAm$vb10 , useNA="always")

  
#estoy viendo si algun pais tiene un monton de missings, pero no problem
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$id_i, useNA = "always")
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$econ_i, useNA = "always")
table (LAPOP_Full_LatAm$CountryYear, LAPOP_Full_LatAm$soc_i, useNA = "always")


#save the non-imputed version
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
write.csv(LAPOP_Full_LatAm, 'MA_db_NOTAmeliaImputed.csv')

#Imputing: gonna use gary kings amelia thing
#I need to eliminate colums that have only missings: prot2
LAPOP_Full_LatAm <- subset(LAPOP_Full_LatAm, select= -prot2)

#also need to eliminate all that don't variate: ie, all the dummies...
colnames(LAPOP_Full_LatAm )
LAPOP_Full_LatAm <- subset(LAPOP_Full_LatAm, select= -c(27:190))

#no se que es e5, e15 fuck that
LAPOP_Full_LatAm <- subset(LAPOP_Full_LatAm, select= -e5)
LAPOP_Full_LatAm <- subset(LAPOP_Full_LatAm, select= -e15)
LAPOP_Full_LatAm <- subset(LAPOP_Full_LatAm, select= -vb10)


#econ i is between the parameters before the imputation,here they are above the parameters, the problem is amelia II
#Here's the problem: Amelia is imputing above the parameters because I was including it as a sqrts variable, not 
#an ordinal one. Solution, first, turn all values in econ_i into intergers rather than floats, 
LAPOP_Full_LatAm$econ_i<-as.integer(LAPOP_Full_LatAm$econ_i)
#an now, in amelia include the new variable as ord, this solves the problem

#NOW, amelia
#el idela?: df_imp<-amelia(x = LAPOP_Full_LatAm, m=5, idvars = "CountryYear", ts = "year", cs = "pais") 
df_imp<-amelia(x = LAPOP_Full_LatAm, m=5, idvars = c("CountryYear", "fecha", "year", "pais"), 
               sqrts = c("age", "asoca", "educ") , 
               noms=c("sex", "partisan","protesta"), 
               ords=c("id_i", "ros1a", "ros4", "soc_i", "econ_i", "cp6", "cp7", "cp8", "cp13", "trstinst", "gentrst", 
                      "income1", "income2")) 
#m=5 is the number of iterations of imputation. 

#Salvar el df con imputaciones
df_impm5<-df_imp$imputations$imp5

#Three levels, individuals nested in country-years nested in countries
#individual level deviation from country-year average
#first I calculate the mean of the individual variable by countryyear
#then, I add that as a new variable, by doing an inner join
#I still need to fix this by looking at what the new db looks like, probably change column name
id_icymn_data<-df_impm5 %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(id_i), list(name=mean))
df_impm5 <- inner_join(df_impm5, id_icymn_data, by="CountryYear") %>%
  rename (id_icymn = name ) #the variable should be id_icymn


econ_icymn_data<-df_impm5 %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(econ_i), list(name=mean))
df_impm5 <- inner_join(df_impm5, econ_icymn_data, by="CountryYear") %>%
  rename (econ_icymn = name ) #the variable should be econ_icymn

soc_icymn_data<-df_impm5 %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(soc_i), list(name=mean))
df_impm5 <- inner_join(df_impm5, soc_icymn_data, by="CountryYear") %>%
  rename (soc_icymn = name ) #the variable should be soc_icymn

#then I calculate the individual level mean deviation
df_impm5$id_imndv <- df_impm5$id_i - df_impm5$id_icymn 
df_impm5$econ_imndv <- df_impm5$econ_i - df_impm5$econ_icymn 
df_impm5$soc_imndv <- df_impm5$soc_i - df_impm5$soc_icymn 

#save squared variables for curvilinear effect
df_impm5$id_imndv2 <- df_impm5$id_imndv * df_impm5$id_imndv
df_impm5$econ_imndv2 <- df_impm5$econ_imndv * df_impm5$econ_imndv 
df_impm5$soc_imndv2 <- df_impm5$soc_imndv * df_impm5$soc_imndv


#Country-year level variables
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/2nd Year/MA paper/GrpahsTables")
countryyearvars <-import('countryyearvariables.csv')  

df_impm5_2 <-merge(df_impm5,countryyearvars,by='CountryYear')

#Generate country average: keep as control for the overall ideology of the government in the country over time
hdicmn_data<-df_impm5_2 %>% 
  group_by(pais) %>%
  summarise_at(vars(HDI), list(name=mean))
df_impm5_2 <- inner_join(df_impm5_2, hdicmn_data, by="pais") %>%
  rename (hdicmn = name )#the variable should be hdicmn

fhcmn_data<-df_impm5_2 %>% 
  group_by(pais) %>%
  summarise_at(vars(FH), list(name=mean))
df_impm5_2 <- inner_join(df_impm5_2, fhcmn_data, by="pais")%>%
  rename (fhcmn = name ) #the variable should be fhcmn
  
#country year deviation from country average
df_impm5_2$hdicymndv <- df_impm5_2$HDI - df_impm5_2$hdicmn
df_impm5_2$fhcymndv <- df_impm5_2$FH - df_impm5_2$fhcmn

MA_db<-df_impm5_2

#save the database at the end
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
write.csv(MA_db, 'MA_db.csv')


###
#All the prior stuff is clean up, the next is analysis
###

#if I need to load it for fast analysis
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
MA_db<-read.csv("MA_db.csv", encoding = "UTF-8" )

#I'm turnin the FE dummies into factors so they can be trated as such in the model
str (MA_db$pais) #string
str (MA_db$year) #integer
MA_db$pais<-as.factor(MA_db$pais)
MA_db$year<-as.factor(MA_db$year) 

table (MA_db$pais)
table (MA_db$year)
table (MA_db$CountryYear)

#Mixed effects logit models 
#the info is taken from https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/
#I have to use the generalized version (glmer) instead of the linear one (lmer), 
#because it is a **logit** me multilevel

#null model, needed to calculate pseudo-r squared
null_m <- glmer(protesta ~ 
                   (1 | CountryYear) #RE
                 , data = MA_db, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0, # should be set to 1 or even 0, otherwise it never converges
                 #and the results are not very different
                 weights = MA_db$wt)
summary(null_m)
#logLik -34216.8

#model of only the controls, for assessing the validity of main variables
control_m <- glmer(protesta ~ trstinst + age + gentrst + sex + educ + partisan + income1 + #controls
                  income2 + asoca + hdicymndv + hdicmn + fhcymndv + fhcmn + #controls
                  pais + year + #FE dummies
                  (1 | CountryYear) #RE
                , data = MA_db, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0, # should be set to 1 or even 0, otherwise it never converges
                #and the results are not very different
                weights = MA_db$wt)
summary(control_m)
#logLik -32221
# Pseudo-R=log(Lc)/log(Lnull)=1-(-32221/-34216.8)=0.0139

#small model, I need this for the graphics to be able to compute CI, to avoid rank deficiency
small_m <- glmer(protesta ~ id_imndv + id_imndv2 + id_icymn + econ_imndv + econ_imndv2 +
                   econ_icymn + soc_imndv + soc_imndv2 + soc_icymn +
                   pais + year + #FE dummies
                   (1 | CountryYear) #RE
                 , data = MA_db, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                 nAGQ = 0, # should be set to 1 or even 0, otherwise it never converges
                 #and the results are not very different
                 weights = MA_db$wt)
summary(small_m)
#logLik -33740.5
# Pseudo-R=log(Lc)/log(Lnull)=1-(-33740.5/-34216.8)=0.0139

#full model, 
full_m <- glmer(protesta ~ id_imndv + id_imndv2 + id_icymn + econ_imndv + econ_imndv2 +
                econ_icymn + soc_imndv + soc_imndv2 + soc_icymn +
                trstinst + age + gentrst + sex + educ + partisan + income1 + #controls
                income2 + asoca + hdicymndv + hdicmn + fhcymndv + fhcmn + #controls
                pais + year + #FE dummies
                (1 | CountryYear) #RE
                , data = MA_db, family = binomial, control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0, # should be set to 1 or even 0, otherwise it never converges
              #and the results are not very different
              weights = MA_db$wt)
summary(full_m) #this is the main table
#problem: fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients

#more info on the model
print(full_m, corr = FALSE)
anova(full_m)
# For McFadden pseudi-R squared: log pseudolikelyhood = -31851.2
# Pseudo-R=log(Lc)/log(Lnull)=1-(-31851.2/-34216.8)=0.069



#To save the model if it takes too long to compute
#saveRDS(full_m, "full_m.rds") #save model
#to read the model, remember to set the working directory
#full_m <- readRDS("full_m.rds") 

#Experomento
#I need to create the predicted values and plot, over that, I cannot use one of the marginaleffects function for plottin
#predict the values
pred<-predictions(small_m) #creates a new dataframe with all the prediction

#join them together, first, rename the first row of pred to "X"
pred<-rename(pred, X = rowid)
MA_db2<-MA_db%>%full_join(pred, by ="X") #merges the two databases

#I use qplot because I need a VERY smoothed line, otherwise theres too much noise thay is impossible to read


qplot(id_imndv.x,predicted,data=MA_db2, geom='smooth', span =0.5, xlab="id", ylab="Predicted probability of protest") #THIS works, needs the smooth
qplot(econ_imndv.x,predicted,data=MA_db2, geom='smooth', span =0.5, xlab="econ", ylab="Predicted probability of protest") #THIS works, needs the smooth
qplot(soc_imndv.x,predicted,data=MA_db2, geom='smooth', span =0.5, xlab="soc", ylab="Predicted probability of protest") #THIS works, needs the smooth


#####Robustness check
#bring in the db WITHOU the imputation with AMELIA II
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
write.csv(LAPOP_Full_LatAm, 'MA_db_NOTAmeliaImputed.csv')
#Finish the cleanup, steps are exactly the same as above
LAPOP_Full_LatAm<-read.csv("MA_db_NOTAmeliaImputed.csv", encoding = "UTF-8" )
id_icymn_data<-LAPOP_Full_LatAm %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(id_i), list(name=mean), na.rm=T) #muy importante poner na.rm=T, si no lo ignora
LAPOP_Full_LatAm <- inner_join(LAPOP_Full_LatAm, id_icymn_data, by="CountryYear") %>%
  rename (id_icymn = name ) 
econ_icymn_data<-LAPOP_Full_LatAm %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(econ_i), list(name=mean), na.rm=T)
LAPOP_Full_LatAm <- inner_join(LAPOP_Full_LatAm, econ_icymn_data, by="CountryYear") %>%
  rename (econ_icymn = name ) 
soc_icymn_data<-LAPOP_Full_LatAm %>% 
  group_by(CountryYear) %>%
  summarise_at(vars(soc_i), list(name=mean), na.rm=T)
LAPOP_Full_LatAm <- inner_join(LAPOP_Full_LatAm, soc_icymn_data, by="CountryYear") %>%
  rename (soc_icymn = name ) 
LAPOP_Full_LatAm$id_imndv <- LAPOP_Full_LatAm$id_i - LAPOP_Full_LatAm$id_icymn 
LAPOP_Full_LatAm$econ_imndv <- LAPOP_Full_LatAm$econ_i - LAPOP_Full_LatAm$econ_icymn 
LAPOP_Full_LatAm$soc_imndv <- LAPOP_Full_LatAm$soc_i - LAPOP_Full_LatAm$soc_icymn 
LAPOP_Full_LatAm$id_imndv2 <- LAPOP_Full_LatAm$id_imndv * LAPOP_Full_LatAm$id_imndv
LAPOP_Full_LatAm$econ_imndv2 <- LAPOP_Full_LatAm$econ_imndv * LAPOP_Full_LatAm$econ_imndv 
LAPOP_Full_LatAm$soc_imndv2 <- LAPOP_Full_LatAm$soc_imndv * LAPOP_Full_LatAm$soc_imndv
setwd("C:/Elias/1 Serious/Academia/University of Pittsburgh/2nd Year/MA paper/GrpahsTables")
countryyearvars <-import('countryyearvariables.csv')  
LAPOP_Full_LatAm_2 <-merge(LAPOP_Full_LatAm,countryyearvars,by='CountryYear')
hdicmn_data<-LAPOP_Full_LatAm_2 %>% 
  group_by(pais) %>%
  summarise_at(vars(HDI), list(name=mean), na.rm=T)
LAPOP_Full_LatAm_2 <- inner_join(LAPOP_Full_LatAm_2, hdicmn_data, by="pais") %>%
  rename (hdicmn = name )
fhcmn_data<-LAPOP_Full_LatAm_2 %>% 
  group_by(pais) %>%
  summarise_at(vars(FH), list(name=mean), na.rm=T)
LAPOP_Full_LatAm_2 <- inner_join(LAPOP_Full_LatAm_2, fhcmn_data, by="pais")%>%
  rename (fhcmn = name ) 
LAPOP_Full_LatAm_2$hdicymndv <- LAPOP_Full_LatAm_2$HDI - LAPOP_Full_LatAm_2$hdicmn
LAPOP_Full_LatAm_2$fhcymndv <- LAPOP_Full_LatAm_2$FH - LAPOP_Full_LatAm_2$fhcmn
LAPOP_Full_LatAm_Cleanedup<-LAPOP_Full_LatAm_2
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
write.csv(LAPOP_Full_LatAm_Cleanedup, 'MA_db_NOTAmeliaImputed2.csv')

#Ok, for the analysis, If I need to bring it in
setwd("C:/Elias/1 Serious/Academia/publicaciones/1 WorkingPapers/MApaper")
LAPOP_Full_LatAm<-read.csv("MA_db_NOTAmeliaImputed2.csv", encoding = "UTF-8" )

#First thing I was asked to do was a Heckman sample selection model, which models as a sigmoid, tobit-like function the 
#probability of the missing value being randomly missing, and then regresses the data including that probability as 
#another explanatory variable
#Heckman model, most basic one, tobit-2 model
#canonical form is this, selection(ys ~ xs, yo ~ xo), bascially ys is the selection formula (the tendency to answer), the 
#yo is the outcome regression, ie the most insteresting one, and I am keeping everything as FE for simplicity
#The two matrices cannot be indetical
heckman_m<-selection(LAPOP_Full_LatAm$protesta ~ LAPOP_Full_LatAm$id_imndv + LAPOP_Full_LatAm$id_imndv2 + 
                       LAPOP_Full_LatAm$id_icymn + LAPOP_Full_LatAm$econ_imndv + LAPOP_Full_LatAm$econ_imndv2 +
                       LAPOP_Full_LatAm$econ_icymn + LAPOP_Full_LatAm$soc_imndv + LAPOP_Full_LatAm$soc_imndv2 + 
                       LAPOP_Full_LatAm$soc_icymn + LAPOP_Full_LatAm$trstinst + LAPOP_Full_LatAm$age + 
                       LAPOP_Full_LatAm$gentrst + LAPOP_Full_LatAm$sex + LAPOP_Full_LatAm$educ + LAPOP_Full_LatAm$partisan + 
                       LAPOP_Full_LatAm$income1 + LAPOP_Full_LatAm$income2 + LAPOP_Full_LatAm$asoca,
                     LAPOP_Full_LatAm$protesta ~ LAPOP_Full_LatAm$id_imndv + LAPOP_Full_LatAm$id_imndv2 + 
                       LAPOP_Full_LatAm$id_icymn + LAPOP_Full_LatAm$econ_imndv + LAPOP_Full_LatAm$econ_imndv2 +
                       LAPOP_Full_LatAm$econ_icymn + LAPOP_Full_LatAm$soc_imndv + LAPOP_Full_LatAm$soc_imndv2 + 
                       LAPOP_Full_LatAm$soc_icymn + LAPOP_Full_LatAm$trstinst + LAPOP_Full_LatAm$age + 
                       LAPOP_Full_LatAm$gentrst + LAPOP_Full_LatAm$sex + LAPOP_Full_LatAm$educ + LAPOP_Full_LatAm$partisan + 
                       LAPOP_Full_LatAm$income1 + LAPOP_Full_LatAm$income2 + LAPOP_Full_LatAm$asoca, method="ml", maxMethod="BHHH")

summary(heckman_m)
#full model, 
full_m <- glmer(protesta ~ id_imndv + id_imndv2 + id_icymn + econ_imndv + econ_imndv2 +
                  econ_icymn + soc_imndv + soc_imndv2 + soc_icymn +
                  trstinst + age + gentrst + sex + educ + partisan + income1 + #controls
                  income2 + asoca + hdicymndv + hdicmn + fhcymndv + fhcmn + #controls
                  pais + year + #FE dummies
                  (1 | CountryYear) #RE
                , data = LAPOP_Full_LatAm, family = binomial, control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0, # should be set to 1 or even 0, otherwise it never converges
                #and the results are not very different
                weights = LAPOP_Full_LatAm$wt)
summary(full_m) #this is the main table
#problem: fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients



####end of main file here


#all of this is experiments, unnecesary
#now should be easy to ggplot2
id_plot<-ggplot(MA_db2, aes(id_imndv.x, predicted))+ #id_imndv.x gets added in merger because the var is repeated between the two datasets
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  geom_line() +
  theme_minimal()
id_plot

#Marginal effects and marginplots
#what i want is average marginal effect, 95CI, y=average marginal effect, x=id_mndv
#in R, marginaleffects::marginaleffects is the equivalent to Stata's margins
mfid <- marginaleffects(full_m,variables=c("id_imndv", "econ_imndv", "soc_imndv")) 
#calculates marginal effects, full documentation https://vincentarelbundock.github.io/marginaleffects/articles/mfx03_mfx.html#marginal-effect-at-the-mean-mem


#plots conditional adjusted predictions, this is the one I should use
?plot_cap
?marginaleffects
plot_cap(small_m, condition = "id_imndv", conf_level = 0.95) #y si cambio Y axis a mas pequeño y reduzco CI
plot_cap(small_m, condition = "econ_imndv", conf_level = 0.95)
plot_cap(small_m, condition = "soc_imndv", conf_level = 0.95)

###Experimento: modificar el ggplot
id_plot<-plot_cap(small_m, condition = "id_imndv", conf_level = 0.95) +
  xlim(-6, 6)
id_plot

econ_plot<-plot_cap(small_m, condition = "econ_imndv", conf_level = 0.95) +
  xlim(-7, 7)
econ_plot

soc_plot<-plot_cap(small_m, condition = "soc_imndv", conf_level = 0.95) +
  xlim(-8, 8)
soc_plot

#plots conditional marginal effects, no this is not useful
plot_cme(small_m, effect = "id_imndv", condition = "id_imndv")
plot_cme(small_m, effect = "econ_imndv", condition = "econ_imndv")
plot_cme(small_m, effect = "soc_imndv", condition = "soc_imndv")

#Point-range plot of average marginal effects, este no es
plot(mfid, conf_level = 0.95)

#this are from ggeffect and do something similar to aonditional adjusted predictions
library(ggeffects)
small_m %>% 
  ggpredict(terms=c("id_imndv [all]")) %>% 
  plot()
small_m %>% 
  ggpredict(terms=c("econ_imndv [all]")) %>% 
  plot()
small_m %>% 
  ggpredict(terms=c("soc_imndv [all]")) %>% 
  plot()



#/*Haumsman test*/, just don't talk about this for now unless they ask
#xtset countryyeara
#xtreg protesta c.id_imndv##c.id_imndv id_icymn c.econ_imndv##c.econ_imndv econ_icymn c.soc_imndv##c.soc_imndv soc_icymn LeftExt RightExt trstinst age gentrst sex educ partisan income1 income2 asoca hdicymndv hdicmn fhcymndv fhcmn, fe 
#estimate store fixed
#xtreg protesta c.id_imndv##c.id_imndv id_icymn c.econ_imndv##c.econ_imndv econ_icymn c.soc_imndv##c.soc_imndv soc_icymn LeftExt RightExt trstinst age gentrst sex educ partisan income1 income2 asoca hdicymndv hdicmn fhcymndv fhcmn, re theta 
#estimate store random
#hausman fixed random /*Chi2=23.12 with 17d.f.; critical value is 27: Difference is not systematic, then use RE because better than FE*/
#/*t-test to use both RE and FE*/
#test id_imndv id_icymn /*insignificant, ergo use both RE and FE*/


#melogit protesta trstinst age gentrst sex educ partisan income1 income2 asoca hdicymndv hdicmn fhcymndv fhcmn pais_* wave* [pweight=wt] || countryyeara: , vce(robust) /*just controls, mcfadden R: 0.0536*/




#here i finish the stata translation

#number of missings
table (PELA5$ROES101)
sum(is.na(PELA5$ROES101))
sum(is.na(PELA5$ROES104))
table(PELA5$VAL1)
sum(is.na(PELA5$VAL1))
table(PELA5$ID2)
sum(is.na(PELA5$ID2))

#distribution for questions



