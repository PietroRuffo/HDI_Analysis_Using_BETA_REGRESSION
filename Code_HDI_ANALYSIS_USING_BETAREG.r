#############librerie e funzioni#############################
library(reshape2)
library(dplyr)
library(betareg)
library(MASS)
source("funzione_correlazione.R")


#lettura dati

dati<-read.csv("_WORLD_INDEXES_.csv",dec=".")
names(dati)
nomi_paesi<-dati$Id

#descrittiva HDI
summary(dati$Human.Development.Index.HDI.2014)
boxplot(dati$Human.Development.Index.HDI.2014,main="Human.Development.Index.HDI.2014", col="green")
hist(dati$Human.Development.Index.HDI.2014,main="Human.Development.Index.HDI.2014", col="blue")


#MODELLO con variabili ECONOMICHE
#costruzione matrice variabili economiche
variabili_economiche<-subset(dati, select=c(
                                            Human.Development.Index.HDI.2014,
                                            Gini.coefficient.2005.2013,
                                            Total.tax.revenue.of.GDP.2005.2013,
                                            Consumer.price.index.2013,
                                            Domestic.credit.provided.by.financial.sector.2013,
                                            Domestic.food.price.level.2009.2014.index,
                                            Domestic.food.price.level.2009.2014.volitility.index,
                                            Exports.and.imports.percentage.GPD.2013,
                                            Foreign.direct.investment.net.inflows.percentage.GDP.2013,
                                            General.government.final.consumption.expenditure...Annual.growth.2005.2013,
                                            General.government.final.consumption.expenditure...Perce.of.GDP.2005.2013,
                                            Gross.domestic.product.GDP.2013,
                                            Gross.fixed.capital.formation.of.GDP.2005.2013,
                                            International.inbound.tourists.thausands.2013,
                                            Private.capital.flows.percentage.GDP.2013,
                                            Remittances.inflows.percentual.GDP.2013,
                                            Taxes.on.income.profit.and.capital.gain.205.2013
                                            
                                            ))

(correlazionevariabili(variabili_economiche,0.8,T))
#nessuna correlazione elevata tra le variabili economiche






#applicazione modello lineare con stepwise regression
modello_variabili_economiche_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_economiche)
modello_variabili_economiche_lm<-stepAIC(modello_variabili_economiche_lm, direction = "both")
#summary del modello lineare con variabili economiche
summary(modello_variabili_economiche_lm) 

modello_variabili_economiche_lm<-update(modello_variabili_economiche_lm, .~. -International.inbound.tourists.thausands.2013)
summary(modello_variabili_economiche_lm)

modello_variabili_economiche_lm<-update(modello_variabili_economiche_lm, .~. -Total.tax.revenue.of.GDP.2005.2013)
summary(modello_variabili_economiche_lm)


#betareg con modello selezionato da stepwise
modello_variabili_economiche_beta<-betareg(Human.Development.Index.HDI.2014 ~ Gini.coefficient.2005.2013 + 
    Domestic.credit.provided.by.financial.sector.2013 + Domestic.food.price.level.2009.2014.index, 
    data = variabili_economiche, hessian=T)

summary(modello_variabili_economiche_beta) #tutt appo



#MODELLO CON VARIABILI SALUTE E QUALITA DELLA VITA

variabili_salute<-subset(dati, select=c(Human.Development.Index.HDI.2014, 
                                        Birth.registration.funder.age.5.2005.2013,
                                        Female.Suicide.Rate.100k.people,
                                        Homicide.rate.per.100k.people.2008.2012,
                                        Infant.Mortality.2013.per.thousands,
                                        MaleSuicide.Rate.100k.people,
                                        Physicians.per.10k.people,
                                        Public.health.expenditure.percentage.of.GDP.2013,
                                        Tuberculosis.rate.per.thousands.2012,
                                        Under.five.Mortality.2013.thousands))
  
(correlazionevariabili(variabili_salute,0.8,T))

#under five mortality e infant mortality altamente correlate, eliminiamo under five mortality
variabili_salute<-subset(variabili_salute, select=-Under.five.Mortality.2013.thousands)

#applicazione modello lineare con stepwise regression variabili salute
modello_variabili_salute_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_salute)
modello_variabili_salute_lm<-stepAIC(modello_variabili_salute_lm, direction = "both")
summary(modello_variabili_salute_lm)

#betareg con modello selezionato da stepwise variabili salute
modello_variabili_salute_beta<-betareg(Human.Development.Index.HDI.2014 ~ Birth.registration.funder.age.5.2005.2013+Infant.Mortality.2013.per.thousands+Physicians.per.10k.people, hessian=T, data = variabili_salute)
summary(modello_variabili_salute_beta)

#MODELLO CON VARIABILI AMBIENTE
variabili_ambiente<-subset(dati, select=c(Human.Development.Index.HDI.2014,
                                            Carbon.dioxide.emissionsAverage.annual.growth,
                                            Carbon.dioxide.emissions.per.capita.2011.Tones,
                                            Change.forest.percentable.1900.to.2012,
                                            Forest.area.percentage.of.total.land.area.2012,
                                            Fossil.fuels.percentage.of.total.2012,
                                            Fresh.water.withdrawals.2005,
                                            Homeless.people.due.to.natural.disaster.2005.2014.per.million.people,
                                            Natural.resource.depletion,
                                          Population.living.on.degraded.land.Percentage.2010,
                                          Renewable.sources.percentage.of.total.2012
                                        ))

(correlazionevariabili(variabili_ambiente,0.8,T))
#elevata corr negativa tra Renewable.sources.percentage.of.total.2012  E Fossil.fuels.percentage.of.total.2012
#eliminiamo Fossil.fuels.percentage.of.total.2012
variabili_ambiente<-subset(variabili_ambiente, select=-Fossil.fuels.percentage.of.total.2012)

#applicazione modello lineare con stepwise regression variabili ambiente
modello_variabili_ambiente_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_ambiente)
modello_variabili_ambiente_lm<-stepAIC(modello_variabili_ambiente_lm, direction = "both")
summary(modello_variabili_ambiente_lm)



#betareg con modello selezionato da stepwise variabili ambiente
modello_variabili_ambiente_beta<-betareg(Human.Development.Index.HDI.2014 ~ Carbon.dioxide.emissionsAverage.annual.growth + 
    Carbon.dioxide.emissions.per.capita.2011.Tones + Change.forest.percentable.1900.to.2012 + 
    Forest.area.percentage.of.total.land.area.2012 + Natural.resource.depletion +  
    Population.living.on.degraded.land.Percentage.2010 + Renewable.sources.percentage.of.total.2012, 
    data = variabili_ambiente, hessian=T)
summary(modello_variabili_ambiente_beta) 



#################################################################### Teconologia
variabili_tecnologia<-subset(dati, select=c(Human.Development.Index.HDI.2014,
                                            Change.mobile.usage.2009.2014,
                                            Electrification.rate.or.population,
                                            Internet.users.percentage.of.population.2014,
                                            Mobile.phone.subscriptions.per.100.people.2014,
                                            Research.and.development.expenditure..2005.2012
                                            ))

(correlazionevariabili(variabili_tecnologia,0.8,T))

#stepwise lm con variabili tecnologia
modello_variabili_tecnologia_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_tecnologia)
modello_variabili_tecnologia_lm<-stepAIC(modello_variabili_tecnologia_lm, direction = "both")
summary(modello_variabili_tecnologia_lm)


#betareg con modello selezionato da stepwise variabili ambiente
modello_variabili_tecnologia_beta<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + 
                                           Internet.users.percentage.of.population.2014 + Mobile.phone.subscriptions.per.100.people.2014 + 
                                           Research.and.development.expenditure..2005.2012, data = variabili_tecnologia,hessian=T)


summary(modello_variabili_tecnologia_beta) 



########################variabili_società############################################################
variabili_società<-subset(dati, select=c(Human.Development.Index.HDI.2014,
                                            Intimate.or.nonintimate.partner.violence.ever.experienced.2001.2011,
                                            Prison.population.per.100k.people,
                                            Net.migration.rate.per.1k.people.2010.2015,
                                            Refugees.by.country.of.origin,
                                            Stock.of.immigrants.percentage.of.population.2013,
                                            Gender.Inequality.Index.2014
                                            ))


(correlazionevariabili(variabili_società,0.8,T))


####lm stepwise variabili società
modello_variabili_società_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_società)
modello_variabili_società_lm<-stepAIC(modello_variabili_società_lm, direction = "both")
summary(modello_variabili_società_lm)


#betareg con variabili selezionate da stepwise lm
modello_variabili_società_beta<-betareg(Human.Development.Index.HDI.2014 ~ Intimate.or.nonintimate.partner.violence.ever.experienced.2001.2011 + 
                                Prison.population.per.100k.people + Stock.of.immigrants.percentage.of.population.2013 + 
                                Gender.Inequality.Index.2014, data = variabili_società,hessian= T)

summary(modello_variabili_società_beta) 


#########################istruzione#####################################
variabili_istruzione<-subset(dati, select=c(Human.Development.Index.HDI.2014,
                                         International.student.mobility.of.total.tetiary.enrolvemnt.2013,
                                         Primary.school.dropout.rate.2008.2014,
                                         Public.expenditure.on.education.Percentange.GDP,
                                         Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                                         
))
(correlazionevariabili(variabili_istruzione,0.8,T))

##stepwise lm istruzione
modello_variabili_istruzione_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_istruzione)
modello_variabili_istruzione_lm<-stepAIC(modello_variabili_istruzione_lm, direction = "both")
summary(modello_variabili_istruzione_lm)


modello_variabili_istruzione_beta<-betareg(Human.Development.Index.HDI.2014 ~ Primary.school.dropout.rate.2008.2014 + 
                                             Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,
                                           data = variabili_istruzione,hessian=T)

summary(modello_variabili_istruzione_beta) 

###############################################summary dei modelli lineari e beta per ogni categoria di variabili##############################################################################

summary(modello_variabili_tecnologia_lm)
summary(modello_variabili_salute_lm)
summary(modello_variabili_economiche_lm)
summary(modello_variabili_ambiente_lm)
summary(modello_variabili_società_lm)
summary(modello_variabili_istruzione_lm)

summary(modello_variabili_tecnologia_beta)
summary(modello_variabili_salute_beta)
summary(modello_variabili_economiche_beta)
summary(modello_variabili_ambiente_beta)
summary(modello_variabili_società_beta)
summary(modello_variabili_istruzione_beta)
################################################################
##############costruzione matrice con variabili significative
selezione_variabili<-subset(dati,select=c(Human.Development.Index.HDI.2014,
Electrification.rate.or.population ,
Internet.users.percentage.of.population.2014 ,  
Mobile.phone.subscriptions.per.100.people.2014 ,  
Research.and.development.expenditure..2005.2012 ,

Birth.registration.funder.age.5.2005.2013 ,
Infant.Mortality.2013.per.thousands ,
Physicians.per.10k.people ,

Gini.coefficient.2005.2013 ,
Domestic.credit.provided.by.financial.sector.2013 , 
Domestic.food.price.level.2009.2014.index ,

Carbon.dioxide.emissionsAverage.annual.growth ,
Carbon.dioxide.emissions.per.capita.2011.Tones ,
Change.forest.percentable.1900.to.2012 ,
Forest.area.percentage.of.total.land.area.2012 ,
Natural.resource.depletion,
Population.living.on.degraded.land.Percentage.2010,
Renewable.sources.percentage.of.total.2012  ,
Intimate.or.nonintimate.partner.violence.ever.experienced.2001.2011,
Prison.population.per.100k.people,Stock.of.immigrants.percentage.of.population.2013,
Stock.of.immigrants.percentage.of.population.2013,
Gender.Inequality.Index.2014,
Primary.school.dropout.rate.2008.2014,
Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
))

((correlazionevariabili(selezione_variabili,0.8,F)))

#dall'anilisi di tali correlazioni quella tra Infant.Mortality.2013.per.thousands    ed       Electrification.rate.or.population -0.8066029
#ci porta ad eliminare infant mortality rate poiche considerato come una chiara conseguenza della seconda varaibile

selezione_variabili<-subset(selezione_variabili,select=-Infant.Mortality.2013.per.thousands)
                                          
#########COSTRUZIONE MODELLO FINALE################
#stepwise lm su selezione variabili
modello_finale_lm<-lm(Human.Development.Index.HDI.2014~.,data=selezione_variabili)
modello_finale_lm<-stepAIC(modello_finale_lm, direction = "both")
summary(modello_finale_lm)


#modello betareg
modello_finale_betareg<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + 
                                  Internet.users.percentage.of.population.2014 + Mobile.phone.subscriptions.per.100.people.2014 + 
                                  Research.and.development.expenditure..2005.2012 + Physicians.per.10k.people + 
                                  Gini.coefficient.2005.2013 + Population.living.on.degraded.land.Percentage.2010 + 
                                  Renewable.sources.percentage.of.total.2012 + Gender.Inequality.Index.2014 + 
                                  Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                                  ,data = selezione_variabili,hessian=T)
summary(modello_finale_betareg)





#Renewable.sources.percentage.of.total.2012 non significativo
modello_finale_betareg<-update(modello_finale_betareg, .~. -Renewable.sources.percentage.of.total.2012)
summary(modello_finale_betareg)

#Gini non significativo, eliminiamolo dal modello PER ORA
modello_finale_betareg<-update(modello_finale_betareg, .~. -Gini.coefficient.2005.2013)
summary(modello_finale_betareg)

###################
#######################test nostro modello vs modello con sola intercetta#############################

##################test su phy################
z<-((114.31-1)/11.76)
z
test1<- 1-(pnorm(z))
test1


#rifiutiamo H0:phi=1 e stimiamo phi
#per stimare phi andiamo a vedere quale regressore può influenzare la variabilità della risposta
#attraverso l'utilizzo del test di breush-pagan

#BP
residui<-modello_finale_betareg$residuals
residui<-residui^2

breush_pagan<-lm(residui~Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                   Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                   Physicians.per.10k.people + Gender.Inequality.Index.2014 + 
                   Primary.school.dropout.rate.2008.2014 +Population.living.on.degraded.land.Percentage.2010+ Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,
data=selezione_variabili
                 )

summary(breush_pagan)



##### stima phi con regressoni altamente significativi nel BP
mf_con_phi<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                      Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                      Physicians.per.10k.people + Gender.Inequality.Index.2014 + 
                      Primary.school.dropout.rate.2008.2014 +Population.living.on.degraded.land.Percentage.2010+ Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                    
                    |   #predittore per phi
                    
                      Physicians.per.10k.people+ Primary.school.dropout.rate.2008.2014 +Gender.Inequality.Index.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                      
                      ,data = selezione_variabili, hessian = T)

summary(mf_con_phi)



#Mobile.phone.subscriptions.per.100.people.2014 non significativo
mf_con_phi<-update(mf_con_phi,.~. -Mobile.phone.subscriptions.per.100.people.2014)
summary(mf_con_phi)

#Population.living.on.degraded.land.Percentage.2010 non significativo
mf_con_phi<-update(mf_con_phi,.~. -Population.living.on.degraded.land.Percentage.2010)
summary(mf_con_phi)





#giunti a questo punto inseriamo l'indice di Gini in maniera quadratica poiche ci viene suggerita tale relazione nel plot tra hdi e gini
selezione_variabili$Gini.coefficient.2005.2013<-(selezione_variabili$Gini.coefficient.2005.2013/100) # cambiamento di scala

plot(selezione_variabili$Human.Development.Index.HDI.2014,selezione_variabili$Gini.coefficient.2005.2013)

GINI2<-selezione_variabili$Gini.coefficient.2005.2013^2


#giunti al modello finale inseriamo l'indice di Gini 
mf_con_phi<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                      + Research.and.development.expenditure..2005.2012 + 
                      Physicians.per.10k.people + Gender.Inequality.Index.2014 + 
                      Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014 +
                      +GINI2
                    
                    |   #predittore per phi
                      
                      Physicians.per.10k.people+ Primary.school.dropout.rate.2008.2014 +Gender.Inequality.Index.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                    
                    ,data = selezione_variabili, hessian = T)

summary(mf_con_phi)



# gini è ora significativo accettando un livello di significatività pari a 7 %

#confronto aic tra modello_finale_betareg che ha solo il predittore lineare per la media e mf_con_phi che presenta anche il predittore per il parametro di precisione
AIC(modello_finale_betareg)
AIC(mf_con_phi)

###################modello finale##############
(finale<-mf_con_phi)

##########################################
#CONSIDERAZIONI FINALI
variabili_del_modello<-subset(selezione_variabili,select=c(Human.Development.Index.HDI.2014,Gini.coefficient.2005.2013,Electrification.rate.or.population , 
                                                           Internet.users.percentage.of.population.2014 , Research.and.development.expenditure..2005.2012 ,
                                                           Physicians.per.10k.people , Gender.Inequality.Index.2014 , Primary.school.dropout.rate.2008.2014 ,
                                                           Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                                                           ))

rownames(variabili_del_modello)<-nomi_paesi




#summary hist e boxplot variabili del modello

#Human.Development.Index.HDI.2014
summary(variabili_del_modello$Human.Development.Index.HDI.2014)
hist(variabili_del_modello$Human.Development.Index.HDI.2014, main="Human.Development.Index.HDI.2014",col="orange")
boxplot(variabili_del_modello$Human.Development.Index.HDI.2014, main="Human.Development.Index.HDI.2014",col="orange")

#Electrification.rate.or.population
summary(variabili_del_modello$Electrification.rate.or.population)
hist(variabili_del_modello$Electrification.rate.or.population, main="Electrification.rate.or.population",col="orange")
boxplot(variabili_del_modello$Electrification.rate.or.population, main="Electrification.rate.or.population",col="orange")

#Internet.users.percentage.of.population.2014
summary(variabili_del_modello$Internet.users.percentage.of.population.2014)
hist(variabili_del_modello$Internet.users.percentage.of.population.2014, main="Internet.users.percentage.of.population.2014",col="orange")
boxplot(variabili_del_modello$Internet.users.percentage.of.population.2014, main="Internet.users.percentage.of.population.2014",col="orange")

#Research.and.development.expenditure..2005.2012
summary(variabili_del_modello$Research.and.development.expenditure..2005.2012)
hist(variabili_del_modello$Research.and.development.expenditure..2005.2012, main= "Research.and.development.expenditure..2005.2012",col="orange")
boxplot(variabili_del_modello$Research.and.development.expenditure..2005.2012, main= "Research.and.development.expenditure..2005.2012",col="orange")

#Physicians.per.10k.people
summary(variabili_del_modello$Physicians.per.10k.people)
hist(variabili_del_modello$Physicians.per.10k.people,main="Physicians.per.10k.people",col="orange")
boxplot(variabili_del_modello$Physicians.per.10k.people,main="Physicians.per.10k.people",col="orange")

#Gender.Inequality.Index.2014
summary(variabili_del_modello$Gender.Inequality.Index.2014)
hist(variabili_del_modello$Gender.Inequality.Index.2014,main="Gender.Inequality.Index.2014",col="orange")
boxplot(variabili_del_modello$Gender.Inequality.Index.2014,main="Gender.Inequality.Index.2014",col="orange")

#Primary.school.dropout.rate.2008.2014
summary(variabili_del_modello$Primary.school.dropout.rate.2008.2014)
hist(variabili_del_modello$Primary.school.dropout.rate.2008.2014, main="Primary.school.dropout.rate.2008.2014",col="orange")
boxplot(variabili_del_modello$Primary.school.dropout.rate.2008.2014, main="Primary.school.dropout.rate.2008.2014",col="orange")

#Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
summary(variabili_del_modello$Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014)
hist(variabili_del_modello$Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,main="Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014",col="orange")
boxplot(variabili_del_modello$Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,main="Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014",col="orange")

#Gini.coefficient.2005.2013
summary(variabili_del_modello$Gini.coefficient.2005.2013)
hist(variabili_del_modello$Gini.coefficient.2005.2013,main="Gini.coefficient.2005.2013",col="orange")
boxplot(variabili_del_modello$Gini.coefficient.2005.2013,main="Gini.coefficient.2005.2013",col="orange")


# filtrare per paesi per considerazioni finali
considerazioni<-variabili_del_modello[rownames(variabili_del_modello) %in% c("United States","Italy","Yemen","Chad"), ]
considerazioni

  



###################prove###########                       # da eliminare
dati1<-dati
names(dati1)
dati1<-dati1[,-2]; dati1<-dati1[,-1]
names(dati1)
full<-betareg(Human.Development.Index.HDI.2014~., data= dati1)
intercetta<-betareg(Human.Development.Index.HDI.2014~1, data= dati1)
summary(full)
summary(intercetta)
library(lmtest)
lrtest(finale, full)
lrtest(intercetta,finale)
summary(finale)

plot(residuals(finale))
names(dati)

     
