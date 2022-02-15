#############librerie e funzioni#############################
library(reshape2)
library(dplyr)
library(betareg)
library(MASS)
source("funzione_correlazione.R")

#lettura dati e summary
dati<-read.csv("world_indexes.csv",dec=".")
nomi_paesi<-dati$Id
dati<-dati[,-2];dati<-dati[,-1]
summary(dati)

#descrittiva HDI
summary(dati$Human.Development.Index.HDI.2014)
boxplot(dati$Human.Development.Index.HDI.2014)
hist(dati$Human.Development.Index.HDI.2014)



#MODELLO con variabili ECONOMICHE
#costruzione matrice variab econom
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




#applicazione modello lineare con stepwise regression
modello_variabili_economiche_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_economiche)
modello_variabili_economiche_lm<-stepAIC(modello_variabili_economiche_lm, direction = "both")


modello_variabili_economiche_lm$fitted.values #notiamo che qualche stima esce da 01


#summary del modello con variabili economiche
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






modello_variabili_economiche_beta$fitted.values

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

variabili_salute<-subset(variabili_salute, select=-Under.five.Mortality.2013.thousands)


#applicazione modello lineare con stepwise regression variabili salute
modello_variabili_salute_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_salute)
modello_variabili_salute_lm<-stepAIC(modello_variabili_salute_lm, direction = "both")

summary(modello_variabili_salute_lm)


modello_variabili_salute_lm$fitted.values


#betareg con modello selezionato da stepwise variabili salute
modello_variabili_salute_beta<-betareg(Human.Development.Index.HDI.2014 ~ Birth.registration.funder.age.5.2005.2013+Infant.Mortality.2013.per.thousands+Physicians.per.10k.people, hessian=T, data = variabili_salute)

summary(modello_variabili_salute_beta) #tutt appo pt2

modello_variabili_salute_beta<-update(modello_variabili_salute_beta, .~. -Birth.registration.funder.age.5.2005.2013)
summary(modello_variabili_salute_beta) #tutt appo pt2

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


variabili_ambiente<-subset(variabili_ambiente, select=-Fossil.fuels.percentage.of.total.2012)


#applicazione modello lineare con stepwise regression variabili ambiente
modello_variabili_ambiente_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_ambiente)
modello_variabili_ambiente_lm<-stepAIC(modello_variabili_ambiente_lm, direction = "both")

summary(modello_variabili_ambiente_lm)

modello_variabili_ambiente_lm$fitted.values


#betareg con modello selezionato da stepwise variabili ambiente
modello_variabili_ambiente_beta<-betareg(Human.Development.Index.HDI.2014 ~ Carbon.dioxide.emissionsAverage.annual.growth + 
    Carbon.dioxide.emissions.per.capita.2011.Tones + Change.forest.percentable.1900.to.2012 + 
    Forest.area.percentage.of.total.land.area.2012 + Natural.resource.depletion +  
    Population.living.on.degraded.land.Percentage.2010 + Renewable.sources.percentage.of.total.2012, 
    data = variabili_ambiente, hessian=T)
                                             

summary(modello_variabili_ambiente_beta) #tutt appo pt2

modello_variabili_ambiente_beta$fitted.values


#################################################################### Teconologia
variabili_tecnologia<-subset(dati, select=c(Human.Development.Index.HDI.2014,
                                            Change.mobile.usage.2009.2014,
                                            Electrification.rate.or.population,
                                            Internet.users.percentage.of.population.2014,
                                            Mobile.phone.subscriptions.per.100.people.2014,
                                            Research.and.development.expenditure..2005.2012
                                            ))

(correlazionevariabili(variabili_tecnologia,0.8,T))

modello_variabili_tecnologia_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_tecnologia)
modello_variabili_tecnologia_lm<-stepAIC(modello_variabili_tecnologia_lm, direction = "both")

summary(modello_variabili_tecnologia_lm)


modello_variabili_tecnologia_lm$fitted.values


#betareg con modello selezionato da stepwise variabili ambiente
modello_variabili_tecnologia_beta<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + 
                                           Internet.users.percentage.of.population.2014 + Mobile.phone.subscriptions.per.100.people.2014 + 
                                           Research.and.development.expenditure..2005.2012, data = variabili_tecnologia,hessian=T)


summary(modello_variabili_tecnologia_beta) #tutt appo pt2


modello_variabili_tecnologia_beta$fitted.values


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


####lm stepwise
modello_variabili_società_lm<-lm(Human.Development.Index.HDI.2014~.,data=variabili_società)
modello_variabili_società_lm<-stepAIC(modello_variabili_società_lm, direction = "both")

summary(modello_variabili_società_lm)

modello_variabili_società_lm$fitted.values


modello_variabili_società_beta<-betareg(Human.Development.Index.HDI.2014 ~ Intimate.or.nonintimate.partner.violence.ever.experienced.2001.2011 + 
                                Prison.population.per.100k.people + Stock.of.immigrants.percentage.of.population.2013 + 
                                Gender.Inequality.Index.2014, data = variabili_società,hessian= T)

summary(modello_variabili_società_beta) #tutt appo pt2


modello_variabili_società_beta$fitted.values


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


modello_variabili_istruzione_lm$fitted.values




modello_variabili_istruzione_beta<-betareg(Human.Development.Index.HDI.2014 ~ Primary.school.dropout.rate.2008.2014 + 
                                             Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,
                                           data = variabili_istruzione,hessian=T)

summary(modello_variabili_istruzione_beta) #tutt appo pt2

modello_variabili_istruzione_beta$fitted.values


#############################################################################################################################
summary(modello_variabili_tecnologia_beta)
summary(modello_variabili_salute_beta)
summary(modello_variabili_economiche_beta)
summary(modello_variabili_ambiente_beta)
summary(modello_variabili_società_beta)
summary(modello_variabili_istruzione_beta)

#modello finale

selezione_variabili<-subset(dati,select=c(Human.Development.Index.HDI.2014,
                                          Electrification.rate.or.population,
                                          Internet.users.percentage.of.population.2014,
                                          Mobile.phone.subscriptions.per.100.people.2014,
                                          Research.and.development.expenditure..2005.2012,
                                          
                                          
                                          Infant.Mortality.2013.per.thousands,
                                          Physicians.per.10k.people,
                                          
                                          Gini.coefficient.2005.2013,
                                          Domestic.credit.provided.by.financial.sector.2013,
                                          Domestic.food.price.level.2009.2014.index,
                                          
                                          Carbon.dioxide.emissionsAverage.annual.growth,
                                         Carbon.dioxide.emissions.per.capita.2011.Tones,
                                        Change.forest.percentable.1900.to.2012,
                                        Forest.area.percentage.of.total.land.area.2012,
                                        Natural.resource.depletion,
                                        Population.living.on.degraded.land.Percentage.2010,
                                        Renewable.sources.percentage.of.total.2012 ,
                                          
                                          Intimate.or.nonintimate.partner.violence.ever.experienced.2001.2011,
                                          Prison.population.per.100k.people,Stock.of.immigrants.percentage.of.population.2013,
                                          Stock.of.immigrants.percentage.of.population.2013,
                                          Gender.Inequality.Index.2014,
                                          
                                          Primary.school.dropout.rate.2008.2014,
                                            Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                                          
                                          
                                          ))

(correlazionevariabili(selezione_variabili,0.8,F))

#dall'anilisi di tali correlazioni quella                    Infant.Mortality.2013.per.thousands           Electrification.rate.or.population -0.8066029
#ci porta ad eliminare infant mortality rate poiche considerato come una chiara conseguenza della seconda varaibile

selezione_variabili<-subset(selezione_variabili,select=-Infant.Mortality.2013.per.thousands)
                                          

#stepwise lm su selezione variabili
modello_finale_lm<-lm(Human.Development.Index.HDI.2014~.,data=selezione_variabili)
modello_finale_lm<-stepAIC(modello_finale_lm, direction = "both")

summary(modello_finale_lm)


modello_finale_lm$fitted.values




modello_finale_betareg<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + 
                                  Internet.users.percentage.of.population.2014 + Mobile.phone.subscriptions.per.100.people.2014 + 
                                  Research.and.development.expenditure..2005.2012 + Physicians.per.10k.people + 
                                  Gini.coefficient.2005.2013 + Population.living.on.degraded.land.Percentage.2010 + 
                                  Renewable.sources.percentage.of.total.2012 + Gender.Inequality.Index.2014 + 
                                  Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014, data = selezione_variabili,hessian=T)
summary(modello_finale_betareg)





#proviamo interazione tra Research.and.development.expenditure..2005.2012 e Renewable.sources.percentage.of.total.2012
modello_finale_betareg_interaz1<-update(modello_finale_betareg, .~. -Renewable.sources.percentage.of.total.2012-Research.and.development.expenditure..2005.2012 +Renewable.sources.percentage.of.total.2012*Research.and.development.expenditure..2005.2012)
summary(modello_finale_betareg_interaz1)
#non significativa quindi Renewable.sources.percentage.of.total.2012 tolta dal modello

modello_finale_betareg<-update(modello_finale_betareg, .~. -Renewable.sources.percentage.of.total.2012)
summary(modello_finale_betareg)



#gini non signif . Infatti paesi con elevati HDI presentano anche casi in cui vi sono elevate disuguaglianze nei redditi
#vedi america 
modello_finale_betareg<-update(modello_finale_betareg, .~. -Gini.coefficient.2005.2013)
summary(modello_finale_betareg)



###################
#######################test nostro modello vs modello con sola intercetta#############################

library(lmtest)
mod_intercept<-betareg(Human.Development.Index.HDI.2014~1,data = selezione_variabili)
lrtest(mod_intercept,modello_finale_betareg)
####nostro modello meglio di sola intercetta


##################test su phy################
z<-((114.31-1)/11.76)
z


test1<- 1-(pnorm(z))
test1


#rifiutiamo ipotesi di omoschedasticità, stimiamo phi
#per stimare phi andiamo a vedere quale regressore può influenzare la variabilità della risposta




#in alternativa procediamo con un test di BP
residui<-modello_finale_betareg$residuals

breush_pagan<-lm(residui^2~Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                   Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                   Physicians.per.10k.people + Population.living.on.degraded.land.Percentage.2010 + Gender.Inequality.Index.2014 + 
                   Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014,
data=selezione_variabili
                 )

summary(breush_pagan)

#si rifiuta omosched residui (da cancellare dallo script sto commento)
#############################

##### stima phi con regressoni altamente significativi nel BP
mf_con_phi<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                      Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                      Physicians.per.10k.people + Population.living.on.degraded.land.Percentage.2010 + Gender.Inequality.Index.2014 + 
                      Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                    
                    |   #predittore per phi
                    
                      Physicians.per.10k.people+ Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014 +
                      Gender.Inequality.Index.2014,
                      data = selezione_variabili, hessian = T)

summary(mf_con_phi)


mf_con_phi<-update(mf_con_phi,.~. -Mobile.phone.subscriptions.per.100.people.2014-Population.living.on.degraded.land.Percentage.2010)
summary(mf_con_phi)


######stima phi normale senza suggerimenti BP
mf_con_phi2<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                      Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                      Physicians.per.10k.people + Population.living.on.degraded.land.Percentage.2010 + Gender.Inequality.Index.2014 + 
                      Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                     |
                       Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
                       Mobile.phone.subscriptions.per.100.people.2014 + Research.and.development.expenditure..2005.2012 + 
                       Physicians.per.10k.people + Population.living.on.degraded.land.Percentage.2010 + Gender.Inequality.Index.2014 + 
                       Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
                     
                     ,data = selezione_variabili, hessian = T)
summary(mf_con_phi2)



#eliminaz var non significative 

mf_con_phi2<-betareg( Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + 
               Internet.users.percentage.of.population.2014 + Research.and.development.expenditure..2005.2012 + 
                Physicians.per.10k.people + Gender.Inequality.Index.2014 + Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014
             |  
              Physicians.per.10k.people + Gender.Inequality.Index.2014 + Primary.school.dropout.rate.2008.2014
             ,data = selezione_variabili)
summary(mf_con_phi2)


variabili_del_modello<-subset(selezione_variabili,select=c(Human.Development.Index.HDI.2014,Electrification.rate.or.population , 
                                                             Internet.users.percentage.of.population.2014 , Research.and.development.expenditure..2005.2012 ,
                                                            Physicians.per.10k.people , Gender.Inequality.Index.2014 , Primary.school.dropout.rate.2008.2014 ,
                                                           Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014))

plot(variabili_del_modello)


#dal plot si percepisce una relazione non del tutto lineare tra
#risposta e research e development expediture
#e risposta e physician per 10k people, proviamo per tanto ad inserire
#nell ultimo modello il log di tali variabili



logmf<-betareg(Human.Development.Index.HDI.2014 ~ Electrification.rate.or.population + Internet.users.percentage.of.population.2014 + 
          log(Research.and.development.expenditure..2005.2012) + log(Physicians.per.10k.people) + Gender.Inequality.Index.2014 + 
          Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014 | 
          Physicians.per.10k.people + Primary.school.dropout.rate.2008.2014 + Pupil.teacher.ratio.primary.school.pupils.per.teacher.2008.2014 + 
          Gender.Inequality.Index.2014, data = selezione_variabili, hessian = T)
summary(logmf)

##########################################

#per finali considerazioni aggiungere paesi esghere
considerazioni<-dataset_con_variabili_che_spiegano_HDI[dataset_con_variabili_che_spiegano_HDI$nomi_paesi %in% c("United States","Italy","Yemen"), ]
considerazioni

