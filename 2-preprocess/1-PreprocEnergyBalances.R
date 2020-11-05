rm(list=ls())
library(tidyverse)
library(readxl)
library(magrittr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
CoreSectors=c("nonpowertrans", "power", "tfc ", "road", "rail", "domesair","domesnav", "intair", "resident", "food_forest", "services", "mining_chemicals", "ironstl", "nonferrmet", "machinery", "cement", "other_manufact", "construc", "ononspec") #"nonenuse"

Fuels <- read_csv("../metadata/EmissionsFactors.csv")
Flow <- read_csv("../metadata/SectorFullLookup.csv")
PJPerktoe <- 0.041868

FinalSubSectors=c("pow","rod","ral","avi","nav","res","ind","foo","srv","mch","irn","nfm","mac","cem","omn","cst","ftr")

ChosenFuels=c("ecy","coa" , "nga" , "gso" , "die"  , "lpg" , "ker", "oop", "bio", "jfu" , "wnd" , "sol" , "hyd", "ore"  , "nuc" )

RawCPATBalances <- read_csv("rawdata/CPATEnergyBalances2018.csv", skip=0) %>%
  rename(CountryCode=country_code, FlowCode=flow_code) %>% mutate(year=2018)  #For now, even though these are 2018 balances!

PowerOutput <- RawCPATBalances %>% filter(FlowCode== "eloutput")
write_csv(PowerOutput,"powerintermediate/PowerOutput.csv")

FossilInputToPower <- RawCPATBalances %>% filter(FlowCode== "power")
write_csv(FossilInputToPower,"powerintermediate/FossilInputToPower.csv")

#if("oil"%in%names(RawCPATBalances)) RawCPATBalances %<>% rename(oop = oil)

CPATEnergyBalances2018 <- RawCPATBalances %>%
  select(c("CountryCode" , "year", "FlowCode", all_of(ChosenFuels))) %>%
  filter(as.integer(year)==2018) %>% select(-year) %>%
  inner_join(Flow)  %>%
  select(CountryCode, SubsectorCode, FlowCode, Sector, FlowIncluded,everything()) %>%
  pivot_longer(cols=6:18,names_to = "FuelType",values_to = "BaseYearEnergy") %>%
  mutate(BaseYearEnergy=PJPerktoe*ifelse(FlowCode %in% c("power","pipeline"), -BaseYearEnergy, BaseYearEnergy)) %>%
  select(CountryCode, SubsectorCode, FlowCode, Sector, FlowIncluded, FuelType, FlowRow, BaseYearEnergy)



#TFC = CPATEnergyBalances2018%>% filter(FlowCode=="tfc")
#CPATEnergyBalances2018.tpeslessnonen = CPATEnergyBalances2018 %>% group_by(CountryCode,FuelType) %>%
#  summarise(BaseYearEnergy= sum(ifelse(FlowCode=="tpes",BaseYearEnergy,0)+ifelse(FlowCode=="nonenuse",-BaseYearEnergy,0)),
#            SubsectorCode="totalnonenergy", FlowCode="totalnonenergy",Sector="TOTAL",FlowIncluded=T, FlowRow=28) %>%
#  arrange(CountryCode,FlowRow,FlowCode,FuelType) %>% relocate(names(CPATEnergyBalances2018))


NonEnergyTransformation = CPATEnergyBalances2018 %>%
  filter(FlowCode%in%c("statdiff","transfer","ownuse", "distloss","tottranf", "power", "pipeline")) %>%
  group_by(CountryCode,FuelType) %>%
  summarise(SubsectorCode="ftr", FlowCode="nonpowertrans",Sector="ind",FlowIncluded=T, FlowRow=27, BaseYearEnergy= -sum(BaseYearEnergy, na.rm = TRUE)) %>%
  relocate(names(CPATEnergyBalances2018))

NonEnergyTransformationOilAggregated=NonEnergyTransformation%>%filter(FuelType%in%c("oop","gso","die","jfu","lpg","ker") )%>%
  group_by(CountryCode, SubsectorCode, FlowCode, Sector, FlowIncluded, FlowRow ) %>%
  summarize(FuelType="oop",BaseYearEnergy=sum(BaseYearEnergy)) %>%
  relocate(names(CPATEnergyBalances2018))


NonEnergyTransformation %<>% mutate(BaseYearEnergy=if_else(FuelType%in%c("gso","die","jfu","lpg","ker"),0,BaseYearEnergy)) %>%
  filter(FuelType != "oop") %>% bind_rows(NonEnergyTransformationOilAggregated)



CPATEnergyBalances2018 = CPATEnergyBalances2018 %>% filter((FlowCode%in%CoreSectors)) %>%
  bind_rows(NonEnergyTransformation) %>% select(CountryCode, SubsectorCode, Sector, FuelType, BaseYearEnergy)

write_csv(CPATEnergyBalances2018,file="output/ProcessedEnergyBalances2018.csv")

PowerDemandFinal=CPATEnergyBalances2018 %>% filter(FuelType=="ecy")
write_csv(PowerDemandFinal,"powerintermediate/PowerDemandFinal.csv")


#TFCestimate=CPATEnergyBalances2018 %>% filter(SubsectorCode
#                                  %in%c("rod","ral","avi","nav","res","ind","foo","srv","mch","irn","nfm","mac","cem","omn","cst")) %>%
#  group_by(CountryCode, FuelType) %>%  summarise(BaseYearEnergyEstimate=sum(BaseYearEnergy))

#TFCCombined=TFC%>% select(CountryCode, FuelType, TFCstated=BaseYearEnergy) %>% left_join(TFCestimate) %>% mutate(residual=TFCstated-BaseYearEnergy)

#CPATEnergyBalances2018 %>% filter(CountryCode=="CHN",SubsectorCode=="oen")


#CPATEnergyBalances2018.totals = CPATEnergyBalances2018 %>%
#  group_by(CountryCode,FuelType) %>%
#  summarise(SubsectorCode="all", FlowCode="all",Sector="ALL",FlowIncluded=F, FlowRow=0, BaseYearEnergy= sum(BaseYearEnergy, na.rm = TRUE)) %>%
#  relocate(names(CPATEnergyBalances2018)) %>%
#  arrange(CountryCode,FlowCode,FuelType)
#CPATEnergyBalances2018.tpeslessnonen
#CPATEnergyBalances2018.totals
#CPATEnergyBalances2018.tpeslessnonen %>% filter(CountryCode=="CHN")
#CPATEnergyBalances2018.totals %>% filter(CountryCode=="CHN")
#a = CPATEnergyBalances2018%>%filter(CountryCode=="CHN")%>% group_by(FuelType) %>% summarise(across(where(is.numeric),sum))
#CPATEnergyBalances2018%>%filter(CountryCode=="CHN",SubsectorCode=="nav")
