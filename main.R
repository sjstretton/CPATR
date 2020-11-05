## Purpose: This script contains the 'main function'.

rm(list=ls())
#Uncomment the line below for this to work in R studio
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(readxl)
library(magrittr)
library(cowplot)
library(ggrepel)



#Import Core Variables
source("0-coremodel/1-ImportVariables.R")
CountryList <- TCAFCountryList ##c("CHN") #Other options: ...,"IND","USA","JPN" #(CountryNameLookup$CountryCode)[1:10] #
SelectedCountryNames = CountryNameLookup %>% filter(CountryCode %in% CountryList) %>% select(CountryName)
SelectedPowerData %<>% mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor) %>%
  mutate(Production.GWh=PowerOutput.GWh, Production.MWy=PowerOutput.MWy, Year = BaseYear,LCOE.USD_kWh=LCOE.TotalCalc.USD_kWh) %>%
  filter(CountryCode %in% CountryList)
DoPower <- TRUE

source("0-coremodel/2-BuildBaseTables.R")
source("0-coremodel/3-CoreModel.R")
source("3-powermodel/1-PowerModel.R")

MainSegmentedDataTable <- BuildBaseTable(CountryList_=CountryList,SubsectorList_=SubsectorList, FuelList_=FuelList,YearList_=YearList,CTRange_=CTRange,BaseYear_=BaseYear,SectorSubsectorLookup_=SectorSubsectorLookup) %>%


    BuildMainSegmentedDataTable(Elasticities_=Elasticities,Fuels_=Fuels,CPATEnergyBalancesBaseYear_=CPATEnergyBalances2018,
                                                 GDPRelativeToBase_=GDPRelativeToBase,FuelPrices_=FuelPrices,CarbonTaxTrajectoryForm_=CarbonTaxTrajectoryForm)


OutputList=RunCoreModel(MainSegmentedDataTable_=MainSegmentedDataTable,BaseYear_=BaseYear,AnalysisEndYear_=AnalysisEndYear,CTRange_=CTRange,DoPower_=DoPower)

OutputSegmentedDataTable=OutputList[[1]]

#################################################################
#TestThat

#Get Coal Cement from CPAT Results.
  ## Find CPAT results that have Coal Cement - yes MST
MST1.new <- read_csv("5-comparison/2ResultsFromCPAT/MST1-new.csv",col_types = "iiccccccidddddddddddddd")

#Found one side!
PJPerktoe <- 0.041868
FilteredInfo1  = MST1.new %>% filter(QuantityCodeMain=="ener" & SubsectorCode=="res" & FuelCode=="nga"& SubscenarioNumber==1L)
ExpectedValue1 = FilteredInfo1 %>% select(`2018`) %>% pull
FilteredInfo2  = OutputSegmentedDataTable%>% filter(CountryCode=="CHN" & SubsectorCode=="res" & FuelType=="nga"& CTScenarioRate==0)
ExpectedValue2 = FilteredInfo2%>% filter(Year =="2018") %>% select(EnergyConsumption)/PJPerktoe

View(FilteredInfo1)
#Now Find the other side!

  ## Where are those results
  ## Create a function to get those results
  ## Create


ResultsList <- RunCoreModel(MainSegmentedDataTable, BaseYear_=BaseYear,AnalysisEndYear_=AnalysisEndYear,CTRange_=CTRange,DoPower_=DoPower)

MainSegmentedDataTable <- ResultsList[[1]] %>% filter(Year %in% (BaseYear:AnalysisEndYear))
SelectedPowerDataByGenType <- ResultsList[[2]] %>% filter(Year %in% (BaseYear:AnalysisEndYear))

save(MainSegmentedDataTable,CarbonTaxTrajectoryForm,CTRange,SelectedPowerDataByGenType,file="4-output/MainDataFile.rda")
write_csv(MainSegmentedDataTable,file="4-output/MainDataFile.csv")
write_csv(SelectedPowerDataByGenType,file="4-output/SelectedPowerDataByGenType.csv")
