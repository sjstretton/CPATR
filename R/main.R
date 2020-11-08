## Purpose: This script contains the 'main function'.

#NExt steps:
#Finish the encapsulation
#Basic documentation

rm(list=ls())
#Uncomment the line below for this to work in R studio
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(readxl)
library(magrittr)
library(cowplot)
library(ggrepel)
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")
MyCountryList <-  "CHN" #TCAFCountryList ##c("CHN") #Other options: ...,"IND","USA","JPN" #(CountryNameLookup$CountryCode)[1:10] #
DoPowerGlobal <- TRUE

AnalysisEndYear <- 2030L
BaseYear <- 2018L
NumberOfYears <- 15L
EndYear <- BaseYear+NumberOfYears-1
RetirementProportion <- 0.04

source("functions.R")

Coremodel=function(CountryList=MyCountryList,DoPower=DoPowerGlobal) {


  ################################### SET UP AND INPUT DATA ###################################
  #Hard Coded Variables
  PJPerktoe <- 0.041868
  FuelList <- c("coa", "nga", "oop", "gso", "die", "lpg", "ker", "jfu", "bio", "ecy")
  PowerFuelTypes <- c("coa", "nga", "oop", "nuc", "wnd", "sol", "ore", "hyd", "bio")

  CountryCategoryList <- as_factor(c("VeryLarge","Large","MediumLarge","Medium","MediumSmall","Small","VerySmall"))

  #Read in Lookup Tables
  Fuels <- read_csv("1-metadata/EmissionsFactors.csv")
  Flow <- read_csv("1-metadata/SectorFullLookup.csv")
  Elasticities <- read_csv("1-metadata/Elasticities.csv") %>% filter(FuelType %in% FuelList)
  SectorSubsectorLookup <- read_csv("1-metadata/SectorsAndSubsectorsNarrowLookup.csv")
  CountriesTable <- read_csv("1-metadata/CountryLookup.csv")
  CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)

  #Define Included Elements of Each Dimension
  YearList <- as.character(BaseYear:(EndYear+1))
  SectorList <- unique(Flow$Sector); SectorList=SectorList[!is.na(SectorList)]
  SubsectorList <- unique(Flow$SubsectorCode); SubsectorList=SubsectorList[!is.na(SubsectorList)]
  ScenariosList <- c("Baseline1","CTaxScen2")


  #Add Energy Balances
  CPATEnergyBalances2018 <- read_csv("2-preprocess/output/ProcessedEnergyBalances2018.csv")
  GDPRelativeToBase <- read_csv("2-preprocess/output/GDPRelativeToBase.csv",col_types="ccd")

  #Add Power Data
  SelectedPowerData=read_csv("2-preprocess/output/SelectedPowerData.csv")

  #Add Fuel Prices
  FuelPricesGlobal <- read_csv("2-preprocess/rawdata/GlobalPricesGJ.csv") %>%
    pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="Price")
  FuelPricesGlobalForPower  <- FuelPricesGlobal %>%  mutate(Year=as.integer(Year))
  FuelPrices <- FuelPricesGlobal

  #Add Base Price
  BaseYearPriceLookup <- FuelPrices %>% filter(Year==as.character(BaseYear)) %>% rename(BaseYearPrice=Price) %>% select(-Year)
  FuelPrices <- FuelPrices %>% rename(BaselinePrice=Price) %>% inner_join(BaseYearPriceLookup)

  #Define Carbon Tax
  CarbonTaxTrajectoryForm <- read_csv("2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd") #Shape of Carbon Tax Trajectory is Currently Standardised
  CTRange=seq(from=0, to=100, by=20)

  PowerCountryList=unique(SelectedPowerData$CountryCode)

  YearsTable=tibble(Year=BaseYear:EndYear,fakecol=1L) #Fake for doing cartesian product join

    print("0")
  print(SelectedPowerData)

  SelectedPowerDataByGenType = SelectedPowerData %>% mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor, ##NOTE THIS MIGHT NOT BE QUITE RIGHT
                                                            EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion) %>%
    ungroup() %>%
    group_by(CountryCode)

  print("1")
  print(SelectedPowerDataByGenType)

  SelectedPowerDataByGenType %<>%
    mutate(MinCostNumber=0,MinCostFuelCostr=0, CostOfCheapestOption=0, RelativeCost=0, LogitNominator=0,
           ProportionOfInvestment=0,CheckSum=0, TotalThisYearDemand.MWy=0,GenerationShare=0) %>%
    select(c("CountryCode","FuelType","Capacity.MW", "PowerOutput.MWy","EffectiveCapacity.MW",
             "CapacityFactor","ThermalEfficiency","LCOE.TotalCalc.USD_kWh","LCOE.USD_kWh",
             "VarOpex.USD_kWh","LCOE.Var.USD_kWh","LCOE.Fixed.USD_kWh","TotalThisYearDemand.MWy"))

  print("2")
  print(SelectedPowerDataByGenType)


    SelectedPowerDataByGenType %<>%
    mutate(FuelCostPerkWh=0 , TotalNonCoalAndGasGeneration=0, RequiredCoalAndGas=0,MaximumGen=0,SumMaximumGen=0,OtherMaximumGen=0,
           MinimumGen = 0, RemainderAfterMinimas=0,
           MinVarCost=0,RelVarCost=0,LogitExp=0,SumLogitExp=0,ProportionOfCoalGasGeneration=0,
           SumProportionOfCoalGasGeneration=0,GenerationShare=0,
           EffectiveEndOfYearRetirements=0 , TotalEffectiveCapacity=0,TotalRetirements=0 , TotalPredictedNextYearDemand.MWy=0 ,
           CostOfCheapestOption=0 , RelativeCost=0 ,
           LogitNominator=0 , SumLogitNominator=0 , ProportionOfInvestment=0 , CheckSum=0 ,
           NamePlateEndOfYearRetirements=0,
           TotalNewEffectiveInvestmentsNeeded =0,
           NewEffectiveInvestments=0,
           NewNameplateInvestments=0,
           NextYearEffectiveCapacity.MW=0,
           NextYearCapacity.MW=0,
           ThisYearGenerationCost=0,
           fakecol=1L)


  #SelectedPowerData %<>%  filter(CountryCode %in% CountryList)
print("3")
    print(YearsTable)
    print(SelectedPowerDataByGenType)
    print(FuelPricesGlobalForPower)
    print(FuelPricesGlobalForPower)
  MainPowerLargeDataTable <-  YearsTable %>%
    inner_join(SelectedPowerDataByGenType) %>%
    left_join(FuelPricesGlobalForPower) %>%
    select(-fakecol) %>%
    select(CountryCode,FuelType,Year,everything())
  print("r")
  MainPowerLargeDataTable

BaseTable <- BuildBaseTable(CountryList_=CountryList,SubsectorList_=SubsectorList, FuelList_=FuelList,YearList_=YearList,CTRange_=CTRange,
                              BaseYear_=BaseYear,SectorSubsectorLookup_=SectorSubsectorLookup,PowerFuelTypes_=PowerFuelTypes)

MainSegmentedDataTable <- BaseTable %>% BuildMainSegmentedDataTable(Elasticities_=Elasticities,Fuels_=Fuels,CPATEnergyBalancesBaseYear_=CPATEnergyBalances2018,
                                                 GDPRelativeToBase_=GDPRelativeToBase,FuelPrices_=FuelPrices,CarbonTaxTrajectoryForm_=CarbonTaxTrajectoryForm)

ResultsList <- RunCoreModel(MainSegmentedDataTable_=MainSegmentedDataTable, MainPowerLargeDataTable_=MainPowerLargeDataTable,
                            BaseYear_=BaseYear,AnalysisEndYear_=AnalysisEndYear,CTRange_=CTRange,DoPower_=DoPower)

return(MainPowerLargeDataTable)
}

ResultsList=Coremodel()

#MainSegmentedDataTable <- ResultsList[[1]] %>% filter(Year %in% (BaseYear:AnalysisEndYear))
#SelectedPowerDataByGenType <- ResultsList[[2]] %>% filter(Year %in% (BaseYear:AnalysisEndYear))

#save(MainSegmentedDataTable,CarbonTaxTrajectoryForm,CTRange,SelectedPowerDataByGenType,file="4-output/MainDataFile.rda")
#write_csv(MainSegmentedDataTable,file="4-output/MainDataFile.csv")
#write_csv(SelectedPowerDataByGenType,file="4-output/SelectedPowerDataByGenType.csv")






################################################################
#TestThat

#Get Coal Cement from CPAT Results.
MST1.new <- read_csv("5-comparison/2ResultsFromCPAT/MST1-new.csv",col_types = "iiccccccidddddddddddddd",na = c("", "NA","#DIV/0!"))

ConvertTibbleWithIDCols = function(tb, idcols=1L, conversion=1) {
  ColsToConvert=setdiff((1:ncol(tb)),idcols)
  tb[,ColsToConvert]=tb[,ColsToConvert]*conversion
  return(tb)
}




#Found one side!
#PJPerktoe <- 0.041868
FindFilterdValueCPAT = function () {
  FilteredInfo1  = MST1.new %>% filter(QuantityCodeMain=="ener" & SubsectorCode=="res" & FuelCode=="coa"& SubscenarioNumber==1L)
  ExpectedValue1 = FilteredInfo1 %>% select(`2018`) %>% deframe
  ExpectedValue1 }

FindFilterdValueR   = function () {
  FilteredInfo2 = MainSegmentedDataTable%>% filter(CountryCode=="CHN" & SubsectorCode=="res" & FuelType=="coa"& CTScenarioRate==0)
  ExpectedValue2 = FilteredInfo2%>% filter(Year =="2018") %>% select(EnergyConsumption)/PJPerktoe
  ExpectedValue2 %<>% pull("EnergyConsumption")
  ExpectedValue2
}

#WideForm2.ktoe = FilteredInfo2 %>% select("CountryCode","Sector","SubsectorCode", "FuelType","Scenario","Model","CTScenarioRate","Time","EnergyConsumption","Year") %>%
#pivot_wider(id_cols=c("CountryCode","Sector","SubsectorCode","FuelType","Scenario","Model","CTScenarioRate"), values_from="EnergyConsumption",names_from="Year") %>%
#ConvertTibbleWithIDCols(idcols=1:7, conversion=1/PJPerktoe)




