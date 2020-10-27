## Purpose: This script contains the 'main function'.

rm(list=ls())
#Uncomment this for easy running
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(readxl)
library(magrittr)
library(cowplot)
library(ggrepel)

AnalysisEndYear <- 2030L
BaseYear <- 2018L
NumberOfYears <- 15L
EndYear <- BaseYear+NumberOfYears-1
DoPower <- FALSE

PJPerktoe <- 0.041868
FuelList <- c("coa", "nga", "oop", "gso", "die", "lpg", "ker", "jfu", "bio", "ecy")
PowerFuelTypes <- c("coa", "nga", "oop", "nuc", "wnd", "sol", "ore", "hyd", "bio")
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")
CountryCategoryList <- as_factor(c("VeryLarge","Large","MediumLarge","Medium","MediumSmall","Small","VerySmall"))

#Read in Lookup Tables
Fuels <- read_csv("metadata/EmissionsFactors.csv")
Flow <- read_csv("metadata/SectorFullLookup.csv")
Elasticities <- read_csv("metadata/Elasticities.csv") %>% filter(FuelType %in% FuelList)
CarbonTaxTrajectoryForm <- read_csv("preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd")
SectorSubsectorLookup <- read_csv("metadata/SectorsAndSubsectorsNarrowLookup.csv")
CountriesTable <- read_csv("metadata/CountryLookup.csv")

#Add Energy Balances
CPATEnergyBalances2018 <- read_csv("preprocess/output/ProcessedEnergyBalances2018.csv")
GDPRelativeToBase <- read_csv("preprocess/output/GDPRelativeToBase.csv",col_types="ccd")

#Add Fuel Prices
FuelPricesGlobal <- read_csv("preprocess/rawdata/GlobalPricesGJ.csv") %>%
  pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="Price")
FuelPricesGlobalForPower  <- FuelPricesGlobal %>%  mutate(Year=as.integer(Year))
FuelPrices <- FuelPricesGlobal

CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)
CountryList <- TCAFCountryList  #c("CHN","IND","USA","JPN") #(CountryNameLookup$CountryCode)[1:10]
SelectedCountryNames = CountryNameLookup %>% filter(CountryCode %in% CountryList) %>% select(CountryName)

SelectedPowerData=read_csv("preprocess/output/SelectedPowerData.csv") %>% mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor) %>%
  mutate(Production.GWh=PowerOutput.GWh, Production.MWy=PowerOutput.MWy, Year = BaseYear,LCOE.USD_kWh=LCOE.TotalCalc.USD_kWh) %>% filter(CountryCode %in% CountryList)

YearList <- as.character(BaseYear:2033)
SectorList <- unique(Flow$Sector); SectorList=SectorList[!is.na(SectorList)]
SubsectorList <- unique(Flow$SubsectorCode); SubsectorList=SubsectorList[!is.na(SubsectorList)]
ScenariosList <- c("Baseline1","CTaxScen2")

BaseYearPriceLookup <- FuelPrices %>% filter(Year==as.character(BaseYear)) %>% rename(BaseYearPrice=Price) %>% select(-Year)
FuelPrices <- FuelPrices %>% rename(BaselinePrice=Price) %>% inner_join(BaseYearPriceLookup)
CTRange=seq(from=0, to=100, by=20)


#################################################################

InvestmentBeta <- 1
DispatchBeta <- 1
GJperKWh <- 0.00360
PJPerktoe <- 0.041868
RetirementProportion <- 0.04
MaximumCoalAndGasCapacity <- 0.9

GDPGrowth <- rep(1,NumberOfYears)
PowerPrices <- rep(1,NumberOfYears)
IncomeElasticityofPowerDemand <- 0.75
PriceElasticityofPowerDemand <- -0.51

PowerCountryList=unique(SelectedPowerData$CountryCode)

YearsTable=tibble(Year=BaseYear:EndYear,fakecol=1L) #Fake for doing cartesian product join

SelectedPowerDataByGenType = SelectedPowerData %>% mutate(EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion) %>%
  ungroup() %>%
  group_by(CountryCode) %>%
  mutate(MinCostNumber=0,MinCostFuelCostr=0, CostOfCheapestOption=0, RelativeCost=0, LogitNominator=0,
         ProportionOfInvestment=0,CheckSum=0, TotalThisYearDemand.MWy=0,GenerationShare=0) %>%
  select(c("CountryCode","FuelType","Capacity.MW", "Production.MWy","EffectiveCapacity.MW",
           "CapacityFactor","ThermalEfficiency","LCOE.TotalCalc.USD_kWh","LCOE.USD_kWh",
           "VarOpex.USD_kWh","LCOE.Var.USD_kWh","LCOE.Fixed.USD_kWh","TotalThisYearDemand.MWy")) %>%
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


MainPowerLargeDataTable <-  YearsTable %>%
  inner_join(SelectedPowerDataByGenType) %>%
  left_join(FuelPricesGlobalForPower) %>%
  select(-fakecol) %>%
  select(CountryCode,FuelType,Year,everything())

#################################################################
ApplyPowerModelToSpecificYear=  function(CurrentYearTemp,CTMaxRate=0,MainPowerDataTable=MainPowerLargeDataTable) {
  i = as.integer(CurrentYearTemp-BaseYear+1)
  CurrentYearPowerFeaturesByFuelType = filter(MainPowerDataTable,Year==CurrentYearTemp)

  CurrentYearPowerFeaturesByFuelType %<>% ungroup() %>% group_by(CountryCode)

  if(i == 1) {
    CurrentYearPowerFeaturesByFuelType %<>% mutate(TotalThisYearDemand.MWy = sum(Production.MWy),
    Capacity.MW=if_else(Capacity.MW>Production.MWy,Capacity.MW,Production.MWy/CapacityFactor)) #Capacity can not be less than generation
  }

  CurrentYearPowerFeaturesByFuelType  %<>%
    mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor,
           TotalEffectiveCapacity=sum(EffectiveCapacity.MW),
           FuelCostPerkWh=Price*GJperKWh/ThermalEfficiency,
           LCOE.Var.USD_kWh = Price*GJperKWh/ThermalEfficiency+VarOpex.USD_kWh,
           LCOE.TotalCalc.USD_kWh=LCOE.Var.USD_kWh+LCOE.Fixed.USD_kWh)

  if(i>1) {CurrentYearPowerFeaturesByFuelType %<>% mutate(Production.MWy=if_else(FuelType%in%c("coa","nga"),0,EffectiveCapacity.MW)) }

    CurrentYearPowerFeaturesByFuelType %<>% mutate(TotalNonCoalAndGasGeneration=sum(if_else(!FuelType%in%c("coa","nga"),Production.MWy,0)),
                                                   RequiredCoalAndGas=max(0,TotalThisYearDemand.MWy-TotalNonCoalAndGasGeneration))

      CoalAndGas = CurrentYearPowerFeaturesByFuelType %>%
    filter(FuelType %in% c("coa","nga")) %>% #ungroup() %>% group_by(CountryCode) %>%
    mutate(MaximumGen=Capacity.MW*0.9,
           SumMaximumGen=sum(MaximumGen),
           OtherMaximumGen=SumMaximumGen-MaximumGen,
           MinimumGen = pmax(0,RequiredCoalAndGas-OtherMaximumGen),
           SumMinimumGen=sum(MinimumGen),
           RemainderAfterMinimas=RequiredCoalAndGas-SumMinimumGen,
           MinVarCost=min(LCOE.Var.USD_kWh),
           RelVarCost=LCOE.Var.USD_kWh/MinVarCost,
           LogitExp=exp(-DispatchBeta*RelVarCost),
           SumLogitExp=sum(LogitExp),
           ProportionOfCoalGasGeneration=LogitExp/SumLogitExp,
           SumProportionOfCoalGasGeneration=sum(ProportionOfCoalGasGeneration)
           ) %>%
        select(CountryCode, FuelType, RequiredCoalAndGas, MaximumGen, SumMaximumGen,
               OtherMaximumGen, MinimumGen, SumMaximumGen, OtherMaximumGen,
               RemainderAfterMinimas,ProportionOfCoalGasGeneration ,MinVarCost,
               RelVarCost,LogitExp,SumLogitExp,SumProportionOfCoalGasGeneration)

  CurrentYearPowerFeaturesByFuelType = rows_update(CurrentYearPowerFeaturesByFuelType, CoalAndGas, by = c("CountryCode", "FuelType"))


  if(i>1) {CurrentYearPowerFeaturesByFuelType %<>% mutate(Production.MWy=if_else(FuelType%in%c("coa","nga"),MinimumGen+RemainderAfterMinimas*ProportionOfCoalGasGeneration,EffectiveCapacity.MW) ) }
   CurrentYearPowerFeaturesByFuelType %<>% mutate(GenerationShare = Production.MWy/sum(Production.MWy))



   CurrentYearPowerFeaturesByFuelType %<>%
     mutate(EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion,
            NamePlateEndOfYearRetirements=EffectiveEndOfYearRetirements/CapacityFactor,
            TotalRetirements=sum(EffectiveEndOfYearRetirements),
            TotalPredictedNextYearDemand.MWy=TotalThisYearDemand.MWy*(GDPGrowth[i]^IncomeElasticityofPowerDemand)*(PowerPrices[i]^PriceElasticityofPowerDemand),
            TotalNewEffectiveInvestmentsNeeded = max(0,TotalPredictedNextYearDemand.MWy - TotalThisYearDemand.MWy + TotalRetirements))

  CurrentYearPowerFeaturesByFuelType %<>%
    mutate(CostOfCheapestOption=min(LCOE.TotalCalc.USD_kWh),
           RelativeCost=LCOE.TotalCalc.USD_kWh/CostOfCheapestOption,
           LogitNominator=exp(-InvestmentBeta*RelativeCost),
           SumLogitNominator=sum(LogitNominator),
           ProportionOfInvestment=LogitNominator/SumLogitNominator,
           CheckSum=sum(ProportionOfInvestment),
           NewEffectiveInvestments=ProportionOfInvestment*TotalNewEffectiveInvestmentsNeeded,
           NewNameplateInvestments=NewEffectiveInvestments/CapacityFactor,
           NextYearEffectiveCapacity.MW=EffectiveCapacity.MW-EffectiveEndOfYearRetirements+NewEffectiveInvestments,
           NextYearCapacity.MW=Capacity.MW-NamePlateEndOfYearRetirements+NewNameplateInvestments
           )


  MainPowerDataTable[MainPowerDataTable$Year==CurrentYearTemp,]=CurrentYearPowerFeaturesByFuelType
  if(i<NumberOfYears) {
  MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("Capacity.MW")]=CurrentYearPowerFeaturesByFuelType$NextYearCapacity.MW
  MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("EffectiveCapacity.MW")]=CurrentYearPowerFeaturesByFuelType$NextYearEffectiveCapacity.MW
  MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("TotalThisYearDemand.MWy")]=CurrentYearPowerFeaturesByFuelType$TotalPredictedNextYearDemand.MWy
  }
  MainPowerDataTable
}

#################################################################


#################################################################

##TODO Filter the CT Range.
PowerBase <- expand_grid(CountryCode=CountryList,SubsectorCode="pow",
                         FuelType=PowerFuelTypes,Year=YearList,Scenario="CarbonTax",CTScenarioRate=CTRange) #For power sector model
NonPowerBase <- expand_grid(CountryCode=CountryList,SubsectorCode=setdiff(SubsectorList, "pow"),
                            FuelType=FuelList,Year=YearList,Scenario="CarbonTax",CTScenarioRate=CTRange)

Base <- NonPowerBase %>% bind_rows(PowerBase) %>%
  mutate(Code = paste( CountryCode, SubsectorCode, FuelType,Year,sep=".")) %>%
  select(Code, everything()) %>%
  inner_join(SectorSubsectorLookup) %>%
  mutate(Time = as.integer(Year)-BaseYear) %>%  select (-FlowCode, -Code)


CombinedStatic <- Base %>%
  left_join(Elasticities) %>%
  left_join(Fuels) %>%
  left_join(CPATEnergyBalances2018)  %>%
  left_join(GDPRelativeToBase)  %>%
  left_join(FuelPrices) %>%
  left_join(CarbonTaxTrajectoryForm) %>%
  mutate(Model="") %>%
  select(CountryCode, Sector, SubsectorCode, FuelType, Scenario, Model, CTScenarioRate, Year, Time, el_inc, el_dem, el_cons, eff, EmissionsFactor,
    ProportionOfMaxCTaxRate, BaseYearEnergy, GDPFactor, BaseYearPrice, BaselinePrice) %>%
  mutate(BaseYearEmissions=BaseYearEnergy*EmissionsFactor, CTRate=0, FuelPrice=0, PriceChangeFactor=1, IncomeEffect=1, EfficiencyEffect=1,
         PriceEffect=1, TotalEffect=1,EnergyConsumption=0, Emissions=0, SensitivityPerDolPerT=0, DeltaPerDolPerT=0)

CombinedNew=CombinedStatic

CombinedAll <- CombinedStatic[0,]




UpdateCoreData = function(CombinedStaticStyleInput,CTMaxRate=CTMaxRate) {

  CombinedStaticStyleInput <- CombinedStaticStyleInput %>%
    mutate(Model="R",

           CTScenarioRate=CTMaxRate,
           CTRate=CTMaxRate*ProportionOfMaxCTaxRate,
           FuelPrice=BaselinePrice + CTMaxRate*ProportionOfMaxCTaxRate*EmissionsFactor,
           PriceChangeFactor=(BaselinePrice + CTMaxRate*ProportionOfMaxCTaxRate*EmissionsFactor)/BaseYearPrice,
           EfficiencyEffect=(1+eff)^((-Time)*(1+el_dem)), #Autonomous Efficiency only
           IncomeEffect=(GDPFactor)^(el_inc),
           PriceEffect=(PriceChangeFactor)^(el_dem+el_cons+el_dem*el_cons),
           TotalEffect=if_else(SubsectorCode%in%c("ral","nav","avi"),IncomeEffect,EfficiencyEffect*IncomeEffect*PriceEffect),
           EnergyConsumption=BaseYearEnergy*TotalEffect,
           Emissions=EmissionsFactor*EnergyConsumption,
           SensitivityPerDolPerT=IncomeEffect*EfficiencyEffect*(EmissionsFactor/BaseYearPrice)*(el_dem+el_cons+el_dem*el_cons),
           DeltaPerDolPerT=IncomeEffect*EfficiencyEffect*(EmissionsFactor/BaseYearPrice)*(el_dem+el_cons+el_dem*el_cons)*BaseYearEnergy
    )
  CombinedStaticStyleInput
}




################################### START OF MAIN LOOP ###################################
for (CTMaxRate in CTRange)  {
  for(FilterYear in (BaseYear:AnalysisEndYear)) {
    FilteredTable = CombinedNew %>% filter(Year==as.character(FilterYear) & CTScenarioRate==CTMaxRate)
    UpdatedFilteredTable = UpdateCoreData(CombinedStaticStyleInput=FilteredTable,CTMaxRate=CTMaxRate)
    CombinedNew[CombinedNew$Year==as.character(FilterYear)& CombinedNew$CTScenarioRate==CTMaxRate,] <- UpdatedFilteredTable
    if(DoPower) MainPowerLargeDataTable=ApplyPowerModelToSpecificYear(CurrentYearTemp=FilterYear,CTMaxRate=CTMaxRate,MainPowerDataTable=MainPowerLargeDataTable)
    }
}
################################### END OF MAIN LOOP ###################################

CombinedNew <- CombinedNew %>% filter(Year %in% (BaseYear:AnalysisEndYear))


CombinedAll=CombinedNew

save(CombinedAll,CarbonTaxTrajectoryForm,CTRange,file="output/MainDataFile.rda")
write_csv(CombinedAll,file="output/MainDataFile.csv")


if(DoPower) {SelectedPowerDataByGenTypeAll=MainPowerLargeDataTable

DisplayData=SelectedPowerDataByGenTypeAll%>% filter(CountryCode=="CHN")

SelectedFinalPowerResults = SelectedPowerDataByGenTypeAll%>% select(CountryCode, FuelType,  Year, Capacity.MW,Production.MWy)

CapacityResults=SelectedFinalPowerResults %>% pivot_wider(id_cols = c(1,2),values_from=Capacity.MW,names_from=Year)

ProductionResults=SelectedFinalPowerResults %>% pivot_wider(id_cols = c(1,2),values_from=Production.MWy,names_from=Year)

ProductionResults.CHN = filter(ProductionResults,CountryCode=="CHN")

save(SelectedPowerDataByGenTypeAll,file="output/PowerDataFile.rda")
write_csv(SelectedPowerDataByGenTypeAll,file="output/PowerDataFile.csv")

DisplayData=SelectedPowerDataByGenTypeAll%>% filter(CountryCode=="CHN")
SelectedFinalPowerResults = SelectedPowerDataByGenTypeAll%>% select(CountryCode, FuelType,  Year, Capacity.MW,Production.MWy)
CapacityResults=SelectedFinalPowerResults %>% pivot_wider(id_cols = c(1,2),values_from=Capacity.MW,names_from=Year)
ProductionResults=SelectedFinalPowerResults %>% pivot_wider(id_cols = c(1,2),values_from=Production.MWy,names_from=Year)
ProductionResults.CHN = filter(ProductionResults,CountryCode=="CHN")


SectoralPowerDemand = CombinedAll %>% filter(FuelType=="ecy" & Sector!="pow" ) %>%
  select(CountryCode,Sector,SubsectorCode,FuelType,Model,CTScenarioRate,Year,Time,EnergyConsumption,EnergyConsumption.ktoe) %>%
  mutate(PowerDemand.GWh= EnergyConsumption*1e6/(60*60),PowerDemand.GWy= EnergyConsumption*1e6/(365*24*60*60))

#write_csv(SectoralPowerDemand,"output/SectoralPowerDemand.csv")

TotalPowerDemand = SectoralPowerDemand %>% ungroup() %>%
  group_by(CountryCode,FuelType,Model,CTScenarioRate,Year,Time) %>%
  summarise(across(EnergyConsumption:PowerDemand.GWy,sum))



}

