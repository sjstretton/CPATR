## Purpose: This script contains the 'main function'.

rm(list=ls())
#Uncomment the line below for this to work in R studio
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(readxl)
library(magrittr)
library(cowplot)
library(ggrepel)

#Hard Coded Variables
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
SectorSubsectorLookup <- read_csv("metadata/SectorsAndSubsectorsNarrowLookup.csv")
CountriesTable <- read_csv("metadata/CountryLookup.csv")
CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)

#Define Included Elements of Each Dimension
YearList <- as.character(BaseYear:2033)
SectorList <- unique(Flow$Sector); SectorList=SectorList[!is.na(SectorList)]
SubsectorList <- unique(Flow$SubsectorCode); SubsectorList=SubsectorList[!is.na(SubsectorList)]
ScenariosList <- c("Baseline1","CTaxScen2")
CountryList <- TCAFCountryList  #c("CHN","IND","USA","JPN") #(CountryNameLookup$CountryCode)[1:10]
SelectedCountryNames = CountryNameLookup %>% filter(CountryCode %in% CountryList) %>% select(CountryName)


#Add Energy Balances
CPATEnergyBalances2018 <- read_csv("preprocess/output/ProcessedEnergyBalances2018.csv")
GDPRelativeToBase <- read_csv("preprocess/output/GDPRelativeToBase.csv",col_types="ccd")

#Add Power Data
SelectedPowerData=read_csv("preprocess/output/SelectedPowerData.csv") %>% mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor) %>%
  mutate(Production.GWh=PowerOutput.GWh, Production.MWy=PowerOutput.MWy, Year = BaseYear,LCOE.USD_kWh=LCOE.TotalCalc.USD_kWh) %>%
  filter(CountryCode %in% CountryList)

#Add Fuel Prices
FuelPricesGlobal <- read_csv("preprocess/rawdata/GlobalPricesGJ.csv") %>%
  pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="Price")
FuelPricesGlobalForPower  <- FuelPricesGlobal %>%  mutate(Year=as.integer(Year))
FuelPrices <- FuelPricesGlobal

#Add Base Price
BaseYearPriceLookup <- FuelPrices %>% filter(Year==as.character(BaseYear)) %>% rename(BaseYearPrice=Price) %>% select(-Year)
FuelPrices <- FuelPrices %>% rename(BaselinePrice=Price) %>% inner_join(BaseYearPriceLookup)

#Define Carbon Tax
CarbonTaxTrajectoryForm <- read_csv("preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd") #Shape of Carbon Tax Trajectory is Currently Standardised
CTRange=seq(from=0, to=100, by=20)

#Source Functions for Power Model
source("powermodel/1-PowerModel.R")

#################################################################

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


if(DoPower) {
  source("powermodel/1-PowerModel.R")


}

