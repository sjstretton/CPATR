#library(rstudioapi)
library(tidyverse)
library(magrittr)#
library(readxl)
#if(!exists("MainRDirectory")) MainRDirectory="C:/Users/wb547395/OneDrive - WBG/Documents/SimpleCPATR/R"
#setwd(MainRDirectory)
setwd("C:/Users/wb547395/OneDrive - WBG/Documents/Preproc/preprocesseddata")

#rm(list=ls())
PJPerktoe <- 0.041868
RetirementProportion=0.04
CountriesTable <- read_excel("temp/Categories.xlsx", sheet = "Countries")  #%>% filter(Type=="Country" & IncomeGroupSymbol!="HIC")
CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)
CountryList <- (CountryNameLookup$CountryCode)

##############

#PowerDemand <- read_csv("C:/Users/wb547395/OneDrive - WBG/Documents/SimpleCPATR/R/newpowerintermediate/PowerDemand.csv")

#5C-FillInLCOEAssumptions.R
PowerOrder=c("coa","nga","oil","nuc","wnd","sol","hyd","ore","bio")

SectoralPowerDemand= read_csv("powerintermediate/SectoralPowerDemand.csv")

TotalPowerDemand = SectoralPowerDemand %>% ungroup() %>%
  group_by(CountryCode,FuelType,Model,CTScenarioRate,Year,Time) %>%
  summarise(across(EnergyConsumption:PowerDemand.GWy,sum)) %>%
  mutate(PowerDemand.GWy = EnergyConsumption*1e6/(365*24*60*60),PowerDemand.TWh = EnergyConsumption*1000/(60*60))

Capacity <- read_csv("powerintermediate/ProcessedElectricalCapacityAndGeneration.csv") %>%
  mutate(FuelType=factor(FuelType,levels=PowerOrder)) %>%
  filter(FuelType != "nsp") %>%
  select(CountryCode,FuelType,CapacityMW,ProductionGWh, ProductionMWy, CapacityFactor) %>%
  rename(Capacity.MW=CapacityMW,Production.GWh=ProductionGWh, Production.MWy=ProductionMWy) %>%
  mutate(FuelType=factor(FuelType,levels=PowerOrder))

PrimaryEnergyInputToPower <- read_csv("powerintermediate/FossilInputToPower.csv") %>% mutate(oil=oop +  gso   +   die +  ker +  lpg +  jfu ) %>%
  select(c("CountryCode", all_of(PowerOrder))) %>%
  mutate(across(where(is.numeric), ~-.x)) %>%
  pivot_longer(where(is.numeric),names_to = "FuelType", values_to = "PrimaryEnergyInputToPower.ktoe") %>%
  mutate(FuelType=factor(FuelType,levels=PowerOrder),PrimaryEnergyInputToPower.PJ=PrimaryEnergyInputToPower.ktoe*PJPerktoe,
         PrimaryEnergyInputToPower.MWy=PrimaryEnergyInputToPower.PJ*1e9/(365.2425*24*60*60))

PowerOutput <- read_csv("powerintermediate/PowerOutput.csv")%>% mutate(oil=oop + gso + die + ker + lpg +  jfu ) %>%
  select(c("CountryCode", all_of(PowerOrder))) %>%
  pivot_longer(where(is.numeric),names_to = "FuelType", values_to = "PowerOutput.GWh") %>%
  mutate(FuelType=factor(FuelType,levels=PowerOrder))%>%
  mutate(PowerOutput.MWy=PowerOutput.GWh*1000/(365*24))


PowerCountryList = intersect(unique(Capacity$CountryCode),intersect(unique(PrimaryEnergyInputToPower$CountryCode),unique(PowerOutput$CountryCode)))


Capacity%<>% filter(CountryCode%in%PowerCountryList) %>% arrange(CountryCode, FuelType)

PrimaryEnergyInputToPower%<>%filter(CountryCode%in%PowerCountryList) %>% arrange(CountryCode, FuelType)

PowerOutput%<>%filter(CountryCode%in%PowerCountryList) %>% arrange(CountryCode, FuelType)

NPVi = function (CashFlowPerYear, RealDiscountRate, RealEscalation, startyear, endyear) {
  xNPVi= CashFlowPerYear * (exp(-(RealDiscountRate - RealEscalation) * startyear) - exp(-(RealDiscountRate - RealEscalation) * endyear)) / (RealDiscountRate - RealEscalation)
  xNPVi
}

LCEi = function(Ci, r, si, T0i, Ti, T0e, Te, LF) {
  xLCEi=   NPVi(Ci, r, si, T0i, Ti) / NPVi(hoursperyr(LF), r, 0, T0e, Te)
  xLCEi
}

hoursperyr= function(LF) LF*24*365

LCOECalc = function(CapEx=0, OMV=0, OMF=0, FuelPrice.kwh=0, Eff=0, LF=0, ConstructionTime=1, LifeTime=0, RealInterestRate=0,Type="Total") {

  LCECapital = LCEi(CapEx/ ConstructionTime, RealInterestRate, 0, 0, ConstructionTime, ConstructionTime, ConstructionTime + LifeTime, LF)
  LCEOMF = LCEi(OMF, RealInterestRate, 0, ConstructionTime, ConstructionTime + LifeTime, ConstructionTime, ConstructionTime + LifeTime, LF)
  LCEOMV = LCEi((OMV * LF * 365 * 24), RealInterestRate, 0, ConstructionTime, ConstructionTime + LifeTime, ConstructionTime, ConstructionTime + LifeTime, LF)
  LCEFuel = LCEi(365 * 24 * LF * FuelPrice.kwh / Eff, RealInterestRate, 0, ConstructionTime, ConstructionTime + LifeTime, ConstructionTime, ConstructionTime + LifeTime, LF)

  Output =  case_when(
    Type=="Capital"~ LCECapital,
    Type=="OMF"~ LCEOMF,
    Type=="OMV"~ LCEOMV,
    Type=="Fuel"~ LCEFuel,
    Type=="Total"~ LCECapital+LCEOMF+LCEOMV+LCEFuel
  )

  Output
}



FuelPricesGlobal = read_csv("rawdata/GlobalPricesGJ.csv") %>% pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="FuelPriceGlobal") %>%
  filter(Year=="2019") %>% select(-Year)

PriceUnitConversions <- read_csv("temp/PriceUnitConversions.csv",col_types = "ccdc")

SimpleCountryCodeCountryLookup <- read_csv("temp/SimpleCountryCodeCountryLookup.csv") %>%
  bind_rows(tibble(CountryCode=c("WLD","WLD","EUR","MENA","SSAFR","KOR"),CountryName=c("World","Global","Europe","MENA","Sub-Saharan-Africa","South Korea")))

FuelPriceAssumptionsUSDRaw <- read_csv("temp/FuelPriceAssumptionsUSD.csv") %>%
  rename(FuelPrice = `Fuel price`) %>%
  separate(UnitAllUSD, sep = "_", into= c("NotTheCurrency","EnergyUnit")) %>%
  separate(`Country-Commodity`, sep = "_", into= c("EnergyType","CountryName","Scenario")) %>%
  arrange(EnergyType, CountryName,   Scenario, NotTheCurrency, EnergyUnit, (Year)) %>%
  left_join(PriceUnitConversions) %>%
  left_join(SimpleCountryCodeCountryLookup)

FuelPriceAssumptionsUSD = FuelPriceAssumptionsUSDRaw %>%
  mutate(FuelPrice.USD_GJ=FuelPrice/GJperunit) %>%
  select(-EnergyType,-EnergyUnit, -FuelPrice, -GJperunit) %>%
  group_by(FuelType, CountryCode,  CountryName,  Scenario,  Year) %>%
  summarize(FuelPrice.USD_GJ=mean(FuelPrice.USD_GJ)) %>%
  #filter(Scenario%in%c("Benchmark","Annual")) %>%
  ungroup()


FuelPriceAssumptionsUSD.2018 = filter(FuelPriceAssumptionsUSD, Year==2018) %>%
  group_by(FuelType, CountryCode) %>%
  summarize(FuelPrice.USD_GJ=mean(FuelPrice.USD_GJ))
#
# AverageFuelPriceAssumptionsUSD = FuelPriceAssumptionsUSD %>% group_by(FuelType, CountryName, CountryCode) %>%
#   summarise(FuelPrice.USD_GJ=mean(FuelPrice.USD_GJ)) %>% ungroup() %>% select(-CountryName)
#
# FuelPriceAssumptionsUSDWider=pivot_wider(FuelPriceAssumptionsUSD,values_from=FuelPrice.USD_GJ,names_from=Year)
#
# write_csv(FuelPriceAssumptionsUSDWider,"powerintermediate/FuelPriceAssumptionsProcessedUSD.csv")


#####################################

LHV.Conversion=tibble(FuelType=c("coa","nga","oil","nuc","wnd","sol","hyd","ore","bio"),Jlhv_Jhhv=c(1.05,1.109,1.052,rep(1,5),21.2/17.0))
#Coal, Oil Gas from https://www.claverton-energy.com/wordpress/wp-content/uploads/2012/08/the_energy_and_fuel_data_sheet1.pdf
#Wood pellets (biomass) from https://en.wikipedia.org/wiki/Heat_of_combustion#Relation_between_heating_values

LCOEAssumptions <- read_csv("rawdata/LCOE.csv") %>%
  transmute(
    CountryCode = CountryCode,
    FuelType=Technology,
    LCOE.USD_kWh=`LCOE ($/MWh)`/1000,
    CapacityFactor=`Capacity factor (%)`,
    CapEx.USD_kW = `CAPEX ($m/MW)`*1000,
    ThermalEfficiency.Je_Jth.ncv=`Thermal Efficiency (%, HHV)`,
    FixedOpex.USD_kW_y=`Fixed opex ($/MW/yr)`/1000,
    VarOpex.USD_kWh=`Varliable opex ($/MWh)`/1000,
    DebtRatio=`Debt ratio (%)`,
    CostOfDebt=`Cost of debt (bps)`/10000,  ##!!! Is bps relative to Libor (or other interest rates) or absolute (in real terms)?
    CostOfEquity=`Cost of equity (%)`,
    CostOfCapital = DebtRatio * CostOfDebt + (1-DebtRatio) * CostOfEquity ) %>%
    left_join(LHV.Conversion) %>%
    mutate(ThermalEfficiency.Je_Jth.ncv=ThermalEfficiency.Je_Jth.ncv/Jlhv_Jhhv) %>%
    select(-DebtRatio ,-CostOfEquity,-CostOfDebt,-Jlhv_Jhhv) %>%
    mutate(ConstructionTime=2.4,Lifetime = 20,RealEscalationRate=0)

LCOEAssumptions %<>% left_join(FuelPricesGlobal) %>%
  left_join(FuelPriceAssumptionsUSD.2018) %>%
  mutate(ThermalEfficiency.Je_Jth.ncv=if_else(is.na(ThermalEfficiency.Je_Jth.ncv) & FuelType=="bio",0.35,ThermalEfficiency.Je_Jth.ncv)) %>%
  replace_na(replace=list(CapacityFactor=1, ThermalEfficiency.Je_Jth.ncv=1, FixedOpex.USD_kW_y=0, VarOpex.USD_kWh=0, CostOfCapital=0.2,FuelPriceGlobal=0)) %>%
  mutate(FuelPrice.GJ=if_else(is.na(FuelPrice.USD_GJ),FuelPriceGlobal,FuelPrice.USD_GJ),FuelPrice.kwh = FuelPrice.GJ*0.0036) %>%
  mutate(FuelCost.kwhe=FuelPrice.kwh/ThermalEfficiency.Je_Jth.ncv) %>% select(-FuelPrice.USD_GJ,-FuelPriceGlobal,-FuelPrice.GJ) %>%
  mutate(LCOE.Var.USD_kWh=FuelCost.kwhe+VarOpex.USD_kWh) %>%
  mutate(LCOE.Fixed.USD_kWh=LCOECalc(CapEx=CapEx.USD_kW, OMV=0, OMF=FixedOpex.USD_kW_y,
                                 FuelPrice.kwh=0, Eff=ThermalEfficiency.Je_Jth.ncv, LF=CapacityFactor, ConstructionTime=ConstructionTime , LifeTime=Lifetime ,
                                 RealInterestRate=CostOfCapital,Type="Total") )%>%
  mutate(LCOE.TotalCalc.USD_kWh=LCOECalc(CapEx=CapEx.USD_kW, OMV=VarOpex.USD_kWh, OMF=FixedOpex.USD_kW_y,
                                  FuelPrice.kwh=FuelPrice.kwh, Eff=ThermalEfficiency.Je_Jth.ncv, LF=CapacityFactor, ConstructionTime=ConstructionTime , LifeTime=Lifetime ,
                                  RealInterestRate=CostOfCapital,Type="Total") )

FuelPricesGlobal = read_csv("rawdata/GlobalPricesGJ.csv") %>%
  pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="FuelPriceGlobal") %>%
  filter(Year=="2018") %>% select(-Year)

LCOEAssumptions %<>% left_join(FuelPricesGlobal,by="FuelType")

LCOEAssumptions %<>% ungroup() %>%
  group_by(CountryCode, FuelType) %>%
  summarise(across(LCOE.USD_kWh:FuelPriceGlobal,mean))

###########################

LCOEAssumptions.Global = LCOEAssumptions %>%
  group_by(FuelType) %>%
  summarise(across(LCOE.USD_kWh:FuelPriceGlobal,mean,na.rm=TRUE)) %>%
  arrange(factor(FuelType, levels = PowerOrder))

WBCountriesTable <- read_csv("temp/Wbcountries.csv")
CountriesToModel=WBCountriesTable$CountryCode
Base.Power <- expand_grid(CountryCode=CountryList,FuelType=PowerOrder)

Base.Power.Global=Base.Power %>% inner_join(LCOEAssumptions.Global)
LCOEAssumptions.All = rows_update(Base.Power.Global,LCOEAssumptions,by=c("CountryCode", "FuelType"))
LCOEAssumptions.ore=LCOEAssumptions.All %>%
  filter(FuelType%in%c("wnd","sol","hyd")) %>%
  ungroup() %>%
  group_by(CountryCode) %>%
  summarise(across(LCOE.USD_kWh:FuelPriceGlobal, ~ mean(.x, na.rm = TRUE))) %>% mutate(FuelType="ore") %>%
  select(CountryCode,FuelType,everything()) %>%
  mutate(across(.cols=c("LCOE.USD_kWh", "CapEx.USD_kW","FixedOpex.USD_kW_y", "VarOpex.USD_kWh", "LCOE.Var.USD_kWh", "LCOE.Fixed.USD_kWh", "LCOE.TotalCalc.USD_kWh"), ~ 2*(.x)))
LCOEAssumptions.All%<>%bind_rows(LCOEAssumptions.ore)
LCOEAssumptions.All %<>%  arrange(CountryCode,factor(FuelType, levels = PowerOrder))

###########################

AllPowerData <- Capacity %>% left_join(PrimaryEnergyInputToPower) %>%
  left_join(PowerOutput) %>% rename(RawCapacityFactor=CapacityFactor) %>%
  inner_join(rename(LCOEAssumptions.All,BNEFCapacityFactor=CapacityFactor)) %>%
  mutate(CapacityFactor = if_else(RawCapacityFactor<0.01 |
                                    RawCapacityFactor>1.000,BNEFCapacityFactor,RawCapacityFactor)) %>%
  mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor) %>%
  mutate(RawThermalEfficiency = Production.MWy/PrimaryEnergyInputToPower.MWy)%>%
  mutate(ThermalEfficiency = if_else(RawThermalEfficiency<0.01 |
                                       RawThermalEfficiency>1.000 | is.na(RawThermalEfficiency),
                                     ThermalEfficiency.Je_Jth.ncv,RawThermalEfficiency)) %>%
  mutate(AvailabilityFactor = if_else(FuelType %in% c("coa","nga"), 0.9, 0))

#write_csv(AllPowerData,path="powerintermediate/AllPowerData.csv")

AllPowerData.Selected = AllPowerData %>% mutate(Year=2018,Country.FuelCode=paste(tolower(CountryCode),FuelType,sep=".")) %>%
  select(Country.FuelCode,CountryCode, FuelType, Year, PrimaryEnergyInputToPower.ktoe,PowerOutput.GWh,
                                                Capacity.MW, PowerOutput.MWy,PrimaryEnergyInputToPower.MWy,
                                                CapacityFactor,ThermalEfficiency,AvailabilityFactor,
                                                LCOE.TotalCalc.USD_kWh,LCOE.Var.USD_kWh, VarOpex.USD_kWh, LCOE.Fixed.USD_kWh,
                                                Production.MWy
                                                )
write_csv(AllPowerData.Selected,path="output/SelectedPowerData.csv")

AllPowerData.Selected.China = AllPowerData.Selected %>% filter(CountryCode=="CHN")
