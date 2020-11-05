
BuildBaseTable = function(CountryList_,SubsectorList_,FuelList_,YearList_,CTRange_,BaseYear_,SectorSubsectorLookup_) {
PowerBase <- expand_grid(CountryCode=CountryList_,SubsectorCode="pow",
                         FuelType=PowerFuelTypes,Year=YearList_,Scenario="CarbonTax",CTScenarioRate=CTRange_) #For power sector model
NonPowerBase <- expand_grid(CountryCode=CountryList_,SubsectorCode=setdiff(SubsectorList_, "pow"),
                            FuelType=FuelList_,Year=YearList_,Scenario="CarbonTax",CTScenarioRate=CTRange_)

Base <- NonPowerBase %>% bind_rows(PowerBase) %>%
  mutate(Code = paste( CountryCode, SubsectorCode, FuelType,Year,sep=".")) %>%
  select(Code, everything()) %>%
  inner_join(SectorSubsectorLookup_) %>%
  mutate(Time = as.integer(Year)-BaseYear_) %>%  select (-FlowCode, -Code)

return(Base)
}

BuildMainSegmentedDataTable= function(Base_, Elasticities_,Fuels_,CPATEnergyBalancesBaseYear_,GDPRelativeToBase_,FuelPrices_,CarbonTaxTrajectoryForm_) {
MainSegmentedDataTable <- Base_ %>%
  left_join(Elasticities_) %>%
  left_join(Fuels_) %>%
  left_join(CPATEnergyBalancesBaseYear_)  %>%
  left_join(GDPRelativeToBase_)  %>%
  left_join(FuelPrices_) %>%
  left_join(CarbonTaxTrajectoryForm_) %>%
  mutate(Model="") %>%
  select(CountryCode, Sector, SubsectorCode, FuelType, Scenario, Model, CTScenarioRate, Year, Time, el_inc, el_dem, el_cons, eff, EmissionsFactor,
         ProportionOfMaxCTaxRate, BaseYearEnergy, GDPFactor, BaseYearPrice, BaselinePrice) %>%
  mutate(BaseYearEmissions=BaseYearEnergy*EmissionsFactor, CTRate=0, FuelPrice=0, PriceChangeFactor=1, IncomeEffect=1, EfficiencyEffect=1,
         PriceEffect=1, TotalEffect=1,EnergyConsumption=0, Emissions=0, SensitivityPerDolPerT=0, DeltaPerDolPerT=0)
return(MainSegmentedDataTable)
}

