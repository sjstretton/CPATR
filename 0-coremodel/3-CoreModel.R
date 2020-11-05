

UpdateCoreData = function(MainSegmentedDataTableStyleInput,CTMaxRate=CTMaxRate) {
  MainSegmentedDataTableStyleInput <- MainSegmentedDataTableStyleInput %>%
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
  return(MainSegmentedDataTableStyleInput)
}

DoPower=T
if(DoPower) {
  source("3-powermodel/1-PowerModel.R")

}

#MainPowerLargeDataTable=ApplyPowerModelToSpecificYear(CurrentYearTemp=2020,CTMaxRate=CTMaxRate,MainPowerDataTable=MainPowerLargeDataTable)

################################### START OF MAIN LOOP ###################################
RunCoreModel = function(MainSegmentedDataTable_,BaseYear_,AnalysisEndYear_,CTRange_=CTRange,DoPower_=DoPower) {
  for (CTMaxRate in CTRange)  {
    for(FilterYear in (BaseYear:AnalysisEndYear_)) {
      FilteredTable = MainSegmentedDataTable_ %>% filter(Year==as.character(FilterYear) & CTScenarioRate==CTMaxRate)
      UpdatedFilteredTable = UpdateCoreData(MainSegmentedDataTableStyleInput=FilteredTable,CTMaxRate=CTMaxRate)
      MainSegmentedDataTable[MainSegmentedDataTable$Year==as.character(FilterYear)& MainSegmentedDataTable$CTScenarioRate==CTMaxRate,] <- UpdatedFilteredTable
      if(DoPower_)
        MainPowerLargeDataTable=ApplyPowerModelToSpecificYear(CurrentYearTemp=FilterYear,CTMaxRate=CTMaxRate,MainPowerDataTable=MainPowerLargeDataTable)
    }
  }
  return(list(MainSegmentedDataTable,MainPowerLargeDataTable))
}
################################### END OF MAIN LOOP ###################################
