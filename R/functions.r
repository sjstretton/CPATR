#' Build Base Table
#'
#' Create the base table.
#'
#' @param CountryList_ List of Countries
#'
#' @return tibble
#' @export
#' @examples
#' BuildBaseTable(CountryList,SubsectorList,FuelList,YearList,CTRange,BaseYear,SectorSubsectorLookup,PowerFuelTypes)


BuildBaseTable = function(CountryList_,SubsectorList_,FuelList_,YearList_,CTRange_,BaseYear_,SectorSubsectorLookup_,PowerFuelTypes_) {
  PowerBase <- expand_grid(CountryCode=CountryList_,SubsectorCode="pow",
                           FuelType=PowerFuelTypes_,Year=YearList_,Scenario="CarbonTax",CTScenarioRate=CTRange_) #For power sector model
  NonPowerBase <- expand_grid(CountryCode=CountryList_,SubsectorCode=setdiff(SubsectorList_, "pow"),FuelType=FuelList_,Year=YearList_,Scenario="CarbonTax",CTScenarioRate=CTRange_)


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



RunCoreModel = function(MainSegmentedDataTable_,MainPowerLargeDataTable_,BaseYear_,AnalysisEndYear_,CTRange_=CTRange,DoPower_=DoPower) {
  for (CTMaxRate in CTRange_)  {
    for(FilterYear in (BaseYear:AnalysisEndYear_)) {
      FilteredTable = MainSegmentedDataTable_ %>% filter(Year==as.character(FilterYear) & CTScenarioRate==CTMaxRate)
      UpdatedFilteredTable = UpdateCoreData(MainSegmentedDataTableStyleInput=FilteredTable,CTMaxRate=CTMaxRate)
      MainSegmentedDataTable_[MainSegmentedDataTable_$Year==as.character(FilterYear)& MainSegmentedDataTable_$CTScenarioRate==CTMaxRate,] <- UpdatedFilteredTable
      if(DoPower_)
        MainPowerLargeDataTable_=ApplyPowerModelToSpecificYear(CurrentYearTemp=FilterYear,CTMaxRate=CTMaxRate,MainPowerDataTable=MainPowerLargeDataTable_)
    }
  }
  return(list(MainSegmentedDataTable_,MainPowerLargeDataTable_))
}


#################################################################
ApplyPowerModelToSpecificYear=  function(CurrentYearTemp,CTMaxRate=0,
                                         MainPowerDataTable=MainPowerLargeDataTable_) {
  i = as.integer(CurrentYearTemp-BaseYear+1)

  InvestmentBeta <- 1
  DispatchBeta <- 1
  GJperKWh <- 0.00360
  PJPerktoe <- 0.041868

  MaximumCoalAndGasCapacity <- 0.9

  GDPGrowth <- rep(1,NumberOfYears)
  PowerPrices <- rep(1,NumberOfYears)
  IncomeElasticityofPowerDemand <- 0.75
  PriceElasticityofPowerDemand <- -0.51



  CurrentYearPowerFeaturesByFuelType = filter(MainPowerDataTable,Year==CurrentYearTemp)

  CurrentYearPowerFeaturesByFuelType %<>% ungroup() %>% group_by(CountryCode)

  if(i == 1) {
    CurrentYearPowerFeaturesByFuelType %<>% mutate(TotalThisYearDemand.MWy = sum(PowerOutput.MWy),
                                                   Capacity.MW=if_else(Capacity.MW>PowerOutput.MWy,Capacity.MW,PowerOutput.MWy/CapacityFactor)) #Capacity can not be less than generation
  }

  CurrentYearPowerFeaturesByFuelType  %<>%
    mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor,
           TotalEffectiveCapacity=sum(EffectiveCapacity.MW),
           FuelCostPerkWh=Price*GJperKWh/ThermalEfficiency,
           LCOE.Var.USD_kWh = Price*GJperKWh/ThermalEfficiency+VarOpex.USD_kWh,
           LCOE.TotalCalc.USD_kWh=LCOE.Var.USD_kWh+LCOE.Fixed.USD_kWh)

  if(i>1) {CurrentYearPowerFeaturesByFuelType %<>% mutate(PowerOutput.MWy=if_else(FuelType%in%c("coa","nga"),0,EffectiveCapacity.MW)) }

  CurrentYearPowerFeaturesByFuelType %<>% mutate(TotalNonCoalAndGasGeneration=sum(if_else(!FuelType%in%c("coa","nga"),PowerOutput.MWy,0)),
                                                 RequiredCoalAndGas=max(0,TotalThisYearDemand.MWy-TotalNonCoalAndGasGeneration))

  CoalAndGas = CurrentYearPowerFeaturesByFuelType %>%
    filter(FuelType %in% c("coa","nga")) %>%
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


  if(i>1) {CurrentYearPowerFeaturesByFuelType %<>% mutate(PowerOutput.MWy=if_else(FuelType%in%c("coa","nga"),
                                                                                  MinimumGen+RemainderAfterMinimas*ProportionOfCoalGasGeneration,EffectiveCapacity.MW) ) }
  CurrentYearPowerFeaturesByFuelType %<>% mutate(GenerationShare = PowerOutput.MWy/sum(PowerOutput.MWy))



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

###########
