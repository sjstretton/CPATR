
################################### SET UP AND INPUT DATA ###################################

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
ApplyPowerModelToSpecificYear=  function(CurrentYearTemp,CTMaxRate=0,
                                         MainPowerDataTable=MainPowerLargeDataTable) {
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
    filter(FuelType %in% c("coa","nga")) %>%
    mutate(MinimumGen = pmax(0,RequiredCoalAndGas-(sum(Capacity.MW*0.9)-Capacity.MW*0.9)),
           RemainderAfterMinimas=RequiredCoalAndGas-sum(MinimumGen),
           LogitExp=exp(-DispatchBeta*LCOE.Var.USD_kWh/min(LCOE.Var.USD_kWh)),
           ProportionOfCoalGasGeneration=LogitExp/sum(LogitExp)) %>%
    select(CountryCode, FuelType, RequiredCoalAndGas, MaximumGen, SumMaximumGen,
           OtherMaximumGen, MinimumGen, SumMaximumGen, OtherMaximumGen,
           RemainderAfterMinimas,ProportionOfCoalGasGeneration ,MinVarCost,
           RelVarCost,LogitExp,SumLogitExp,SumProportionOfCoalGasGeneration)

  CurrentYearPowerFeaturesByFuelType = rows_update(CurrentYearPowerFeaturesByFuelType, CoalAndGas, by = c("CountryCode", "FuelType"))


  if(i>1) {CurrentYearPowerFeaturesByFuelType %<>% mutate(Production.MWy=if_else(FuelType%in%c("coa","nga"),
                                                                                 MinimumGen+RemainderAfterMinimas*ProportionOfCoalGasGeneration,EffectiveCapacity.MW) ) }
  CurrentYearPowerFeaturesByFuelType %<>% mutate(GenerationShare = Production.MWy/sum(Production.MWy))



  CurrentYearPowerFeaturesByFuelType %<>%
    mutate(EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion,
           NamePlateEndOfYearRetirements=EffectiveEndOfYearRetirements/CapacityFactor,
           TotalRetirements=sum(EffectiveEndOfYearRetirements),
           TotalPredictedNextYearDemand.MWy=TotalThisYearDemand.MWy*(GDPGrowth[i]^IncomeElasticityofPowerDemand)*(PowerPrices[i]^PriceElasticityofPowerDemand),
           TotalNewEffectiveInvestmentsNeeded = max(0,TotalPredictedNextYearDemand.MWy - TotalThisYearDemand.MWy + TotalRetirements))

  CurrentYearPowerFeaturesByFuelType %<>%
    mutate(CostOfCheapestOption=min(LCOE.TotalCalc.USD_kWh),
           LogitNominator=exp(-InvestmentBeta*LCOE.TotalCalc.USD_kWh/CostOfCheapestOption),
           SumLogitNominator=sum(LogitNominator),
           NewEffectiveInvestments=TotalNewEffectiveInvestmentsNeeded*LogitNominator/SumLogitNominator,
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
