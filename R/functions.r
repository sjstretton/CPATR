Sys.setenv(TZ='GMT')

#' Build Base Table
#'
#' Create the base table.
#'
#' @param CountryList_ List of Countries
#' @param SubsectorList_ Parameter
#' @param FuelList_ Parameter
#' @param YearList_ Parameter
#' @param CTRange_ Parameter
#' @param BaseYear_ Parameter
#' @param SectorSubsectorLookup_ Parameter
#' @param PowerFuelTypes_ Parameter
#' @return tibble

#' @export
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


#' BuildMainSegmentedDataTable
#'
#' Create the MainSegmentedDataTable
#'
#' @param Base_ Parameter
#' @param Elasticities_ Parameter
#' @param Fuels_ Parameter
#' @param CPATEnergyBalancesBaseYear_ Parameter
#' @param GDPRelativeToBase_ Parameter
#' @param FuelPrices_ Parameter
#' @param CarbonTaxTrajectoryForm_ Parameter
#' @return tibble

#' @export
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


#' UpdateCoreData
#'
#' Update Core Data
#' @param MainSegmentedDataTableStyleInput Parameter
#' @param CTMaxRate Parameter
#' @export
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

#' RunCoreModel
#'
#' Run Core Model
#'
#' @param MainSegmentedDataTable_ Parameter
#' @param MainPowerLargeDataTable_ Parameter
#' @param BaseYear_ Parameter
#' @param AnalysisEndYear_ Parameter
#' @param CTRange_ CTRange
#' @param DoPower_ DoPower
#' @param NumberOfYears_ NumberOfYears
#' @param RetirementProportion_ RetirementProportion
#'
#' @export
RunCoreModel = function(MainSegmentedDataTable_,MainPowerLargeDataTable_,BaseYear_,AnalysisEndYear_,CTRange_=CTRange,
                        DoPower_=DoPower,NumberOfYears_=NumberOfYears,RetirementProportion_=RetirementProportion) {
  for (CTMaxRate in CTRange_)  {
    for(FilterYear in (BaseYear_:AnalysisEndYear_)) {
      FilteredTable = MainSegmentedDataTable_ %>% filter(Year==as.character(FilterYear) & CTScenarioRate==CTMaxRate)
      UpdatedFilteredTable = UpdateCoreData(MainSegmentedDataTableStyleInput=FilteredTable,CTMaxRate=CTMaxRate)
      MainSegmentedDataTable_[MainSegmentedDataTable_$Year==as.character(FilterYear)& MainSegmentedDataTable_$CTScenarioRate==CTMaxRate,] <- UpdatedFilteredTable
      if(DoPower_)
        MainPowerLargeDataTable_=ApplyPowerModelToSpecificYear(CurrentYearTemp=FilterYear,
                                                               CTMaxRate=CTMaxRate,MainPowerDataTable=MainPowerLargeDataTable_,
                                                               BaseYear__=BaseYear_,NumberOfYears__=NumberOfYears_,
                                                               RetirementProportion__=RetirementProportion_)
    }
  }
  return(list(MainSegmentedDataTable_,MainPowerLargeDataTable_))
}


#' ApplyPowerModelToSpecificYear
#'
#' ApplyPowerModelToSpecificYear
#' @param CurrentYearTemp Parameter
#' @param CTMaxRate Parameter
#' @param MainPowerDataTable MainPowerLargeDataTable_
#' @param BaseYear__ BaseYear_
#' @param NumberOfYears__ NumberOfYears_
#' @param RetirementProportion__ RetirementProportion_
#' @export
ApplyPowerModelToSpecificYear=  function(CurrentYearTemp,CTMaxRate=0,
                                         MainPowerDataTable=MainPowerLargeDataTable_,BaseYear__=BaseYear_,
                                         NumberOfYears__=NumberOfYears_,RetirementProportion__=RetirementProportion_
                                         ) {
  i = as.integer(CurrentYearTemp-BaseYear__+1)

  InvestmentBeta <- 1
  DispatchBeta <- 1
  GJperKWh <- 0.00360
  PJPerktoe <- 0.041868

  MaximumCoalAndGasCapacity <- 0.9

  GDPGrowth <- rep(1,NumberOfYears__)
  PowerPrices <- rep(1,NumberOfYears__)
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
    mutate(EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion__,
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
  if(i<NumberOfYears__) {
    MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("Capacity.MW")]=CurrentYearPowerFeaturesByFuelType$NextYearCapacity.MW
    MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("EffectiveCapacity.MW")]=CurrentYearPowerFeaturesByFuelType$NextYearEffectiveCapacity.MW
    MainPowerDataTable[MainPowerDataTable$Year==(CurrentYearTemp+1),c("TotalThisYearDemand.MWy")]=CurrentYearPowerFeaturesByFuelType$TotalPredictedNextYearDemand.MWy
  }
  MainPowerDataTable
}

#' OverallCoremodel
#'
#' Run OverallCoremodel
#'
#' @param CountryList gCountryList
#' @param DoPower gDoPower
#' @param BaseYear gBaseYear
#' @param NumberOfYears gNumberOfYears
#' @param AnalysisEndYear gAnalysisEndYear
#' @param EndYear gEndYear
#' @param RetirementProportion gRetirementProportion
#' @param CarbonTaxTrajectoryForm gCarbonTaxTrajectoryForm
#' @param CTRange gCTRange
#' @export
OverallCoremodel=function(CountryList=gCountryList,DoPower=gDoPower,
                   BaseYear=gBaseYear,NumberOfYears=gNumberOfYears,
                   AnalysisEndYear=gAnalysisEndYear, EndYear=gEndYear,
                   RetirementProportion=gRetirementProportion,
                   CarbonTaxTrajectoryForm=gCarbonTaxTrajectoryForm,
                   CTRange=gCTRange
                   ) {
  ################################### SET UP AND INPUT DATA ###################################
  #Hard Coded Variables
  PJPerktoe <- 0.041868
  FuelList <- c("coa", "nga", "oop", "gso", "die", "lpg", "ker", "jfu", "bio", "ecy")
  PowerFuelTypes <- c("coa", "nga", "oop", "nuc", "wnd", "sol", "ore", "hyd", "bio")

  CountryCategoryList <- as_factor(c("VeryLarge","Large","MediumLarge","Medium","MediumSmall","Small","VerySmall"))

  #Read in Lookup Tables
  Fuels <- read_csv("R/1-metadata/EmissionsFactors.csv")
  Flow <- read_csv("R/1-metadata/SectorFullLookup.csv")
  Elasticities <- read_csv("R/1-metadata/Elasticities.csv") %>% filter(FuelType %in% FuelList)
  SectorSubsectorLookup <- read_csv("R/1-metadata/SectorsAndSubsectorsNarrowLookup.csv")
  CountriesTable <- read_csv("R/1-metadata/CountryLookup.csv")
  CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)

  #Define Included Elements of Each Dimension
  YearList <- as.character(BaseYear:(EndYear+1))
  SectorList <- unique(Flow$Sector); SectorList=SectorList[!is.na(SectorList)]
  SubsectorList <- unique(Flow$SubsectorCode); SubsectorList=SubsectorList[!is.na(SubsectorList)]
  ScenariosList <- c("Baseline1","CTaxScen2")


  #Add Energy Balances
  CPATEnergyBalances2018 <- read_csv("R/2-preprocess/output/ProcessedEnergyBalances2018.csv")
  GDPRelativeToBase <- read_csv("R/2-preprocess/output/GDPRelativeToBase.csv",col_types="ccd")

  #Add Power Data
  SelectedPowerData=read_csv("R/2-preprocess/output/SelectedPowerData.csv")

  #Add Fuel Prices
  FuelPricesGlobal <- read_csv("R/2-preprocess/rawdata/GlobalPricesGJ.csv") %>%
    pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="Price")
  FuelPricesGlobalForPower  <- FuelPricesGlobal %>%  mutate(Year=as.integer(Year))
  FuelPrices <- FuelPricesGlobal

  #Add Base Price
  BaseYearPriceLookup <- FuelPrices %>% filter(Year==as.character(BaseYear)) %>% rename(BaseYearPrice=Price) %>% select(-Year)
  FuelPrices <- FuelPrices %>% rename(BaselinePrice=Price) %>% inner_join(BaseYearPriceLookup)


  PowerCountryList=unique(SelectedPowerData$CountryCode)

  YearsTable=tibble(Year=BaseYear:EndYear,fakecol=1L) #Fake for doing cartesian product join

  SelectedPowerDataByGenType = SelectedPowerData %>% mutate(EffectiveCapacity.MW=Capacity.MW*CapacityFactor, ##NOTE THIS MIGHT NOT BE QUITE RIGHT
                                                            EffectiveEndOfYearRetirements=EffectiveCapacity.MW*RetirementProportion) %>%
    ungroup() %>% group_by(CountryCode)

  SelectedPowerDataByGenType %<>%
    mutate(MinCostNumber=0,MinCostFuelCostr=0, CostOfCheapestOption=0, RelativeCost=0, LogitNominator=0,
           ProportionOfInvestment=0,CheckSum=0, TotalThisYearDemand.MWy=0,GenerationShare=0) %>%
    select(c("CountryCode","FuelType","Capacity.MW", "PowerOutput.MWy","EffectiveCapacity.MW",
             "CapacityFactor","ThermalEfficiency","LCOE.TotalCalc.USD_kWh","LCOE.USD_kWh",
             "VarOpex.USD_kWh","LCOE.Var.USD_kWh","LCOE.Fixed.USD_kWh","TotalThisYearDemand.MWy"))

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

  MainPowerLargeDataTable <-  YearsTable %>%
    inner_join(SelectedPowerDataByGenType) %>%
    left_join(FuelPricesGlobalForPower) %>%
    select(-fakecol) %>%
    select(CountryCode,FuelType,Year,everything())

  BaseTable <- BuildBaseTable(CountryList_=CountryList,SubsectorList_=SubsectorList, FuelList_=FuelList,YearList_=YearList,CTRange_=CTRange,
                              BaseYear_=BaseYear,SectorSubsectorLookup_=SectorSubsectorLookup,PowerFuelTypes_=PowerFuelTypes)

  MainSegmentedDataTable <- BaseTable %>% BuildMainSegmentedDataTable(Elasticities_=Elasticities,Fuels_=Fuels,CPATEnergyBalancesBaseYear_=CPATEnergyBalances2018,
                                                                      GDPRelativeToBase_=GDPRelativeToBase,FuelPrices_=FuelPrices,CarbonTaxTrajectoryForm_=CarbonTaxTrajectoryForm)

  ResultsList <- RunCoreModel(MainSegmentedDataTable_=MainSegmentedDataTable, MainPowerLargeDataTable_=MainPowerLargeDataTable,
                              BaseYear_=BaseYear,AnalysisEndYear_=AnalysisEndYear,CTRange_=CTRange,DoPower_=DoPower,NumberOfYears_=NumberOfYears,
                              RetirementProportion_=RetirementProportion)

  return(ResultsList)
}


###########



#' ConvertTibbleWithIDCols
#'
#' ConvertTibbleWithIDCols
#'
#' @export
#' @param tb Parameter
#' @param idcols Parameter
#' @param conversion Parameter
ConvertTibbleWithIDCols = function(tb, idcols=1L, conversion=1) {
  ColsToConvert=setdiff((1:ncol(tb)),idcols)
  tb[,ColsToConvert]=tb[,ColsToConvert]*conversion
  return(tb)
}




#' FindFilterdValueCPAT
#'
#' FindFilterdValueCPAT
#'
#' @export
FindFilterdValueCPAT = function () {
  FilteredInfo1  = MST1.new %>% filter(QuantityCodeMain=="ener" & SubsectorCode=="res" & FuelCode=="coa"& SubscenarioNumber==1L)
  ExpectedValue1 = FilteredInfo1 %>% select(`2018`) %>% deframe
  ExpectedValue1 }


#' FindFilterdValueR
#'
#' FindFilterdValueR
#'
#' @export
FindFilterdValueR   = function () {
  FilteredInfo2 = MainSegmentedDataTable%>% filter(CountryCode=="CHN" & SubsectorCode=="res" & FuelType=="coa"& CTScenarioRate==0)
  ExpectedValue2 = FilteredInfo2%>% filter(Year =="2018") %>% select(EnergyConsumption)/PJPerktoe
  ExpectedValue2 %<>% pull("EnergyConsumption")
  ExpectedValue2
}


