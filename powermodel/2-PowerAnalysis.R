SelectedPowerDataByGenTypeAll=MainPowerLargeDataTable

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
