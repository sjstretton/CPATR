rm(list=ls())
load(file="4-output/MainDataFile.rda")
CountryCategoryList <- as_factor(c("VeryLarge","Large","MediumLarge","Medium","MediumSmall","Small","VerySmall"))
CountriesTable <- read_csv("1-metadata/CountryLookup.csv")
PJPerktoe <- 0.041868
BaseYear <- 2018L
AnalysisEndYear <- 2030L
BaselineEmissionsTable <-
  MainSegmentedDataTable %>%
  filter(CTScenarioRate==0) %>%
  rename(BaselineEmissions=Emissions) %>%
  select(CountryCode, Sector, SubsectorCode, FuelType, Model,Year,BaselineEmissions)

PowerResults <- SelectedPowerDataByGenType

MainSegmentedDataTable %<>% left_join(BaselineEmissionsTable) %>% mutate(EmissionsReduction=BaselineEmissions-Emissions)

CountryCategoryTable <- BaselineEmissionsTable %>%
  filter(Year==AnalysisEndYear) %>%
  group_by(Model,CountryCode) %>%
  summarise(BaselineEmissionsBaseYear=sum(BaselineEmissions,na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(desc(BaselineEmissionsBaseYear)) %>%  #Fix this with Cut
  mutate(CountryCategory=factor(case_when(BaselineEmissionsBaseYear>1000 ~ "VeryLarge",
                                          BaselineEmissionsBaseYear>500  ~ "Large",
                                          BaselineEmissionsBaseYear>350  ~ "MediumLarge",
                                          BaselineEmissionsBaseYear>200  ~ "Medium",
                                          BaselineEmissionsBaseYear>125  ~ "MediumSmall",
                                          BaselineEmissionsBaseYear>50   ~ "Small",
                                          TRUE                           ~ "VerySmall"),
                                          levels=CountryCategoryList)) %>%
  inner_join(CountriesTable) %>%
  select(CountryCode,CountryName,CountryCategory,BaselineEmissionsBaseYear)

MainSegmentedDataTable <- left_join(select(CountryCategoryTable,CountryCode,CountryName,CountryCategory), MainSegmentedDataTable)

MainSegmentedDataTable %<>% mutate(EnergyConsumption.ktoe= EnergyConsumption/PJPerktoe)



CO2ByCountry <- read_csv("2-preprocess/rawdata/CO2ByCountry.csv",col_types = "cccddd") %>% filter(Year==BaseYear) %>% select(-Year) %>%
  rename(CO2owid2017= CO2Emissions)


SummaryResults.Baseline.ByCountry <- MainSegmentedDataTable %>%
  filter(CTScenarioRate==0)  %>%
  group_by(Scenario,CountryCode,Year) %>%
  summarise(BaselineEmissions=sum(BaselineEmissions,na.rm=TRUE),Emissions=sum(Emissions,na.rm=TRUE),EmissionsReduction=sum(EmissionsReduction,na.rm=TRUE)) %>%
  inner_join(CountryCategoryTable) %>%
  select(Scenario,  Year, CountryCategory, CountryCode, CountryName, BaselineEmissions, Emissions, EmissionsReduction) %>%
  arrange(CountryCategory, CountryCode,Scenario, Year )

CO2Compared = SummaryResults.Baseline.ByCountry %>%
  filter(Year%in%c(as.character(BaseYear),"2030")) %>%
  pivot_wider(id_cols=c("CountryCode","CountryName"),names_from=Year,values_from=Emissions) %>%
  inner_join(CO2ByCountry) %>%
  select("CountryCode","CountryName","CO2owid2017", as.character(BaseYear), as.character(2030))

SummaryResults.ByCountry.2030 <- MainSegmentedDataTable %>%
  filter(Year=="2030")  %>%
  group_by(Scenario,CTScenarioRate,CountryCode) %>%
  summarise(BaselineEmissions=sum(BaselineEmissions,na.rm=TRUE),Emissions=sum(Emissions,na.rm=TRUE),EmissionsReduction=sum(EmissionsReduction,na.rm=TRUE)) %>%
  inner_join(CountryCategoryTable) %>%
  select(Scenario,  CTScenarioRate, CountryCategory, CountryCode, CountryName, BaselineEmissions, Emissions, EmissionsReduction) %>%
  arrange(CountryCategory, CountryCode,Scenario, CTScenarioRate )

SummaryResults.BySector.2030 <- MainSegmentedDataTable %>%
  filter(Year%in%c("2030"))  %>%
  inner_join(CountryCategoryTable) %>%
  group_by(Scenario,CTScenarioRate,CountryCategory,CountryCode,CountryName,Sector) %>%
  summarise(BaselineEmissions=sum(BaselineEmissions,na.rm=TRUE), Emissions=sum(Emissions,na.rm=TRUE),EmissionsReduction=sum(EmissionsReduction,na.rm=TRUE)) %>%
  arrange(CountryCategory, CountryCode, Scenario, CTScenarioRate, Sector  )

SelectedCountry2025and2030 <- MainSegmentedDataTable %>%
  filter(Year%in%c("2025","2030")& CountryCode=="CHN")  %>%
  group_by(Scenario,CTScenarioRate,CountryCode,Year) %>%
  summarise(BaselineEmissions=sum(BaselineEmissions,na.rm=TRUE),Emissions=sum(Emissions,na.rm=TRUE),EmissionsReduction=sum(EmissionsReduction,na.rm=TRUE)) %>%
  select(Scenario,  Year, CTScenarioRate, CountryCode, BaselineEmissions, Emissions, EmissionsReduction) %>%
  arrange(CountryCode,Scenario, CTScenarioRate )



