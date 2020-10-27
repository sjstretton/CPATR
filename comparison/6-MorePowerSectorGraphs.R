
library(tidyverse)
library(readxl)
library(magrittr)
conv_ktoe_to_gwh = 11.63
CorePowerTypes=c("coa","oil","nga","nuc","hyd","bio","wnd","sol","ore")
CountriesTable <- read_excel("preprocesseddata/Categories.xlsx", sheet = "Countries")  #%>% filter(Type=="Country" & IncomeGroupSymbol!="HIC")
CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)

FuelTypeLookup=tribble(
  ~Fuel, ~FuelType,
  "coa", "Coal",
  "oil", "Oil",
  "nga", "Natural gas",
  "nuc", "Nuclear",
  "ren", "Renewables",
  "hyd", "Hydro",
  "bio", "Bioenergy",
  "wnd", "Wind",
  "geo", "Geothermal",
  "sol", "Solar PV",
  "csp", "CSP",
  "mar", "Marine")

FuelGroupings=tribble(
  ~Fuel, ~FuelGroup,
  "coa", "coa",
  "oil", "oil",
  "nga", "nga",
  "nuc", "nuc",
  "ren", "ren",
  "hyd", "hyd",
  "bio", "bio",
  "wnd", "wnd",
  "geo", "ore",
  "sol", "sol",
  "csp", "sol",
  "mar", "ore",
  "ore", "ore")



SelectedYears=c(2017L,2025L,2030L)
#PowerResults %<>% filter(Year %in% SelectedYears)
#ResidentialResults %<>% filter(Year %in% SelectedYears)
#ChinaPowerResults %<>% filter(Year %in% SelectedYears)
IEAcols=cols(
  Region = col_character(),
  Scenario = col_character(),
  Quantity = col_character(),
  FuelType = col_character(),
  Unit = col_character(),
  `2017` = col_double(),
  `2018` = col_double(),
  `2025` = col_double(),
  `2030` = col_double(),
  `2035` = col_double(),
  `2040` = col_double()
)

IEAScenarios <- read_csv("comparison/3IEAData/IEAScenarios.csv", col_types=IEAcols) %>%
  pivot_longer(cols = 6:11, names_to = "Year",values_to = "Value",values_drop_na=TRUE) %>%
  mutate (Year = as.integer(Year)) %>%
  inner_join(FuelTypeLookup) %>%
  filter(Fuel!="ren") %>% rename(Country=Region)

CPShistory = filter(IEAScenarios, Scenario == "History") %>% mutate(Scenario = "CPS")
SPShistory = filter(IEAScenarios, Scenario == "History") %>% mutate(Scenario = "SPS")
SDShistory = filter(IEAScenarios, Scenario == "History") %>% mutate(Scenario = "SDS")
IEAScenarios = rbind(filter(IEAScenarios, Scenario!="History"),CPShistory,SPShistory,SDShistory)

BigCountries <- c("United States",  "Brazil", "South Africa", "Russia","China", "India", "Japan")
SelectedCountries <- c("United States", "Brazil", "China", "India", "Japan")

SetOfRegions <- c("North America", "Central and South America",  "Europe", "Africa", "Middle East", "Eurasia", "Asia Pacific", "Southeast Asia")
OtherRegions <- c("World", "OECD", "NonOECD", "Developing", "Advanced")

BigCountries.Generation = IEAScenarios %>%
  filter(Year %in% SelectedYears & Country %in% SelectedCountries & Quantity=="Electricity generation" & FuelType!= "Total generation") %>%
  mutate(Power.GWh = Value*1000) %>% mutate(Model = "IEA") %>%
  select(Scenario, Model, Country,Fuel, Year, Power.GWh)




LongFormColSpec=cols(
  Country = col_character(),
  Quantity = col_character(),
  Scenario = col_character(),
  CarbonTax = col_double(),
  Model = col_character(),
  Fuel = col_character(),
  `2017` = col_double(),
  `2018` = col_double(),
  `2019` = col_double(),
  `2020` = col_double(),
  `2021` = col_double(),
  `2022` = col_double(),
  `2023` = col_double(),
  `2024` = col_double(),
  `2025` = col_double(),
  `2026` = col_double(),
  `2027` = col_double(),
  `2028` = col_double(),
  `2029` = col_double(),
  `2030` = col_double(),
  `2031` = col_double(),
  `2032` = col_double(),
  `2033` = col_double()
)

ScenarioDefinition <- read_csv("comparison/2ResultsFromCPAT/ScenarioDefinition.csv") %>% rename(Superscenario=`Superscenario Number`)

LatestCPATResults <- read_csv("comparison/2ResultsFromCPAT/MST1.csv", na = c("", "NA","#DIV/0!","#N/A")) %>%
  separate(`CPAT Code With Country Code`,into=c("CountryCode",NA,NA,NA,NA,NA),sep="[.]") %>%
  mutate(CountryCode=toupper(CountryCode)) %>%
  left_join(CountryNameLookup) %>%  left_join(ScenarioDefinition,by="Superscenario") %>%
  rename(CarbonTax=`Max Rate`) %>%
  select(c("Country","CountryCode","QuantityCodeMain","SubsectorCode","FuelCode","CarbonTax",as.character(2018:2030)))



LatestCPATResultsLong = LatestCPATResults %>% filter(QuantityCodeMain%in%c("elec.ela","elec.eng")) %>%
  separate(QuantityCodeMain,into=c("Quantity","Model")) %>%
  pivot_longer(cols=as.character(2018:2030),names_to="Year",values_to="Power.ktoe") %>%
  mutate(Scenario=if_else(CarbonTax==0,"Baseline","Carbon tax")) %>% rename(Fuel=FuelCode) %>%
mutate(Quantity=if_else(Quantity=="elec","Power Supplied ktoe",Quantity)) %>%
mutate(Model=case_when(
  Model=="ela" ~ "Old",
  Model=="eng" ~ "New",
  TRUE ~ "Model")  ) %>% filter(!Fuel%in%c("tot","Total"))  %>%
select("Country","Quantity","Scenario","CarbonTax","Model","Fuel","Year","Power.ktoe")


Results <- read_csv("comparison/2ResultsFromCPAT/LongFormResults.csv", col_types = LongFormColSpec,na = c("", "NA","#DIV/0!","#N/A"))

LongFormResults = Results %>% filter(!Fuel%in%c("tot","Total")) %>%
  filter(Quantity=="Power Supplied ktoe") %>%
  pivot_longer(7:23,names_to="Year",values_to="Power.ktoe")

###Add this or not #LongFormResults = LatestCPATResultsLong

LongFormResults$Year = as.integer(LongFormResults$Year)
LongFormResults$CarbonTax = as.integer(LongFormResults$CarbonTax)

LongFormResults$ScenarioModel=paste0(LongFormResults$Scenario,"-",LongFormResults$Model,"-",LongFormResults$CarbonTax)
LongFormResults = LongFormResults %>%
  filter(Year %in% c(2017,2025,2030)) %>%
  filter(CarbonTax %in% c(0,25,50)) %>%
  filter(Country %in% SelectedCountries) %>%
  mutate(Power.GWh=conv_ktoe_to_gwh*Power.ktoe)%>%
  filter(Scenario == "Carbon tax") %>%
  select(Scenario, Model, Country, CarbonTax, Fuel,   Year, Power.GWh)

IEACarbonTaxScenarios <- read_csv("comparison/3IEAData/IEACarbonTaxScenarios.csv",col_types=cols(  Scenario = col_character(),  Year = col_integer(),   CarbonTax = col_double()))

BigCountries.Generation %<>% inner_join(IEACarbonTaxScenarios) %>% select(Scenario, Model, Country, CarbonTax, Fuel,   Year, Power.GWh)

CombinedPowerData = rbind(LongFormResults,BigCountries.Generation) %>% filter (Fuel %in% CorePowerTypes)

CorePowerTypes=c("coa","oil","nga","nuc","hyd","bio","wnd","sol","ore")

CombinedPowerDataGraph <- function(CountrySelected) {
  CombinedPowerData.SelectedCountry=CombinedPowerData %>% filter(Country==CountrySelected)
  PowerPlotGenerated=ggplot(CombinedPowerData.SelectedCountry, aes(x = Year, y = Power.GWh, fill = Fuel)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(Model~CarbonTax)+ ggtitle(paste0("Power Sector Models: ",CountrySelected))
  cowplot::save_plot(plot=PowerPlotGenerated,filename=paste0("output/",CountrySelected,"Plot.png"),base_height=7)
  print(PowerPlotGenerated)
}

purrr::map(SelectedCountries, CombinedPowerDataGraph)

#source("2-CountrySpecificAnalysis.R")

Generation.China = BigCountries.Generation %>% filter(Country=="China")

AllPower=rbind(PowerResults,BigCountries.Generation)
AllPower$Year = as.integer(AllPower$Year)
CarbonTaxScenarios <- read_csv("comparison/3IEAData/IEAandCPATCarbonTaxScenarios.csv",col_types=cols(  Scenario = col_character(),  Year = col_integer(),   CarbonTax = col_double()))
AllPower = CarbonTaxScenarios %>%
  inner_join(AllPower) %>%
  filter(Fuel!="Total") %>% inner_join( FuelGroupings) %>%
  group_by (Scenario, Year, CarbonTax, Model, Country, FuelGroup) %>%
  summarise(Power.GWh=sum(Power.GWh)) %>%
  rename(Fuel=FuelGroup) %>%
  mutate(ScenarioModel=paste0(Model,"-", Scenario)) %>%
  select(Country, Year, ScenarioModel, Fuel, Power.GWh)

AllPower.China=filter(AllPower, Country=="China")

ggplot(AllPower.China, aes(x = Year, y = Power.GWh, fill = Fuel)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~ScenarioModel)

ggplot(AllPower.China, aes(x = Year, y = Power.GWh, fill = ScenarioModel)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Fuel)

