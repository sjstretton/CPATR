#Intermodel Comparison
#rm(list=ls())
library(tidyverse)
library(readxl)

CountriesTable <- read_csv("R/1-metadata/CountryLookup.csv")

CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)
#CPATResults <- read_csv("comparisondata/CPATResults.csv") %>%  arrange(CountryCode,Scenario)

#IEAScenarios <- read_csv("R/5-comparison/3IEAData/IEAScenarios.csv",col_types = "icccccccdddddddd",na=c("NA","n.a.")) %>% pivot_longer(cols=9:16)

IPCCData <- read_csv("R/5-comparison/5OtherData/SSP_IAM_V2_201811.csv")
#View(IPCCData)

IPCCData = IPCCData %>%
  filter(REGION=="World") %>%
  separate(VARIABLE, into = c("VarA","VarB","VarC","VarD"),
           sep = "[|]", remove=FALSE)

CarbonPrice = IPCCData %>%
  select(-c("VarA","VarB","VarC","VarD")) %>%
  filter(VARIABLE %in% c("Price|Carbon","Emissions|CO2","Primary Energy") ) %>%
  pivot_longer(cols=6:16, names_to = "YEAR",names_ptypes=list(integer()) ) %>%
  select(-UNIT) %>%
  filter(YEAR==2030) %>%
  pivot_wider(names_from = VARIABLE) %>%
  separate(SCENARIO, into = c("SSP","RCP")) %>%
  rename(Emissions=`Emissions|CO2`, CPrice=`Price|Carbon`, PrimaryEnergy=`Primary Energy`) %>%
  mutate(Emissions  = replace_na(Emissions,0), CPrice  = replace_na(CPrice , 0))

InterModelComparison <- ggplot(data = CarbonPrice, aes(x=CPrice,y=Emissions,color=SSP)) +
  geom_point() +
  xlim(0,100) +
  ylim(0,60000) +
  geom_line() +
  facet_wrap(vars(MODEL))

####################
RFFCarbonTaxScenarios <- read_csv("R/5-comparison/5OtherData/rff-cpc.csv",col_names = c("Year","0","25","50","70"),skip=1) %>%  pivot_longer(cols=2:5,names_to="CarbonTax",values_to="Emissions") %>% mutate(Country="United States") %>% select(Country, CarbonTax, Year, Emissions)

RFFCarbonTaxScenarios.metadata = tibble(Quantity=c("0","25","50","70"),
                                        CTax=c(0,25,50,70),
                                        EscalationRate=c(0,0,0,0),
                                        RevUsage = c("HHDiv","HHDiv","HHDiv","HHDiv"))



MACC_ByDataType_Tabular <- read_csv("R/5-comparison/1ReadInMACC/1RawData/MACC-ByDataType-Tabular.csv")

#Not MACC
ISO2toISO3 <- read_csv("R/5-comparison/4EnerdataOther/ISO2toISO3.csv")

#ElectricalCapacityAndGeneration <- read_csv("powerdata/ElectricalCapacityAndGeneration.csv") %>% inner_join(ISO2toISO3)

Energy_Flows <- read_csv("R/5-comparison/4EnerdataOther/Enerdata-Energy-Flows.csv") %>% inner_join(ISO2toISO3)

Energy_Related_CO2_Emissions <- read_csv("R/5-comparison/4EnerdataOther/Enerdata-Energy-Related-CO2-Emissions.csv") %>% inner_join(ISO2toISO3)

