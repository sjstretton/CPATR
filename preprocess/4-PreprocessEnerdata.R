  rm(list=ls())
  #Set Working Directory
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))
library(tidyverse)
library(magrittr)

ISO2toISO3CountryCodes <- read_csv("../metadata/CountryLookup.csv") %>% select(CountryName,CountryCode, TwoLetterCountryCode)

ItemCodeLookup=read_csv("enerdata/ItemCodeLookup.csv")

EnerdataElectricalCapacityAndGeneration <- read_csv("enerdata/EnerdataElectricalCapacityAndGeneration.csv",
                                                    col_types="cccidcccc",na=c("n.a.")) %>%
  rename( ItemCode=`Item code`, TwoLetterCountryCode=`ISO code`) %>%
  inner_join(select(ISO2toISO3CountryCodes,-CountryName)) %>%
  left_join(ItemCodeLookup)  %>% filter(FuelType != "nsp")%>%
  select(CountryCode, TwoLetterCountryCode, FuelType,Year, Quantity,Value) %>%
  group_by(CountryCode, TwoLetterCountryCode, FuelType,  Year, Quantity  ) %>%
  summarise(Value=sum(Value,na.rm=TRUE)) %>%
  pivot_wider(names_from=Quantity,values_from=Value) %>%
  mutate(ProductionMWy=ProductionGWh*1000/(365*24)) %>%
  mutate(OrigProdGreaterThanOrigCap=(ProductionMWy>CapacityMW)) %>%
  mutate(CapacityMW= if_else(OrigProdGreaterThanOrigCap,ProductionMWy,CapacityMW)) %>%
  mutate(CapacityFactor=if_else(abs(CapacityMW)<0.0001 | is.na(CapacityMW)| is.na(ProductionMWy),0, ProductionMWy/CapacityMW)) %>%
  filter(Year==2018)

write_csv(EnerdataElectricalCapacityAndGeneration,"powerintermediate/ProcessedElectricalCapacityAndGeneration.csv")
#GlobalSummary = EnerdataElectricalCapacityAndGeneration %>% ungroup() %>% group_by(FuelType,Year) %>% summarise_if(is.numeric,sum) %>% mutate(CapacityFactor=ProductionMWy/CapacityMW)
#write_csv(GlobalSummary,"GlobalSummary.csv")

GenerationGreaterThanCapacity=EnerdataElectricalCapacityAndGeneration%>% filter(OrigProdGreaterThanOrigCap)
