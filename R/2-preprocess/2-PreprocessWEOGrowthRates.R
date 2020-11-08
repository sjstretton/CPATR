rm(list=ls())
library(tidyverse)
library(readxl)
library(magrittr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
GrowthDataStartDate=2017
BaseYear=2018
WEOGrowthRates <- read_csv("rawdata/WEOGrowthRates.csv")
WEOGrowthRates[,as.character(GrowthDataStartDate:BaseYear)] <- 0
WEOGrowthRatesCountryCode <- WEOGrowthRates$CountryCode
WEOGrowthRates <- as.matrix(WEOGrowthRates[,2:18],dimnames=list(WEOGrowthRatesCountryCode,colnames(WEOGrowthRates)[2:18]))
rownames(WEOGrowthRates) <- WEOGrowthRatesCountryCode
GDPRelativeToBase <- t(apply(1+WEOGrowthRates,FUN = cumprod, MARGIN = 1))
GDPRelativeToBase <- tibble(CountryCode=WEOGrowthRatesCountryCode,as.data.frame(GDPRelativeToBase)) %>% pivot_longer(2:18,names_to= "Year",values_to = "GDPFactor") %>% filter(as.integer(Year) >= BaseYear)
rm(WEOGrowthRates, WEOGrowthRatesCountryCode)
write_csv(GDPRelativeToBase,"output/GDPRelativeToBase.csv")
