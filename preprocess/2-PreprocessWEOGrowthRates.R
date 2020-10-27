rm(list=ls())
library(tidyverse)
library(readxl)
library(magrittr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

WEOGrowthRates <- read_csv("rawdata/WEOGrowthRates.csv")
WEOGrowthRates[,"2017"] <- 0
WEOGrowthRatesCountryCode <- WEOGrowthRates$CountryCode
WEOGrowthRates <- as.matrix(WEOGrowthRates[,2:18],dimnames=list(WEOGrowthRatesCountryCode,colnames(WEOGrowthRates)[2:18]))
rownames(WEOGrowthRates) <- WEOGrowthRatesCountryCode
GDPRelativeToBase <- t(apply(1+WEOGrowthRates,FUN = cumprod, MARGIN = 1))
GDPRelativeToBase <- tibble(CountryCode=WEOGrowthRatesCountryCode,as.data.frame(GDPRelativeToBase)) %>% pivot_longer(2:18,names_to= "Year",values_to = "GDPFactor")
rm(WEOGrowthRates, WEOGrowthRatesCountryCode)
write_csv(GDPRelativeToBase,"output/GDPRelativeToBase.csv")
