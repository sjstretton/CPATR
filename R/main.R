## Purpose: This script contains the 'main function'.

#NExt steps:
#Finish the encapsulation
#Basic documentation

rm(list=ls())
#Uncomment the line below for this to work in R studio
Sys.setenv(TZ='GMT')


################################### SETUP AND INPUT DATA ###################################
library(tidyverse)
library(magrittr)
library(cowplot)
library(ggrepel)
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")
# gCountryList <-  "CHN" #TCAFCountryList ##c("CHN") #Other options: ...,"IND","USA","JPN" #(CountryNameLookup$CountryCode)[1:10] #
# gDoPower <- TRUE
# gAnalysisEndYear <- 2030L
# gBaseYear <- 2018L
# gNumberOfYears <- 15L
# gEndYear <- gBaseYear+gNumberOfYears-1
# gRetirementProportion <- 0.04
# gCarbonTaxTrajectoryForm <- read_csv("R/2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd")  #Shape of Carbon Tax Trajectory is Currently Standardised
# gCTRange=seq(from=0, to=100, by=20)



source("R/functions.R")

#gResultsList=OverallCoremodel(CountryList=gCountryList,DoPower=TRUE,
#                      BaseYear=2018L,NumberOfYears=15L,
#                      AnalysisEndYear=2030L, EndYear=2033,
#                      RetirementProportion=0.04,
#                      CarbonTaxTrajectoryForm=read_csv("R/2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd"),
#                      CTRange=seq(from=0, to=100, by=20))

#MainSegmentedDataTable <- gResultsList[[1]] %>% filter(Year %in% as.character(gBaseYear:gAnalysisEndYear))   #Note that MainSegmentedDataTable uses character years
#SelectedPowerDataByGenType <- gResultsList[[2]] %>% filter(Year %in% as.integer(gBaseYear:gAnalysisEndYear)) #Note that SelectedPowerDataByGenType uses integer years

#save(MainSegmentedDataTable,gCarbonTaxTrajectoryForm,gCTRange,SelectedPowerDataByGenType,file="data/MainDataFile.rda")

#write_csv(MainSegmentedDataTable,file="4-output/MainDataFile.csv")
#write_csv(SelectedPowerDataByGenType,file="4-output/SelectedPowerDataByGenType.csv")


#Get Coal Cement from CPAT Results.
MST1.new <- read_csv("R/5-comparison/2ResultsFromCPAT/MST1-new.csv",col_types = "iiccccccidddddddddddddd",na = c("", "NA","#DIV/0!"))



