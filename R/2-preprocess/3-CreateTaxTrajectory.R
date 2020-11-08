rm(list=ls())
library(tidyverse)
library(readxl)
library(magrittr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

################################### TRAJECTORY TO BE SCALED ###################################


StartYear <- 2019
MaxYear <- 2020
PreviousRate <- 0
StartRate <- 0
MaxRate <- 1
Increment=(MaxRate-StartRate)/(MaxYear-StartYear)
CarbonTaxTrajectoryForm=rep(0,17)#,nrow=1,ncol=17)
names(CarbonTaxTrajectoryForm) <- 2017:2033
CarbonTaxTrajectoryForm <- CarbonTaxTrajectoryForm+ifelse(as.integer(names(CarbonTaxTrajectoryForm))<StartYear,PreviousRate,0)
CarbonTaxTrajectoryForm <- CarbonTaxTrajectoryForm+ifelse((as.integer(names(CarbonTaxTrajectoryForm))>=StartYear)&
                                                            (as.integer(names(CarbonTaxTrajectoryForm))<=MaxYear),
                                                          (as.integer(names(CarbonTaxTrajectoryForm))-StartYear)*Increment+StartRate,0)
CarbonTaxTrajectoryForm <- CarbonTaxTrajectoryForm+ifelse(as.integer(names(CarbonTaxTrajectoryForm))>MaxYear,MaxRate,0)
CarbonTaxTrajectoryDates <- as.integer(names(CarbonTaxTrajectoryForm))
CarbonTaxTrajectoryForm <- tibble(Year=CarbonTaxTrajectoryDates,ProportionOfMaxCTaxRate=CarbonTaxTrajectoryForm)
ggplot(CarbonTaxTrajectoryForm, aes(x=Year,y=ProportionOfMaxCTaxRate) ) +   geom_line()
CarbonTaxTrajectoryForm$Year <- as.character(CarbonTaxTrajectoryForm$Year)
rm(CarbonTaxTrajectoryDates,StartYear,MaxYear,PreviousRate,StartRate,Increment)

############################# END BUILD CARBON TAX TRAJECTORY ################################
write_csv(CarbonTaxTrajectoryForm,file="output/CarbonTaxTrajectoryForm.csv")

