#rm(list=ls()) #Clears the working memory
setwd("C:/Users/wb547395/OneDrive - WBG/Documents/CPAT-R")
library(tidyverse)
library(readxl)
library(magrittr)
library(cowplot)
library(ggrepel)

#7-Comparison Graphs
PJPerktoe <- 0.041868

library(tidyverse)
CountriesSelected=c("China","India","United States","Russia","South Africa","Brazil")

RFF <- read_csv("5-comparison/7RFF/RFF.csv")

#tidyverse: functions that work together e.g. with the pipe and tidy data
#Tibble, a form of data frame (which are R  tables).
#Note that R has a separate matrix/array class which are a single, but multidimensional, variable
#Difference is that data frames can have columns of different types
#tidy data: id variables and then 'quantity variables', otherwise 'long' (eg years are rows not columns)

#pipe(%>%): x %>% PipeCompatibleFunction(y,z) is the same as  PipeCompatibleFunction(x,y,z)
#filter: Subset the rows
#mutate: New variable
#rename: NewVarName= OldVarName#
#separate: Separates a combo-text-field into component parts e.g. Orig="BP 2018" becomes Final1="BP" and Final2="2018"
# & 'and' logical operation on vectors
#ggplot: graphs with a 'grammar'
#facet_wrap(~Country) -- create 'facets' on Country.
#also see facet_grid that can do two dimensions: x~y -- facet_grid(Country~Provider)

RFF.selected <- RFF %>%
  filter((region %in% CountriesSelected) & (sector=="All") & (energy=="All"))  %>%
  filter(elg>0) %>%
  mutate(CarbonTax=0) %>%
  rename(PowerCons.TWh=elg,Country=region,Year=year,Scenario=outlook) %>%
  mutate(Year=as.integer(Year)) %>%
  select(Scenario,Country,CarbonTax,Year,PowerCons.TWh)

RFF.selected <- RFF.selected %>%    # One can also use %<>% for the pattern  x <- x %>%
  separate(col=Scenario , sep=c("[()]"),into=c("Main","Subscenario")) %>%  #This [()] is a regular expression
  separate(col=Main , sep=c("[ ]"),into=c("ScenarioYear","Provider")) %>%
select(Provider,ScenarioYear,Subscenario,ScenarioYear,Country,CarbonTax,Year,PowerCons.TWh)

#View(RFF.selected)

LongFormResults <- read_csv("5-comparison/2ResultsFromCPAT/LongFormResults.csv") %>%
  pivot_longer(cols=7:23, names_to = "Year", values_to = "value") %>%
  filter(Quantity=="Power Supplied ktoe" & Model=="Old")

LongFormResults$value = as.numeric(LongFormResults$value)
#LongFormResults$year = as.integer(LongFormResults$year)

LongFormResults = LongFormResults %>%  mutate(PJ=value*PJPerktoe,PowerCons.TWh=PJ*1e3/(60*60)) %>%
  mutate( ScenarioYear="2017",Provider="CPAT-Excel") %>%
  mutate(Year=as.integer(Year)) %>%
  filter(Fuel=="Total",Scenario=="Baseline",CarbonTax==0,Country%in%CountriesSelected) %>%
  mutate(Scenario= paste0(Scenario, "-", Model )) %>%
  separate(col=Scenario , sep=c("[-]"),into=c("Scenario","Subscenario")) %>%
  select(Provider,ScenarioYear,Subscenario,ScenarioYear,Country,CarbonTax,Year,PowerCons.TWh)


#View(LongFormResults)

AllResults=bind_rows(LongFormResults,RFF.selected) %>%
  filter((Year > 2017) & (Year < 2030 )) %>%
  filter(PowerCons.TWh > 1) %>%
  mutate(Subscenario=if_else(Subscenario=="Old","Reference",Subscenario)) %>%
  mutate(ProviderYear=paste(Provider,ScenarioYear,sep="-")) %>%
  mutate(ScenarioYearSubScenario=paste(ScenarioYear,Subscenario,sep="-")) %>%
  mutate(ProviderScenarioYearSubScenario=paste(Provider,ScenarioYear,Subscenario,sep="-")) %>%
  mutate(ProviderSubScenario=paste(Provider,Subscenario,sep="-")) %>%
  mutate(TypeOfModel=if_else(Provider=="CPAT-Excel","CPAT Excel","External Model"))


ggplot(data=AllResults) +
  geom_point(mapping = aes(x=Year,y=PowerCons.TWh,color=ProviderYear,fill=Subscenario,shape=ScenarioYear), show.legend = TRUE) +
  facet_wrap(~Country,scales="free_y")

results2020 = AllResults %>% filter(Year==2020L)

CompareOutlookModels=function(AllResults_=AllResults,CountriesChosen=c("Brazil","China","India","United States")) {
  AllResults_ = AllResults_ %>% filter(Country %in% CountriesChosen)
  outlookplot=ggplot(data=AllResults_) +
  geom_point(mapping = aes(x=Year,y=PowerCons.TWh,color=ProviderScenarioYearSubScenario,shape=TypeOfModel), show.legend = TRUE) +
  facet_wrap(~Country,scales="free_y") +
  geom_line(mapping = aes(x=Year,y=PowerCons.TWh,group=ProviderScenarioYearSubScenario))
return(outlookplot)
  }

library(cowplot)


cowplot::save_plot(plot=CompareOutlookModels(),
                   filename="4-output/CompareOutlookModels.png",
                   base_height=4)
#ggplot(data=filter(AllResults,Country=="China")) +
#  geom_point(mapping = aes(x=Year,y=PowerCons.TWh,color=ProviderSubScenario,shape=ScenarioYear), show.legend = TRUE) +
#  geom_line(mapping = aes(x=Year,y=PowerCons.TWh,group=ProviderScenarioYearSubScenario))
