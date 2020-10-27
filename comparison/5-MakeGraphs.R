TCAFCountryList <- c("CHN","IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM", "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")

DropdownCountryList=sort(unique(SummaryResults.ByCountry.2030$CountryCode))

SummaryResults.ByCountry.2030.selected= SummaryResults.ByCountry.2030%>%filter(CountryCode %in% TCAFCountryList)

############################ MAKE C-TAX DEPENDENCE GRAPH ###############################

CountryRank = SummaryResults.ByCountry.2030.selected%>% filter(CTScenarioRate==0) %>% mutate(Rank = rank(-Emissions)) %>% arrange((Rank)) %>% ungroup() %>%
  select(CountryCode, CountryName,Rank)
CountryRank=CountryRank[!is.na(CountryRank$CountryName),]


SummaryResults.ByCountry.2030.selected%<>% mutate(Labels=if_else(CTScenarioRate==100,CountryCode,NA_character_)) %>%
  inner_join(CountryRank) %>%
  arrange(Rank)

SpecifiedCountryTable=unique(SummaryResults.ByCountry.2030.selected%>%ungroup()%>% select(CountryCode, CountryName))

CountriesToGraph=SpecifiedCountryTable$CountryName
CountryCodesToGraph=SpecifiedCountryTable$CountryCode

ncountries=nrow(SpecifiedCountryTable)
basecolors = col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#800000', '#aaffc3', '#000075', '#808080')
nbasecountries=length(basecolors)
repititions=ncountries%/%nbasecountries
remainder=ncountries%%nbasecountries
mycolors=c(rep(basecolors,repititions),basecolors[1:remainder])
nb.cols <- ncountries

#SummaryResults.ByCountry.2030.selected%<>%mutate(CountryCategory=as.character(CountryCategory))

PlotTitle="Modelled Emissions in 2030 at different levels of carbon tax for major middle- and low-income countries"

country2030curves=ggplot(data=(SummaryResults.ByCountry.2030.selected)) +
  geom_smooth(mapping=aes(x=CTScenarioRate, y=Emissions, color = CountryCode)) +
  geom_label_repel(aes(x=CTScenarioRate, y=Emissions,label= Labels),
                   label.size = 0.125,size = 2.5) +
  facet_wrap(~CountryCategory,scales="free_y") +
    scale_color_manual(values = mycolors, limits= CountryCodesToGraph) +
  expand_limits(x=c(0,125)) +
  labs(subtitle=PlotTitle,
       x="Carbon Tax in 2030 ($/tCO2)",
       y="Emissions (MtCO2/year)",
       caption="Definitions: VeryLarge=BAU 2030 Emissions over 1Gt/y; Large: between 400Mt/y and 1Gt/y
       \n MediumLarge: between 300 and 400Mt/y, Medium: between 200 and 300Mt/y, Small: less than 200Mt/y Source: Author's Model/CPAT"
   )


print(country2030curves)
#cowplot::save_plot(plot=country2030curves,filename="output/country2030curves.png",base_height=7)



################################## MAKE BAR CHARTS ###################################
ChosenRate=100

SummaryResults.BySector.2030.selected=filter(SummaryResults.BySector.2030, CountryCode%in%TCAFCountryList,CTScenarioRate==ChosenRate)


sectoralemissionsfaceted = ggplot(SummaryResults.BySector.2030.selected, aes(fill=Sector, x=CountryName, y=EmissionsReduction)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~CountryCategory,scales="free") +
  theme(axis.text.x = element_text(angle=90, vjust=0.0, hjust=1.0)) +
  labs(subtitle=paste0("Modelled Emissions Reductions in 2030 due to a USD",ChosenRate," per tCO2 carbon tax"),
       x="Country",
       y="Abatement (MtCO2/year)",
       caption="Definitions: VeryLarge=BAU 2030 Emissions over 1Gt/y; Large: between 400Mt/y and 1Gt/y
          \n MediumLarge: between 300 and 400Mt/y, Medium: between 200 and 300Mt/y,
       Small: less than 200Mt/y Source: Author's Model/CPAT")


print(sectoralemissionsfaceted)
#cowplot::save_plot(plot=sectoralemissionsfaceted,filename="output/SectoralEmissionsReductionByCountryFaceted.png",base_height=7)


#######################

PresentedTable.Emissions = SummaryResults.ByCountry.2030.selected %>%
  pivot_wider(id_cols=c("CountryCode","CountryName"),names_from=CTScenarioRate,values_from=Emissions)

PresentedTable.EmissionsReduction = SummaryResults.ByCountry.2030.selected %>%
  pivot_wider(id_cols=c("CountryCode","CountryName"),names_from=CTScenarioRate,values_from=EmissionsReduction)

#write_csv(PresentedTable.Emissions, path="output/PresentedTable.Emissions.csv")
#write_csv(PresentedTable.EmissionsReduction, path="output/PresentedTable.EmissionsReduction.csv")

