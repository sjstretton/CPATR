#Hard Coded Variables
AnalysisEndYear <- 2030L
BaseYear <- 2018L
NumberOfYears <- 15L
EndYear <- BaseYear+NumberOfYears-1

PJPerktoe <- 0.041868
FuelList <- c("coa", "nga", "oop", "gso", "die", "lpg", "ker", "jfu", "bio", "ecy")
PowerFuelTypes <- c("coa", "nga", "oop", "nuc", "wnd", "sol", "ore", "hyd", "bio")
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")
CountryCategoryList <- as_factor(c("VeryLarge","Large","MediumLarge","Medium","MediumSmall","Small","VerySmall"))

#Read in Lookup Tables
Fuels <- read_csv("1-metadata/EmissionsFactors.csv")
Flow <- read_csv("1-metadata/SectorFullLookup.csv")
Elasticities <- read_csv("1-metadata/Elasticities.csv") %>% filter(FuelType %in% FuelList)
SectorSubsectorLookup <- read_csv("1-metadata/SectorsAndSubsectorsNarrowLookup.csv")
CountriesTable <- read_csv("1-metadata/CountryLookup.csv")
CountryNameLookup <-  CountriesTable %>% select(CountryCode, CountryName)

#Define Included Elements of Each Dimension
YearList <- as.character(BaseYear:(EndYear+1))
SectorList <- unique(Flow$Sector); SectorList=SectorList[!is.na(SectorList)]
SubsectorList <- unique(Flow$SubsectorCode); SubsectorList=SubsectorList[!is.na(SubsectorList)]
ScenariosList <- c("Baseline1","CTaxScen2")


#Add Energy Balances
CPATEnergyBalances2018 <- read_csv("2-preprocess/output/ProcessedEnergyBalances2018.csv")
GDPRelativeToBase <- read_csv("2-preprocess/output/GDPRelativeToBase.csv",col_types="ccd")

#Add Power Data
SelectedPowerData=read_csv("2-preprocess/output/SelectedPowerData.csv")

#Add Fuel Prices
FuelPricesGlobal <- read_csv("2-preprocess/rawdata/GlobalPricesGJ.csv") %>%
  pivot_longer(cols=as.character(2018:2035),names_to="Year",values_to="Price")
FuelPricesGlobalForPower  <- FuelPricesGlobal %>%  mutate(Year=as.integer(Year))
FuelPrices <- FuelPricesGlobal

#Add Base Price
BaseYearPriceLookup <- FuelPrices %>% filter(Year==as.character(BaseYear)) %>% rename(BaseYearPrice=Price) %>% select(-Year)
FuelPrices <- FuelPrices %>% rename(BaselinePrice=Price) %>% inner_join(BaseYearPriceLookup)

#Define Carbon Tax
CarbonTaxTrajectoryForm <- read_csv("2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd") #Shape of Carbon Tax Trajectory is Currently Standardised
CTRange=seq(from=0, to=100, by=20)
