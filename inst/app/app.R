#0-Shiny
rm(list=ls())
TCAFCountryList <- c("CHN", "IND", "IRN", "IDN", "MEX", "BRA", "ZAF", "TUR", "THA", "MYS", "KAZ", "EGY", "VNM",
                     "PAK", "UKR", "IRQ", "PHL", "DZA", "BGD", "UZB", "NGA", "COL", "TKM", "ROU", "MAR")
MyCountryList <-  "CHN" #TCAFCountryList ##c("CHN") #Other options: ...,"IND","USA","JPN" #(CountryNameLookup$CountryCode)[1:10] #
DoPowerGlobal <- TRUE
AnalysisEndYear <- 2030L
BaseYear <- 2018L
NumberOfYears <- 15L
EndYear <- BaseYear+NumberOfYears-1
RetirementProportion <- 0.04
CarbonTaxTrajectoryForm <- read_csv("R/2-preprocess/output/CarbonTaxTrajectoryForm.csv",col_types = "cd") #Shape of Carbon Tax Trajectory is Currently Standardised
CTRange=seq(from=0, to=100, by=20)

library(tidyverse)
library(readxl)
library(magrittr)
#library(RColorBrewer)
library(cowplot)
library(ggrepel)
#setwd("C:/Users/wb547395/OneDrive - WBG/Documents/CPAT-R") #Temporary
SelectedCarbonTax=10
Sys.setenv(TZ='GMT')
CalledFromShinyScript=TRUE
source("R/main.R") #At present!
source("R/5-comparison/3-PostProcessing.R")
source("R/5-comparison/4-ModelComparison.R")
source("R/5-comparison/5-MakeGraphs.R")
CarbonTaxInTime <-  CarbonTaxTrajectoryForm %>% mutate(CarbonTaxRate=ProportionOfMaxCTaxRate*SelectedCarbonTax)
ChosenCountry = "CHN"
SelectedCarbonTax = 100

ui <- fluidPage(
  titlePanel("Simple CPAT-R"),
  mainPanel(
    tabsetPanel(
      tabPanel("Big Developing Countries",
               plotOutput("country2030curves", width = "1000px", height = "700px")),
      tabPanel("Pick Specific Country",
               selectInput("ChosenCountry", "Select Country", DropdownCountryList),
               selectInput("SelectedCarbonTax", "Select Carbon Tax", CTRange),
               tableOutput("static")
               ),
      tabPanel("Sectoral comparison", plotOutput("sectoralsplit", width = "1000px", height = "700px")),
      tabPanel("Intermodel comparison", plotOutput("modelcomparison", width = "1000px", height = "700px"))
    )
  )
)


server <- function(input, output, session) {

  output$static <- renderTable(filter(SummaryResults.ByCountry.2030,CountryCode == input$ChosenCountry &
                                                CTScenarioRate == input$SelectedCarbonTax) %>%
                                 select(CountryName, BaselineEmissions, Emissions, EmissionsReduction))

  output$country2030curves <- renderPlot({
    country2030curves
  }, res = 96)

  output$sectoralsplit <- renderPlot({
    sectoralemissionsfaceted
    }, res = 96)

  output$modelcomparison <- renderPlot({
    InterModelComparison
  }, res = 96)

}

shinyApp(ui, server)