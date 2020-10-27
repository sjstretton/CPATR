# Welcome

Welcome to CPAT-R. This is a substantially updated version.
There is a web interface at: https://rtaw.shinyapps.io/CPAT-R/ (this will be slow as the model has to recalculate for all countries)

It really has four components, each 'encapsulated':

1) Preprocessing routines, which create data tables both for this R model and for the main Excel CPAT

2) The R version of the mitigation module

3) Graphing routines comparing results for different countries

4) A shiny interface to (2) and (3) which can be posted to the web


# FAQs 

The normal problems with running R programs are:
a) The required libraries aren't installed. Please install:
tidyverse, readxl, magrittr, cowplot and ggrepel (and any others that come up as not present)

b) The working directory is not defined. Please start by running CPAT-R.Rproj. If needed also run 
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
ie uncomment it in the main file to make sure the working directory is correct. This needs to be commented out for the shiny file to run and deploy, that's why it is commented out by default.

