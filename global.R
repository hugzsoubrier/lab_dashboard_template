# global.R script for GUINEA dashboard

#load packages ---------------------------------------------------------
# Ensures the package "pacman" is installed
if (!require("pacman")) {install.packages("pacman")}

pacman::p_load(
  
#Shiny related
shiny,
shinydashboard,
shinyjs,
shinyWidgets,
shinycssloaders,
#reactlog,

#data wrangling and cleaning
rio,
here,
magrittr,
lubridate,
utils,
naniar,
janitor,
tidyverse,

#visualisations
ggplot2,
highcharter,
DT,

#spatial
sf,
rgdal,
leaflet
)

# Load Functions ----------------------------------------------------------

files.sources <- list.files("funcs")
files.sources <- paste0("funcs/", files.sources)

lapply(files.sources, source)


 # Load Tabs & Modules ------------------------------------------------------

#sample_analysis
source("tabs/modules/sample_analysis_module.R", local = T)

#data visualisation 
source("tabs/modules/data_vis_module.R", local = T)

#lab activities
source("tabs/modules/lab_activities_module.R", local = T)

#pathogen_analysis module
source("tabs/modules/pathogen_analysis_module.R", local = T)

# Home 
source("tabs/home.R", local = T)

#Laboratory 2
source("tabs/laboratory_2.R", local = T)

#Laboratory 1
source("tabs/laboratory_1.R", local = T)

# Load Tests + Pathogen dictionary 

tests_dic <- import("data/tests_dictionary.xlsx")
path_dic <-  import("data/tests_dictionary.xlsx", sheet = "pathogen dictionary") 
