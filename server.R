#Server GUI_dashboard_V0

library(shiny)

options(shiny.reactlog = T)

server <- function(input, output, session) {
  

# --- HOME -----
  
source("tabs/home.R", local = T)
  

# --- Laboratory 1 -----
  
lab_1_server()
  
# --- Laboratory 2 -----
  
lab_2_server()
  
#end session when closing window
session$onSessionEnded(stopApp) 

}

