# APP UI GUI_dashboard_VO

ui <- fluidPage(
    
    useShinyjs(), 
    useShinydashboard(),
    
        navbarPage(title = "Diagnostic Laboratory dashboard", 
                   
                   selected = NULL, 
                   
                   theme = "style.css",
                   
                   #Tabs
                   
                   home,
                   
                   lab_1_ui,
                   
                   lab_2_ui,
                   
                   
    
    
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: right;
                           }
                           .navbar-nav {
                           float: right;
                           }
                           .navbar-nav {
                           float: right;
                           }
                           ")))
                ))

















