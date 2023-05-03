# Pathogen Analyis UI -----------------------------------------------------

pathogen_analysis_ui <- function(id) { 
  
  tabPanel("Pathogen Analysis", 
           
           column(12, align = "center", 
                  
                  h1("Analysis per pathogen"),
                  
                  hr(),
                  
                  #selector for pathogen 
                  pickerInput(inputId = NS(id,"select_pathogen"), 
                              
                              label = h3("Select a Pathogen"), 
                              
                              choices = NULL,
                              
                              multiple = F)), 
           
           fluidRow(
             
             # Value boxes
             withSpinner(valueBoxOutput(NS(id,"n_requested_pathogen_box"), width = 3)),
             withSpinner(valueBoxOutput(NS(id,"n_tested_pathogen_box"), width = 3)),
             withSpinner(valueBoxOutput(NS(id,"n_positive_pathogen_box"), width = 3)),
             withSpinner(valueBoxOutput(NS(id,"n_negative_pathogen_box"), width = 3))
           ),
           
           br(), 
           
           column(12,
                  
                  # Sent by piechart 
                  box(width = 6, 
                      status = 'danger', 
                      title =  "Samples origins", 
                      
                      withSpinner(highchartOutput(NS(id,"pathogen_sentby_pie")))),
                  
                  #  Tests type pie chart 
                  box(width = 6, 
                      status = 'danger', 
                      title = "Tests type",
                      
                      withSpinner(highchartOutput(NS(id,"pathogen_test_pie")))) ),
           
           br(),
           
           
           column(12, align = "center",
                  
                  withSpinner(highchartOutput(NS(id,"pathogen_test_barplot")))),
           
           hr(),
           
           column( 12, align = "center", 
                   
                   pickerInput(inputId = NS(id,"select_test"), label = h3("Select Tests"), 
                               
                               choices = NULL, 
                               multiple = T )), 
           
           
           column(12, align = "center", 
                  
                  withSpinner(highchartOutput(NS(id,"pathogen_barplot")))),
           
           
           box( width = 12, align = "center", status = "danger",
                
                h2("Analysis of Positive Samples"),
                
                hr(),
                
                column(12, align = "center", 
                       
                       div(withSpinner(DT::dataTableOutput(NS(id,"raw_data"), width = "90%")),), 
                       
                       style = "font-size:80%" ),
                
                tags$head(tags$style(type = "text/css", "#pathogen_analysis raw_data td {line-height:50%;}")),
                
                br(),
                
                column(12, 
                       
                       box(width = 6, title = "Map of positive cases", 
                           
                           column(12, align = 'center', selectizeInput(inputId = NS(id, "admin_level"),
                                                                       
                                                                       choices = list("Admin 1" = "admin_1",
                                                                                      "Admin 2" = "admin_2",
                                                                                      "Admin 3" = "admin_3"),
                                                                       
                                                                       label = "Select an Admin level",
                                                                       selected = "admin_1")), 
                           
                           withSpinner(leafletOutput(NS(id, "map_pos"))) ), 
                       
                       box(width = 6, align = "left", 
                           
                           title = "Ct values distribution",
                           
                           
                           column(6, 
                                  
                                  sliderTextInput(NS(id, "breaks"),
                                                  label = "Select number of rough breaks", 
                                                  choices = (seq(from = 1, to = 50, by = 1)),
                                                  selected = 20, 
                                                  grid = F, 
                                                  width = "100%"), 
                                  
                                  pickerInput(inputId = NS(id,"select_test2"), 
                                              label = h5("Select Tests"), 
                                              choices = NULL, 
                                              multiple = T )), 
                           
                           
                           column(6, 
                                  
                                  withSpinner(valueBoxOutput(NS(id,"mean_ct"), width = 12))),
                           
                           highchartOutput(NS(id, "ct_distribution"))
                           
                       ))  
                
           )
           
  )
  
}


# PATHOGEN ANALYSIS SERVER ------------------------------------------------

#create Reactive Expressions that filters the databases based on the Pathogen - This is based on the previous databases filtered by Dates

#All_tests, filtered on DATE TEST

pathogen_analysis_server <- function(id, all_tests_dated, clean_data_dated, units, dates, tests_dic, path_dic, all_tests_multitarget_dated) {
  
  moduleServer(id, function(input, output, session) {  
    
    # Update the pathogen selector using choices from the dictionary 
    observe({
      
      updatePickerInput("select_pathogen", 
                        session = session,
                        choices = setNames(path_dic$pathogen, path_dic$pathogen_name),
                        selected = "ebola")
    })
    
    #1. filter all the data based on the selected input
    
    ###### All tests 
    all_tests_dated_pathogen <- reactive({
      
      req(input$select_pathogen)
      
      filter(all_tests_dated(), str_detect(test_realised, input$select_pathogen)) } )
    
    
    #keep only the positives
    all_tests_dated_pathogen_pos <- reactive(filter(all_tests_dated_pathogen(), result %in% "positive"))
    
    ###### Clean data
    clean_data_dated_pathogen <-  reactive(clean_data_dated() %>% select(1:25, contains(input$select_pathogen)) %>% 
                                             filter(if_any(starts_with("result"), .fns =  ~ !is.na(.))) ) 
    # keep only the positives
    clean_data_dated_pathogen_pos <- reactive(clean_data_dated_pathogen() %>% 
                                                filter(if_any(starts_with("result"), .fns =  ~  . %in% "positive")) )
    
    ###### All tests multitarget
    all_tests_multitarget_dated_pathogen <- reactive({
      
      req(input$select_pathogen)
      
      filter(all_tests_multitarget_dated(), str_detect(test_realised, input$select_pathogen)) } )
    
    
    #keep only the positives
    all_tests_multitarget_dated_pathogen_pos <- reactive(filter(all_tests_multitarget_dated_pathogen(), result %in% "positive"))
    
    
    
    # 2. server for the value boxes 
    
    #value box - N requested pathogen
    output$n_requested_pathogen_box <- renderValueBox( {valueBox(value = n_requested_pathogen(clean_data_dated_pathogen(), path_dic, input$select_pathogen),
                                                                 subtitle = "Requested",
                                                                 color = "aqua", 
                                                                 icon = icon(name = "clipboard", lib = "font-awesome")) })
    
    #value box - N tested pathogen
    output$n_tested_pathogen_box <- renderValueBox( {valueBox(value = n_tests_pathogen(all_tests_dated_pathogen()),
                                                              subtitle = "Tests performed",
                                                              color = "yellow", 
                                                              icon = icon(name = "microscope", lib = "font-awesome")) })
    
    #value box - N positives pathogen
    output$n_positive_pathogen_box <- renderValueBox( {valueBox(value = n_positive_pathogen(all_tests_dated_pathogen()),
                                                                subtitle = "Positives tests",
                                                                color = "red", 
                                                                icon = icon(name = "biohazard", lib = "font-awesome")) })
    #value box - N negatives pathogen
    output$n_negative_pathogen_box <- renderValueBox( {valueBox(value = n_negative_pathogen(all_tests_dated_pathogen()),
                                                                subtitle = "Negatives tests",
                                                                color = "green", 
                                                                icon = icon(name = "check-circle", lib = "font-awesome")) })
    
    
    # 3. server for the Pie chart 
    
    # sent by piechart graph
    output$pathogen_sentby_pie <- renderHighchart(sentby_pie(data = clean_data_dated_pathogen()))
    
    # test type piechart
    output$pathogen_test_pie <- renderHighchart(tests_pie(data = all_tests_dated_pathogen()))
    
    
    #4. Server for Barplots 
    
    #Bar plot of Pathogen tested over time, colored by the type of tests
    output$pathogen_test_barplot <- renderHighchart( { pathogen_test_barplot(all_tests_dated_pathogen(), units = units()) %>% 
        
        hc_title(text = paste0("Number and type of tests performed for the detection of <b>", matchmaker::match_vec(input$select_pathogen, path_dic, from = "pathogen", to = "pathogen_name"), "</b> between <b>", dates()[1],"</b> and <b>", dates()[2], "</b> grouped by <b>", units(), "</b>"),
                 
                 align = "left",
                 margin = 10,
                 x = 50,
                 style = list(useHTML = T, color = "#34495E", fontSize = 13)) 
      
    }  )
    
    #Dynamic Input for tests selector
    observeEvent(all_tests_dated_pathogen(), {
      
      updatePickerInput(session = session, 
                        inputId = "select_test", 
                        choices = unique(filter(tests_dic, pathogen == input$select_pathogen)$test_name), 
                        selected = unique(filter(tests_dic, pathogen == input$select_pathogen)$test_name))
      
    })
    
    
    #Bar plot of pathogen depending on the tests select, colored by POS/NEG
    output$pathogen_barplot <- renderHighchart(pathogen_barplot_highcharter(all_tests_dated_pathogen(), 
                                                                            units(), 
                                                                            dates = dates(), 
                                                                            test = input$select_test, 
                                                                            pathogen = input$select_pathogen, 
                                                                            path_dic = path_dic ))
    
    
    # POS analysis ------
    
    #table of positives samples
    output$raw_data <- DT::renderDT(wide_table2(clean_data_dated_pathogen_pos(), tests = input$select_test), options = list(autoWidth = T, scrollX = T)) 
    
    
    #filter the tests_dic to keep the pathogen selected and create a choice_label var to be displayed in selector (important for tests that have multitarget)
    tests_dic_path <- reactive({ 
      
      tests_dic %>% 
        
        filter(pathogen == input$select_pathogen) %>% 
        
        mutate(choice_label = case_when(is.na(target) ~ test_name,
                                        !is.na(target) & test_type == "rdt" ~ test_name,
                                        
                                        T ~ str_c(test_name, target, sep = "_"))) 
    })
    
    #Dynamic Input for tests selector 2
    observeEvent(all_tests_dated_pathogen(), {
      
      updatePickerInput(session = session, 
                        inputId = "select_test2", 
                        
                        #if pathogen includes multitarget tests then the choices are the one from dic
                        choices = unique(tests_dic_path()$choice_label), 
                        
                        selected = unique(tests_dic_path()$choice_label))
      
    })
    
    
    #distribution of Ct values
    output$ct_distribution <- renderHighchart( {
      
      if ( any(!is.na(tests_dic_path()$target)) ) { #ie: there is multitargets tests 
        
        ct_distribution(all_tests_multitarget_dated_pathogen_pos(), tests = input$select_test2, breaks = input$breaks) 
        
      } else{ 
        
        ct_distribution(all_tests_dated_pathogen_pos(), tests = input$select_test2, breaks = input$breaks) 
        
      } 
      
    })
    
    
    #value box - mean Ct values
    output$mean_ct <- renderValueBox( { valueBox( value = if ( any(!is.na(tests_dic_path()$target)) ) {
      
      
      mean_ct(all_tests_multitarget_dated_pathogen_pos(), tests = input$select_test2)
      
      
    } else{ mean_ct(all_tests_dated_pathogen_pos(), tests = input$select_test2) 
      
    },
    
    subtitle = "mean Ct value",
    color = "light-blue",
    
    icon = icon(name = "chart-line", lib = "font-awesome")) })
    
    #map of positives
    output$map_pos <- renderLeaflet(leaflet_map_pos(clean_data_dated_pathogen_pos(), 
                                                    input$admin_level, 
                                                    pathogen = matchmaker::match_vec(input$select_pathogen, path_dic, from = "pathogen", to = "pathogen_name"),
                                                    shp_data = shp_loading(input$admin_level)))
    
    
  }
  )
}