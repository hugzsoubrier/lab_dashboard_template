#Laboratory Activities module 

# Lab Activities UI ------------------------------------------------------
lab_activities_ui <-  function(id) { 
  
  tabPanel( "Laboratory Activities Analysis",
            
            fluidRow(
              
              br(),
              
              withSpinner(valueBoxOutput(NS(id,"n_received_box"), width = 4)),
              
              withSpinner(valueBoxOutput(NS(id,"n_tests_box"), width = 4)),
              
              withSpinner(valueBoxOutput(NS(id,"n_test_per_day_box"), width = 4)),
              
              br(),
              
              column(12, align = "center",
                     
                     switchInput(inputId = NS(id,"reception_or_test"),
                                 onLabel = "Reception", 
                                 offLabel = "Tests",
                                 onStatus = "info", 
                                 offStatus = "danger",
                                 label = "switch graph",
                                 labelWidth = "80px",
                                 value = T)),
              hr(), 
              
              column( 12, 
                      
                      conditionalPanel(
                        condition = "input.reception_or_test == true", ns = NS(id),  
                        withSpinner(highchartOutput(NS(id,"reception_barplot")))),
                      
                      conditionalPanel(
                        condition = "input.reception_or_test == false", ns = NS(id), withSpinner(highchartOutput(NS(id,"test_barplot")))) ),
              
              br(),
              
              column( 12, 
                      
                      # Sample Type piechart 
                      box(width = 6,
                          status = "danger",
                          title = "Samples types", 
                          withSpinner(highchartOutput(NS(id,"sample_type_pie")))), 
                      
                      # Diagnostic Timeline
                      box(width = 6, 
                          status = "danger", 
                          title = "Diagnostic Timeline", 
                          withSpinner(highchartOutput(NS(id, "timeline"))))
                      
              ), 
              
              column(12,  
                     
                     # Sent by piechart 
                     box(width = 6,
                         status = "warning",
                         title = "Samples origins", 
                         withSpinner(highchartOutput(NS(id,"sentby_pie")))), 
                     
                     #Map of origins
                     box(width = 6, 
                         title = "Map of origins",
                         status = "warning",
                         
                         column(12, 
                                align = 'center', 
                                selectizeInput(inputId = NS(id, "admin_level"),
                                               choices = list("Admin 1" = "admin_1",
                                                              "Admin 2" = "admin_2",
                                                              "Admin 3" = "admin_3"),
                                               label = "Select an Admin level",
                                               selected = "admin_1")), 
                         withSpinner(leafletOutput(NS(id, "map_origins"))) 
                         
                     ) 
              ), 
              
              column(12, 
                     
                     # Tests piechart
                     box(width = 6, 
                         status = 'info', 
                         title = "Tests type",
                         
                         withSpinner(highchartOutput(NS(id,"test_pie")))), 
                     
                     ## Progress bar of Pathogen tested
                     box(width = 6,
                         status = "info",
                         title = "Pathogens tested",
                         withSpinner(uiOutput(NS(id,"top_pathogen")))) 
                     
              )
            )
  )
}


# Lab Activities Server ---------------------------------------------------

lab_activities_server <- function(id, clean_data_dated, all_tests_dated, units, dates, name_lab) {
  
  
  moduleServer(id, function(input, output, session) { 
    
    # Value Boxes 
    
    output$n_received_box <- renderValueBox({valueBox(value = n_received(clean_data_dated()),
                                                      subtitle = "Samples received", 
                                                      color = "aqua", 
                                                      icon = icon(name = "briefcase", lib = "font-awesome"))}) 
    
    output$n_tests_box <- renderValueBox( {valueBox(value = n_tests(all_tests_dated()), 
                                                    subtitle = "Tests performed *", 
                                                    color = "blue",
                                                    icon = icon(name = "microscope", lib = "font-awesome"))}) 
    
    output$n_test_per_day_box <- renderValueBox( {valueBox(value = round(digits = 2, mean_tests(all_tests_dated(), units())),
                                                           subtitle = paste0("Mean number of tests per ", units() ,"*"),
                                                           color = "purple",
                                                           icon = icon(name = "vials", lib = "font-awesome"))})
    
    
    #reception distribution
    output$reception_barplot <- renderHighchart(graph_reception_highcharter(clean_data_dated(), units = units(), dates = dates()))
    
    #tests distribution
    output$test_barplot <- renderHighchart(test_barplot(data = all_tests_dated(), units = units(), dates = dates()))
    
    #sent by piechart
    output$sentby_pie <- renderHighchart(sentby_pie(data = clean_data_dated()))
    
    #sample type piechart
    output$sample_type_pie <- renderHighchart(sample_type_pie(data = clean_data_dated()))
    
    #test type piechart
    output$test_pie <- renderHighchart(tests_pie(data = all_tests_dated()))
    
    #Top pathogen 
    output$top_pathogen <- renderUI({ 
      
      pathogen_tests <- progress_data(all_tests_dated())
      
      tags$div(
        map(seq_len(min(nrow(pathogen_tests), nrow(sum(pathogen_tests$n)))), ~ {
          
          progressGroup(pathogen_tests$Virus_name[[.]], pathogen_tests$n[[.]], max = sum(pathogen_tests$n))
          
        }
        )
      )
    }
    )
    
    # Timeline 
    output$timeline <- { renderHighchart({ 
      
      data_timespan(clean_data_dated()) %>% 
        
        hc_timespan_data_median( ) %>%  
        
        hc_timeline(. , title = "timeline of median time between steps of diagnostic steps") 
      
    }) 
      
    }
    
    # Map origins 
    output$map_origins <- { renderHighchart({ 
      
      leaflet_map_origins(clean_data_dated(), admin_level = input$admin_level, shp_data = shp_loading(input$admin_level))
      
      
    }) 
      
    }
    
    
    
    
    
    
  }
  ) 
}
