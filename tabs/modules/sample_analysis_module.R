# This is the Sample-Analysis Module


# Sample Analysis UI ------------------------------------------------------

sample_analysis_ui <- function(id) {
  
  tabPanel("Samples - Analysis", 
           
           fluidRow( tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))), 
                     
                     br(), 
                     
                     column(5, align = "center", 
                            
                            # selector for dates
                            dateRangeInput(NS(id, "dates"), label = h3("Date range"), start = "2022-01-01", end = "2023-12-31", width = "50%")), 
                     
                     column(2),
                     
                     column(5, align = "center",
                            
                            #selector for time units
                            selectInput(NS(id,"select_units"), label = h3("Time units"), 
                                        
                                        choices = list("Days" = "day", 
                                                       
                                                       "Weeks" = "week", 
                                                       
                                                       "Months" = "month", 
                                                       
                                                       "Year" = "year"), 
                                        selected = "month")),
                     
                     br()),
           
           tabsetPanel( type = "tabs",
                        
                        data_vis_ui(NS(id, "data_vis")),
                        
                        
                        lab_activities_ui(NS(id, "lab_activities")),
                        
                        
                        pathogen_analysis_ui(NS(id, "pathogen_analysis")),
                        
                        # footer = "* For diagnostic tests investigating several genes from a same pathogen, each targeted gene is considered as a separate test. For example, Test 2 for LASV targeting gene GPC and gene L will appear as 2 tests."
                        
           )
  )
}


# Sample Analysis Server --------------------------------------------------

sample_analysis_server <- function(id, clean_data, all_tests, name_lab, tests_dic, path_dic) {
  
  
  moduleServer(id, function(input, output, session) { 
    
    # Identify the tests multitargets and filter out their results except final result to prevent duplication - the multitarget data are used in pathogen analysis later
    
    #filter the test_dic to only keep the RT-PCR/gx tests that are multitarget (ie: they have are not NA in target var and not RDT) 
    
    multitarget_dic <- tests_dic %>% 
      filter(!is.na(target), test_type != "rdt")
    
    #create a vector of test name that are multitarget using the tests_dic 
    multitarget_pattern <- str_c(unique(multitarget_dic$test_name), collapse = "|" )
    
    # a. filter all_tests to keep only multitargets test
    all_tests_multitarget <- reactive({ 
      
      all_tests() %>%
        
        filter(str_detect(test_realised, multitarget_pattern), !str_detect(test_realised, "_final"))  })
    
    # b. remove multitargets from the all_tests - and rename to remove the _final
    all_tests_nomultitarget <- reactive({
      
      all_tests() %>% 
        filter( !c( str_detect(test_realised, multitarget_pattern) & !str_detect(test_realised, "_final")) ) %>% 
        mutate(test_realised = str_remove(test_realised, "_final"))
      
    })
    
    
    #create Reactive Expressions that filters the databases based on the input dates ------------
    
    #Clean_data, filtered on RECEPTION dates
    clean_data_dated <- reactive( { 
      
      clean_data() %>%  
        
        filter(between(date_reception, as.Date(input$dates[1]), as.Date(input$dates[2])))
      
    })
    
    #All_tests, filtered on DATE TEST
    all_tests_dated <- reactive({ 
      
      all_tests_nomultitarget() %>% 
        
        filter(between(date_test, as.Date(input$dates[1]), as.Date(input$dates[2])))
      
    })
    #All_tests multitargets, filtered on DATE TEST
    all_tests_multitarget_dated <- reactive({ 
      
      all_tests_multitarget() %>% 
        
        filter(between(date_test, as.Date(input$dates[1]), as.Date(input$dates[2]))) 
      
    })
    
    #Module servers
    
    data_vis_server(id = "data_vis",
                    clean_data = reactive( clean_data_dated() ),
                    dates = reactive(input$dates ),
                    tests_dic = tests_dic,
                    path_dic = path_dic, 
                    name_lab = name_lab
    )
    
    lab_activities_server(id = "lab_activities",
                          clean_data_dated = reactive(clean_data_dated()),
                          all_tests_dated =  reactive(all_tests_dated()),
                          units = reactive(input$select_units),
                          dates = reactive(input$dates),
                          name_lab = name_lab
    )
    
    pathogen_analysis_server(id = "pathogen_analysis",
                             all_tests_dated = reactive(all_tests_dated()),
                             clean_data_dated = reactive(clean_data_dated()),
                             units = reactive(input$select_units),
                             dates = reactive(input$dates),
                             tests_dic = tests_dic,
                             path_dic = path_dic, 
                             all_tests_multitarget_dated = reactive( all_tests_multitarget_dated())  )
    
    
  } )}



