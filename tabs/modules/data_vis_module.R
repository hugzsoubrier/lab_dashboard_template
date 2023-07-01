# This is the Database Visualistion module 

# UI ----------------------------------------------------------------------

data_vis_ui <- function(id) {   
  
  tabPanel( "Raw Data Visualisation", 
            
            div( class = "reactive-width",
            
            fluidRow(
              br(),
              
              h4(textOutput(NS(id, "title")))), 
            
            hr(), 
            
            dropdown( prettyRadioButtons(NS(id, "display"), 
                                         "Display Data", 
                                         status = "info", 
                                         shape = "round", 
                                         animation = "smooth", 
                                         icon = icon("eye"),
                                         
                                         choices = c("Head", "All", "Tail"),
                                         selected = "All"),
                      
                      prettyRadioButtons(NS(id, "sensitive"), 
                                         "Display Sensitive  Data ?", 
                                         status = "danger", 
                                         shape = "round", 
                                         animation = "pulse", 
                                         icon = icon("exclamation"),
                                         
                                         choices = c("yes", "no"),
                                         selected = "yes"),
                      
                      pickerInput( NS(id, "pathogen"),
                                   label = "Select Pathogen", 
                                   choices = NULL,
                                   multiple = TRUE), 
                      
                      pickerInput( NS(id, "tests"),
                                   label = "Select Tests", 
                                   choices = NULL, 
                                   multiple = TRUE ),
                      
                      style = "jelly", 
                      icon = icon("gear"),
                      status = "warning", 
                      animate = animateOptions(
                        enter = animations$fading_entrances$fadeInLeftBig,
                        exit = animations$fading_exits$fadeOutRightBig), 
                      
                      downloadBttn(NS(id, "download"), 
                                   label = "Download the data", 
                                   size = "xs"
                      )
                      
            ),
            
            br(), 
            
            div(withSpinner(DT::dataTableOutput(NS(id, "table"))), 
                
                style = "font-size:80%" ),
            
            tags$head(tags$style(type = "text/css", "#data_viz table td {line-height:50%;}")),
            
            hr(), 
            
  ) ) 
  
}


# SERVER ------------------------------------------------------------------


data_vis_server <- function(id, clean_data, dates, tests_dic, path_dic, name_lab) { 
  
  moduleServer(id, function(input, output, session) {  
    
    #use the pathogen dictionary to fill choices of pathogen selector
    observe({
      
      updatePickerInput(
        "pathogen", 
        session = session,
        choices = setNames(path_dic$pathogen, path_dic$pathogen_name), 
        selected = path_dic$pathogen)
    })
    
    #create the string for the title
    output$title <- renderText(paste0("Raw diagnostic data from the ", dates()[1], " to the ", dates()[2]))
    
    #build string pattern from pathogen selection 
    pathogen_select <- reactive({ 
      
      req(input$pathogen)
      
      str_c(input$pathogen, collapse = "|") 
      
    })
    
    #filter the dictionary based on pathogen selection
    tests_dic_path <- reactive( { 
      
      req(input$pathogen)
      
      tests_dic %>% filter(str_detect(pathogen, pathogen_select())) 
      
    } )
    
    #update PickerInput for tests based on pathogen selected
    observeEvent(input$pathogen, {
      
      #create list of choices for tests selection 
      tests_choices <- unique( tests_dic_path()$test_name)
      
      updatePickerInput( "tests", 
                         session = session,
                         choices = tests_choices, 
                         selected = tests_choices
      )
    } 
    )
    
    #build string pattern from test selection 
    tests_select <- reactive(str_c(input$tests, collapse = "|") )
    
    #select variables of database based on input$pathogen and input$test 
    data_path_tests <- reactive( {
      
      req(input$pathogen)
      req(input$tests)
      
      select(clean_data(), 
             1:25, #select all 25 first variables
             matches("date") & matches(tests_select()), #matches the data and the test to get the date-test var that do not include pathogen name
             matches(pathogen_select()) & matches(tests_select()) ) #select all that matches the pathogen + test selected
    }      
    )
    
    
    #filter to keep or not the sensitive data : input$sensitive
    
    
    data_sen <- reactive({
      
      if (input$sensitive == "no") { 
        
        return(select(data_path_tests(), !c(surname, first_name, dob, age, age_unit, gender, profession, admin_2, admin_3, admin_4)) )
        
      } else { return(data_path_tests() ) } 
      
    })
    
    
    #server for the table
    output$table <- DT::renderDT({ 
      
      # if else statement of input$display 
      if (input$display == "Head") { 
        
        return(head(data_sen(), n = 10 )) 
        
      } else if (input$display == "Tail") { 
        
        return(tail(data_sen(), n = 10))
        
      } else { 
        
        return(data_sen())
        
      }
    }, options = list(autoWidth = F, 
                      scrollX = T ) )
    
    
    #server for the download button
    
    output$download <- downloadHandler(
      
      #function for filename
      filename <- paste0('data_', name_lab, "_", dates()[1], "-", dates()[2], '.csv', sep = ''),
      
      content = function(file) { write.csv(data_sen(), file)
        
      }
    )
    
    
    
  }
  )
} 
