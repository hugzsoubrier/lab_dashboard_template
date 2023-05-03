# Laboratory 1 TAB

# UI ----------------------------------------------------------------------

lab_1_ui <- tabPanel( h3("Laboratory 1"), 
                      
                      div(tabsetPanel(type = "pills", 
                                      
                                      sample_analysis_ui("lab1")
                                      
                      ))) 

# SERVER ------------------------------------------------------------------

lab_1_server <- function(input, output, session){
  
  # Loading Laboratory 1 data ---------------------
  
  #read the excel files
  clean_data_lab1 <-  import( "data/linelist/fake_diagnostic_linelist_lab1.csv") %>% 
    
    remove_empty(which = "rows") %>% 
    
    data_cleaning() #clean the excel using my function in utils_funcs.R
  
  #arrange data by order of reception
  clean_data_lab1 <- clean_data_lab1 %>% 
    arrange(date_reception) %>% 
    select(everything(), comments)
  
  #pivot to long format --> creating all_tests data
  all_tests_lab1 <- clean_to_tests(clean_data_lab1) %>% 
    select(lab_id, test_realised, test_type, date_test, result, ct_value)
  
  # Tests and Pathogen dictionaries --------------------------------------------------
  
  # Filter the tests and Pathogen dictionary to keep only available tests / pathogen --------------------------------
  # vectors can then be used in choices selection for input
  
  #1. identify the tests that are present in the data col and not empty - removing all results columns not filled ----
  
  linelist_tests_lab1 <- clean_data_lab1 %>% 
    
    dplyr::select(contains("result")) %>% 
    colnames() %>% 
    str_remove(., "result_") %>%
    str_remove_all(., "\\_.*" ) %>% unique(.) %>% 
    
    str_c(collapse = "|")  #create vector of test_name present in linelist
  
  
  #2. filter the test dictionary to only keep tests that are in the linelist
  
  test_dic_lab1 <- tests_dic %>% 
    
    filter( str_detect(test_name, linelist_tests_lab1 ))  #keep only vars that are present in the linelist using the pre-defined vector
  
  
  #3. Create a filtering vector to filter the pathogen dictionary and keep only available pathogen 
  
  linelist_pathogen_lab1 <- str_c(unique(test_dic_lab1$pathogen), collapse = "|")
  
  #4. filter the pathogen dictionary to only keep the available pathogens
  
  path_dic_lab1 <- path_dic %>% 
    
    filter( str_detect(pathogen, linelist_pathogen_lab1 )) #keep only vars that are present in the linelist using the pre-defined vector
  
  # Loading the tabs servers ------------------------------------------------------------------
  
  sample_analysis_server("lab1",
                         clean_data = reactive(clean_data_lab1),
                         all_tests = reactive(all_tests_lab1),
                         name_lab = "Laboratory 1",
                         tests_dic = test_dic_lab1,
                         path_dic = path_dic_lab1
                         
  )
  
}

