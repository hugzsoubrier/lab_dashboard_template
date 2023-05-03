#Data loading, cleaning, transformation functions

library(janitor)
library(tidyverse)

# Data Cleaning function --------------------------------------------------

data_cleaning <- function(data) {
  
  #1. make sure character variables names are cleaned
  clean_data <- clean_names(data)
  
  #2. remove  prefilled lab_id 
  clean_data <- clean_data %>% dplyr::filter(!(!is.na(lab_id) & if_all(2:last_col(offset = 1), is.na)))
  
  #3. Clean variables that need tidying- 
  #create vector of such variables
  needtidy_vars <- clean_data %>% dplyr::select(contains("id"), 
                                                gender, 
                                                profession,
                                                age_unit,
                                                sent_by, 
                                                sample_type,
                                                patient_condition, 
                                                home_country,
                                                admin_1,
                                                admin_2, 
                                                admin_3,
                                                admin_4, 
                                                requested_diagnostic, 
                                                contains("result") ) %>% colnames()
  
  #apply cleaning function from janitor to the variables to be cleaned -- FIND A BETTER WAY
  
  #clean_data <- clean_data %>% mutate(across(all_of(needtidy_vars), ~ make_clean_names(.x) ))
  
  # clean_data[needtidy_vars] <- clean_data %>% dplyr::select(all_of(needtidy_vars)) %>% clean_data()
  
  #4. Assign appropriate  types to variables
  
  #Create vector for each var type
  character_vars <- clean_data %>% dplyr::select(contains("_id"), 
                                                  first_name,
                                                  surname, 
                                                  requested_diagnostic,
                                                  comments) %>% colnames()
  
  date_vars <- clean_data %>% dplyr::select(contains("date"), dob) %>% colnames()
  
  numeric_vars <- clean_data %>% dplyr::select(age, 
                                               contains("ct_"), 
                                               aliquots) %>% colnames()
  
  fct_vars <- clean_data %>% dplyr::select(gender, 
                                           age_unit,
                                           profession,
                                           patient_condition,
                                           home_country,
                                           admin_1,
                                           admin_2,
                                           admin_3,
                                           admin_4,
                                           sent_by,
                                           sample_type,
                                           sample_condition,
                                           requested_diagnostic, 
                                           contains("result")) %>% colnames()
  
  #change var type accordingly - only numeric, character and factors
  clean_data <- clean_data %>%
    
    mutate(across(numeric_vars, as.double), 
           across(fct_vars, as.factor), 
           across(character_vars, as.character), 
           across(date_vars, as.Date))

  #if dates remained numeric after this manually force them using excel origin
  
  clean_data <- clean_data %>%  
    mutate(across( contains("date") & where(is.numeric), ~ as.Date(.x, origin = "1899-12-30")))

  return(clean_data)
  
}


# Data Transformation function --------------------------------------------

clean_to_tests <- function(clean_data) { 
  
  #pivot the table in Long format using the Result variables to test_realised - EACH result is now a test
  
  Test_long <- clean_data %>% pivot_longer(cols = contains("result"), 
                                         names_to = "test_realised",
                                         values_to = "result", 
                                         names_prefix = "result_",
                                         values_drop_na = T) 
  
  
  #pivot the table in Long format using the Date of Test variable to date_name - 
  
  Date_long <- clean_data %>% pivot_longer(cols = contains("date_test"), 
                                     names_to = "date_name", 
                                     values_to = "date_test", 
                                     names_prefix = "date_test_",
                                     values_drop_na = F) %>% 
    
    dplyr::select(lab_id, date_name, date_test) %>% 
    
    mutate(date_name = str_remove(date_name, pattern = "date_") )
  
  
  #pivot the table in long format using the Ct value - merge test_realised and Ct values - remove rows that do not have Ct values
  
  Ct_long <- clean_data %>% pivot_longer(cols = contains("ct_"), 
                                         names_to = "ct_name",
                                         values_to = "ct_value",
                                         values_drop_na = T) %>% 
    
    dplyr::select(lab_id, ct_name, ct_value) %>%
    
    mutate(ct_name = str_remove(ct_name, pattern = "ct_") )
  
  
  #merge the pivotted tables -
  
  all_tests <- left_join(Test_long, Date_long, by = "lab_id") %>%
    
    filter(str_detect(test_realised, date_name)) %>% 
    
    left_join(., Ct_long, by = c("lab_id", "test_realised" = "ct_name")) %>% 
    
    #create a Test type variable by removing the last "_"
    
    mutate(test_type = case_when(str_detect(test_realised, "rt_pcr") ~ "real-time RT-PCR", 
                                 str_detect(test_realised, "gx") ~ "Gene Xpert", 
                                 str_detect(test_realised, "rdt") ~ "RDT" ))
  
  # Ensure all date test have a date - if date is missing then take reception date 
  all_tests <- all_tests %>% 
    
    mutate(date_test = ifelse(is.na(date_test), date_reception, date_test)) %>% 
    
    mutate(date_test = as_date(date_test))
  
  return(all_tests)
  
}

