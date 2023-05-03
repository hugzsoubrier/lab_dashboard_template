# This function creates a wide table sumarising the main informations for all Positive samples 
# - Also want to add based on test -- NEEDS to deal with variable name issues

wide_table <- function(pos_data, tests=NULL) { 
  
  tests <- str_c(tests, collapse = "|")
  
  pos_data %>%

  { if ( is.null(tests) ) { pos_data }

       else {

         filter(., str_detect(test_realised, tests))  }}  %>%


    #select interesting variables to have in Table
    select(contains("id"),  
           sent_by, 
           date_reception, 
           sample_type, 
           requested_diagnostic, 
           aliquots, 
           date_test, 
           date_name, 
           test_realised, 
           result, 
           ct_value,
           ct_name) %>%
    
    
    # pivot the data
    
    pivot_wider(id_cols = NULL,
                
                names_from = c(test_realised),

                values_from = c(result, date_test, ct_value)) %>%

    #put column in right order
    
    select(contains("id"), sent_by, sample_type, requested_diagnostic, aliquots, contains("date_test"), contains("result"), contains("ct_"))
  
}



wide_table2 <- function(pos_data, tests =NULL) {
  
 table_data <-  pos_data %>%
    
    { if (is.null(tests)) { pos_data }
      
      else {
        
        select(., 1:25, contains(tests)) %>% 
          
          filter(if_any(contains(tests), ~!is.na(.)))
        
      } }
 
 remove_empty(table_data)
}
