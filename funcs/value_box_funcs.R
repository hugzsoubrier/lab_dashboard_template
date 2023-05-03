#these functions provides details on N for reception/tests/types etc...
#Used for the box values

# How many samples arrived on these date 

n_received <- function(data) { data %>% summarise(n()) }

# How many tested on these dates
n_tests <- function(data) { 
  
  data %>% 
    
    group_by(test_type) %>% 
    summarise(n = n()) %>% 
    summarise(n = sum(n))
}

max_test <- function(data) {
  
  data %>% count(test_type) %>% 
    
    filter(n == max(n))
}

#which kind of sample for these dates ? 

n_types <- function(data) { 
  
  data %>%  group_by(sample_type) %>% count()
  
}

max_type <-  function(data) { 
  
  data %>%  group_by(sample_type) %>% 
    count() %>% 
    mutate( percent = (n / sum(.$n)) * 100 ) %>% ungroup() %>% filter(percent == max(percent))
  
}

#values for maximum sender of samples
max_sentby <- function(data) {
  
  n_total <- data %>% 
    group_by(sent_by) %>% 
    count() %>% ungroup() %>% summarise(total = sum(n))
  
  max_sent_by <- data %>% 
    group_by(sent_by) %>% 
    count() %>% ungroup() %>% filter(n == max(n)) %>% mutate(percent = ((n/n_total$total) * 100))
}


# number of distinct senders of samples
n_senders <- function(data) { data %>% group_by(sent_by) %>%
    count() %>% nrow() }

# average number of test per units of time

mean_tests <- function(data, units) {
  
  data %>% 
    
    group_by(Date = floor_date(date_test, as.character(units))) %>% 
    
    count() %$% mean(n) }


# N individual
n_individual <- function(data) { 
  
  data %>% count() }

#Pathogen that is the most tested

max_pathogen <- function(data){ 
  
  data %>% filter(n == max(n) ) }

# ------------------------------------------------------------------------- Analysis per pathogen  --------------------------------------------------

#How many tests requested ? 

n_requested_pathogen <- function(data, path_dic, selected_pathogen) { 
  
  #create a string of alternative pathogen naming using the path_dic and the selected pathogen 
  pathogen_string <- str_replace_all(filter(path_dic, pathogen == selected_pathogen)$pathogen_alternative, ", ", "|" )
  
  #filter the clean_data and count rows 
  return( nrow(data %>% filter(str_detect(requested_diagnostic, pathogen_string))) )
  
}

# N tested 

n_tests_pathogen <- function(data) { 
  
  data %>% 
    
    count()  }


# N Positives

n_positive_pathogen <- function(data) { 
  
  data %>% 
    
    filter(result %in% "positive") %>% 
    
    count() }


# N Negatives

n_negative_pathogen <- function(data) { 
  
  data %>%     
    filter(result %in% "negative") %>% 
    
    count() }


# N individual
n_individual_pathogen <- function(data) { 
  
  data %>%     
    
    filter(n_tests_pos >= 1) %>% 
    
    count() }

# Mean Ct value

mean_ct <- function(all_tests_pos, tests= NULL) {
  
  if (nrow(all_tests_pos) == 0) { 
    
    return(h5("No positive samples"))
    
  } else if (all(is.na(all_tests_pos$ct_value))) { 
    
    return(h5("No CT values")) } else {
      
      tests <- str_c(str_remove(tests, "\\_.*"), collapse = "|")
      
      all_tests_pos %>% 
        
        { if (is.null(tests)) { all_tests_pos }
          
          else { filter(. , str_detect(test_realised, tests))  }} %$%  
        
        round(digits = 2, mean(ct_value, na.rm = T))
      
    } 
  
} 




