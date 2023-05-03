# These are all the functions involving Pie Chart

# Sample Type Piechart ----------------------------------------------------

sample_type_pie <- function(data) { 
  
  data  %>% 
    
    group_by(sample_type) %>% count() %>% 
    
    #convert all NAs in factor to unknown
    mutate(sample_type = fct_explicit_na(sample_type, "unknown"), 
           
           color = case_when(sample_type == "whole blood" ~ "#C17C74", 
                             sample_type == "buccal swab" ~ "#ACD7EC" , 
                             sample_type == "nasopharyngeal swab" ~ "#DDB967", 
                             sample_type == "plasma (EDTA)" ~ "#EAF0CE", 
                             sample_type == "serum" ~ "#FAD4D8")) %>% 
    
    hchart( "pie", 
            hcaes(x = sample_type, y = n, color = color), 
            name = "sample_type_plot") %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML =  '<b>' + this.point.sample_type + '</b><br> N samples received: ' + this.point.n  + ' (' + Math.round(this.point.percentage) + '%)'
     return(outHTML)
     
     }") )
  
}

# Sent By Piechart --------------------------------------------------------

sentby_pie <- function(data) { 
  
  data  %>% 
    
    group_by(sent_by) %>% count() %>% 
    
    #convert all NAs in factor to unknown
    mutate(sent_by = fct_explicit_na(sent_by, "unknown")) %>% 
    
    hchart( "pie", 
            hcaes(x = sent_by, y = n), 
            name = "sent_by_plot") %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML =  '<b>' + this.point.sent_by + '</b><br> N samples received: ' + this.point.n  + ' (' + Math.round(this.point.percentage) + '%)'
     return(outHTML)
     
     }") ) %>% 
    
    hc_colors( c("#4A5759", "#B0C4B1", "#DEDBD2", "#F7E1D7", "#D9B26F", "#795C5F"))
}

# Test type Piechart --------------------------------------------------------

tests_pie <- function(data) { 
  
  data  %>% 
    
    group_by(test_type) %>% count() %>% 
    
    #convert all NAs in factor to unknown
    mutate(sent_by = fct_explicit_na(test_type, "unknown"), 
           
           color = case_when(test_type == "real-time RT-PCR" ~ "#425436", 
                             test_type == "Gene Xpert" ~ "#025076", 
                             test_type == "RDT" ~ "#A5614C" ) ) %>% 
    
    hchart( "pie", 
            hcaes(x = sent_by, y = n, color = color), 
            name = "sent_by_plot") %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML =  '<b>' + this.point.test_type + '</b><br> N tests performed: ' + this.point.n  + ' (' + Math.round(this.point.percentage) + '%)'
     return(outHTML)
     
     }") ) 
}




