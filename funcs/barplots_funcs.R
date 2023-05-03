# These functions creates barplot 

# RECEPTION barplot -------------------------------------------------------
#This is a barplot graph that shows the number of samples received (and the type) by the labs according to the date range and time units selected

graph_reception_highcharter <- function(data, units, dates = NULL) {
  
  data %>% 
    
    group_by(Date = floor_date(date_reception, as.character(units)), sample_type) %>%
    
    count(Date) %>% group_by(Date) %>% 
    
    #add percent for each year
    mutate(percent = round(n / sum(n), digits = 2)) %>% 
    
    #convert all NAs in factor to unknown
    mutate(sample_type = fct_explicit_na(sample_type, "unknown" )) %>% 
    
    hchart("column", hcaes(x = Date, y = n , 
                           group = sample_type),
           
           stacking = "normal") %>%  
    
    hc_title(text = paste0("Number and type of samples received between <b>", dates[1],"</b> and <b>", dates[2], "</b> grouped by <b>", units, "</b>"), 
             
             align = "left", 
             margin = 10,
             x = 50,
             style = list(useHTML = T, color = "#34495E", fontSize = 13)) %>% 
    
    hc_colors(c("#ACD7EC", "#DDB967", "#EAF0CE", "#FAD4D8",  "#C17C74")) %>% 
    
    hc_exporting(enabled = T) %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS(
                 "
     function(){
     outHTML =  '<b>' + this.point.sample_type + '</b>' + ' - <i>' + this.point.Date + '</i><br> <b> N received: </b>' + this.point.n + ' (' + this.point.percent + ' %)'
     return(outHTML)
     }
     ")) %>% 
    
    hc_navigator( enabled = TRUE )
} 


# TESTS barplot -----------------------------------------------------------
#This is a barplot graph that shows the number and type of tests realised by the labs according to the date range and time units selected

test_barplot <- function(data, units, dates = NULL){
  
  data  %>% 
    
    group_by(Date = floor_date(date_test, as.character(units)), test_type) %>% 
    
    count() %>% 
    
    group_by(Date) %>% 
    
    mutate(n_year = sum(n), 
           percent = round(digits = 2, n/n_year * 100) ) %>% 
    
    hchart("column", stacking = "normal", hcaes(x = Date, y = n, group = test_type)) %>%  
    
    hc_colors(c("#425436", "#A5614C", "#025076" )) %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML =  '<b>' + this.point.test_type + '</b> - <i>' + this.point.Date + '</i><br> N tests performed: ' + this.point.n  + ' (' + this.point.percent + '%)'
     return(outHTML)
     
     }") ) %>% 
    
    hc_title(text = paste0("Number and type of tests performed between <b>", dates[1], "</b> and <b>", dates[2], "</b> grouped by <b>", units, "</b>"), 
             
             align = "left", 
             margin = 10,
             x = 50,
             style = list(useHTML = T, color = "#34495E", fontSize = 13)) %>% 
    
    hc_plotOptions(column = list(pointPadding = 0, 
                                 borderWidth = 0 )) %>% 
    
    hc_exporting(enabled = T) %>% 
    
    hc_navigator( enabled = TRUE )
  
  
  
}

# ------------------------------------------------------------------------- PATHOGEN analysis -------------------------------------------------------

#Pathogen tests barplot 

pathogen_test_barplot <- function(data, units, dates = NULL){
  
  data  %>% 
    
    group_by(Date = floor_date(date_test, as.character(units)), test_realised) %>% 
    
    count() %>% 
    
    group_by(Date) %>% 
    
    mutate(n_year = sum(n), 
           percent = round(digits = 2, n/n_year * 100), 
           test_realised = str_remove_all(test_realised, "\\_.*")) %>% 
    
    hchart("column", stacking = "normal", hcaes(x = Date, y = n, group = test_realised))  %>% 
    
    hc_colors(c("#6FAACA", "#D1D1D1", "#A5614C", "#3B3E2D", "#025076", "#684B45", "#128ECC", "#504041", "#e1bd8d" )) %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML =  '<b>' + this.point.test_realised + '</b> - <i>' + this.point.Date + '</i><br> N tests performed: ' + this.point.n  + ' (' + this.point.percent + '%)'
     return(outHTML)
     
     }") ) %>% 
    
    hc_title(text = paste0("Number and name of tests performed between <b>", dates[1], "</b> and <b>", dates[2], "</b> grouped by <b>", units, "</b>"), 
             
             align = "left", 
             margin = 10,
             x = 50,
             style = list(useHTML = T, color = "#34495E", fontSize = 13)) %>% 
    
    hc_plotOptions(column = list(pointPadding = 0, 
                                 borderWidth = 0 )) %>% 
    
    hc_exporting(enabled = T) %>% 
    
    hc_navigator( enabled = TRUE )
  
  
  
}

# Pathogen POS/NEG barplot ------------------------------------------------
#this is a barplot that shows the distribution of results for the pathogen selected, based on time, units and tests defined

pathogen_barplot_highcharter <- function(data, units, test = NULL, dates, pathogen, path_dic) {
  
  test1 <- str_c(test, collapse = "|")
  
  test2 <- { if (is.null(test)) { test } else { paste0(" and filtered by <b>", str_c(test, collapse = ", "), " </b>") } }
  
  pathogen_barplot <- data %>%
    
    filter(str_detect(test_realised, test1)) %>%
    
    group_by(Date = floor_date(date_test, units), result) %>% 
    
    count() %>%
    
    group_by(Date) %>% 
    
    mutate(total = sum(n), 
           
           percent = round(digits = 2, n/total * 100), 
           
           pathogen = matchmaker::match_vec("denv", path_dic, from = "pathogen", to = "pathogen_name")) %>% 
    
    hchart("column", hcaes(x = Date, y = n, group = result), stacking = "normal") %>%  
    
    hc_title(text = paste0("Number and results of tests performed for the detection of <b>", matchmaker::match_vec(pathogen, path_dic, from = "pathogen", to = "pathogen_name"), "</b> between <b>", dates[1],"</b> and <b>", dates[2], "</b> grouped by <b>", units, "</b>", test2), 
             
             align = "left", 
             margin = 10,
             x = 50,
             style = list(useHTML = T, color = "#34495E", fontSize = 13)) %>%
    
    hc_colors(., c("#12B225", "#B80717")) %>% hc_exporting(enabled = T) %>% 
    
    hc_tooltip(useHTML = T,
               formatter = JS("
    function(){
    
    outHTML = this.point.pathogen + ' - <b>' + this.point.result + '</b><br><i>' + this.point.Date + '</i><br> N tests performed: ' + this.point.n  + ' (' + this.point.percent + '%)'
     return(outHTML)
     
     }") )
  
  pathogen_barplot 
  
}

# Ct distribution ---------------------------------------------------------
#this graphs shows the Ct value distribution for positives of a selected pathogen

ct_distribution <- function(all_tests_pos, tests, breaks = 5 ) {
  
  req(tests)
  
  tests <- str_c(str_remove(tests, "\\_.*"), collapse = "|")

all_tests_pos <- all_tests_pos %>% filter(., str_detect(test_realised, tests)) 

if ( all(is.na(all_tests_pos$ct_value)) ) { stop("There is no CT value") } else {
  
  hist <- hist(all_tests_pos$ct_value, freq = F, breaks = breaks)
  
  hchart(hist, showInLegend = F) %>%
    
    hc_exporting(enabled = T) }
}





