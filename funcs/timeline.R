# this is the script of the timespan and timeline functions 

#create a database with all timespan
data_timespan <- function(data) { data %>% 
    
    #compute the intervals between onset-entry, entry-sampling, sampling-reception, reception-test
    
    mutate(date_first_test = do.call(pmin, c(select(., contains("date_test")), na.rm = T)), #get the date of earliest test
           
           timespan_ons_entry = time_length(interval(date_onset, date_entry), "days"),
           timespan_entry_sampl = time_length(interval(date_entry, date_sampling), "days"),
           timespan_sampl_recep = time_length(interval(date_sampling, date_reception), "days"),
           timespan_recep_test = time_length(interval(date_reception, date_first_test), "days")) %>% 
    
    #filter all timespan that have a negative value !!
    #filter(!if_any(contains("timespan"), ~.x<0)) %>% 
    
    select(lab_id, date_onset, date_entry, date_sampling, date_reception, date_first_test, contains("timespan"))
  
}

#this groups and summarise and prepare timespan data for hc graph -- using MEDIAN 
hc_timespan_data_median <- function(data) { 
  
  data %>% 
    
    select(contains("timespan"), date_first_test) %>% 
    
    #compute mean time
    summarise(across(contains("timespan"), median, na.rm = T)) %>% 
    
    #pivot longer 
    pivot_longer(cols = everything(),
                 
                 names_to = "event", 
                 values_to = "timespan")  %>% 
    
    #round the average data
    mutate(timespan = round(digits = 2, timespan)) %>% 
    
    #add rows for precise timepoint
    add_row(event = c("Symptoms onset", "Hospital entry", "Sampling", "Lab reception", "First test")) %>% 
    
    mutate(event = str_remove(event, "timespan_"), 
           x = NA, 
           y = 0) %>% 
    
    mutate(
      
      #create a label column 
      label = ifelse(timespan == 0, "< 1 day", ifelse(timespan == 1, paste0(timespan, " day"), paste0(timespan, " days"))),
      
      #add + 1 to all timespan to prevent 0 values 
      timespan = timespan + 1, 
      
      #define the starting and ending point of each section using the values of average timespan.
      start = case_when(event == "ons_entry" ~ 0,
                        
                        event == "entry_sampl" ~ 0+lag(timespan), 
                        event == "sampl_recep" ~ 0+lag(timespan, n = 2)+lag(timespan, n = 1), 
                        event == "recep_test" ~ 0+lag(timespan, n = 2)+lag(timespan, n = 1)++lag(timespan, n = 3)),
      
      #define the end point of section
      end = case_when(event == "ons_entry" ~ timespan, 
                      event == "entry_sampl" ~ start+timespan, 
                      event == "sampl_recep" ~ start+timespan, 
                      event == "recep_test" ~ start+timespan), 
      
      #define value of each precise point 
      x = case_when(event == "Symptoms onset" ~ 0, 
                    
                    event == "Hospital entry" ~start[event == "entry_sampl"], 
                    
                    event == "Sampling" ~ start[event == "sampl_recep"], 
                    
                    event == "Lab reception" ~ start[event == "recep_test"], 
                    
                    event == "First test" ~ end[event == "recep_test"])
    ) %>% 
    
    #creates colors 
    mutate(color = case_when( event == "ons_entry" ~  "#caf0f8", 
                              event == "entry_sampl" ~ "#48cae4", 
                              event == "sampl_recep" ~ "#0096c7", 
                              event == "recep_test" ~ "#023e8a",
                              
                              #points colors
                              event %in% c("Symptoms onset", "Hospital entry", "Sampling", "Lab reception", "First test") ~ "#9b2226")
    )
}

#Highcharter graph of the timeline 
hc_timeline <- function(hc_timespan_data, title = NULL) { 
  
  hc_timespan_data <- hc_timespan_data %>%  
    
    mutate( event = case_when(event == "ons_entry" ~ "Onset - Hospital entry", 
                              event == "entry_sampl" ~ "Hospital entry - Sampling", 
                              event == "sampl_recep" ~ "Sampling - Lab reception", 
                              event == "recep_test" ~ "Lab reception - First test", 
                              T ~ event) )
  
  
  highchart() %>% 
    
    hc_add_series("xrange",
                  name = "timespan", 
                  data = filter(hc_timespan_data, str_detect(event, " - ")),
                  hcaes(x = start, 
                        x2 = end, 
                        color = color), 
                  showInLegend = F, 
                  enableMouseTracking = T)  %>% 
    
    hc_add_series("scatter",
                  name = "event",
                  data = filter(hc_timespan_data, !str_detect(event, " - ")), 
                  hcaes(x = x, 
                        y = y, 
                        color = color),
                  showInLegend = F, 
                  enableMouseTracking = F) %>% 
    
    hc_title(text = title,
             align = "left", 
             style = list(useHTML = T, color = "#34495E", fontSize = 13)) %>% 
    
    hc_xAxis(visible = F) %>% 
    
    hc_yAxis(visible = F) %>% 
    
    hc_plotOptions(
      
      scatter = list(dataLabels = list(enabled = T,
                                       y = +25,
                                       position = "left",
                                       allowOverlap = F,
                                       style = list(fontSize = "10px", 
                                                    color = "black"),
                                       
                                       formatter = JS(
                                         "function(){
                                         
                                         outHTML =  this.point.event
                                         
                                         return(outHTML) }"),
                                       
                                       marker = list(radius = 4) )),
      
      
      xrange = list(pointWidth = 15, 
                    opacity = .7, 
                    
                    dataLabels = list(enabled = T, 
                                      y = -15,
                                      style = list(fontSize = "9px", 
                                                   color = "#34495E"),
                                      formatter = JS(
                                        "function(){
                                       outHTML =  this.point.label
                                       return(outHTML)}")) )
      
    ) %>% 
    
  hc_tooltip(useHTML = T,
             formatter = JS(
               "
             function(){

             if(this.series.name == 'event') {outHTML = '<b>' + this.point.event

             } else

             outHTML =  '<b>' + this.point.event  + '</b> <br>' +
             'median number of days: ' + this.point.label

             return(outHTML)
             }")

             ) %>%
  
  hc_exporting(enabled = T)
  
}
