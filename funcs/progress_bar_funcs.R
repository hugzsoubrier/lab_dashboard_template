# PROGRESS BARS 

# from: https://github.com/rstudio/shinydashboard/issues/119

# function to prepare the data first

progress_data <- function(data) {
  
  data %>% select(test_realised) %>% 
    
    mutate(Virus_name = case_when( str_detect(test_realised, "ebov") ~ "Ebola Virus Zaire (EBOV)",
                                   str_detect(test_realised, "ebola") ~ "Ebola Virus species",
                                   str_detect(test_realised, "marv") ~ "Marburg virus", 
                                   str_detect(test_realised, "lasv")~ "Lassa virus", 
                                   str_detect(test_realised, "yfv") ~ "Yellow Fever virus", 
                                   str_detect(test_realised, "denv") ~ "Dengue Fever virus", 
                                   str_detect(test_realised, "malaria") ~ "Malaria",
                                   TRUE ~ "Others")) %>% 
    
    group_by(Virus_name) %>% count(sort = T) %>% ungroup()
}

progressBar <- function(
    max,
    value = 0,
    label = FALSE,
    color = "aqua",
    size = NULL,
    striped = FALSE,
    active = FALSE) {
  
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  
  
  text_value <- paste0(round(value/max * 100), "%")
  
  style <- htmltools::css(width = text_value, `min-width` = "0%")
  
  tags$div(
    
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      
      # role = "progressbar",
      # `aria-valuenow` = value,
      # `aria-valuemin` = 0,
      # `aria-valuemax` = 1,
      tags$span(class = if (!label) "sr-only", value)
    )
  )
}

#' Progress Group
progressGroup <- function(text, value, min = 0, max, color = "aqua") {
  
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(value, max = max, active = T, striped = T, color = "red", size = "sm")
    
  )
}
