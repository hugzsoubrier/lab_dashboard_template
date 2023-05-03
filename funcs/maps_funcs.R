#this functions uses a data from a given shp, and the diagnostic linelist to create a map of the received samples origins

#this function loads the right .shp file depending on the selected admin_level 
shp_loading <- function(admin_level) {
  
  #Create the vectors to use
  adminvars <- c("home_country", "admin_1", "admin_2", "admin_3")
  
  admin_string <- paste0("^", admin_level, "$")
  
  admin_number <- if (admin_level == "admin_1") { 1 
    
  } else if (admin_level == "admin_2") { 2 
    
  } else if (admin_level == "admin_3") { 3
    
  } 
  
  name_var <- paste0("NAME_", admin_number)
  name_var_string <- paste0("^", name_var, "$")
  
  #read and clean spatial data according to admin level
  if ( admin_level == "admin_1") { 
    
    guinea_shp <- st_read("Data/Spatial/GIN_shp/gadm36_GIN_1.shp") %>% 
      dplyr::select(matches(name_var_string), geometry) %>% 
      dplyr::rename("admin_level" = name_var)
    
  } else if (admin_level == "admin_2" ) {
    
    guinea_shp <- st_read("Data/Spatial/GIN_shp/gadm36_GIN_2.shp") %>% 
      dplyr::select(matches(name_var_string), geometry) %>% 
      dplyr::rename("admin_level" = name_var)
    
  } else { 
    
    guinea_shp <- st_read("Data/Spatial/GIN_shp/gadm36_GIN_3.shp") %>% 
      dplyr::select(matches(name_var_string), geometry) %>% 
      dplyr::rename("admin_level" = name_var)
  }
  
  return(guinea_shp)
  
}

# Map of received samples------------------------------------------------------------------
leaflet_map_origins <- function(data, admin_level, shp_data) {
  
  #Create the vectors of geographical variable
  adminvars <- c("home_country", "admin_1", "admin_2", "admin_3")
  
  if (nrow(data) == 0) { 
    
    return("No received samples")
    
  } else {
    
    #prepare and summarise the data by selected admin level
    n_data <- data %>% 
      
      select(adminvars) %>% 
      
      count(data[admin_level]) %>% 
      
      rename("admin_level"  = admin_level)
    
    #merge summary data and spatial data
    data_sf <- dplyr::left_join(shp_data, n_data, by = "admin_level" )
    
    #create a labels variable to be used in sprintsf 
    data_sf <- data_sf %>% 
      
      mutate(labels = case_when(is.na(n) ~ paste0( "<b>", admin_level, "</b>", "<br>", "no samples received"), 
                                T ~ paste0( "<b>", admin_level, "</b>", "<br>", n, " samples")))
    
    #create labels to be displayed
    labels <- sprintf(data_sf$labels) %>% lapply(htmltools::HTML)
    
    # create numeric palette 
    pal <- colorNumeric(palette = "YlOrRd" , domain = data_sf$n, na.color = "#CBCBCB")
    
    leaflet(data_sf, 
            
            options = leafletOptions(zoomControl = FALSE, 
                                     minZoom = 6.5, 
                                     maxZoom = 6.5, 
                                     dragging = F)) %>% 
      
      addPolygons(fillColor = ~pal(n),
                  weight  = 1.3,
                  color = "black", 
                  highlightOptions = highlightOptions(weight = 5, 
                                                      color = "red", 
                                                      bringToFront = T, 
                                                      opacity = .5), 
                  label = labels,
                  
                  labelOptions = labelOptions(
                    
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    direction = "auto" )) %>% 
      
      addLegend(pal = pal, 
                title = "N",
                values = ~n, 
                opacity = .4) %>% 
      
      addControl(paste0(html = "<span style ='font-size:10px;color:#34495E;'>Chloropleth map of received samples by <b>", admin_level , "</b></span>"), 
                 position = "bottomleft")
  }
  
} 

# map of positives samples  -------------------------------------------------------------------
leaflet_map_pos <- function(data, admin_level, pathogen, shp_data) {
  
  #Create the vectors of geographical variable
  adminvars <- c("home_country", "admin_1", "admin_2", "admin_3")
  
  #prepare and summarise the data by selected admin level
  n_data <- data %>% 
    
    select(adminvars) %>% 
    
    count(data[admin_level]) %>% 
    
    dplyr::rename("admin_level" = admin_level) 
  
  #merge summary data and spatial data
  
  data_sf <- dplyr::left_join(shp_data, n_data, by = "admin_level") %>%
    
    #create a label var to display in map 
    mutate(labels = case_when( is.na(n) ~ paste0( "<b>", admin_level, "</b>", "<br>", "no positive samples"), 
                               T ~ paste0( "<b>", admin_level, "</b>", "<br>", n, " positive samples"))) 
  
  #create labels to be displayed
  labels <- sprintf(data_sf$labels) %>% lapply(htmltools::HTML)
  
  #define the breaks depending on the number of samples
  pal <- colorBin(palette = "YlOrRd" , domain = data_sf$n, na.color = "#CBCBCB") 
  
  # create map
  leaflet(data_sf, 
          
          options = leafletOptions(zoomControl = FALSE, 
                                   minZoom = 6.5, 
                                   maxZoom = 6.5, 
                                   dragging = F)) %>% 
    
    addPolygons(fillColor = ~pal(n),
                weight  = 1.3,
                color = "black", 
                highlightOptions = highlightOptions(weight = 5, 
                                                    color = "red", 
                                                    bringToFront = T, 
                                                    opacity = .5), 
                label = labels,
                
                labelOptions = labelOptions(
                  
                  style = list("font-weight" = "normal", padding = "3px 8px"), 
                  direction = "auto" )) %>% 
    
    addLegend(pal = pal, values = ~ n, opacity = .4) %>% 
    
    
    addControl(paste0(html = "<span style ='font-size:10px;color:#34495E;'>Chloropleth map of positive cases for <b>", pathogen, "</b> by <b>", admin_level , "</b></span>"), 
               position = "bottomleft")
  
}
