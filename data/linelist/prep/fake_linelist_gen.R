#this script is used to create fake linelist data based on my linelists designed in Guinea

pacman::p_load(rio, 
               ids,
               sf,
               tidyverse)
############################################# LABORATORY 1 ####################################################

#import the fake data 

data <- import("data/linelist/prep/fake_diagnostic_linelist_variables.xlsx", skip = 3)

glimpse(data)

# generate random IDs -----------------------------------------------------

data <- data %>% mutate(lab_id = ids::random_id(2500, bytes = 4), 
                        patient_id = ids::random_id(2500, bytes = 5))

#generate random names

names <- ids::adjective_animal(2500, max_len = 6)

names <- data.frame("full_names" = names) %>% tibble()

names <- cbind(names, stringr::str_split_fixed(names$full_names, "_", n = 2) ) %>% rename( "first_name" = "1", "surname" = "2")

data <- data %>% mutate(surname = names$surname, 
                        first_name = names$first_name)


# Demographics ------------------------------------------------------------

#age 
data <- data %>% mutate(age_unit = sample(c("months", "year", NA_character_), 2500, replace = T, prob = c(0.2, 0.7, 0.1)), 
                        
                        age = ifelse(is.na(age_unit), NA, ifelse(age_unit == "months", sample(1:11, replace = T), sample(1:99, replace = T))))

#gender 
data <- data %>% mutate(gender = sample(c("male", "female", NA_character_), 2500, replace = T, prob = c(0.55, 0.4, 0.05)))

#profession
prof <- tibble(profession = c("unemployed", "household", "tailor", "health_related", "business", "merchant", "farmer", "musician", "driver", "construction", "carpenter", NA))

# probabilities vectors
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 12)
sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )

prof <- prof %>% mutate(prob = prob)

data <- data %>% mutate(profession = sample(prof$profession, 2500, replace = T, prob = prof$prob))


# Administrative ----------------------------------------------------------

#read shapefile 

guinea_3 <- st_read("data/spatial/gin_shp/gadm36_gin_3.shp")

data <- data %>% mutate(home_country = sample(c(unique(guinea_3$NAME_0), "country 2", NA_character_), replace = T, 2500, prob = c(.8, .05, .05)))

#fill admin variables using probabilities and case_when
data <- data %>% mutate( 
  
  #admin_1
  admin_1 = case_when( home_country == "Guinea" ~ sample(c("Conakry", "Faranah", "Mamou", "Kindia", "Nzérékoré"), 
                                                         2500, 
                                                         replace = TRUE, 
                                                         prob = c(0.3, 0.2, 0.1, 0.1 ,0.3)), 
                       T ~ NA_character_),
  
  #admin_2
  admin_2 = case_when(admin_1 == "Conakry" ~ 
                        
                        sample( unique(filter(guinea_3, NAME_1 == "Conakry")$NAME_2), 
                               2500, 
                               replace = T, 
                               prob = 1 ),
                      
                      admin_1 == "Faranah" ~ 
                        
                        sample( unique(filter(guinea_3, NAME_1 == "Faranah")$NAME_2), 
                               2500, 
                               replace = T, 
                               prob = c(0.2, 0.2, 0.3, 0.3)), 
                      
                      admin_1 == "Mamou" ~ 
                        
                        sample(unique(filter(guinea_3, NAME_1 == "Mamou")$NAME_2), 
                               2500, 
                               replace = T, 
                               prob = c(0.2, 0.6, 0.2)), 
                      
                      admin_1 == "Kindia" ~ 
                        
                        sample(unique(filter(guinea_3, NAME_1 == "Kindia")$NAME_2), 
                               2500, 
                               replace = T, 
                               prob = c(0.25, 0.25, 0.3, 0.1, 0.1)), 
                      
                      admin_1 == "Nzérékoré" ~ 
                        
                        sample(unique(filter(guinea_3, NAME_1 == "Nzérékoré")$NAME_2), 
                               2500, 
                               replace = T, 
                               prob = c(0.15, 0.15, 0.2, 0.1, 0.3, 0.1))
                    
  ), 
  
  #admin_3
  admin_3 = case_when(
    
    admin_2 == "Conakry"~ sample(c(unique(filter(guinea_3, NAME_2 == "Conakry")$NAME_3), NA), 
                                      2500, 
                                      replace = T, 
                                      prob = c(0.15, 0.15, 0.3, 0.2, 0.19, 0.01)),
    
    admin_2 == "Dabola"~ sample(c(unique(filter(guinea_3, NAME_2 == "Dabola")$NAME_3), NA), 
                                      2500, 
                                      replace = T),
    
    admin_2 == "Dinguiraye"~ sample(c(unique(filter(guinea_3, NAME_2 == "Dinguiraye")$NAME_3), NA), 
                                      2500, 
                                      replace = T),
    
    admin_2 == "Faranah"~ sample(c(unique(filter(guinea_3, NAME_2 == "Faranah")$NAME_3), NA), 
                                    2500, 
                                    replace = T),
    
    admin_2 == "Kissidougou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Kissidougou")$NAME_3), NA), 
                                    2500, 
                                    replace = T),
    
    admin_2 == "Dalaba"~ sample(c(unique(filter(guinea_3, NAME_2 == "Dalaba")$NAME_3), NA), 
                                     2500, 
                                     replace = T),
    
    admin_2 == "Mamou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Mamou")$NAME_3), NA), 
                                     2500, 
                                     replace = T),
    
    admin_2 == "Pita"~ sample(c(unique(filter(guinea_3, NAME_2 == "Pita")$NAME_3), NA), 
                                     2500, 
                                     replace = T),
    
    admin_2 == "Coyah"~ sample(c(unique(filter(guinea_3, NAME_2 == "Coyah")$NAME_3), NA), 
                              2500, 
                              replace = T),
    
    admin_2 == "Dubréka"~ sample(c(unique(filter(guinea_3, NAME_2 == "Dubréka")$NAME_3), NA), 
                              2500, 
                              replace = T),
    
    admin_2 == "Forécariah"~ sample(c(unique(filter(guinea_3, NAME_2 == "Forécariah")$NAME_3), NA), 
                              2500, 
                              replace = T),
    
    admin_2 == "Kindia"~ sample(c(unique(filter(guinea_3, NAME_2 == "Kindia")$NAME_3), NA), 
                              2500, 
                              replace = T),
    
    admin_2 == "Télimélé"~ sample(c(unique(filter(guinea_3, NAME_2 == "Télimélé")$NAME_3), NA), 
                              2500, 
                              replace = T),
    
    admin_2 == "Beyla"~ sample(c(unique(filter(guinea_3, NAME_2 == "Beyla")$NAME_3), NA), 
                                  2500, 
                                  replace = T),
    
    admin_2 == "Guéckédou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Guéckédou")$NAME_3), NA), 
                                  2500, 
                                  replace = T),
    
    admin_2 == "Lola"~ sample(c(unique(filter(guinea_3, NAME_2 == "Lola")$NAME_3), NA), 
                                  2500, 
                                  replace = T),
    
    admin_2 == "Macenta"~ sample(c(unique(filter(guinea_3, NAME_2 == "Macenta")$NAME_3), NA), 
                                  2500, 
                                  replace = T),
    
    admin_2 == "Nzérékoré"~ sample(c(unique(filter(guinea_3, NAME_2 == "Nzérékoré")$NAME_3), NA), 
                                  2500, 
                                  replace = T),
    
    admin_2 == "Yamou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Yamou")$NAME_3), NA), 
                                  2500, 
                                  replace = T)), 
  
  
  #admin_4
  admin_4 = sample(c("village1", "village2", "village3", "village4", "village5", NA ), 
                   2500, 
                   replace = T, 
                   prob = c(0.15, 0.15, 0.15, 0.15, 0.05, 0.35)),
  
  #sent_by 
  sent_by = sample(c("regional hospital", "health clinic", "Red cross", "sampled by lab"), 2500, c(0.4, 0.2, 0.1, 0.3), replace = T)
  
)

# Dates  ------------------------------------------------------------------

# create the var date_reception - No NA over two years. then from it create other dates by substracting random number of days

data <- data %>% mutate(
  
  date_reception = sample(c(seq(as.Date("2022/01/01"), as.Date("2023/12/31"), by = "day"), NA), 2500, replace = T) )

#random numbers of days to be substracted to previous dates

#date sampling

# probabilities vectors
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 8)
sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob

sampling <- sample(c(seq(0, 7)), 2500, replace = T, prob = prob ) #reception - sampling


#date entry

# probabilities vectors
set.seed(3333333)
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 6)
prob <- append(prob, 80)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

entry <- sample(c(seq(0, 5), NA), 2500, replace = T, prob = prob ) #reception - entry


#date death

# probabilities vectors
set.seed(24758493)
x <- seq(from = 1, to = 100, by = 1)
prob <- sample(x, 7)
prob <- append(prob, 900)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

death <- sample(c(seq(0, 6), NA), 2500, replace = T, prob = prob)

#date onset

# probabilities vectors
set.seed(1222)
x <- seq(from = 1, to = 100, by = 1)
prob <- sample(x, 20)
prob <- append(prob, 99)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

onset <- sample(c(seq(0, 19), NA), 2500, replace = T, prob = prob )

#apply the substraction vectors  
data <- data %>% mutate(
  
  date_sampling = date_reception - sampling,
  date_entry = date_sampling - entry,
  date_onset = date_entry - onset,
  date_death = date_sampling + death
  
)


# Sample data -------------------------------------------------------------

#create a random combination of requested tests 

request_1 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 2500, replace = T, prob = c(0.4, 0.15, 0.05, 0.2, 0.1, 0.1) )
request_2 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 2500, replace = T, prob = c(0.05, 0.05, 0.05, 0.1, 0.05, 0.7) )
request_3 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 2500, replace = T, prob = c(0.02, 0.02, 0.01, 0.05, 0.1, 0.8) )

requests <- data.frame(request_1, request_2, request_3)

requests <- requests  %>% mutate(final = ifelse(request_1 != request_2 & request_2 != request_3 & request_1 != request_3, paste(request_1,request_2,request_3, sep = ", "), request_1), 
                                 
                                 
                                 final = str_remove_all(final, ", none|none")) 

#mutate sample data 
data <- data %>% mutate(
  
  patient_condition = sample(c( NA ,"low","moderate","acute", "critical"), 2500, replace = T, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)), 
  
  sample_type = sample(c("whole blood","plasma (EDTA)","buccal swab", "nasopharyngeal swab", "serum"), 2500, replace = T, prob = c(0.2, 0.5, 0.1, 0.1, 0.1)), 
  
  sample_condition = sample(c("good", "ok", "hemolysed", NA), 2500, replace = T, prob = c(0.7, 0.1, 0.05, 0.15)), 
  
  aliquots = sample(1:2, 2500, replace = T, prob = c(0.4, 0.6)), 
  
  requested_diagnostic = requests$final
  
)

# Tests data --------------------------------------------------------------


#Test 1 
data <- data %>% mutate(result_test1_rt_pcr_ebola = sample(c(NA, "negative", "positive"), 2500, replace = T, prob = c( 0.4, 0.5, 0.1)), 
                        ct_test1_rt_pcr_ebola = ifelse(result_test1_rt_pcr_ebola == "positive", sample(seq(from = 20, to = 40, by = 0.1), replace = T), NA), 
                        
                        result_test1_rt_pcr_marv = case_when(result_test1_rt_pcr_ebola == "negative" ~ sample(c("negative", "positive", NA), 2500, replace = T, prob = c(0.9, 0.001, 0.099)), 
                                                             result_test1_rt_pcr_ebola == "positive" ~ sample(c("negative", NA), replace = T, 2500, prob = c(0.95, 0.05)), 
                                                             T ~ NA_character_), 
                        
                        ct_test1_rt_pcr_marv = ifelse(result_test1_rt_pcr_marv == "positive", sample(seq(from = 20, to = 40, by = 0.1), replace = T), NA), 
                        
                        date_test1_rt_pcr = ifelse(!is.na(result_test1_rt_pcr_ebola) | !is.na(result_test1_rt_pcr_marv), sample(1:5, replace = T), NA ), 
                        
                        date_test1_rt_pcr = date_reception + date_test1_rt_pcr)

#Test 2

data <- data %>% mutate(
  
  result_test2_rt_pcr_lasv_gpc = sample(c("positive", "negative", NA), 2500, replace = T, prob = c(0.2, 0.6, 0.2)), 
  ct_test2_rt_pcr_lasv_gpc = ifelse(result_test2_rt_pcr_lasv_gpc == "positive", sample(seq(20, 40, by = 0.1), replace = T), NA), 
  
  result_test2_rt_pcr_lasv_l = case_when(result_test2_rt_pcr_lasv_gpc == "positive" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(0.8, 0.15, 0.05)), 
                                         
                                         result_test2_rt_pcr_lasv_gpc == "negative" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(0.15, 0.8, 0.05))), 
  
  ct_test2_rt_pcr_lasv_l = ifelse(result_test2_rt_pcr_lasv_l == "positive", sample(seq(20, 40, by = .1), replace = T), NA), 
  
  result_test2_rt_pcr_lasv_final = ifelse(result_test2_rt_pcr_lasv_gpc == "positive" | result_test2_rt_pcr_lasv_l == "positive", "positive", ifelse(is.na(result_test2_rt_pcr_lasv_gpc) & is.na(result_test2_rt_pcr_lasv_l), NA, "negative")), 
  
  date_test2_rt_pcr = ifelse(!is.na(result_test2_rt_pcr_lasv_final), sample(1:5, replace = T), NA), 
  
  date_test2_rt_pcr = date_reception + date_test2_rt_pcr)

#test 3

data <- data %>% mutate(result_test3_gx_ebov_target1 = sample(c("positive", "negative", NA), replace = T, 2500, prob = c(.1, .3, .6)), 
                        
                        result_test3_gx_ebov_target1 = ifelse(!is.na(result_test1_rt_pcr_ebola), NA, result_test3_gx_ebov_target1), 
                        ct_test3_gx_ebov_target1 = ifelse(result_test3_gx_ebov_target1 == "positive", sample(seq(20, 40, by = 0.1), replace = T), NA), 
                        
                        result_test3_gx_ebov_target2 = case_when(result_test3_gx_ebov_target1 == "positive" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(0.9, 0.05, 0.05)), 
                                                                 result_test3_gx_ebov_target1 == "negative" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(0.01, 0.9, 0.99)), 
                                                                 T ~ NA_character_),
                        
                        ct_test3_gx_ebov_target2 = ifelse(result_test3_gx_ebov_target2 == "positive", sample(seq(15, 35, by = 0.2), replace = T), NA), 
                        
                        result_test3_gx_ebov_final = ifelse(result_test3_gx_ebov_target2 == "positive" | result_test3_gx_ebov_target1 == "positive", "positive", ifelse(is.na(result_test3_gx_ebov_target1) & is.na(result_test3_gx_ebov_target2), NA, "negative")), 
                        
                        date_test3_gx = ifelse(!is.na(result_test3_gx_ebov_final),sample(0:3, replace = T), NA ), 
                        
                        date_test3_gx = date_reception + date_test3_gx
)

#Test 4 

data <- data %>% mutate(result_test4_rt_pcr_yfv = sample(c("positive", "negative", NA), replace = T, 2500, prob = c(0.05, 0.2, 0.75)), 
                        ct_test4_rt_pcr_yfv = ifelse(result_test4_rt_pcr_yfv == "positive", sample(seq(25, 34, by = .5), replace = T), NA), 
                        date_test4_rt_pcr = ifelse( !is.na(result_test4_rt_pcr_yfv),sample(0:6, 2500, replace = T), NA), 
                        date_test4_rt_pcr = date_reception + date_test4_rt_pcr)

#Test 5 
data <- data %>% mutate(result_test5_rt_pcr_yfv = sample(c("positive", "negative", NA), replace = T, 2500, prob = c(0.02, 0.1, 0.88)), 
                        ct_test5_rt_pcr_yfv = ifelse(result_test5_rt_pcr_yfv == "positive", sample(seq(25, 34, by = .5), replace = T), NA), 
                        date_test5_rt_pcr = ifelse( !is.na(result_test5_rt_pcr_yfv),sample(0:6, 2500, replace = T), NA), 
                        date_test5_rt_pcr = date_reception + date_test5_rt_pcr)

#Test 6 
data <- data %>% mutate(result_test6_rt_pcr_denv = sample(c("positive", "negative", NA), replace = T, 2500, prob = c(0.05, 0.05, 0.90)), 
                        ct_test6_rt_pcr_denv = ifelse(result_test6_rt_pcr_denv == "positive", sample(seq(25, 34, by = .5), replace = T), NA), 
                        date_test6_rt_pcr = ifelse( !is.na(result_test6_rt_pcr_denv),sample(0:9, 2500, replace = T), NA), 
                        date_test6_rt_pcr = date_reception + date_test6_rt_pcr)

#Test 7
data <- data %>% mutate(result_test7_rdt_denv_ns1 = sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.1, .3, .6)),
                        result_test7_rdt_denv_igg = case_when(result_test7_rdt_denv_ns1 == "negative" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                              result_test7_rdt_denv_ns1 == "positive" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                              T ~ NA_character_), 
                        result_test7_rdt_denv_igm = case_when(result_test7_rdt_denv_ns1 == "negative" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                              result_test7_rdt_denv_ns1 == "positive" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                              T ~ NA_character_), 
                        
                        date_test7_rdt = ifelse(!is.na(result_test7_rdt_denv_ns1), sample(0:2, replace = T, prob = c(.8,.15,.05)), NA), 
                        date_test7_rdt = date_reception + date_test7_rdt)


#Test 8
data <- data %>% mutate(result_test8_rdt_malaria_falciparum = sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.1, .6, .3)),
                        result_test8_rdt_malaria_mixed = case_when(result_test8_rdt_malaria_falciparum == "negative" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                                   result_test8_rdt_malaria_falciparum == "positive" ~ sample(c("positive", "negative", NA), 2500, replace = T, prob = c(.05, .9, .05)), 
                                                                   T ~ NA_character_), 
                        date_test8_rdt = ifelse(!is.na(result_test8_rdt_malaria_falciparum), sample(0:2, replace = T, prob = c(.8,.15,.05)), NA), 
                        date_test8_rdt = date_reception + date_test8_rdt)

#export the linelist ------------------------------

export(data, "data/linelist/fake_diagnostic_linelist_lab1.csv", na = NA)


############################################# LABORATORY 2 ####################################################

#import the fake data2 

data2 <- import("data/linelist/prep/fake_diagnostic_linelist_variables.xlsx", skip = 3)[1:1250,]

glimpse(data2)

# generate random IDs -----------------------------------------------------

data2 <- data2 %>% mutate(lab_id = ids::random_id(1250, bytes = 4), 
                          patient_id = ids::random_id(1250, bytes = 5))

#generate random names

names <- ids::adjective_animal(1250, max_len = 6)

names <- data.frame("full_names" = names) %>% tibble()

names <- cbind(names, stringr::str_split_fixed(names$full_names, "_", n = 2) ) %>% rename( "first_name" = "1", "surname" = "2")

data2 <- data2 %>% mutate(surname = names$surname, 
                          first_name = names$first_name)


# Demographics ------------------------------------------------------------

#age 
data2 <- data2 %>% mutate(age_unit = sample(c("months", "year", NA_character_), 1250, replace = T, prob = c(0.2, 0.7, 0.1)), 
                          
                          age = ifelse(is.na(age_unit), NA, ifelse(age_unit == "months", sample(1:11, replace = T), sample(1:99, replace = T))))

#gender 
data2 <- data2 %>% mutate(gender = sample(c("male", "female", NA_character_), 1250, replace = T, prob = c(0.55, 0.4, 0.05)))

#profession
prof <- tibble(profession = c("unemployed", "household", "tailor", "health_related", "business", "merchant", "farmer", "musician", "driver", "construction", "carpenter", NA))

# probabilities vectors
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 12)
sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )

prof <- prof %>% mutate(prob = prob)

data2 <- data2 %>% mutate(profession = sample(prof$profession, 1250, replace = T, prob = prof$prob))



# Administrative ----------------------------------------------------------

#read shapefile 

guinea_3 <- st_read("data/spatial/gin_shp/gadm36_gin_3.shp")

data2 <- data2 %>% mutate(home_country = sample(c(unique(guinea_3$NAME_0), "country 2", NA_character_), replace = T, 1250, prob = c(.8, .05, .05)))

#fill admin variables using probabilities and case_when
data2 <- data2 %>% mutate( 
  
  #admin_1
  admin_1 = case_when( home_country == "Guinea" ~ sample(c("Kindia", "Nzérékoré"), 
                                                         1250, 
                                                         replace = TRUE, 
                                                         prob = c(0.1, 0.9)), 
                       T ~ NA_character_),
  
  #admin_2
  admin_2 = case_when(admin_1 == "Kindia" ~ 
                        
                        sample(unique(filter(guinea_3, NAME_1 == "Kindia")$NAME_2), 
                               1250, 
                               replace = T, 
                               prob = c(0.2, 0.1, 0.4, 0.15, 0.15)), 
                      
                      admin_1 == "Nzérékoré" ~ 
                        
                        sample(unique(filter(guinea_3, NAME_1 == "Nzérékoré")$NAME_2), 
                               1250, 
                               replace = T, 
                               prob = c(0.05, 0.05, 0.4, 0.3, 0.1, 0.1)) ), 
  
  #admin_3
  admin_3 = case_when(
    
    admin_2 == "Coyah"~ sample(c(unique(filter(guinea_3, NAME_2 == "Coyah")$NAME_3), NA), 
                               1250, 
                               replace = T),
    
    admin_2 == "Dubréka"~ sample(c(unique(filter(guinea_3, NAME_2 == "Dubréka")$NAME_3), NA), 
                                 1250, 
                                 replace = T),
    
    admin_2 == "Forécariah"~ sample(c(unique(filter(guinea_3, NAME_2 == "Forécariah")$NAME_3), NA), 
                                    1250, 
                                    replace = T),
    
    admin_2 == "Kindia"~ sample(c(unique(filter(guinea_3, NAME_2 == "Kindia")$NAME_3), NA), 
                                1250, 
                                replace = T),
    
    admin_2 == "Télimélé"~ sample(c(unique(filter(guinea_3, NAME_2 == "Télimélé")$NAME_3), NA), 
                                  1250, 
                                  replace = T),
    
    admin_2 == "Beyla"~ sample(c(unique(filter(guinea_3, NAME_2 == "Beyla")$NAME_3), NA), 
                               1250, 
                               replace = T),
    
    admin_2 == "Guéckédou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Guéckédou")$NAME_3), NA), 
                                   1250, 
                                   replace = T),
    
    admin_2 == "Lola"~ sample(c(unique(filter(guinea_3, NAME_2 == "Lola")$NAME_3), NA), 
                              1250, 
                              replace = T),
    
    admin_2 == "Macenta"~ sample(c(unique(filter(guinea_3, NAME_2 == "Macenta")$NAME_3), NA), 
                                 1250, 
                                 replace = T),
    
    admin_2 == "Nzérékoré"~ sample(c(unique(filter(guinea_3, NAME_2 == "Nzérékoré")$NAME_3), NA), 
                                   1250, 
                                   replace = T),
    
    admin_2 == "Yamou"~ sample(c(unique(filter(guinea_3, NAME_2 == "Yamou")$NAME_3), NA), 
                               1250, 
                               replace = T)), 
  
  
  #admin_4
  admin_4 = sample(c("village1", "village2", "village3", "village4", "village5", NA ), 
                   1250, 
                   replace = T, 
                   prob = c(0.15, 0.15, 0.15, 0.15, 0.05, 0.35)),
  
  #sent_by 
  sent_by = sample(c("regional hospital", "health clinic", "Red cross", "sampled by lab"), 1250, c(0.4, 0.2, 0.1, 0.3), replace = T)
  
)

# Dates  ------------------------------------------------------------------

# create the var date_reception - No NA over two years. then from it create other dates by substracting random number of days

data2 <- data2 %>% mutate(
  
  date_reception = sample(c(seq(as.Date("2022/01/01"), as.Date("2023/12/31"), by = "day"), NA), 1250, replace = T) )

#random numbers of days to be substracted to previous dates

#date sampling

# probabilities vectors
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 8)
sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob

sampling <- sample(c(seq(0, 7)), 1250, replace = T, prob = prob ) #reception - sampling


#date entry

# probabilities vectors
set.seed(3333333)
x <- seq( from = 1, to = 100, by = 1)
prob <- sample(x, 6)
prob <- append(prob, 80)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

entry <- sample(c(seq(0, 5), NA), 1250, replace = T, prob = prob ) #reception - entry


#date death

# probabilities vectors
set.seed(24758493)
x <- seq(from = 1, to = 100, by = 1)
prob <- sample(x, 7)
prob <- append(prob, 900)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

death <- sample(c(seq(0, 6), NA), 1250, replace = T, prob = prob)

#date onset

# probabilities vectors
set.seed(1222)
x <- seq(from = 1, to = 100, by = 1)
prob <- sample(x, 20)
prob <- append(prob, 99)#add NA prob manually

sum_prob <- sum(prob)
prob <- round(digits = 2, prob/sum_prob )
prob 

onset <- sample(c(seq(0, 19), NA), 1250, replace = T, prob = prob )

#apply the substraction vectors  
data2 <- data2 %>% mutate(
  
  date_sampling = date_reception - sampling,
  date_entry = date_sampling - entry,
  date_onset = date_entry - onset,
  date_death = date_sampling + death
  
)


# Sample data2 -------------------------------------------------------------

#create a random combination of requested tests 

request_1 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 1250, replace = T, prob = c(0.4, 0.15, 0.05, 0.2, 0.1, 0.1) )
request_2 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 1250, replace = T, prob = c(0.05, 0.05, 0.05, 0.1, 0.05, 0.7) )
request_3 <- sample(c("Ebola virus", "Marburg virus", "Lassa virus", "Malaria", "Yellow Fever", "none"), 1250, replace = T, prob = c(0.02, 0.02, 0.01, 0.05, 0.1, 0.8) )

requests <- data.frame(request_1, request_2, request_3)

requests <- requests  %>% mutate(final = ifelse(request_1 != request_2 & request_2 != request_3 & request_1 != request_3, paste(request_1,request_2,request_3, sep = ", "), request_1), 
                                 
                                 
                                 final = str_remove_all(final, ", none|none")) 

#mutate sample data2 
data2 <- data2 %>% mutate(
  
  patient_condition = sample(c( NA ,"low","moderate","acute", "critical"), 1250, replace = T, prob = c(0.3, 0.2, 0.2, 0.2, 0.1)), 
  
  sample_type = sample(c("whole blood","plasma (EDTA)","buccal swab", "nasopharyngeal swab", "serum"), 1250, replace = T, prob = c(0.2, 0.5, 0.1, 0.1, 0.1)), 
  
  sample_condition = sample(c("good", "ok", "hemolysed", NA), 1250, replace = T, prob = c(0.7, 0.1, 0.05, 0.15)), 
  
  aliquots = sample(1:2, 1250, replace = T, prob = c(0.4, 0.6)), 
  
  requested_diagnostic = requests$final
  
)

# Tests data2 --------------------------------------------------------------


#Test 1 
data2 <- data2 %>% mutate(result_test1_rt_pcr_ebola = sample(c(NA, "negative", "positive"), 1250, replace = T, prob = c( 0.4, 0.5, 0.1)), 
                          ct_test1_rt_pcr_ebola = ifelse(result_test1_rt_pcr_ebola == "positive", sample(seq(from = 20, to = 40, by = 0.1), replace = T), NA), 
                          
                          result_test1_rt_pcr_marv = case_when(result_test1_rt_pcr_ebola == "negative" ~ sample(c("negative", "positive", NA), 1250, replace = T, prob = c(0.9, 0.001, 0.099)), 
                                                               result_test1_rt_pcr_ebola == "positive" ~ sample(c("negative", NA), replace = T, 1250, prob = c(0.95, 0.05)), 
                                                               T ~ NA_character_), 
                          
                          ct_test1_rt_pcr_marv = ifelse(result_test1_rt_pcr_marv == "positive", sample(seq(from = 20, to = 40, by = 0.1), replace = T), NA), 
                          
                          date_test1_rt_pcr = ifelse(!is.na(result_test1_rt_pcr_ebola) | !is.na(result_test1_rt_pcr_marv), sample(1:5, replace = T), NA ), 
                          
                          date_test1_rt_pcr = date_reception + date_test1_rt_pcr)

#Test 2

data2 <- data2 %>% mutate(
  
  result_test2_rt_pcr_lasv_gpc = sample(c("positive", "negative", NA), 1250, replace = T, prob = c(0.2, 0.6, 0.2)), 
  ct_test2_rt_pcr_lasv_gpc = ifelse(result_test2_rt_pcr_lasv_gpc == "positive", sample(seq(20, 40, by = 0.1), replace = T), NA), 
  
  result_test2_rt_pcr_lasv_l = case_when(result_test2_rt_pcr_lasv_gpc == "positive" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(0.8, 0.15, 0.05)), 
                                         
                                         result_test2_rt_pcr_lasv_gpc == "negative" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(0.15, 0.8, 0.05))), 
  
  ct_test2_rt_pcr_lasv_l = ifelse(result_test2_rt_pcr_lasv_l == "positive", sample(seq(20, 40, by = .1), replace = T), NA), 
  
  result_test2_rt_pcr_lasv_final = ifelse(result_test2_rt_pcr_lasv_gpc == "positive" | result_test2_rt_pcr_lasv_l == "positive", "positive", ifelse(is.na(result_test2_rt_pcr_lasv_gpc) & is.na(result_test2_rt_pcr_lasv_l), NA, "negative")), 
  
  date_test2_rt_pcr = ifelse(!is.na(result_test2_rt_pcr_lasv_final), sample(1:5, replace = T), NA), 
  
  date_test2_rt_pcr = date_reception + date_test2_rt_pcr)

#test 3

data2 <- data2 %>% mutate(result_test3_gx_ebov_target1 = sample(c("positive", "negative", NA), replace = T, 1250, prob = c(.1, .3, .6)), 
                          
                          result_test3_gx_ebov_target1 = ifelse(!is.na(result_test1_rt_pcr_ebola), NA, result_test3_gx_ebov_target1), 
                          ct_test3_gx_ebov_target1 = ifelse(result_test3_gx_ebov_target1 == "positive", sample(seq(20, 40, by = 0.1), replace = T), NA), 
                          
                          result_test3_gx_ebov_target2 = case_when(result_test3_gx_ebov_target1 == "positive" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(0.9, 0.05, 0.05)), 
                                                                   result_test3_gx_ebov_target1 == "negative" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(0.01, 0.9, 0.99)), 
                                                                   T ~ NA_character_),
                          
                          ct_test3_gx_ebov_target2 = ifelse(result_test3_gx_ebov_target2 == "positive", sample(seq(15, 35, by = 0.2), replace = T), NA), 
                          
                          result_test3_gx_ebov_final = ifelse(result_test3_gx_ebov_target2 == "positive" | result_test3_gx_ebov_target1 == "positive", "positive", ifelse(is.na(result_test3_gx_ebov_target1) & is.na(result_test3_gx_ebov_target2), NA, "negative")), 
                          
                          date_test3_gx = ifelse(!is.na(result_test3_gx_ebov_final),sample(0:3, replace = T), NA ), 
                          
                          date_test3_gx = date_reception + date_test3_gx
)

#Test 4 
data2 <- data2 %>% mutate(result_test4_rt_pcr_yfv = sample(c("positive", "negative", NA), replace = T, 1250, prob = c(0.05, 0.2, 0.75)), 
                          ct_test4_rt_pcr_yfv = ifelse(result_test4_rt_pcr_yfv == "positive", sample(seq(25, 34, by = .5), replace = T), NA), 
                          date_test4_rt_pcr = ifelse( !is.na(result_test4_rt_pcr_yfv),sample(0:6, 1250, replace = T), NA), 
                          date_test4_rt_pcr = date_reception + date_test4_rt_pcr)

#Test 4 
data2 <- data2 %>% mutate(result_test5_rt_pcr_yfv = sample(c("positive", "negative", NA), replace = T, 1250, prob = c(0.05, 0.2, 0.75)), 
                          ct_test5_rt_pcr_yfv = ifelse(result_test5_rt_pcr_yfv == "positive", sample(seq(25, 34, by = .5), replace = T), NA), 
                          date_test5_rt_pcr = ifelse(!is.na(result_test5_rt_pcr_yfv),sample(0:6, 1250, replace = T), NA), 
                          date_test5_rt_pcr = date_reception + date_test5_rt_pcr)


#Test 8
data2 <- data2 %>% mutate(result_test8_rdt_malaria_falciparum = sample(c("positive", "negative", NA), 1250, replace = T, prob = c(.1, .6, .3)),
                          result_test8_rdt_malaria_mixed = case_when(result_test8_rdt_malaria_falciparum == "negative" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(.05, .9, .05)), 
                                                                     result_test8_rdt_malaria_falciparum == "positive" ~ sample(c("positive", "negative", NA), 1250, replace = T, prob = c(.05, .9, .05)), 
                                                                     T ~ NA_character_), 
                          date_test8_rdt = ifelse(!is.na(result_test8_rdt_malaria_falciparum), sample(0:2, replace = T, prob = c(.8,.15,.05)), NA), 
                          date_test8_rdt = date_reception + date_test8_rdt)

data2 <- data2 %>% select(-c(contains("test6"), contains("test7")))

#export the linelist 
export(data2, "data/linelist/fake_diagnostic_linelist_lab2.csv", na = NA)
















