library(tidyr)
single.to.multiple <- function(r, col){
  scol <- 2
  r[!is.na(r[,col]) & r[,col] == "",col] <- NA
  if(any(is.na(r[,col]))){
    scol <- 3
  }
  test <- r %>% mutate(value = 1) %>% select(X_uuid, one_of(col), value) %>% 
    pivot_wider(names_from = one_of(col), values_from = value)
  names(test)[scol:ncol(test)] <- paste(col, names(test)[scol:ncol(test)], sep = ".")
  test[is.na(test)] <- 0
  r <- cbind(r, test[,scol:ncol(test)])
  return(r)
}
if(F){
  #RECODE SELECT MULTIPLE VARIABLES FROM T/F TO BINARY VARIABLE
  data[c(which(startsWith(names(dummy), "school_handwashing")))] <- 
    ifelse(dummy[c(which(startsWith(names(dummy), "school_handwashing")))] == "TRUE", 1, 0)
  
  dummy[c(which(startsWith(names(dummy), "activities_affected")))] <- 
    ifelse(dummy[c(which(startsWith(names(dummy), "activities_affected")))] == "TRUE", 1, 0)
  
  dummy[c(which(startsWith(names(dummy), "handwashing_access")))] <- 
    ifelse(dummy[c(which(startsWith(names(dummy), "handwashing_access")))] == "TRUE", 1, 0)
  
  dummy[c(which(startsWith(names(dummy), "waste_receptacle")))] <- 
    ifelse(dummy[c(which(startsWith(names(dummy), "waste_receptacle")))] == "TRUE", 1, 0)
  
  data[,c("hhh", "employment_respondent", "displace_status", "displace_status_returnee", "displace_status_idp", "idp_first_place","bottled_water", "access_private_shared_watertank","safety_watertank", "shortages_water_point", "consulted_facilities", "female_consulted_facilities", "share_facility", "toilet_maintained", "sufficient_containers", "visible_sewage", "smell_sewage","household_soap", ""  )]<- ifelse(data[,c("var1,var2")] == "TRUE", 1,0)
}




#CALCULATING AVERAGES (BASED ON GROUP SIZE) FOR THE VULNERABLE POPULATIONS AND FOR THE 
#VARIOUS AGE GROUPS
calc_avgs <- function(avg) {
  avg$people_share_tank_avg <- avg$people_share_tank / avg$household_number
  avg$refilltimes_avg <- avg$refill_times / avg$household_number
  avg$total_shortages_water_point_avg <- avg$total_shortages_water_point / avg$household_number
  avg$total_share_facility_avg <- avg$total_share_facility / avg$household_number
  avg$frequency_diarrhoea_avg <- avg$frequency_diarrhoea / avg$household_number
  avg$frequency_cholora_avg <- avg$frequency_cholora / avg$household_number
  avg$frequency_infection_avg <- avg$frequency_infection/ avg$household_number
  avg$household_member_male_18_avg <- avg$household_member_male_18 / avg$household_number
  avg$household_member_female_18_avg <- avg$household_member_female_18 / avg$household_number
  avg$household_member_male_under18_avg <- avg$household_member_male_under18 / avg$household_number
  avg$household_member_female_under18_avg <- avg$household_member_female_under18 / avg$household_number
  avg$drinking_water_cost_avg<- avg$drinking_water_cost / avg$income_respondent
  return(avg)
}

recodingchoices <-function(r) {
  
  
  # recoding choices for direct reporting on looking for yes answers instead of 1
  yes_indicators = c("visible_sewage", "household_soap", "sufficient_containers","sufficient_hygiene_items", "handwashing_access", "menstrual_hygiene", "experienced_floods", "shelter_affected", "flood_improved")
  for (i in 1:length(yes_indicators)){
    r[,paste0(yes_indicators[i], "_recoded")] <- ifelse(r[,yes_indicators[i]] == "yes", 1, 0)
  }
  
  r$sewage_visability_recoded <- ifelse(r$sewage_visability %in% c("always", "sometimes"), 1, 0)
  r$treat_water_recoded <- ifelse(r$treat_water %in% c("always", "sometimes"), 1, 0)
  
  # 
  # # r$drinking_water_source <- ifelse(rowSums(r[,c("drinking_water_source.network_private","drinking_water_source.network_comm",
  #                             "drinking_water_source.dug_well", "drinking_water_source.prot_well",
  #                             "drinking_water_source.rainwater", "drinking_water_source.river_spring",
  #                             "drinking_water_source.bottled_water","drinking_water_source.river_spring",
  #                             "drinking_water_source.purchase_water", "drinking_water_source.water_trucking",
  #                             "drinking_water_source.illegal_connection", "drinking_water_source.unprot_tank",
  #                             "drinking_water_source.unprot_well",   "drinking_water_source.iunprot_spring",
  #                             "drinking_water_source.surface_water",   "drinking_water_source.other",)]) > 2,3,0)
  
  r$improved_water_source <- ifelse(r$drinking_water_source %in% 
                                      c("network_private", "network_comm", "dug_well", 
                                        "prot_well", "rainwater", "river_spring",
                                        "bottled_water", "purchase_water", "water_trucking"), 1, 0)
  
  # Recoding select one and select multiples for preliminary analysis
  
  r$latrine_accessible <- ifelse(r$latrine_facility.accessible == 1, 1, 0)
  
  r$latrine_facility_all <- ifelse(rowSums(r[,c("latrine_facility.flush", "latrine_facility.drainage",
                                                "latrine_facility.segregation", "latrine_facility.privacy",
                                                "latrine_facility.structure", "latrine_facility.lock",
                                                "latrine_facility.lighting", "latrine_facility.clean",
                                                "latrine_facility.convenient", "latrine_facility.accessible")]) == 10, 1, 0)
  
  r$hygiene_practise_all <-ifelse(rowSums(r[,c("hygiene_practise.critical_times", "hygiene_practise.water_handling",
                                               "hygiene_practise.water_treatment", "hygiene_practise.waste_disposal",
                                               "hygiene_practise.hygiene")]) == 5, 1, 0) 
  
  r$latrine_private_female <- ifelse(r$latrine_facility.privacy == 1 & r$gender_respondent == "female", 1, 
                                     ifelse(r$gender_respondent == "male", NA, 0))
  
  
  r$unprotected_contingency1 <- ifelse(r$water_contingency.none == 1, 0, 1)
  r$unprotected_contingency2 <- ifelse(rowSums(r[,c("secondary_water_source.illegal_connection", 
                                                    "secondary_water_source.unprot_tank",
                                                    "secondary_water_source.unprot_well",
                                                    "secondary_water_source.unprot_spring",
                                                    "secondary_water_source.surface_water")]) > 0, 1, 0)
  
  r$waste_disposal_outside <- ifelse(r$waste_disposal %in% c("inside_residential", 
                                                             "outside_residential", 
                                                             "burying", "burning"), 1, 0)
  
  
  r$coping_mechanisms_family <- ifelse(rowSums(r[,c("sanitation_family.less_preferred", "sanitation_family.dangerous_sanitation", "sanitation_family.open_defecation")])> 0, 1, 0)
  
  r$household_health_problems <- ifelse(rowSums(r[,c("household_health.diarrhoea", "household_health.cholera", "household_health.infection")])> 0, 1, 0)
  
  
  r$gender_respondent_female <- ifelse(r$gender_respondent %in% c("female") > 0, 1, 0)
  
  r$share_facility_recoded <- ifelse(r$share_facility == "yes", 1, 0)
  
  r$sufficient_access_sanitation_recoded <- ifelse(r$sufficient_access_sanitation %in% 
                                                     c("morethan_sufficient", "sufficient"),
                                                   1, 0)
  r$sufficient_access_water_recoded <- ifelse(r$sufficient_access %in% 
                                                c("morethan_sufficient", "sufficient"),
                                              1, 0)
  r$household_shower_recoded <- ifelse(r$household_shower %in% 
                                         c("private_household", "private_shower", 
                                           "private_shower_previous"), 1, 0)
  
  r$waterpoint_time_recoded <- ifelse(r$waterpoint_time %in% c("water_on_premises", "lessthan_5",
                                                               "between5_15", "between16_30"), 1,
                                      ifelse(r$waterpoint_time == "don't_know", NA, 0))
  
  r <- single.to.multiple(r, "drinking_water_source")
  
  r$handwashing_less_5min <- ifelse(r$handwashing_access == "yes" &
                                      (r$handwashing_access_communal == "no" |
                                         r$handwash_duration %in% c("less_than_five",
                                                                    "on_premises")),
                                    1, 0)
  
  r$income_from_employment <- ifelse(r$source_income %in% c("cash_crop", "livestock_faming",
                                                            "sales", "unskilled_labour",
                                                            "employment"), 1, 0)
  
  r <- single.to.multiple(r, "sanitation_facility")
  r <- single.to.multiple(r, "access_sanitation_problems")
  r <- single.to.multiple(r, "sector_respondent")
  
  r$waste_frequency_recoded <- ifelse(r$waste_frequency %in% c("every_week", "twice",
                                                               "everyday"), 1, 0)
  
  r$receptacles_frequency_recoded <- ifelse(r$receptacles_frequency %in% c("every_week", 
                                                                           "twice",
                                                                           "everyday"), 1, 0)
  
  r$latrine_drain_recoded <- ifelse(r$latrine_drain %in% c("septic_tank", "communal_line"),
                                    1, 0)
  
  r$waste_visibility_recoded <- ifelse(rowSums(r[,c("waste_visibility.animal_faeces",
                                                    "waste_visibility.feaces")], 
                                               na.rm = T) > 0, 1, 0)
  
  r$sewage_visability_recoded %>% table(useNA="always")
  return(r) 
}

