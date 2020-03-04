# library(tidyr)
# single.to.multiple <- function(r, col){
#   scol <- 2
#   r[!is.na(r[,col]) & r[,col] == "",col] <- NA
#   if(any(is.na(r[,col]))){
#     scol <- 3
#   }
#   test <- r %>% mutate(value = 1) %>% select(X_uuid, one_of(col), value) %>% 
#     pivot_wider(names_from = one_of(col), values_from = value)
#   names(test)[scol:ncol(test)] <- paste(col, names(test)[scol:ncol(test)], sep = ".")
#   test[is.na(test)] <- 0
#   r <- cbind(r, test[,scol:ncol(test)])
#   return(r)
# }

# single.to.multiple <- function(r, col){
#   r[!is.na(r[,col]) & r[,col] == "",col] <- NA
#   test <- r %>% mutate(value = 1) %>% select(X_uuid, one_of(col), value) %>% 
#     pivot_wider(names_from = one_of(col), values_from = value)
#   if(any(is.na(r[,col]))){
#     scol <- 2:ncol(test)
#     scol <- scol[-which(scol == which(names(test) == 'NA'))]
#     
#   }
#   names(test)[scol] <- paste(col, names(test)[scol], sep = ".")
#   test[is.na(test)] <- 0
#   r <- merge(r, test[,c(1,scol)], by="X_uuid", all=T)
#   return(r)
# }
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

  # adding column of nation into dataset
    
  # allDataMerge$GovMarg <- paste("10")
  # r$nation <- paste(Iraq)

  
  r <- as.data.frame(r) 
  r <-response
  
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
  
  r$improved_drinking_water_source <- ifelse(r$drinking_water_source %in% 
                                      c("network_private", "network_comm", "dug_well", 
                                        "prot_well", "rainwater", "river_spring",
                                        "bottled_water", "purchase_water", "water_trucking"), 1, 0)
  r$unimproved_drinking_water_source <-ifelse(r$drinking_water_source %in%
                                       c("illegal_connection", "unprot_well", "unprot_tank",
                                         "unprot_spring"), 1, 0)
  r$surface_water_drinking_source <-ifelse(r$drinking_water_source %in%
                                             c("surface_water"), 1, 0)
  r$improved_sanitation_facility <- ifelse(r$sanitation_facility %in%
                                             c("flush", "pit_vip", "latrine_slab"), 1,0)
  r$unimproved_sanitation_facility <- ifelse(r$sanitation_facility %in%
                                               c("latrine_without_slab", "open_hole", 
                                                 "bucket_toilet", "plastic_bag","hanging_toilet"), 1,0)
  r$open_defecation <- ifelse(r$sanitation_facility %in%
                                c("none_of"), 1, 0)                                              
                                              
  r$basic_handwashing_facility <-ifelse(r$household_soap == "yes" & 
                                          (r$handwashing_access_communal == "no"), 1, 0)
                                              
                                        
  r$limited_handwashing_facility <- ifelse(r$household_soap == "no" & 
                                             (r$handwashing_access_communal == "no"), 1, 0)
  # r$no_handwashing_facility <- ifelse (r$handwashing_access == "no", 1, 0)

  r$safe_waste_disposal <- ifelse (r$waste_disposal %in%
                                     c("septic_tank", "communal_line"), 1,0)
  
  r$unsafe_waste_disposal <- ifelse (r$waste_disposal %in%
                                       c("hole", "stagnant_field", 
                                         "nothing"), 1, 0)
  
                                       
                                   
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                      
                                     

 
  
  
  
                                         
                                      
                                        
                                         
                                      
                                      
  
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
  r$water_contingency_unprotected <-ifelse(r$water_contingency %in% 
                                             c("less_preffered", "rely_surface_water",
                                               "rely_less_preferred_source", "rely_surface_water_other"), 1,0)
  
  
  r$waterpoint_time_recoded <- ifelse(r$waterpoint_time %in% c("water_on_premises", "lessthan_5",
                                                               "between5_15", "between16_30"), 1,
                                      ifelse(r$waterpoint_time == "don't_know", NA, 0))
  
  
  r$handwashing_less_5min <- ifelse(r$handwashing_access == "yes" &
                                      (r$handwashing_access_communal == "no" |
                                         r$handwash_duration %in% c("less_than_five",
                                                                    "on_premises")), 1, 0)
  
  r$female_hhh <- ifelse(r$hhh== "yes" & (r$gender_respondent == "female"), 1, 0)
  r$children_avg <- ifelse(r$household_member_male_under18 & (r$household_member_female_under18), 1, 0)
  r$flood_experienced_shelter <- ifelse(r$experienced_floods == "yes" & (r$shelter_affected == "yes"), 1,0)                                                           
  r$income_from_employment <- ifelse(r$source_income %in% c("cash_crop", "livestock_faming",
                                                            "sales", "unskilled_labour",
                                                            "employment"), 1, 0)
  
  
  # r <- single.to.multiple(r, "drinking_water_source")
  # r <- single.to.multiple(r, "sanitation_facility")
  #  # r[c(which(startsWith(names(r), "sanitation_facility.")))] <- as.numeric(unlist(r[c(which(startsWith(names(r), "sanitation_facility.")))]))
  #  r <- single.to.multiple(r, "access_sanitation_problems")
  #  r[c(which(startsWith(names(r), "access_sanitation_problems.")))] <- as.numeric(unlist(r[c(which(startsWith(names(r), "access_sanitation_problems")))]))
  # r <- single.to.multiple(r, "sector_respondent")
  # r[c(which(startsWith(names(r), "sector_respondent.")))] <- as.numeric(unlist(r[c(which(startsWith(names(r), "sector_respondent.")))]))
  # r[c(which(startsWith(names(r), "drinking_water_source.")))] <- as.numeric(unlist(r[c(which(startsWith(names(r), "drinking_water_source.")))]))
  #

  r$waste_frequency_recoded <- ifelse(r$waste_frequency %in% c("every_week", "twice",
                                                               "everyday"), 1, 0)
  
  r$receptacles_frequency_recoded <- ifelse(r$receptacles_frequency %in% c("every_week", 
                                                                           "twice",
                                                                           "everyday"), 1, 0)
  
  r$latrine_drain_recoded <- ifelse(r$latrine_drain %in% c("septic_tank", "communal_line"),
                                    1, 0)
  # r$water_access_problems <- ifelse(r$problems_access %in% c("waterpoints_toofar", "waterpoints_difficult", 
  #                                                            "water_dangerous", "groups_noaccess",
  #                                                            "insufficient_acccess", "waterpoints_dysfunctional",
  #                                                            "market_notavailable", "water_expensive",
  #                                                            "containers_notenough", "taste_quality_bad"), 1, 0)
  

  r$water_access_problems_exclnoproblems <- r$problems_access_reasons
  r$water_access_problems_exclnoproblems <- ifelse(r$water_access_problems_exclnoproblems == "no_problems", 
                                                   NA, r$water_access_problems_exclnoproblems)
  r$water_contingency_exclnone <- r$water_contingency
  r$water_contingency_exclnone <- ifelse(r$water_contingency_exclnone== "none",
                                         NA, r$water_contingency_exclnone)
   r$menstrual_hygiene_excl <- r$menstrual_hygiene
   r$menstrual_hygiene_excl <- ifelse(r$menstrual_hygiene=="refuse_answer"| r$menstrual_hygiene=="refuse_ask",
                                      NA, r$menstrual_hygiene_excl)
  
  r %<>% mutate_if(startsWith(names(r), "problems_access_reasons"), as.numeric)
  r %<>% mutate_if(startsWith(names(r), "water_contingency"), as.numeric)
  # r %<>% mutate_if(startsWith(names(df), "waste_disposal"), as.numeric)
  # which(is.na(as.numeric(as.character(df[["waste_disposal"]]))))
  
  r$sewage_visability_recoded %>% table(useNA="always")
  
   r[r=="#N/A"] <- NA
  
  
  return(r) 
}

 topthree <-function(r) {


 topthree <- read.csv("output/20200302_preliminary_camp.csv")
 topthree[c(which(endsWith(names(r), "_min")))] <- NULL
 topthree[c(which(endsWith(names(r), "_max")))] <- NULL
 topthree <- topthree[-c(2:5),]

 drinking_water_source <- topthree[c(which(startsWith(names(topthree), "drinking_water_source.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
 drinking_water_source <- cbind(repeat.var.value, drinking_water_source)
drinking_water_source[,c(2:length(drinking_water_source))] <- unlist(drinking_water_source[,c(2:length(drinking_water_source))])

 sector_respondent <- topthree[c(which(startsWith(names(topthree), "sector_respondent.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
 sector_respondent <- cbind(repeat.var.value, sector_respondent)
 sector_respondent[,c(2:length(sector_respondent))] <- unlist(sector_respondent[,c(2:length(sector_respondent))])

 why_treat <- topthree[c(which(startsWith(names(topthree), "why_treat.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
why_treat <- cbind(repeat.var.value, why_treat)
 why_treat[,c(2:length(why_treat))] <- unlist(why_treat[,c(2:length(why_treat))])

 problems_access_reasons <- topthree[c(which(startsWith(names(topthree), "problems_access_reasons.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
 problems_access_reasons <- cbind(repeat.var.value, problems_access_reasons)
 problems_access_reasons[,c(2:length(problems_access_reasons))] <- unlist(problems_access_reasons[,c(2:length(problems_access_reasons))])

 water_contingency<- topthree[c(which(startsWith(names(topthree), "water_contingency.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
 water_contingency <- cbind(repeat.var.value, water_contingency)
 water_contingency[,c(2:length(water_contingency))] <- unlist(water_contingency[,c(2:length(water_contingency))])
 
 sanitation_facility <- topthree[c(which(startsWith(names(topthree), "sanitation_facility.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
 sanitation_facility <- cbind(repeat.var.value, sanitation_facility)
 sanitation_facility[,c(2:length(sanitation_facility))] <- unlist(sanitation_facility[,c(2:length(sanitation_facility))])
 
 how_activities_affected <- topthree[c(which(startsWith(names(topthree), "how_activities_affected.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
how_activities_affected <- cbind(repeat.var.value, how_activities_affected)
 how_activities_affected[,c(2:length(how_activities_affected))] <- unlist(how_activities_affected[,c(2:length(how_activities_affected))])

 waste_disposal <- topthree[c(which(startsWith(names(topthree), "waste_disposal.")))]
 repeat.var.value <- topthree[,c("repeat.var.value")]
  waste_disposal <- cbind(repeat.var.value, waste_disposal)
  waste_disposal[,c(2:length(waste_disposal))] <- unlist(waste_disposal[,c(2:length(waste_disposal))])
  return(r)}

# r[r=="#N/A"] <- NA


# #subset_age_groups <-function(r) {
# r <- response
# 
# 
# r$children_under_18 <- r$household_member_male_under18 + r$household_member_female_under18
# 
# 
# r$female_total <- as.numeric(apply(r[,c(
#   "household_member_female_under18", 
#   "household_member_female_18")], 
#   1, sum))
# 
# r$male_total <- as.numeric(apply(r[,c("household_member_male_under18",
#                                                              "household_member_male_18")], 
#                                  1, sum))
# 
# subset_age_groups <- r[, c("household_member_female_under18", "household_member_female_18", 
#                            "household_member_male_under18", 
#                            "household_member_male_18", 
#                            "weights", 
#                            "female_total", "male_total")]
# 
# 
# subset_age_groups[,c("household_member_female_under18", "household_member_female_18",
#                      "household_member_male_under18", "household_member_male_18",
#                      "male_total", "female_total")] <- subset_age_groups[,c("household_member_female_under18", "household_member_female_18",
#                                                                             "household_member_male_under18", "household_member_male_18", 
#                                                                             "male_total", "female_total")] * subset_age_groups$weights
# 
# 
# subset_age_groups <- data.frame(t(colSums(subset_age_groups, na.rm=F)))
# 
# subset_age_groups[, c("household_member_female_under18", "household_member_female_18",
#                       "household_member_male_under18", "household_member_male_18", 
#                       "weights", "male_total", "female_total")] <- subset_age_groups[,c("household_member_female_under18", "household_member_female_18",
#                                                                                         "household_member_male_under18", "household_member_male_18", 
#                                                                                         "weights", "male_total", "female_total")] / subset_age_groups$weights
# 
# subset_age_groups[, c("household_member_male_under18", "household_member_male_18",
#                       "male_total")] <- subset_age_groups[,c("household_member_male_under18", "household_member_male_18","male_total")] / subset_age_groups$male_total
# 
# subset_age_groups[, c("household_member_female_under18", "household_member_female_18",
#                       "female_total")] <- subset_age_groups[,c("household_member_female_under18", "household_member_female_18","female_total")] / subset_age_groups$female_total
# pop_group_average <- subset_age_groups(response)
# return(subset_age_groups)
# 
# }
# 
