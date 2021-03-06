#' matching all inputs:
#' 1. combine in and out of camp data for each, HH and loops 
#' 2. put together questionnaire
#' 3. prepare sampling frames:
#'     3.1 prepare columns in out of camp cluster level sampling frame
#'     3.2 aggregate out-of-camp to stratum level
#'     3.3.make strata id for in-camp sampling frame
#'     3.4.combine the stratum sampling frames
#'     3.5.add strata  ids to the dataset
#'     3.6.add cluster ids to the dataset
#'     3.6. throw error if any don't match



# rbind in and out of camp
# idp_in_camp$population_group <- "idp_in_camp"
# cresponse <- rbind.fill(response, idp_in_camp)
# loop <- rbind.fill(loop, loop_in_camp)



# questionnaire

questionnaire <- load_questionnaire(response,questions,choices)



# prepare samplingframes

# # ## cluster sampling frame: make values that can be matched with data
# samplingframe$popgroup[samplingframe$popgroup=="idp"]<-"idp"
# samplingframe$district <- cluster_lookup_table$new_ID[match(samplingframe$psu, cluster_lookup_table$psu)]
# samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")


## add cluster ids to data:

### any pop group or cluster ids that can't be found individually?
# `%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

# response$population_group %find those not in% samplingframe$popgroup
# response$cluster_location_id[response$population_group != "idp_in_camp"] %find those not in% samplingframe$cluster_ID

### create id in samplingframe and in data
# samplingframe$cluster_strata_ID <- paste(samplingframe$cluster_ID, samplingframe$popgroup, sep = "_")
# response$cluster_id <- paste(response$cluster_location_id, response$population_group, sep = "_")

# any id that can't be found in combination?
# response$cluster_id %find those not in% samplingframe$cluster_strata_ID %>% paste0(collapse="\n") %>% cat


### reverse not matching?
# samplingframe$cluster_strata_ID %find those not in% response$cluster_id

## unique cluster ids for IDP in camp (unique):

# in_camp <- response$population_group=="idp_in_camp"
#  in_camp_ids<-paste0("NO_CLUSTER_",1:length(which(response$population_group=="idp_in_camp")),"_idp_in_camp")
#  in_camp_cluster_sampling_frame<-data.frame(cluster_strata_ID= in_camp_ids,pop=1)

# response$cluster_id[in_camp]<-in_camp_ids














# samplingframe$stratum<-gsub("idp$","idp_out_camp",samplingframe$stratum) # should not be run; sampling frame  

# ## aggregate to stratum level
# samplingframe_strata <- samplingframe %>%
#   group_by(stratum) %>% 
#   dplyr::summarize(population = sum(population))
# 
# samplingframe_strata<-as.data.frame(samplingframe_strata)


# ## in camp: make values match data
#  samplingframe_in_camp$stratum<-paste0(samplingframe_in_camp$camp,"idp_in_camp")
# 
# ## in camp: make columns match other sampling frame:
#  samplingframe_in_camp <- samplingframe_in_camp %>% dplyr::select(stratum,"population" = population..july.cccm.)
# ## combine in camp and out of camp sampling frames
#  samplingframe_strata <- rbind(samplingframe_strata,samplingframe_in_camp)

# add strata names to data

## out of camp:
# districtdata <- read.csv("input/combined_sample_ids.csv",
#                          stringsAsFactors=F, check.names=F)
# columns <- c("district")


#

# response <- response %>% mutate(population_group = ifelse(calc_idp == 1, "idp", ifelse(calc_returnee == 1, "returnee", 
                                                                                       # ifelse(calc_remainee == 1, "host", NA))))

# response <- response %>% 
#   mutate(district = cluster_lookup_table$district[match(cluster_location_id,cluster_lookup_table$new_ID)])

# response <- response %>% 
#   mutate(strata = paste0(district,population_group))

## in camp: (replace district with camp name for idp_in_camp population group)
# response$strata[response$population_group=="idp_in_camp"]<- (paste0(response$camp,response$population_group))[response$population_group=="idp_in_camp"]



## add rows to cluster samplingframe where no cluster sampling was used
# samplingframe<-bind_rows(samplingframe,in_camp_cluster_sampling_frame)



# samplingframe_strata <- samplingframe_in_camp
# ## Check if all match:
# response <- idp_in_camp
# if(any(!(response$camp_name %in% samplingframe_strata$camp))){
#   warning("some strata not found in samplingframe")
#   warning(which(!(response$camp_name %in% samplingframe_strata$camp)) %>% length)
# }
# 
# if(any(is.na(response$camp_name))){
#   warning("strata can not be NA")
# }



