# setup
setwd("~/REACH24919/WASH")
library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(koboquest) # manage kobo questionnairs
library(kobostandards) # check inputs for inconsistencies
library(xlsformfill) # generate fake data for kobo
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
source("postprocessing_functions.R")
source("to_alpha_lowercase.R")

#' load input files & make everything match:
source("load_inputsincamp.R",local = T)
#' creates objects:
#' 
#'    response
#'    analysisplan
#'    choices
#'    questions
#'    cluster_lookup_table
#'    idp_in_camp
#'    loop
#'    loop_in_camp
#'    samplingframe
#'    samplingframe_in_camp

source("match_inputs_incamp.R", local = T)
#' matching all inputs:
#' 1. combine in and out of camp data for each, HH and loops 
#' 2. put together questionnaire
#' 3. prepare sampling frames:
#'     3.1 prepare columns in out of camp cluster level sampling frame
#'     3.2 aggregate out-of-camp to stratum level
#'     3.3.make strata id for in-camp sampling frame
#'     3.4.combine the stratum sampling frames
#'     3.5.add strata ids to the dataset
#'     3.6. throw error if any don't match


# any further problems with the sampling frame matching?



# strata_samplingframe_issues <- as.data.frame(response[which(!response$camp_name %in% samplingframe_strata$camp), c("X_uuid", "camp_name")])
# if(nrow(strata_samplingframe_issues)!=0){
#   print(strata_samplingframe_issues)
#   warning("something's not right with the strata id matching!")

# TO BE DELETED. BE CAREFUL it deletes some data
# response <- response[-which(response$X_uuid %in% strata_samplingframe_issues$X_uuid),]
# 
# cluster_samplingframe_issues <- as.data.frame(response[which(!response$cluster_id[which(response$population_group != "idp_in_camp")] %in% samplingframe$cluster_strata_ID), c("X_uuid", "strata")])
# if(nrow(cluster_samplingframe_issues)!=0){
#   print(cluster_samplingframe_issues)
#   warning("something's not right with the cluster id matching!")
# }

### IGNORING CLUSTER LEVEL WEIGHTING FOR NOW
#### it's been under debate..



# remove records not in cluster samplingframe:

# nrow_before<- nrow(response)
# response<-response %>% filter((cluster_id %in% samplingframe$cluster_strata_ID) | population_group=="idp_in_camp")

# if any disappeared, give a warning:
# if(nrow(response)!=nrow_before){
#   warning(paste("lost ",nrow_before-nrow(response), " records; their cluster id is not in the cluster sampling frame"))
# }

# clusters_weight_fun <- map_to_weighting(sampling.frame= samplingframe,
#                                         sampling.frame.population.column = "pop",
#                                         sampling.frame.stratum.column = "cluster_strata_ID",
#                                         data.stratum.column = "cluster_id",
#                                         data = response[response$population_group!="idp_in_camp",])


# only in camp idps have cluster weight of 1:


# cluster_weight_fun<-function(df){
#  weights<-rep(NA,nrow(df))
#    in_camp<-df$population_group=="idp_in_camp"
#  weights[!in_camp]<-clusters_weight_fun_out_of_camp(df[!in_camp,])
#    weights[in_camp]<-1
#    weights
#    }

strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_strata,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "camp",
                                      data.stratum.column = "camp_name",
                                      data = idp_in_camp)

# weight_fun <- combine_weighting_functions(strata_weight_fun, clusters_weight_fun)
attempt<-strata_weight_fun


# response$weights<-weight_fun(response)
idp_in_camp$attempt<-attempt(idp_in_camp)
# write.csv(response, "temp.csv", row.names = F)
#  # for speedy speed we can not recalculate weights on every run):
#  weight_fun<-function(df){
#    df$weights
#  }

source("Recoding_incamp.R")
response_with_composites <- calc_avgs(idp_in_camp)
response_with_composites <- recodingchoices(response_with_composites)
# names(response_with_composites)<-make.names(names(response_with_composites))
# names(df)<-make.names(names(df))
# write.csv(response_with_composites,sprintf("output/responsewithcompositesincamp.csv", name), row.names=F)
# read.csv("output/responsewithcompositesincamp.csv")
# write.csv(response_with_composites,sprintf("output/responsewithcompositesincamp3.csv", name), row.names=F)
# response_with_composites <- read.csv("output/responsewithcompositesincamp2.csv")


# table(response_with_composites[, c("sufficient_containers_recoded")][which(response_with_composites$district == "erbil")], useNA="always")
# table(response_with_composites$population_group, useNA="always")
#which(response_with_composites$district == "al.hatra")

# # Correcting for random sampled districts
# simple_random_strata <- samplingframe$stratum[which(samplingframe$sampling.type == "2 stages random - st1")]
# simple_random_records <- response_with_composites$strata %in% simple_random_strata
# response_with_composites$cluster_id[simple_random_records]<-
#   paste("simple random unique cluster id - ",1:length(which(simple_random_records)))

dap_name <- "preliminaryincamp"
analysisplan <- read.csv(sprintf("input/dap_%s.csv",dap_name), stringsAsFactors = F)
#analysisplan <- analysisplan[-which(analysisplan$ignore),]
# analysisplan <- analysisplan[which(startsWith(analysisplan$dependent.variable, "flood_causes") 
#  | startsWith(analysisplan$dependent.variable, "s7") 
#  | startsWith(analysisplan$dependent.variable, "s21") 
#  | startsWith(analysisplan$dependent.variable, "s22")
# ),]
analysisplan <- analysisplan_nationwide(analysisplan)
analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan <- analysisplan[which(analysisplan$independent.variable == ""),]

result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = attempt, 
                                          cluster_variable_name = NULL,
                                          questionnaire = questionnaire, confidence_level = 0.9)


name <- "20191218_preliminary_pop_group_disaggregated_nationwide"
saveRDS(result,paste(sprintf("output/result_%s.RDS", name)))
#summary[which(summary$dependent.var == "g51a"),]
# 
# lookup_in_camp<-load_samplingframe("./input/sampling_frame_in_camp.csv")
# names(lookup_in_camp)[which(names(lookup_in_camp) == "camp")] <- "name"
# names(lookup_in_camp)[which(names(lookup_in_camp) == "camp.long.name")] <- "english"
# names(lookup_in_camp)[which(names(lookup_in_camp) == "governorate")] <- "filter"

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))

write.csv(summary, sprintf("output/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = F, camp = F)
  write.csv(df, sprintf("output/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE)
  }
}
