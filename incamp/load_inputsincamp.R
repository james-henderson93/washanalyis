#IMPORT CLEANED INCAMP DATASET
idp_in_camp <- read.csv("input/data_cleaned_anonymised_incamp.csv")

# questionnaire
questions <- read.csv("input/kobo_questions.csv", 
                      stringsAsFactors=F, check.names=F)
# might need changing

choices <- read.csv("input/kobo_choices.csv", 
                    stringsAsFactors=F, check.names=F)

# sampling

# cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
                                 # stringsAsFactors=F, check.names=F)
lookup_table <- read.csv("input/lookup_table_names.csv", stringsAsFactors = F)


# samplingframe <- load_samplingframe("Strata_clusters_population.csv")
samplingframe_in_camp<-load_samplingframe("input/sampling_frame_in_camp2.csv")


# data
# response <- read.csv("data_cleaned_anonymised.csv",
#                      stringsAsFactors = F, check.names = F)

idp_in_camp$camp_name <- to_alphanumeric_lowercase(idp_in_camp$camp_name)
samplingframe_in_camp$camp <- to_alphanumeric_lowercase(samplingframe_in_camp$camp)

idp_in_camp$district <- to_alphanumeric_lowercase(samplingframe_in_camp$district[
  match(idp_in_camp$camp_name, samplingframe_in_camp$camp)])
# DUMMY DATA FOR 3 CAMPS 
# 
#  simple_random_strata[,1]<-to_alphanumeric_lowercase(simple_random_strata[,1])
# simple_random_strata<-paste0(simple_random_strata[,1],simple_random_strata[,2])


