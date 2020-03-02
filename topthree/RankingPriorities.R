library("readxl")
library("reshape2")

getwd()

#LOAD DATA
source("Priorities_HH_level.R")
######################################################################1
############RANK VALUES: VERSION 2.0##################
#df == Dataframe of columns -- NOTE THAT IT MUST BE THE ID COLUMNS AND THE REST ARE THE COLUMNS TO BE RANKED
#aggunit == IN QUOTATIONS: Aggregation unit
#toprank == Top-n ranking (e.g., 5 produces the top 5)
#direction == write: "highest" or "lowest" : highest (top X) or lowest (bottom X) X indicators
rank_money2 <- function(df, aggunit, toprank, direction) {
  callag <- melt(df, id.vars = c(aggunit))
  print(callag)
  id_index <- grep(paste("^",aggunit,"$", sep=""),colnames(callag)) #FIND ID OF GEOGRAPHIC/AGGREGATION UNIT
  unique_units <- unique(callag[id_index]) #UNIQUE GEOGRAPHIC UNITS FOR JOINING LATER
  unique_units<-as.data.frame(unique_units) 
  if(direction == "highest"){ #CHOOSE IF TOP OR BOTTOM X INDICATORS
    direction <- TRUE
  } else(
    direction <- FALSE
  )
  snowflakes <- vector("list")
  for (i in 1:nrow(unique_units)){   #SUBSET DATA BY GEOGRAPHIC UNIQUE (LIST OF DATAFRAMES)
    snowflakes[[i]] <- subset(callag, get(aggunit) == unique_units[i,])
  }
  snowflakes<-  lapply(snowflakes, function(x) x[!duplicated(x), ]) #REMOVE DUPLICATE ROWS (PROBABLY NOT NECESSARY ANYMORE)
  sorted_dataframes_list <- lapply(snowflakes, function(df){   #SORT EACH DATAFRAMES IN THE LIST
    df[order(df$value,decreasing = direction),]   #WHERE TOP OR BOTTOM X IS DEFINED
  })
  rankked <- lapply(sorted_dataframes_list,head,n=toprank) #TAKE THE TOP X ROWS FROM EACH GEOGRAPHIC/AGGREGATION UNIT
  castedd <- lapply(rankked, function(df){
    units_variable <- as.formula(paste0(as.symbol(aggunit),"~", "factor(",as.symbol("variable"),",levels=unique(",as.symbol("variable"),"))","+",as.symbol("value")))
    dcast(df, units_variable) 
  }) 
  trimcast <- lapply(castedd, function(df){
    sub("_[^_]+$", "", names(df[2:(toprank+1)]))  #REMOVE SPECIAL CHARACTERS FROM NAME
  }) 
  for (k in 1: nrow(unique_units)){  #CREATE SEPARATE DATAFRAMES FOR VALUES AND NAMES
    for (j in (toprank+2):(toprank+1+toprank)){
      castedd[[k]][j]<-NA
    }
  }
  for (k in 1: nrow(unique_units)){  
    for (j in 1: toprank){
      castedd[[k]][j+toprank+1] <- trimcast[[k]][j] 
    }
  }
  named <-c()  
  for (h in 1:toprank){  #ADD COLUMN HEADERS TO THE VALUE COLUMNS
    named[h] <- paste0("rank",h,sep="")
  }
  ranknamed <-c() 
  for (l in 1:toprank ){   #ADD COLUMN HEADERS TO THE NAME COLUMNS
    ranknamed[l] <- paste0("name",l,sep="")
  }
  titles <- c("geounit", named,ranknamed) #ADD HEADER TO GEOGRAPHIC/AGGREGATION COLUMN
  castedd <- lapply(castedd, setNames, titles)  #COMBINE GEO-AGG and RANK NAME, & VALUES
  locations <- df[grep(paste0("^",aggunit,"$"),colnames(df))] 
  locations <- unique(locations) #ENSURE LOCATIONS ARE UNIQUE
  ordername <- data.frame(matrix(unlist(castedd), nrow=nrow(unique_units), byrow=T),stringsAsFactors=FALSE)  #UNLIST LIST OF MERGED DATAFRAMES
  colnames(ordername) <- titles
  for (j in 1: toprank+1){
    ordername[j]<-round(as.numeric(unlist(ordername[j])),4)  #ROUND RANKED VALUES
  }
  ordername$geounit<-locations
  ordername[ordername == 0] <- NA
  names(ordername)[1]<-aggunit
  for(i in 2:(1+toprank)){
    ordername[,i+toprank] <- ifelse(is.na(ordername[,i]),NA,ordername[,i+toprank]) #REMOVE NAMES IF ASSOCIATED WITH AN "NA" VALUE
  }
  return(ordername)
}

#RUN SCRIPTS AND RENAME VARIABLES
rank_sanitation_facility <- rank_money2(sanitation_facility,"district",3,"highest")
names(rank_sanitation_facility) <- c("district", "SanitationPC1", "SanitationPC2", "SanitationPC3", "Sanitation1", "Sanitation2", "Sanitation3")
rank_sector_respondent <- rank_money2(sector_respondent,"district",3,"highest")
names(rank_sector_respondent) <- c("district", "sectorPC1", "sectorPC2", "sectorPC3", "sector1", "sector2", "sector3")
rank_drinking_water_source <- rank_money2(drinking_water_source,"district",3,"highest")
names(rank_drinking_water_source) <- c("district", "DWatersourcePC1", "DWatersourcePC2", "DWatersourcePC3", "DWatersource1", "DWatersource2", "DWatersource3")
rank_why_treat <- rank_money2(why_treat,"district",3,"highest")
names(why_treat) <- c("district", "why_treatPC1", "why_treat2", "why_treatPC3", "why_treat1", "why_treat2", "why_treat3")
names(rank_drinking_water_source) <- c("district", "drinkingwaterPC1", "treatPC2", "treatPC3", "treat1", "treat2", "treat3")
rank_problems_access_reasons <- rank_money2(problems_access_reasons,"district",3,"highest")
names(rank_problems_access_reasons) <- c("district", "accessproblemsPC1", "accessproblemsPC2", "accessproblemsPC3", "accessproblems1", "accessproblems2", "accessproblems3")
rank_water_contingency <- rank_money2(water_contingency,"district",3,"highest")
names(rank_water_contingency) <- c("district", "watercontingencyPC1", "watercontingencyPC2", "watercontingencyPC3", "watercontingency1", "watercontingency2", "watercontingency3")
rank_how_activities_affected <- rank_money2(how_activities_affected,"district",3,"highest")
names(rank_how_activities_affected) <- c("district", "floodactivitiesPC1", "floodactivitiesPC2", "floodactivitiesPC3", "floodactivities1", "floodactivities2", "floodactivities3")
rank_waste_disposal <- rank_money2(waste_disposal,"district",3,"highest")
names(rank_waste_disposal) <- c("district", "waste_disposalPC1", "waste_disposalPC2", "waste_disposalPC3", "waste_disposal1", "waste_disposal2", "waste_disposal3")

# ramadi not included

#CHANGE TO DELIVERABLE NAMES: LOAD FILE FORMATTED THE SAME AS THE EXAMPLE EXCEL FILE (KOBO HEADER AND DESIRED NAME FOR OUTPUT)
#setwd("~/REACH24919/IDP Camp Directory/2020/RSkripts/Dataset")
old_new_names <- read.csv("name_translate1.csv")
old_new_names$...2 <- NULL
#LOOP THROUGH THE ENTIRE NAME CHANGE SHEET AND APPLY TO THE WHOLE RANKED OUTPUT
names(old_new_names)

## Ranked sanitation facility
for(i in 1:nrow(old_new_names)){
  rank_sanitation_facility <- as.data.frame(lapply(rank_sanitation_facility, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
## Ranked respondent sectors
for(i in 1:nrow(old_new_names)){
  rank_sector_respondent <- as.data.frame(lapply(rank_sector_respondent, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
## Ranked drinking water sources
for(i in 1:nrow(old_new_names)){
  rank_drinking_water_source <- as.data.frame(lapply(rank_drinking_water_source, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
##Ranked treatment techniques
for(i in 1:nrow(old_new_names)){
  rank_why_treat <- as.data.frame(lapply(rank_why_treat, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
##Ranked water access problems
for(i in 1:nrow(old_new_names)){
  rank_problems_access_reasons <- as.data.frame(lapply(rank_problems_access_reasons, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
## Ranked type water contingency
for(i in 1:nrow(old_new_names)){
  rank_water_contingency <- as.data.frame(lapply(rank_water_contingency, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
## Ranked flood activities
for(i in 1:nrow(old_new_names)){
  rank_how_activities_affected <- as.data.frame(lapply(rank_how_activities_affected, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
## Ranked waste disposal
for(i in 1:nrow(old_new_names)){
  rank_waste_disposal <- as.data.frame(lapply(rank_waste_disposal, function(x) replace(x, grep(as.character(old_new_names[i,1]), x), as.character(old_new_names[i,2]))),stringsAsFactors = FALSE)
}
# 
# preliminaryranking <- merge(rank_sector_respondent,  rank_drinking_water_source[,-1], rank_sanitation_facility[,-1], 
#                             rank_why_treat[,-1], rank_problems_access_reasons[,-1], rank_water_contingency[,-1],
#                             rank_how_activities_affected[,-1] = intersect(names(district), all=TRUE))


preliminaryranking <- Reduce(function(x,y) merge(x,y,by="district",all=T) ,list(rank_sector_respondent, rank_drinking_water_source, rank_sanitation_facility, rank_why_treat, 
                                                                       rank_how_activities_affected, rank_water_contingency ,rank_problems_access_reasons))

write.csv(preliminaryranking, "output/final/preliminaryranking.csv")
