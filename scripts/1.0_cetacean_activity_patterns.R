##Packages we will use ---------------------------------------------------
# For manipulating character strings
library(stringr)
# For controlling working directory
library(here)
# For plotting phylogenetic trees
library(ggplot2)
library(ggtree)
# For loading from google sheet
library(gsheet)
#saving dataframes
library(gridExtra)
#manipulating dataframes
library(dplyr)
#reading in excel sheets
library(readxl)
#open tree of life
library(rotl)
#adds timescale
#library(deeptime)
#colours
library(RColorBrewer)
#apply two separate colour palettes
library(ggnewscale)
#more colours
library(pals)
#useful
library(tidyr)
#package to work with shapefiles
#install.packages("sf")
library(sf)
#also helps with shapefiles
library(raster)
#colours
library(viridis)
#sankey diagram
# install.packages("ggsankey")
library(ggsankey)
library(ggpubr)
## Packages for phylogenetic analysis in R (overlapping uses)
## They aren't all used here, but you should have them all
library(ape) 
library(phytools)
library(geiger)
library(corHMM)
library(phangorn)
library(DescTools)
library(ggbeeswarm)
library(rstatix)

#extra packages
library(lubridate)
#install.packages("deeptime")
library(scales)
#install.packages("ggforce")
library(ggforce)
library(forcats)
#install.packages("ggdist")
library(ggdist)
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("webshot")
library(webshot)
library(forcats)
library(ggpmisc)
library(stats)
library(phyloint)
library(patchwork)
library(ggridges)
library(rlang)
#pantheria database
#library(trait.data)
# Set the working directory and source the functions (not used yet)
setwd(here())

source("scripts/fish_sleep_functions.R")
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))

# Section 1: Transform cetacean data into wide format -------------------------------
url <- 'https://docs.google.com/spreadsheets/d/1eG_WIbhDzSv_g-PY90qpTMteESgPZZZt772g13v-H1o/edit?usp=sharing'
diel_full <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

#take only the columns we're interested in
diel_full <- diel_full[, c(1, 3, 9:16)] 

diel_full$tips <- str_replace(diel_full$Species_name, pattern = " ", replacement = "_")

#remove species without any information, drops from 98 species to 90
diel_full <- diel_full[!is.na(diel_full$Confidence),]

#convert strings to lowercase
diel_full$Conf_1 <- tolower(diel_full$Conf_1)
diel_full$Conf_2_daytime <- tolower(diel_full$Conf_2_daytime)
diel_full$Conf_2_nighttime <- tolower(diel_full$Conf_2_nighttime)
diel_full$Conf_3_PAM <- tolower(diel_full$Conf_3_PAM)
diel_full$Conf_3_Stomach_bycatch <- tolower(diel_full$Conf_3_Stomach_bycatch)
diel_full$Conf_4 <- tolower(diel_full$Conf_4)
diel_full$Conf_5 <- tolower(diel_full$Conf_5)

#separate all entries in a category of evidence (1-5) into separate columns
diel_full <- separate(data = diel_full, col = Conf_1, into = c("Conf1.1", "Conf1.2", "Conf1.3", "Conf1.4", "Conf1.5"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_2_daytime, into = c("Conf2.1", "Conf2.2", "Conf2.3", "Conf2.4", "Conf2.5"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_2_nighttime, into = c("Conf2N.1", "Conf2N.2"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_3_PAM, into = c("Conf3.1", "Conf3.2", "Conf3.3", "Conf3.4", "Conf3.5", "Conf3.6", "Conf3.7", "Conf3.8"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_3_Stomach_bycatch, into = c("Conf3ByS.1", "Conf3ByS.2"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_4, into = c("Conf4.1", "Conf4.2", "Conf4.3", "Conf4.4", "Conf4.5", "Conf4.6", "Conf4.7", "Conf4.8"), sep = ",")
diel_full <- separate(data = diel_full, col = Conf_5, into = c("Conf5.1", "Conf5.2", "Conf5.3", "Conf5.4"), sep = ",")

#replace strings to standardized categories
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("cathemeral-variable", "cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("cathemeral-invariate", "cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-nocturnal/crepuscular", "nocturnal/crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-nocturnal", "nocturnal/cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-diurnal", "diurnal/cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("cathemeral-dvm", "cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("nocturnal-dvm", "nocturnal", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("-", "/", x)}))

#save out
write.csv(diel_full, file = here("cetacean_confidence_wide.csv"), row.names = FALSE)

# Section 2: Transform cetacean data into long format -------------
#read in wide format data
diel_full <- read.csv(here("cetacean_confidence_wide.csv"))
#convert into long format
diel_full_long <- diel_full %>% pivot_longer(cols = Conf1.1:Conf5.4, names_to = "column", values_to = "value")
#remove whitespace
diel_full_long$value <- str_trim(diel_full_long$value)

#deal with partially unclear values which are all in confidence level 2
#keep since they are weighted low in the pipeline anyway
#the 2 Inia species it effects the final day-night preference call for are not in the final mammal tree 
diel_full_long$value <- str_replace_all(diel_full_long$value, pattern = "unclear/diurnal", replacement = "diurnal")
diel_full_long$value <- str_replace_all(diel_full_long$value, pattern = "unclear/cathemeral", replacement = "cathemeral")
diel_full_long$value <- str_replace_all(diel_full_long$value, pattern = "unclear/nocturnal", replacement = "nocturnal")

#remove unclear as an option since it gives no new information
#this removes 57 entries all from confidence level 1 and 2 from 36 species
diel_full_long[diel_full_long == "unclear"] <- NA

#remove rows with empty values
diel_full_long <- diel_full_long[diel_full_long$value != "",]
diel_full_long <- diel_full_long[!is.na(diel_full_long$value),]

#take only the first part of the column name (ie conf1, conf2)
diel_full_long$column <- gsub("\\..*","",diel_full_long$column)
#we won't worry about different types of level 3 and level 2 evidence
diel_full_long$column <- str_replace(diel_full_long$column, pattern = "Conf3ByS", replacement = "Conf3")
diel_full_long$column <- str_replace(diel_full_long$column, pattern = "Conf2N", replacement = "Conf2")

#check for any weird values
unique(diel_full_long$value)

#save out
write.csv(diel_full_long, file = here("cetacean_confidence_long_df.csv"), row.names = FALSE)

# Section 3: Objective method for making calls on activity patterns  ------------------------------------------------------------

#read in the confidence data in long format
diel_long <- read.csv(here("cetacean_confidence_long_df.csv"))

#reclassify partially sources as cathemeral, does not change the call on species activity
diel_long$value <- str_replace_all(diel_long$value, pattern = "nocturnal/cathemeral", replacement = "cathemeral")
diel_long$value <- str_replace_all(diel_long$value, pattern = "diurnal/cathemeral", replacement = "cathemeral")

#replace "unclear" with NA since it adds no new information
diel_long[diel_long == "unclear"] <- NA #84 species with some data
diel_long <- filter(diel_long, !is.na(value))

unique(diel_long$value)

#separate out crepuscularity into its own column
#confidence level 2 data will be unclear/crepuscular since they don't show evidence of nocturnal or diurnal activity
diel_long <- separate(diel_long, col = value, into = c("new_diel", "crepuscular"), sep = "/")

#a custom function to take the mode where x is a list of numbers corresponding to the number of times each diel pattern appears for a given species
#if x = c(2,1,3) then for that species diurnal appeared twice, nocturnal appeared once and cathemeral appeared thrice
which.max.simple=function(x,na.rm=TRUE,tie_value="NA"){
  if(na.rm)
  {
    x=x[!is.na(x)] #removes NA values
  }
  if(length(x)==0) #if there is no activity pattern data return NA
  {
    return(NA)
  }
  if(length(x)==1) #if there is no activity pattern (will return an integer zero) data return NA
  { if(x == 0){
    return(NA)
  }
  }
  maxval=max(x) #takes the list of occurrences and picks whichever is the largest (ie cathemeral appears 3 times)
  if(is.na(maxval)) #if the highest value is NA then return NA
  {
    return(NA)
  }
  if(sum(x %in% maxval) > 1) #if the highest number appears twice or more (ie there's a tie)
  {
    # Ties exist, figure out what to do with them. Two options
    if(tie_value=="NA") #if there's a tie return NA
    {
      return(NA)
    }
    
    if(tie_value=="random") #if there's a tie, randomly chose one over the other
    {
      tie_postions=which(x==maxval)
      return(sample(tie_postions,size=1))
    }
    
    if(tie_value=="first") #if there's a tie, chose the first value
    {
      tie_postions=which(x==maxval)
      return(tie_postions[1])
    }
    
  } else
  {
    return(which.max(x)) #return the maximum value
  }
}

#the below function takes the all the entries for a given species in the specified confidence levels (ie for first pass its conf4)
#first checks is there is a mode for that confidence level or if its inconclusive (will return an NA)
#takes the diel patterns in each entry for that species and assigns it a number by matching it to a position the list of unique values
#example 1 = diurnal, 2 = nocturnal, 3 = cathemeral, tabulate counts the number of times each of these numbers appears in the input
#example, diurnal appears twice, nocturnal appears once and cathemeral appears three times. So it returns cathemeral as the activity pattern

tabulateFuncCet <- function(x) {
  if(x %>% filter(column == "Conf4") %>% nrow() > 1 & !is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA"))){
    activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA")]
    activity_pattern <- paste(activity_pattern, "T")
  } else {
    if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4", "Conf3")], unique(x$new_diel))), tie_value = "NA"))|(nrow(x[x$column %in% c("Conf3", "Conf4"),]) == 1)){
      if (nrow(x[x$column %in% c("Conf4"),]) < 1) {
        if (nrow(x[x$column %in% c("Conf3"),]) != 1){
          if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3")], unique(x$new_diel))), tie_value = "NA"))) {
            if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3", "Conf1")], unique(x$new_diel))), tie_value = "NA")) | nrow(x[x$column %in% c("Conf3", "Conf4", "Conf5"),]) > 0){
              activity_pattern <- "cathemeral-variable Z"
            } else{
              activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3", "Conf1")], unique(x$new_diel))), tie_value = "NA")]
              activity_pattern <- paste(activity_pattern, "Y")
            }
          } else {
            activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3")], unique(x$new_diel))), tie_value = "NA")]
            activity_pattern <- paste(activity_pattern, "X")
          }
        } else {
          activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3")], unique(x$new_diel))), tie_value = "NA")]
          activity_pattern <- paste(activity_pattern, "W")
        }
      } else {
        activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA")]
        activity_pattern <- paste(activity_pattern, "V")
      }
    } else {
      activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4", "Conf3")], unique(x$new_diel))), tie_value = "NA")]
      activity_pattern <- paste(activity_pattern, "U")}
  }
  return(activity_pattern)
}

#run each species through this function, 82 species with activity pattern data (di, noc or cath)
activity_pattern_df <- diel_long[!is.na(diel_long$new_diel),] %>% group_by(Species_name) %>% do(tabulated_diel_pattern = tabulateFuncCet(.)) %>% unnest()

activity_pattern_df <- separate(activity_pattern_df, col = "tabulated_diel_pattern", into = c("tabulated_diel_pattern", "level"), sep = " ")
activity_pattern_df$level <- activity_pattern_df$level %>% replace_na("G")
table(activity_pattern_df$level)

#Step T: mode of multiple Conf4 sources, Step U: mode of Conf3 and Conf4, Step V: Single Conf3 source. Step W: Single conf4 source, 
#Step X: Mode of Conf3, Conf4, Conf5. Step Y: Mode of Conf1,3,4,5. Step Z: If still unresolved call cathemeral-variable

table(activity_pattern_df$tabulated_diel_pattern)

#replace cathemeral-variable with cathemeral since we aren't delineating between the two in the analysis
activity_pattern_df$tabulated_diel_pattern <- str_replace(activity_pattern_df$tabulated_diel_pattern, pattern = "cathemeral-variable", replacement = "cathemeral")

#percentage of total sources that call a given species crepuscular
diel_long[is.na(diel_long)] <- "0" #replaces all the NAs in crepuscular column with 0
diel_long$crepuscular <- str_replace(diel_long$crepuscular, pattern = "crepuscular", replacement = "1")
diel_long$crepuscular <- as.numeric(diel_long$crepuscular) #mark all crepuscular species with a value of 1
diel_long$total <- 1 #used to calculate the percentage of crepuscular sources out of the total sources

#if majority of conf2-4 sources call a species crepuscular, evaluate to crepuscular
crep_percent <- diel_long %>% filter(column %in% c("Conf2", "Conf3", "Conf4")) %>% group_by(Species_name, column) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>% 
  pivot_wider(id_cols = !c(sum_total, sum_crep), names_from = "column", values_from = percent_crep)

crep_df <- diel_long  %>% filter(column %in% c("Conf2", "Conf3", "Conf4")) %>% group_by(Species_name) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>%
  merge(., crep_percent[, c(1:4)], by = "Species_name") %>%
  mutate(tabulated_crep = case_when(
                                    Conf4 > 0 ~ "crepuscular", #if there is a single level 4 source, evaluate to crepusuclar
                                    percent_crep > 50  ~ "crepuscular",
                                    percent_crep < 50  ~ NA,
                                    #when there is a tie use higher confidence source as tiebreaker
                                    percent_crep == 50 & Conf3 >= 50 & Conf2 <= 50 ~ "crepuscular",
                                    percent_crep == 50 & Conf3 <= 50 & Conf2 >= 50 ~ NA,
                                    percent_crep == 50 & Conf3 >= 50 & Conf4 < 20 ~ NA,
                                    #if the sources are in the same confidence level, evaluate to crep
                                    percent_crep == 50 & Conf4 == 50 ~ "crepuscular",
                                    percent_crep == 50 & Conf3 == 50 ~ "crepuscular",
                                    percent_crep == 50 & Conf2 == 50 ~ "crepuscular"
  ))


final_df <- merge(crep_df, activity_pattern_df, by = "Species_name", all = TRUE)
final_df <- final_df %>% mutate(tabulated_diel = case_when(is.na(tabulated_crep) ~ tabulated_diel_pattern,
                                                           tabulated_crep == "crepuscular" ~ paste(tabulated_diel_pattern, tabulated_crep, sep = "/")))
  
current_dataset  <- final_df[, c("Species_name", "tabulated_diel")]
previous_dataset <- read.csv(here("cetacean_tabulated_full.csv")) %>% select(Species_name, tabulated_diel)

table(previous_dataset$tabulated_diel)
table(current_dataset$tabulated_diel)

#check that nothing about the data has changed since running it last 
all(previous_dataset == current_dataset)
#current_dataset[!previous_dataset$tabulated_diel == current_dataset$tabulated_diel,] #check what doesn't match
if(all(previous_dataset == current_dataset) == FALSE) stop("Dataset is not the same!")

#add a column for tips, formatted as the species names appear in the phylogenetic tree
final_df$tips <- str_replace(final_df$Species_name, pattern = " ", replacement = "_")

#save out the new tabulated activity pattern dataframe
write.csv(final_df[, c("Species_name", "tabulated_diel", "tips")], here("cetacean_tabulated_full.csv"), row.names = FALSE)

# Section 4: Save out cetacean data frame with additional details -------
#load in the dataframe with the tabulated activity patterns (calls based on source concordance)
cetaceans_tabulated_full <- read.csv(here("cetacean_tabulated_full.csv")) #82 species with data 

#load in full primary source dataframe, 98 species and subspecies
url <- 'https://docs.google.com/spreadsheets/d/1-5vhk_YOV4reklKyM98G4MRWEnNbcu6mnmDDJkO4DlM/edit?usp=sharing'
cetaceans_full <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
#add in the tabulated diel patterns
cetaceans_full <- cetaceans_full %>% select(-c(Diel_Pattern_1, Diel_Pattern_2, Diel_Pattern_3))
cetaceans_full <- merge(cetaceans_full, cetaceans_tabulated_full, by = "Species_name", all.x = TRUE)

#remove unnecessary columns
cetaceans_full <- cetaceans_full[,c("Species_name", "Confidence", "Parvorder", "Family", "tabulated_diel", "tips")]
cetaceans_full$tips <- str_replace(cetaceans_full$Species_name, pattern = " ", replacement = "_")
colnames(cetaceans_full) <- c("Species_name", "Confidence", "Parvorder", "Family", "Diel_Pattern", "tips")

#add suborder taxonomic info for future reference
cetaceans_full$Suborder <- "Whippomorpha"
cetaceans_full$Order <- "Artiodactyla"

#Diel_Pattern includes all 6 possible trait states: di, di/crep, noc, noc/crep, cath, cath/crep
#Max_crep will include 4 trait states and maximize crepuscularity: di, noc, cath and crep (di/crep, noc/crep, cath/crep)
cetaceans_full$max_crep <- cetaceans_full$Diel_Pattern
cetaceans_full$max_crep <- str_replace(cetaceans_full$max_crep, pattern = "nocturnal/crepuscular", replacement = "crepuscular")
cetaceans_full$max_crep <- str_replace(cetaceans_full$max_crep, pattern = "diurnal/crepuscular", replacement = "crepuscular")
cetaceans_full$max_crep <- str_replace(cetaceans_full$max_crep, pattern = "cathemeral/crepuscular", replacement = "crepuscular")

#create a column with the max confidence level for that species (out of the confidence level for all sources)
#the confidence values are characters so convert to numerics and then take the maximum value
cetaceans_full$Confidence[is.na(cetaceans_full$Confidence)] <- 0
cetaceans_full$Confidence <- gsub(",", "\\1 ", cetaceans_full$Confidence)
cetaceans_full$Confidence <- strsplit(cetaceans_full$Confidence, " ")
cetaceans_full$Confidence <- lapply(cetaceans_full$Confidence, max)
cetaceans_full$Confidence <- unlist(cetaceans_full$Confidence)
cetaceans_full[cetaceans_full == 0.5] <- 1 #these are all the inaturalist observations

cetaceans_full <- cetaceans_full %>% select("Species_name", "Order", "Suborder", "Parvorder", "Family", "Diel_Pattern", "max_crep", "Confidence", "tips")

#save out a local copy in case google goes bankrupt
write.csv(cetaceans_full, file = here("cetaceans_full.csv"), row.names = FALSE)

# Section 5: Concordance by activity pattern -------------------------------------------
diel_full_long <- read.csv(here("cetacean_confidence_long_df.csv"))
#read in the tabulated activity patterns
diel_full <- read.csv(here("cetaceans_full.csv"))
diel_full <- merge(diel_full[, c("Species_name", "Parvorder", "Diel_Pattern", "max_crep")], diel_full_long[c("Species_name", "column", "value")])

diel_full$column <- substr(diel_full$column, 1,5)

#for max crep dataset
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("unclear/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("nocturnal/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("diurnal/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("cathemeral/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("diurnal/cathemeral", "cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("nocturnal/cathemeral", "cathemeral", x)}))

unique(diel_full$value)

#replace "unclear" with NA since it adds no new information
diel_full[diel_full == "unclear"] <- NA 
diel_full <- filter(diel_full, !is.na(value))

#filter
mulitple_sources <- diel_full %>% count(Species_name) %>% filter(n>1)
diel_full_filtered <- diel_full[diel_full$Species_name %in% mulitple_sources$Species_name,]
#this removes 12 species without an informative second source
#"Berardius arnuxii", "Caperea marginata", "Inia boliviensis","Inia humboldtiana", "Lagenorhynchus albirostris","Lissodelphis borealis", "Lissodelphis peronii","Mesoplodon hotaula"        
#"Mesoplodon mirus", "Orcaella heinsohni","Phocoena sinus", "Sousa teuszii"   

#check to see if there is a difference in accuracy in mysticetes vs odontocetes
#diel_full_filtered <- filter(diel_full_filtered, Parvorder == "Mysticeti")
#diel_full_filtered <- filter(diel_full_filtered, Parvorder == "Odontoceti")

concordance <- as.data.frame(table(diel_full_filtered$max_crep, diel_full_filtered$value))
colnames(concordance) <- c("actual", "predicted", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
colnames(totals_df) <- c("actual", "total")
concordance <- merge(concordance, totals_df, by = "actual")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)

confusion_plot_cet <-
  ggplot(concordance, aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label = paste0(percent, "%")), size = 3) +
  scale_fill_gradient(low = "#F5FBFF", high = "#0070D1") + 
  labs(x = "Actual (final activity pattern)", y = "Predicted (activity pattern of individual source)") + 
  theme_classic() +
  scale_x_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  scale_y_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.title = element_text(size = 11))

confusion_plot_cet

# Section 6: Concordance by category of evidence ----------------------
diel_full_long <- read.csv(here("cetacean_confidence_long_df.csv"))
unique(diel_full$value)

diel_full_long[diel_full_long == "unclear"] <- NA
diel_full_long <- diel_full_long[!is.na(diel_full_long$value),]

#check which diel patterns we are comparing
unique(diel_full_long$value)

#get a list of all the species with more than one source (should be most of them)
species_list <- table(diel_full_long$Species_name)
#should be 72 species with all cetaceans w multiple sources, 67 with only cetaceans in tree with multiple sources
species_list <- names(species_list[species_list > 1])

#function for comparing entries
compTwo <- function(comp1 = "comp1", comp2 = "comp2") {
  
  if(any(is.na(c(comp1, comp2)))) {
    return(NA)
  } else {
    #splits any entries with a backslash into two components (ie nocturnal/crepuscular into nocturnal and crepuscular)
    comp1 <- str_split(comp1, "/")[[1]]
    comp2 <- str_split(comp2, "/")[[1]]
    #then compares if any of the components match
    if(any(comp1 %in% comp2)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
}

#apply this function across all species with multiple entries
output <- lapply(species_list, function(species) {
  
  #filter for one species at a time
  df <- diel_full_long[diel_full_long$Species_name == species,]
  #rename the column names to be unique for every entry for this species (ie for multiple column 2 entries column 2.1, 2.2 etc)
  df$column <- make.unique(df$column)
  
  #converts the dataframe so it compares every entry with each other (ie for A,B,C A-A, A-B, A-C, B-A, B-B, B-C, etc)
  df_lists_comb <- expand(df, nesting(var = column, vector = value), nesting(var2 = column, vector2 = value), .name_repair = "universal")
  
  df_lists_comb <- df_lists_comb %>% filter(var != var2) %>% arrange(var, var2) %>% mutate(vars = paste0(var, ".", var2)) %>% select(contains("var"), everything())
  
  #evaluates the activity patterns for each of these sources and returns if they agree or not (TRUE or FALSE)
  comparisons <- df_lists_comb %>% group_by(vars) %>% mutate(comp = compTwo(comp1 = vector, comp2 = vector2))
  #manipulate the strings for both variable names to revert them back to the original name (back to column 2 from col 2.1)
  comparisons$var <- str_sub(comparisons$var, start = 1, end = 5)
  comparisons$var2 <- str_sub(comparisons$var2, start = 1, end = 5)
  
  #create a column returning the comparison being made (ie col2-col2, col1-col2, etc)
  comparisons$var_final <- paste(comparisons$var, comparisons$var2, sep = "_")
  
  #return just the comparison result column (TRUE or FALSE match) and the comparison being made (ie col1 vs col1)
  return(comparisons[,c("comp","var_final")])
})

#combine this list of results 
output <- Reduce(rbind, output)

table <- table(output$var_final)
# prop.table(table, margin = 1)
table2 <- as.data.frame(prop.table(table(output$var_final, output$comp), margin = 1))
table2$Comp1 <- sapply(str_split(table2$Var1, "_"), `[`, 1)
table2$Comp2 <- sapply(str_split(table2$Var1, "_"), `[`, 2)
table2 <- table2[table2$Var2 == TRUE,]
table2$count <- table

#want to make a plot that has both the frequency and the counts
table2$freq_count <- paste0((round(table2$Freq, 2) * 100), "%", "\n", "(n=", table2$count, ")")
plot_countfreq_cet <- 
  table2[c(1:5, 7:10, 13:15, 19:20, 25), ]  %>% 
  ggplot(., aes(x = Comp1, y = Comp2, fill = Freq, label = freq_count)) +
  geom_tile() + geom_text(size = 3) + scale_fill_viridis(begin = 0.2, end = 1, limits = c(1,0)) + 
  theme_minimal() + ylab("Primary source category") + xlab("Secondary source category") +
  scale_x_discrete(labels = c("A", "B", "C", "D", "E")) +
  scale_y_discrete(labels = c("A", "B", "C", "D", "E")) +
  theme(legend.position = "none")

plot_countfreq_cet


# Section 6.5: Concordance by category of evidence by parvorder --------------------------

#divided by suborder
#check to see if there is a difference in concordance for mysticeti vs odontoceti
diel_full_long_odont <- filter(diel_full_long, Parvorder == "Odontoceti")
diel_full_long_myst <- filter(diel_full_long, Parvorder == "Mysticeti")

#get a list of all the species with more than one source (should be most of them)
species_list1 <- table(diel_full_long_odont$Species_name)
species_list2 <- table(diel_full_long_myst$Species_name)
#should be 72 species with all cetaceans w multiple sources, 67 with only cetaceans in tree with multiple sources
species_list1 <- names(species_list1[species_list1 > 1])
species_list2 <- names(species_list2[species_list2 > 1])

output1 <- lapply(species_list1, function(species) {
  
  #filter for one species at a time
  df <- diel_full_long_odont[diel_full_long_odont$Species_name == species,]
  #rename the column names to be unique for every entry for this species (ie for multiple column 2 entries column 2.1, 2.2 etc)
  df$column <- make.unique(df$column)
  
  #converts the dataframe so it compares every entry with each other (ie for A,B,C A-A, A-B, A-C, B-A, B-B, B-C, etc)
  df_lists_comb <- expand(df, nesting(var = column, vector = value), nesting(var2 = column, vector2 = value), .name_repair = "universal")
  
  df_lists_comb <- df_lists_comb %>% filter(var != var2) %>% arrange(var, var2) %>% mutate(vars = paste0(var, ".", var2)) %>% select(contains("var"), everything())
  
  #evaluates the activity patterns for each of these sources and returns if they agree or not (TRUE or FALSE)
  comparisons <- df_lists_comb %>% group_by(vars) %>% mutate(comp = compTwo(comp1 = vector, comp2 = vector2))
  #manipulate the strings for both variable names to revert them back to the original name (back to column 2 from col 2.1)
  comparisons$var <- str_sub(comparisons$var, start = 1, end = 5)
  comparisons$var2 <- str_sub(comparisons$var2, start = 1, end = 5)
  
  #create a column returning the comparison being made (ie col2-col2, col1-col2, etc)
  comparisons$var_final <- paste(comparisons$var, comparisons$var2, sep = "_")
  
  #return just the comparison result column (TRUE or FALSE match) and the comparison being made (ie col1 vs col1)
  return(comparisons[,c("comp","var_final")])
})
output2 <- lapply(species_list2, function(species) {
  
  #filter for one species at a time
  df <- diel_full_long_myst[diel_full_long_myst$Species_name == species,]
  #rename the column names to be unique for every entry for this species (ie for multiple column 2 entries column 2.1, 2.2 etc)
  df$column <- make.unique(df$column)
  
  #converts the dataframe so it compares every entry with each other (ie for A,B,C A-A, A-B, A-C, B-A, B-B, B-C, etc)
  df_lists_comb <- expand(df, nesting(var = column, vector = value), nesting(var2 = column, vector2 = value), .name_repair = "universal")
  
  df_lists_comb <- df_lists_comb %>% filter(var != var2) %>% arrange(var, var2) %>% mutate(vars = paste0(var, ".", var2)) %>% select(contains("var"), everything())
  
  #evaluates the activity patterns for each of these sources and returns if they agree or not (TRUE or FALSE)
  comparisons <- df_lists_comb %>% group_by(vars) %>% mutate(comp = compTwo(comp1 = vector, comp2 = vector2))
  #manipulate the strings for both variable names to revert them back to the original name (back to column 2 from col 2.1)
  comparisons$var <- str_sub(comparisons$var, start = 1, end = 5)
  comparisons$var2 <- str_sub(comparisons$var2, start = 1, end = 5)
  
  #create a column returning the comparison being made (ie col2-col2, col1-col2, etc)
  comparisons$var_final <- paste(comparisons$var, comparisons$var2, sep = "_")
  
  #return just the comparison result column (TRUE or FALSE match) and the comparison being made (ie col1 vs col1)
  return(comparisons[,c("comp","var_final")])
})

#combine this list of results 
output1 <- Reduce(rbind, output1)
output2 <- Reduce(rbind, output2)

table <- table(output1$var_final)
# prop.table(table, margin = 1)
table2 <- as.data.frame(prop.table(table(output1$var_final, output1$comp), margin = 1))
table2$Comp1 <- sapply(str_split(table2$Var1, "_"), `[`, 1)
table2$Comp2 <- sapply(str_split(table2$Var1, "_"), `[`, 2)
table2 <- table2[table2$Var2 == TRUE,]
table2$count <- table
#want to make a plot that has both the frequency and the counts
table2$freq_count <- paste0((round(table2$Freq, 2) * 100), "%", "\n", "(n=", table2$count, ")")

plot_countfreq_cet_odont <- 
  table2[c(1:5, 7:10, 13:15, 19:20, 25), ]%>% #for odontocetes
  ggplot(., aes(x = Comp1, y = Comp2, fill = Freq, label = freq_count)) +
  geom_tile() + geom_text(size = 3) + scale_fill_viridis(begin = 0.2, end = 1, limits = c(1,0)) + 
  theme_minimal() + ylab("Primary source category") + xlab("Secondary source category") +
  scale_x_discrete(labels = c("A", "B", "C", "D", "E")) +
  scale_y_discrete(labels = c("A", "B", "C", "D", "E")) +
  theme(legend.position = "none")

table <- table(output2$var_final)
# prop.table(table, margin = 1)
table2 <- as.data.frame(prop.table(table(output2$var_final, output2$comp), margin = 1))
table2$Comp1 <- sapply(str_split(table2$Var1, "_"), `[`, 1)
table2$Comp2 <- sapply(str_split(table2$Var1, "_"), `[`, 2)
table2 <- table2[table2$Var2 == TRUE,]
table2$count <- table
#want to make a plot that has both the frequency and the counts
table2$freq_count <- paste0((round(table2$Freq, 2) * 100), "%", "\n", "(n=", table2$count, ")")

plot_countfreq_cet_myst <- 
    table2[c(1:4, 5:7, 10:12, 16:17, 21), ] %>% 
    ggplot(., aes(x = Comp1, y = Comp2, fill = Freq, label = freq_count)) +
    geom_tile() + geom_text(size = 3) + scale_fill_viridis(begin = 0.2, end = 1, limits = c(1,0)) + 
    theme_minimal() + ylab("Primary source category") + xlab("Secondary source category") +
    scale_x_discrete(labels = c("A", "B", "C", "D", "E")) +
    scale_y_discrete(labels = c("A", "B", "C", "D", "E")) +
    theme(legend.position = "none")

pdf(here("Figure_folder/parvorder_category_confusion_plots.pdf"), width = 8.5, height = 3)
plot_countfreq_cet_odont + plot_countfreq_cet_myst + plot_annotation(tag_levels = "a")
dev.off()

# Section 7: Day-night preference sankey pipeline -------------------------------------------

#create dataframe of the number of species that had activity patterns determined at each step
df <- data.frame(
  step_6 = c(rep("A. Multiple category D \n source majority",84)),
  step_5 = c(rep("B. Return category D \n (n = 26)", 26), rep("C. Category D + C \n source majority?", 58)),
  step_4 = c(rep(NA, 26), rep("D. Return category D + C \n (n = 25)", 25), rep("E. Single category \n D source?", 33)),
  step_3 = c(rep(NA, 51), rep("F. Return category D  \n (n = 5)", 5), rep("G. Single category \n C source?", 28)),
  step_2 = c(rep(NA, 56), rep("H. Return category C \n (n = 13)", 13), rep("I. Category E + D + C \n source majority?", 15)),
  step_1 = c(rep(NA, 69), rep("J. Return category E + D + C \n (n = 1)", 1), rep("K. Category A + E + D + C \n source majority?", 14)),
  step_0 = c(rep(NA, 70), rep("L. Return category A + E + D + C \n (n = 9)", 9), rep("M. Else return \n cathemeral (n = 5)", 5)))

#convert to long format for geomsankey
df <- df %>% make_long(step_0, step_1, step_2, step_3, step_4, step_5, step_6)
df <- df[!is.na(df$node), ]

blues <- c("#010661", "#070E8A","#070E8A", "#0044A3","#0044A3", "#0070D1","#0070D1","#2E9DFF","#2E9DFF","#8AC8FF","#8AC8FF","#B8DEFF","#B8DEFF")

sankey_cet <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, label = substr(node, 4, 300))) +
  geom_sankey(flow.alpha= 0.5, node.color = 0.5) + geom_sankey_label(size = 3, color = 1, fill = "white")  + 
  theme_sankey(base_size = 11) + scale_fill_manual(values = blues) +
  theme(legend.position = "none", axis.text.x = element_blank(), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', colour = NA)) + labs(x = NULL) 

sankey_cet + coord_flip()

#save out to figure folder
pdf(here("Figure_folder/Amelia_figures/cet_sankey_plots.pdf"), width = 4.25, height = 5, bg = "transparent")
(sankey_cet + coord_flip())
dev.off()

# Section 8: Crepuscular preference sankey pipeline -------------------------------------------

#create dataframe of the number of species that had activity patterns determined at each step
df <- data.frame(
  step_5 = c(rep("A. Category B + C + D \n source majority? ", 84)),
  step_4 = c(rep("B. Yes \n (n = 12)", 12), rep("C. Tie \n (n = 7)", 7),
             rep("D. No \n (n = 57)", 57), 
             rep("E. Category A + E \n sources (n = 8)",8)),
  step_3 = c(rep("F. ", 12),
             rep("G. Use source \n D > C > B", 4),
             rep("H. Sources in \n same \n category", 3),
             rep("I. Category D \n crepuscular evidence?", 57),
             rep("J. ", 8)),
  step_2 = c(rep("K. Crepuscular \n (n = 23)", 12),
             rep("K. Crepuscular \n (n = 23)", 2),
             rep("L. Non-crepuscular \n (n = 61)", 2),
             rep("K. Crepuscular \n (n = 23)", 3),
             rep("K. Crepuscular \n (n = 23)", 6),
             rep("L. Non-crepuscular \n (n = 61)", 51),
             rep("L. Non-crepuscular \n (n = 61)", 8)))

#convert to long format for geomsankey
df <- df %>% make_long(step_2, step_3, step_4, step_5)
df <- df[!is.na(df$node), ]

#colours by nodes
greens <- c("darkgreen", rep("darkgreen", 4), "orange", rep("green",3), "yellow", "orange", "yellow") 

sankey_crep_cet <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, label = substr(node, 4, 300))) +
  geom_sankey(flow.alpha= 0.5, node.color = 0.5) + geom_sankey_label(size = 3, color = 1, fill = "white")  + 
  theme_sankey(base_size = 11) +  scale_fill_manual(values = greens) +
  theme(legend.position = "none", axis.text.x = element_blank(), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', colour = NA)) + labs(x = NULL) 

sankey_crep_cet + coord_flip()

#save out to figure folder
pdf(here("Figure_folder/cetacean_crep_flowchart.pdf"), height = 3.75, width = 14.3)
sankey_crep_cet + coord_flip()
dev.off()
