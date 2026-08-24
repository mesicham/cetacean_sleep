# Section 1: Load in and format artiodactyla data ------------------------------------------------
url <- 'https://docs.google.com/spreadsheets/d/1JGC7NZE_S36-IgUWpXBYyl2sgnBHb40DGnwPg2_F40M/edit?gid=562902012#gid=562902012'
diel_full <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
#filter for entries that have an activity pattern, I'm ignoring the 7th source because only two species have seven sources
diel_full <- diel_full[!is.na(diel_full$Confidence_primary_source), c(1, 2, 6:17)]
#convert to lower 
diel_full$Diel_Pattern_primary <- tolower(diel_full$Diel_Pattern_primary)
diel_full$Diel_Pattern_secondary <- tolower(diel_full$Diel_Pattern_secondary)
diel_full$Diel_Pattern_tertiary <- tolower(diel_full$Diel_Pattern_tertiary)
diel_full$Diel_pattern_4th_source <- tolower(diel_full$Diel_pattern_4th_source)
diel_full$Diel_Pattern_5th_source <- tolower(diel_full$Diel_Pattern_5th_source)
diel_full$Diel_Pattern_6th_source <- tolower(diel_full$Diel_Pattern_6th_source)
#contains data on 235 of 255 species

diel_full <- diel_full %>% pivot_longer(cols = c(Confidence_primary_source, Confidence_secondary_source, Confidence_tertiary_source, Confidence_4th_source, Confidence_5th_source, Confidence_6th_source), names_to = "column", values_to = "values")

#for each species we can see what confidence level the primary, secondary, tertiary sources are
diel_full$Confidence_level <- paste(diel_full$column, diel_full$values, sep = "_")

#filter for each source (primary -4th) to match each entry with its confidence level
diel_full_1 <- diel_full %>% filter(column == "Confidence_primary_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_Pattern_primary) 
diel_full_1 <- diel_full_1[, c(1, 2, 10:14)]
diel_full_2 <- diel_full %>% filter(column == "Confidence_secondary_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_Pattern_secondary)
diel_full_2 <- diel_full_2[, c(10:15)]
diel_full_3 <- diel_full %>% filter(column == "Confidence_tertiary_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_Pattern_tertiary)
diel_full_3 <- diel_full_3[, c(10:14)]
diel_full_4 <- diel_full %>% filter(column == "Confidence_4th_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_pattern_4th_source)
diel_full_4 <- diel_full_4[, c(10:14)]
diel_full_5 <- diel_full %>% filter(column == "Confidence_5th_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_Pattern_5th_source)
diel_full_5 <- diel_full_5[, c(10:15)]
diel_full_6 <- diel_full %>% filter(column == "Confidence_6th_source") %>% pivot_wider(names_from = Confidence_level, values_from = Diel_Pattern_6th_source)
diel_full_6 <- diel_full_6[, c(10:12)]

diel_full <- cbind(diel_full_1, diel_full_2, diel_full_3, diel_full_4, diel_full_5, diel_full_6)
#rename the columns to the confidence level, R will add .1 for every duplicate so they will end up with unique identifiers
colnames(diel_full) <- paste("Conf", str_sub(colnames(diel_full), -1, -1), sep = "")
diel_full <- diel_full[,order(colnames(diel_full))]

#drop the columns with only NA values
diel_full <- diel_full %>% select(-c("ConfA", "ConfA.1", "ConfA.2", "ConfA.3", "ConfA.4"))

diel_full <- diel_full %>% relocate("Confe", .before = "Conf1")
diel_full <- diel_full %>% relocate("Confy", .after = "Confe")
colnames(diel_full) <- c("Species_name", "Family", colnames(diel_full)[3:27])

diel_full[diel_full == ""] <- NA

#replace strings with standard format
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("diurnal/weakly-crepuscular", "diurnal/crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-nocturnal/crepuscular", "nocturnal/cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-nocturnal", "nocturnal/cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-diurnal/crepuscular", "diurnal/crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-diurnal", "diurnal/cathemeral", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("weakly-cathemeral/crepuscular", "cathemeral/crepuscular", x)}))
#check what this is
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("diurnal-maybe", "diurnal", x)}))
#and this
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("unclear/diurnal", "diurnal", x)}))

write.csv(diel_full, here("confidence_artio_wide.csv"), row.names = FALSE)


# Section 2: Transform data into long format -----------------------------------
#read in wide data
diel_full <- read.csv(here("confidence_artio_wide.csv"))

#convert into long format
diel_full_long <- diel_full %>% pivot_longer(cols = c(3:27), names_to = "column", values_to = "value")

diel_full_long <- diel_full_long[!is.na(diel_full_long$value),]
unique(diel_full_long$value)

#replace strings, can check to see later if these change anything
diel_full_long$value <- str_replace(diel_full_long$value, pattern = "nocturnal/cathemeral", replacement = "cathemeral")
diel_full_long$value <- str_replace(diel_full_long$value, pattern = "diurnal/cathemeral", replacement = "cathemeral")

#check for any unconventional strings
unique(diel_full_long$value)

#check what the highest confidence level is for each species and return that number
confidence_list <- lapply(unique(diel_full_long$Species_name), function(x){
  diel_filtered <- diel_full_long %>% filter(Species_name == x)
  Confidence <- max(diel_filtered$column)
  substr(Confidence, 5,5)
})

#we will add this data to the final dataframe in section 4
confidence_df <- data.frame(Species_name = unique(diel_full_long$Species_name))
confidence_df$Confidence <- as.numeric(confidence_list)

write.csv(diel_full_long, here("confidence_artio_long.csv"), row.names = FALSE)

# Section 3: Objective method of calling activity patterns ------------------

diel_full_long <- read.csv(here("confidence_artio_long.csv"))

#only take the first five characters of the confidence column (ie conf1 instead of conf1.2)
diel_full_long$column <- gsub("\\..*","",diel_full_long$column)

#separate out crepuscularity into its own column
#confidence level 2 data will be unclear/crepuscular since they don't show evidence of nocturnal or diurnal activity
diel_full_long <- separate(diel_full_long, col = value, into = c("new_diel", "crepuscular"), sep = "/")

#set unclear to NA since it does not provide any additional information
diel_full_long[diel_full_long == "unclear"] <- NA

#pipeline for just ruminants
#diel_full_long <- diel_full_long %>% filter(Family %in% c("Bovidae", "Cervidae", "Moschidae", "Tragulidae", "Giraffidae", "Antilocapridae"))

#unlike cetaceans, artiodactyla activity patterns are less cryptic and have informative confidence level 1 sources (ie encyclopedias)
#there are also many more species with only level 1 (60 sps) or 2 data (52 sps)
#revised the function so that when level 3, 4, and 5 are inconclusive or missing it uses level 1 and 2 to make a call
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

tabulateFuncArt <- function(x){
  if(x %>% filter(column == "Conf4") %>% nrow() > 1 & !is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA"))){
    activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA")]
    activity_pattern <- paste(activity_pattern, "A")} else {
      if(is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4", "Conf5")], unique(x$new_diel))), tie_value = "NA"))|x %>% filter(column == "Conf4"|column == "Conf5") %>% nrow() <2){
        if(is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3")], unique(x$new_diel))), tie_value = "NA"))|x %>% filter(column == "Conf4"|column == "Conf5"| column == "Conf3") %>% nrow() <3){
          if (nrow(x[x$column %in% c("Conf4"),]) < 1){
            if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5")], unique(x$new_diel))), tie_value = "NA"))){
              if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3")], unique(x$new_diel))), tie_value = "NA"))|x %>% filter(column == "Conf3") %>% nrow() != 1){
                if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3")], unique(x$new_diel))), tie_value = "NA"))){
                  if (is.na(which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5", "Conf4", "Conf3", "Conf1")], unique(x$new_diel))), tie_value = "NA"))) {
                    activity_pattern <- "cathemeral-variable I"
                  } else {
                    activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3", "Conf4", "Conf5", "Conf1")], unique(x$new_diel))), tie_value = "NA")]
                    activity_pattern <- paste(activity_pattern, "H")}
                } else {
                  activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3")], unique(x$new_diel))), tie_value = "NA")]
                  activity_pattern <- paste(activity_pattern, "G")}
              } else  {
                activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf3")], unique(x$new_diel))), tie_value = "NA")]
                activity_pattern <- paste(activity_pattern, "F")}
            } else {
              activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf5")], unique(x$new_diel))), tie_value = "NA")]
              activity_pattern <- paste(activity_pattern, "E")}
          } else {
            activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4")], unique(x$new_diel))), tie_value = "NA")]
            activity_pattern <- paste(activity_pattern, "D")}
        } else {
          activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4", "Conf5", "Conf3")], unique(x$new_diel))), tie_value = "NA")]
          activity_pattern <- paste(activity_pattern, "C")}
      } else {
        activity_pattern <- unique(x$new_diel)[which.max.simple(tabulate(match(x$new_diel[x$column %in% c("Conf4", "Conf5")], unique(x$new_diel))), tie_value = "NA")]
        activity_pattern <- paste(activity_pattern, "B")}
      
    }
  return(activity_pattern)
}

activity_pattern_df <- diel_full_long[!is.na(diel_full_long$new_diel),] %>% group_by(Species_name) %>% do(tabulated_diel_pattern = tabulateFuncArt(.)) %>% unnest()
#run each species through this function, x species with activity pattern data (di, noc or cath)
activity_pattern_df <- separate(activity_pattern_df, col = "tabulated_diel_pattern", into = c("tabulated_diel_pattern", "level"), sep = " ")
activity_pattern_df$level <- activity_pattern_df$level %>% replace_na("G")
table(activity_pattern_df$level)

unique(activity_pattern_df$tabulated_diel_pattern)

#convert cathemeral-variable to cathemeral
activity_pattern_df$tabulated_diel_pattern <- str_replace(activity_pattern_df$tabulated_diel_pattern, pattern = "cathemeral-variable", replacement = "cathemeral")

#now determine whether or not each species is crepuscular based on majority evidence from all sources
diel_full_long[is.na(diel_full_long)] <- "0"
diel_full_long$crepuscular <- str_replace(diel_full_long$crepuscular, pattern = "crepuscular", replacement = "1")
diel_full_long$crepuscular <- as.numeric(diel_full_long$crepuscular)
diel_full_long$total <- 1
diel_full_long$column <- substr(diel_full_long$column, start = 1, stop = 5)

crep_percent <- diel_full_long %>% filter(column %in% c("Conf1", "Conf2", "Conf3", "Conf4", "Conf5")) %>% group_by(Species_name, column) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>% 
  pivot_wider(id_cols = !c(sum_total, sum_crep), names_from = "column", values_from = percent_crep)

#if majority of conf2-4 sources call a species crepuscular, evaluate to crepuscular
crep_df <- diel_full_long %>% filter(column %in% c("Conf1", "Conf2", "Conf3", "Conf4", "Conf5")) %>% group_by(Species_name) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>%
  merge(., crep_percent[, c(1:6)], by = "Species_name") %>%
  mutate(tabulated_crep = case_when(
    percent_crep > 50  ~ "crepuscular",
    percent_crep < 50  ~ NA,
    #when there is a tie use higher confidence source as tiebreaker
    percent_crep == 50 & Conf3 >= 50 & Conf2 <= 50 ~ "crepuscular",
    percent_crep == 50 & Conf3 <= 50 & Conf2 >= 50 ~ NA,
    percent_crep == 50 & Conf3 >= 50 & Conf4 < 20 ~ NA,
    percent_crep == 50 & Conf2 >= 50 & Conf1 < 50 ~ "crepuscular",
    percent_crep == 50 & Conf2 <= 50 & Conf1 >= 50 ~ NA,
    percent_crep == 50 & Conf4 >= 50 & Conf1 <= 50 ~ "crepuscular",
    percent_crep == 50 & Conf4 <= 50 & Conf1 >= 50 ~ NA,
    percent_crep == 50 & Conf4 >= 50 & Conf3 <= 50 ~ "crepuscular",
    percent_crep == 50 & Conf4 <= 50 & Conf3 >= 50 ~ NA,
    percent_crep == 50 & Conf3 >= 50 & Conf1 <= 50 ~ "crepuscular",
    percent_crep == 50 & Conf3 <= 50 & Conf1 >= 50 ~ NA,
    # #if the sources are in the same confidence level, evaluate to crep
    percent_crep == 50 & Conf5 == 50 ~ "crepuscular",
    percent_crep == 50 & Conf4 == 50 ~ "crepuscular",
    percent_crep == 50 & Conf3 == 50 ~ "crepuscular",
    percent_crep == 50 & Conf2 == 50 ~ "crepuscular",
    percent_crep == 50 & Conf1 == 50 ~ "crepuscular"
  ))

final_df <- merge(crep_df, activity_pattern_df, by = "Species_name", all = TRUE)
final_df <- final_df %>% mutate(tabulated_diel = case_when(is.na(tabulated_crep) ~ tabulated_diel_pattern,
                                                           tabulated_crep == "crepuscular" ~ paste(tabulated_diel_pattern, tabulated_crep, sep = "/")))

unique(final_df$tabulated_diel)

final_df <- final_df[, c("Species_name", "tabulated_diel")]

#check that nothing about the data has changed since running it last 
previous_dataset <- read.csv(here("artio_tabulated_full.csv")) %>% select(Species_name, tabulated_diel)
current_dataset <- final_df

table(previous_dataset$tabulated_diel)
table(current_dataset$tabulated_diel)

all(previous_dataset == current_dataset)
current_dataset[previous_dataset$tabulated_diel != current_dataset$tabulated_diel,] #check what doesn't match
if(all(previous_dataset == current_dataset) == FALSE) stop("Dataset is not the same!")

#add a column for tips, formatted as the species names appear in the phylogenetic tree
final_df$tips <- str_replace(final_df$Species_name, pattern = " ", replacement = "_")

#save out the new tabulated activity pattern dataframe
write.csv(final_df, here("artio_tabulated_full.csv"), row.names = FALSE)

# Section 4: Save out artiodactyla dataframe with taxonomic and confidence info ------------------------
#load in the full dataset 113 species. Should be 235 with data and 255 total. 232 with data in final tree
artio_full <- read.csv(here("confidence_artio_wide.csv"))
#read in the newly categorized dataset, 113 species
artio_tabulated_full <- read.csv(here("artio_tabulated_full.csv"))

#add in the tabulated diel patterns
artio_full <- merge(artio_full, artio_tabulated_full, by = "Species_name", all.x = TRUE)
#save out full version with sources
write.csv(artio_full, here("sleepy_artiodactyla_with_sources.csv"))

#remove unnecessary columns
artio_full <- artio_full[c("Species_name", "Family", "tabulated_diel")]
#add a column for tips, formatted as the species names appear in the phylogenetic tree
artio_full$tips <- str_replace(artio_full$Species_name, pattern = " ", replacement = "_")
colnames(artio_full) <- c("Species_name", "Family", "Diel_Pattern", "tips")

#add taxonomic info for future reference (also to match with cetacean dataset)
artio_full$Parvorder <- "non-cetacean"
#add in order information
artio_full$Order <- "Artiodactyla"

#rename the row names to be the tip names so it's easier to subset by the tree tip labels later
row.names(artio_full) <- artio_full$tips

#Diel_Pattern includes all 6 possible trait states: di, di/crep, noc, noc/crep, cath, cath/crep
#Max_crep will include 4 trait states and maximize crepuscularity: di, noc, cath and crep (di/crep, noc/crep, cath/crep)
artio_full$max_crep <- artio_full$Diel_Pattern
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "nocturnal/crepuscular", replacement = "crepuscular")
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "diurnal/crepuscular", replacement = "crepuscular")
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "cathemeral/crepuscular", replacement = "crepuscular")

artio_full$Suborder <- "Unknown"
for(i in 1:length(artio_full$Species_name)){
  if(artio_full[i, "Family"] %in% c("Camelidae")){
    artio_full[i, "Suborder"] <- "Tylopoda"}
  else if(artio_full[i, "Family"] %in% c("Suidae", "Tayassuidae")){
    artio_full[i, "Suborder"] <- "Suina"}
  else if(artio_full[i, "Family"] %in% c("Bovidae", "Cervidae", "Antilocapridae", "Giraffidae", "Tragulidae", "Moschidae")){
    artio_full[i, "Suborder"] <- "Ruminantia"}
  else if(artio_full[i, "Family"] %in% c("Hippopotamidae")){
    artio_full[i, "Suborder"] <- "Whippomorpha"}
} 

#add in maximum confidence data
artio_full <- merge(artio_full, confidence_df, by = "Species_name")

#put into same order as cetaceans
artio_full <- artio_full %>% select("Species_name", "Order", "Suborder", "Parvorder", "Family", "Diel_Pattern", "max_crep", "Confidence", "tips")

#save out a local copy in case google goes bankrupt
write.csv(artio_full, file = here("sleepy_artiodactyla_minus_cetaceans.csv"), row.names = FALSE)

#save out a version with just ruminants, 103 species
ruminants <- artio_full %>% filter(Suborder == "Ruminantia")
write.csv(ruminants, here("ruminants_full.csv"),row.names = FALSE)

#save out a high confidence version of ruminants
ruminants <- read.csv(here("ruminants_full.csv")) # should be 235 sps(includes NA species)
ruminants_high_conf <- ruminants %>% filter(Confidence %in% c(3,4,5)) #should be x species
write.csv(ruminants_high_conf, file = here("ruminants_high_conf.csv"), row.names = FALSE)

#save out a version with cetaceans, hippos are already in artiodactyla minus cetaceans
cetaceans_full <- read.csv(here("cetaceans_full.csv"))
artiodactyla_full <- rbind(cetaceans_full, artio_full)
write.csv(artiodactyla_full, file = here("sleepy_artiodactyla_full.csv"), row.names = FALSE)

#save out a version of cetacean dataset with hippos
whippomorpha <- read.csv(here("cetaceans_full.csv"))
hippo <- artio_full[artio_full$Family == "Hippopotamidae", ]
whippomorpha <- rbind(whippomorpha, hippo)
write.csv(whippomorpha, file = here("whippomorpha.csv"), row.names = FALSE)

#save out a version with only high confidence data (level 3-5)
whippomorpha <- read.csv(here("whippomorpha.csv")) # should be 100 sps(includes NA species)
whippomorpha_high_conf <- whippomorpha %>% filter(Confidence %in% c(3,4,5)) #should be 76 species
write.csv(whippomorpha_high_conf, file = here("whippomorpha_high_conf.csv"), row.names = FALSE)

# Section 5: Concordance by activity patterns -------------------------
diel_full_long <- read.csv(here("confidence_artio_long.csv"))
#read in the tabulated activity patterns
diel_full <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv"))
#diel_full <- diel_full %>% filter(Suborder == "Ruminantia")
diel_full <- merge(diel_full[, c("Species_name", "Diel_Pattern", "max_crep")], diel_full_long[c("Species_name", "column", "value")])

diel_full$column <- substr(diel_full$column, 1,5)

diel_full <- data.frame(lapply(diel_full, function(x) {gsub("unclear/crepuscular", "crepuscular", x)}))

#for max crep dataset
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("cathemeral/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("diurnal/crepuscular", "crepuscular", x)}))
diel_full <- data.frame(lapply(diel_full, function(x) {gsub("nocturnal/crepuscular", "crepuscular", x)}))

diel_full[diel_full == "unclear"] <- NA
diel_full <- diel_full[!is.na(diel_full$value),]

#filter
mulitple_sources <- diel_full %>% count(Species_name) %>% filter(n>1)
diel_full_filtered <- diel_full[diel_full$Species_name %in% mulitple_sources$Species_name,]
#this removes 115 species without an informative second source (120 out of 235 have a second source)

concordance <- as.data.frame(table(diel_full_filtered$max_crep, diel_full_filtered$value))
colnames(concordance) <- c("actual", "predicted", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
colnames(totals_df) <- c("actual", "total")
concordance <- merge(concordance, totals_df, by = "actual")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)

confusion_plot_rum <-
  ggplot(concordance, aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label= paste0(percent, "%")), size = 3) +
  scale_fill_gradient(low = "#F5FBFF", high = "#0070D1") + 
  labs(x = "Actual (final activity pattern)", y = "Predicted (activity pattern of sources)") + 
  theme_classic() +
  scale_x_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  scale_y_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.title = element_text(size = 11))

#save out combined plots
pdf(here("Figure_folder/combined_confusion_matrix.pdf"), width = 4.3, height = 4.3)
(confusion_plot_cet + labs(x = "", y = "")) /
  (confusion_plot_rum + theme(axis.title = element_blank()))
dev.off()

# Section 6: Concordance by categories of evidence ---------------------------------------------
diel_full_long <- read.csv(here("confidence_artio_long.csv"))

#filter for just ruminants
#diel_full_long <- diel_full_long %>% filter(Family %in% c("Bovidae", "Cervidae", "Moschidae", "Tragulidae", "Giraffidae", "Antilocapridae"))

#remove unclear since it gives no new information
diel_full_long$value <- str_replace(diel_full_long$value, pattern = "unclear/", replacement = "")
diel_full_long[diel_full_long == "unclear"] <- NA
diel_full_long <- diel_full_long[!is.na(diel_full_long$value),]

species_list <- table(diel_full_long$Species_name) #235 species
species_list <- names(species_list[species_list > 1]) #120 species with multiple sources

#function Max wrote for comparing entries
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
  
  #??? idk
  df_lists_comb <- df_lists_comb %>% filter(var != var2) %>% arrange(var, var2) %>% mutate(vars = paste0(var, ".", var2)) %>% select(contains("var"), everything())
  
  #evaluates the activity patterns for each of these sources and returns if they agree or not (TRUE or FALSE)
  comparisons <- df_lists_comb %>% group_by(vars) %>% mutate(comp = compTwo(comp1 = vector, comp2 = vector2))
  #manipulate the strings for both variable names to revert them back to the original name (back to column 2 from col 2.1)
  comparisons$var <- str_sub(comparisons$var, start = 1, end = 5)
  comparisons$var2 <- str_sub(comparisons$var2, start = 1, end = 5)
  
  #create a column returning the comparison being made (ie col2-col2, col1-col2, etc)
  comparisons$var_final <- paste(comparisons$var, comparisons$var2, sep = "-")
  
  #return just the comparison result column (TRUE or FALSE match) and the comparison being made (ie col1 vs col1)
  return(comparisons[,c("comp","var_final")])
})

#combine this list of results 
output <- Reduce(rbind, output)

table <- table(output$var_final)
# prop.table(table, margin = 1)
table2 <- as.data.frame(prop.table(table(output$var_final, output$comp), margin = 1))
table2$Comp1 <- sapply(str_split(table2$Var1, "-"), `[`, 1)
table2$Comp2 <- sapply(str_split(table2$Var1, "-"), `[`, 2)
table2 <- table2[table2$Var2 == TRUE,]
table2$count <- table

#plot both the frequency and the counts
table2$freq_count <- paste0((round(table2$Freq, 2) * 100), "%", "\n", "(n=", table2$count, ")")

plot_countfreq_rum <- table2[c(1:5, 7:10, 13:15, 19:20, 25), ] %>% 
  ggplot(., aes(x = Comp1, y = Comp2, fill = Freq, label = freq_count)) +
  geom_tile() + geom_text(size = 3) + scale_fill_viridis(begin = 0.2, end = 1, limits = c(1,0)) + 
  theme_minimal() + ylab("Primary source category") + xlab("Secondary source category") +
  scale_x_discrete(labels = c("A", "B", "C", "D", "E")) +
  scale_y_discrete(labels = c("A", "B", "C", "D", "E")) +
  theme(legend.position = "none")

plot_countfreq_rum

#ave out the combined confusion plots for cetaceans and terrestrial artiodactyls
pdf(here("Figure_folder/combined_category_confusion_plots.pdf"), width = 8.5, height = 3)
plot_countfreq_cet + plot_countfreq_rum
dev.off()

# Section 7: Day-night preference sankey pipeline ------------------------------------------

#non-cetacean artiodactyls
df <- data.frame(
  step_8 = c(rep("A. Multiple category D \n source majority?", 235)),
  step_7 = c(rep("B. Return category D \n (n = 11)", 11), rep("C. Category D + E \n source majority?", 224)),
  step_6 = c(rep(NA, 11), rep("D. Return category D + E \n (n = 1)", 1), rep("E. Category C + D + E \n source majority?", 223)),
  step_5 = c(rep(NA, 12), rep("F. Return category C + D + E \n (n = 36)", 36), rep("G. Single category \n D source?", 187)),
  step_4 = c(rep(NA, 48), rep("H. Return category D source \n (n = 40)", 40), rep("I. Multiple category E \n source majority?", 147)),
  step_3 = c(rep(NA, 88), rep("J. Return category E (n = 5)",5), rep("K. Multiple category C \n source majority?", 142)),
  step_2 = c(rep(NA, 93), rep("L. Return category C (n = 67)", 67), rep("M. Single category \n C source?", 75)),
  step_1 = c(rep(NA, 160), rep("N. Return category C \n source (n = 8)", 8), rep("O. Category A + C + D + E \n source majority?", 67)),
  step_0 = c(rep(NA, 168), rep("P. Return category A + C + D + E \n (n = 58)",58), rep("Q. Else return \n cathemeral (n = 9)", 9)))


#convert to long format for geomsankey
df <- df %>% make_long(step_0, step_1, step_2, step_3, step_4, step_5, step_6, step_7, step_8)
df <- df[!is.na(df$node), ]

blues <- c("#010661", "#070E8A","#070E8A", "#0044A3","#0044A3", "#0070D1","#0070D1","#2E9DFF", "#2E9DFF","#5CB3FF","#5CB3FF","#8AC8FF","#8AC8FF","#B8DEFF","#B8DEFF","#E6F3FF" , "#E6F3FF")

sankey_rum <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, label = substr(node, 4, 300))) +
  geom_sankey(flow.alpha= 0.5, node.color = 0.5) + geom_sankey_label(size = 3, color = 1, fill = "white")  + 
  theme_sankey(base_size = 10) + scale_fill_manual(values = blues) +
  #scale_fill_manual(values = rep("transparent", 17)) +
  theme(legend.position = "none", axis.text.x = element_blank(), panel.background = element_rect(fill='transparent', colour = NA), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', colour = NA)) + labs(x = NULL) 

sankey_rum + coord_flip()

#save out to figure folder
pdf(here("Figure_folder/rum_sankey_plots.pdf"), width =10.25, height = 5, bg = "transparent")
(sankey_rum + coord_flip())
dev.off()

# Section 8: Crepuscular preference sankey pipeline -------------------------------------------

#create dataframe of the number of species that had activity patterns determined at each step, artio
df <- data.frame(
  step_5 = c(rep("A. Category A + B + C + D \n + E source majority? ", 235)),
  step_4 = c(rep("B. Yes \n (n = 123)", 123), rep("C. Tie \n (n = 28)", 28),
             rep("D. No \n (n = 84)", 84)),
  step_3 = c(rep("F. ", 123),
             rep("G. Use source \n D > C > B > A", 19),
             rep("H. Sources in \n same category", 9),
             rep("I. Category D \n crepuscular evidence?", 84)),
  step_2 = c(rep("K. Crepuscular \n (n = 138)", 123),
             rep("K. Crepuscular \n (n = 138)", 6),
             rep("L. Non-crepuscular \n (n = 97)", 13),
             rep("K. Crepuscular \n (n = 138)", 9),
             rep("L. Non-crepuscular \n (n = 97)", 84))
)

#convert to long format for geomsankey
df <- df %>% make_long(step_2, step_3, step_4, step_5)
df <- df[!is.na(df$node), ]

#colours by nodes
greens <- c("darkgreen", rep("darkgreen", 3), "orange", rep("green",3),"orange", "yellow") 

sankey_crep_rum <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = node, label = substr(node, 4, 300))) +
  geom_sankey(flow.alpha= 0.5, node.color = 0.5) + geom_sankey_label(size = 3, color = 1, fill = "white")  + 
  theme_sankey(base_size = 11) +  scale_fill_manual(values = greens) +
  theme(legend.position = "none", axis.text.x = element_blank(), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', colour = NA)) + labs(x = NULL) 

sankey_crep_rum + coord_flip()  

#save out to figure folder
pdf(here("Figure_folder/ruminant_crep_flowchart.pdf"), height = 3.75, width = 4.3)
sankey_crep_rum + coord_flip()
dev.off()

# Section 9: Number of sources in each category-------------------------------------------------------
cet_diel_long <- read.csv(here("cetacean_confidence_long_df.csv"))
cet_diel_long$column <- gsub("\\..*","",cet_diel_long$column)

sources_df <- as.data.frame(table(cet_diel_long$column)) %>% mutate(Clade = "Cetacea")

art_full_long <- read.csv(here("confidence_artio_long.csv"))
art_full_long$column <- gsub("\\..*","",art_full_long$column)
sources_df <- rbind(sources_df, (as.data.frame(table(art_full_long$column))) %>% mutate(Clade = "Terrestrial \nartiodactyls"))

number_sources <- ggplot(sources_df, aes(x = Var1, y = Freq, fill = Clade)) + geom_col(position = position_dodge()) +
  labs(y = "Number of sources", x = "Data category") + theme_classic() + scale_fill_manual(values = c("#070E8A","#2E9DFF")) +
  scale_x_discrete(labels = c("A", "B", "C", "D", "E")) +
  theme(panel.grid = element_blank(), legend.position = "none")
number_sources

#plot number of sources per species
sources_df <- as.data.frame(table(test_diel_long$Species_name)) %>% mutate(Clade = "Cetacea")
sources_df <- rbind(sources_df, (as.data.frame(table(diel_full_long$Species_name))) %>% mutate(Clade = "Terrestrial \nartiodactyls"))

#more than six group together
sources_df <- sources_df %>% mutate(Freq = case_when(Freq > 6 ~ 7,
                                                     Freq <= 6 ~ Freq))

total_sources <- sources_df %>% group_by(Clade, Freq) %>% summarize(Freq = Freq, total_species = n()) %>%
  ggplot(., aes(x = Freq, y = total_species, fill = Clade)) +
  geom_col(position = position_dodge()) +
  #geom_histogram(position = position_dodge(width = 0.9), binwidth = 1, bins = 7) +
  labs(y = "Number of species", x = "Number of sources per species") + theme_classic() + scale_fill_manual(values = c("#070E8A","#2E9DFF")) +
  scale_x_continuous(breaks = 1:7, labels = c(1,2,3,4,5,6, "7 or more")) +
  theme(panel.grid = element_blank(), legend.position = "inside", legend.position.inside = c(0.85, 0.8))
total_sources

pdf(here("Figure_folder/source_count_barchart.pdf"), height = 2.5, width = 8)
number_sources + total_sources 
dev.off()

# Section 10: Cetacean and ruminant df with sources -------------------------------------

#cetaceans
url <- 'https://docs.google.com/spreadsheets/d/1eG_WIbhDzSv_g-PY90qpTMteESgPZZZt772g13v-H1o/edit?usp=sharing'
#remove any sources I've now updated to be unclear (should all be level 2)
sources <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings=c("", "Unclear", "unclear"))

sources <- sources[!is.na(sources$Source.1), c(1, 23:33)]

sources$all_sources <- paste(sources$Source.1, sources$Source.2, sources$Source.3,sources$Source.4,sources$Source.5,sources$Source.6,sources$Source.7,sources$Source.8,sources$Source.9,sources$Source.10,sources$Source.11, sep = ";")
cet_sources <- sources %>% mutate(all_sources = str_replace_all(all_sources, pattern = ";NA", replacement = "")) %>%
  select(Species_name, all_sources) %>%  separate_longer_delim(all_sources, delim = ";")

#give every source a unique reference number
cet_sources$all_sources_numbered <- paste(1:nrow(cet_sources), cet_sources$all_sources, sep = ". ")
cet_sources$numbered <- 1:nrow(cet_sources)

cetacean_numbers <- cet_sources %>% select(Species_name, numbered) %>% pivot_wider(names_from = "Species_name", values_from = "numbered") %>% pivot_longer(everything(), names_to = "Species_name", values_to = "numbered")

#replace sources in the dataset with their reference numbers
cetaceans_full <- read.csv(here("cetaceans_full.csv"))
cetaceans_full <- cetaceans_full[!is.na(cetaceans_full$Diel_Pattern), c(1, 3, 5:7)]
cetaceans_full <- merge(cetaceans_full, cetacean_numbers, by = "Species_name")

colnames(cetaceans_full) <- c("Species_name", "Suborder", "Family", "Activity_pattern", "Maximum_crepuscularity_activity_pattern", "References")

#terrestrial artiodactyls
url <- 'https://docs.google.com/spreadsheets/d/1JGC7NZE_S36-IgUWpXBYyl2sgnBHb40DGnwPg2_F40M/edit?gid=562902012#gid=562902012'
sources <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE, na.strings = "")
sources <- sources[!is.na(sources$Source.1), c(1, 26:32)]

sources$all_sources <- paste(sources$Source.1, sources$Source.2, sources$Source.3,sources$Source.4,sources$Source.5,sources$Source.6,sources$Source.7, sep = ";")
artio_sources <- sources %>% mutate(all_sources = str_replace_all(all_sources, pattern = ";NA", replacement = "")) %>%
  select(Species_name, all_sources) %>% 
  mutate(all_sources = str_replace(all_sources, pattern = "CO;2", replacement = "CO:2")) %>% #manually change links with a ; character
  separate_longer_delim(all_sources, delim = ";") %>% 
  mutate(all_sources = str_replace(all_sources, pattern = "CO:2", replacement = "CO;2"))#change the links back to a ;

#give every source a unique reference number (starting from the end of the cetacean df)
artio_sources$all_sources_numbered <- paste((max(cet_sources$numbered)+1):((max(cet_sources$numbered))+nrow(artio_sources)), artio_sources$all_sources, sep = ". ")
artio_sources$numbered <- (max(cet_sources$numbered)+1):((max(cet_sources$numbered))+nrow(artio_sources))

artio_numbers <- artio_sources %>% select(Species_name, numbered) %>% pivot_wider(names_from = "Species_name", values_from = "numbered") %>% pivot_longer(everything(), names_to = "Species_name", values_to = "numbered")

artio_full <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv"))
artio_full <- artio_full[!is.na(artio_full$Diel_Pattern), c(1, 3, 5:7)]

artio_full <- merge(artio_full, artio_numbers, by = "Species_name")
colnames(artio_full) <- c("Species_name", "Suborder", "Family", "Activity_pattern", "Maximum_crepuscularity_activity_pattern", "References")

artio_full <- rbind(cetaceans_full, artio_full)
artio_sources <- rbind(cet_sources, artio_sources) %>% select(Species_name, all_sources_numbered) 
colnames(artio_sources) <- c("Species_name", "Reference")

artio_full$References <- lapply(artio_full$References,as.numeric)
artio_full <- artio_full %>% mutate(References = gsub("c(", "", References, fixed=TRUE)) %>%
  mutate(References = gsub(")", "", References, fixed=TRUE))

#save to figures folder
write.csv(artio_full, here("Figure_folder/artiodactyla_with_sources.csv"), row.names = FALSE)
write.csv(artio_sources, here("Figure_folder/artiodactyla_references.csv"), row.names = FALSE)
