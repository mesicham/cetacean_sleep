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

#collapse columns
#diel_full$Conf1

diel_full[diel_full == ""] <- NA

#replace strings
#may change to cathemeral/nocturnal/crepuscular in future, 
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


# Section 2: Artiodactyla long format -----------------------------------
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

write.csv(diel_full_long, here("artio_confidence_long_final.csv"), row.names = FALSE)

#pipeline for just ruminants
#diel_full_long <- diel_full_long %>% filter(Family %in% c("Bovidae", "Cervidae", "Moschidae", "Tragulidae", "Giraffidae", "Antilocapridae"))

#unlike cetaceans, artiodactyla activity patterns are less cryptic and have informative confidence level 1 sources (ie encyclopedias)
#there are also many more species with only level 1 (60 sps) or 2 data (52 sps)
#revise the function so that when level 3, 4, and 5 are inconclusive or missing it uses level 1 and 2 to make a call
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

x <- filter(diel_full_long, Species_name == "Cephalophus jentinki")

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

#run each species through this function, x species with activity pattern data (di, noc or cath)
#activity_pattern_df <- diel_full_long[!is.na(diel_full_long$new_diel),] %>% group_by(Species_name) %>% do(tabulated_diel_pattern = tabulateFunc3(.)) %>% unnest()

activity_pattern_df <- diel_full_long[!is.na(diel_full_long$new_diel),] %>% group_by(Species_name) %>% do(tabulated_diel_pattern = tabulateFuncArt(.)) %>% unnest()
#run each species through this function, x species with activity pattern data (di, noc or cath)
activity_pattern_df <- separate(activity_pattern_df, col = "tabulated_diel_pattern", into = c("tabulated_diel_pattern", "level"), sep = " ")
activity_pattern_df$level <- activity_pattern_df$level %>% replace_na("G")
table(activity_pattern_df$level)

unique(activity_pattern_df$tabulated_diel_pattern)

#convert cathemeral-variable to cathemeral
activity_pattern_df$tabulated_diel_pattern <- str_replace(activity_pattern_df$tabulated_diel_pattern, pattern = "cathemeral-variable", replacement = "cathemeral")

#now determine whether or not each species is crepuscular
diel_full_long[is.na(diel_full_long)] <- "0"
diel_full_long$crepuscular <- str_replace(diel_full_long$crepuscular, pattern = "crepuscular", replacement = "1")
diel_full_long$crepuscular <- as.numeric(diel_full_long$crepuscular)
diel_full_long$total <- 1
diel_full_long$column <- substr(diel_full_long$column, start = 1, stop = 5)

#x <- filter(diel_full_long, Species_name == "Aepyceros melampus")

tabulateCrep = function(x){
  df <- aggregate(x$crepuscular, by = list(Category = x$column), FUN = sum)
  #confidence level 1 and 5 don't have any information on crepuscularity so remove them, they will always be zero
  df2 <- aggregate(x$total, by = list(Category = x$column), FUN = sum)
  df2 <- merge(df, df2, by = "Category")
  df2$percentage <- round((df2$x.x/df2$x.y) * 100, digits = 1)
  #define what percentage of each category we want to call a species crepuscular,
  df3 <- data.frame(Category = c("Conf1", "Conf2", "Conf3", "Conf4", "Conf5"), cutoff = c(50, 50, 50, 20,20))
  df2 <- merge(df2, df3, by = "Category")  #species can have Conf2, Conf3 and/or Conf4 evidence, when merged any missing categories will be dropped
  df2$crep_evidence <- df2$percentage >= df2$cutoff
  df2[df2$crep_evidence == TRUE, "crep_evidence"] <- "crepuscular"
  df2[df2$crep_evidence == FALSE, "crep_evidence"] <- "non"
  unique_percents <- unique(df2$crep_evidence)
  
  #check if there is a tie, if so take the higher confidence level
  if(nrow(df2[df2$crep_evidence == "crepuscular",]) == nrow(df2[df2$crep_evidence == "non",]) ){
    #take the higher confidence level to break ties
    total_evidence <- df2[which(df2$Category == max(df2$Category)), "crep_evidence"]
    #alternative method: take whatever has more sources
    #total_evidence <- df2[which.max(df2$x.y), "crep_evidence"]
  } else {
    #if no tie then take the maximum value
    total_evidence <- unique_percents[which.max(tabulate(match(df2$crep_evidence, unique_percents)))]
  }
  
  #additional screen: if there is level 4 evidence then evaluate to true 
  if(nrow(filter(df2, Category == "Conf4"))==1){
    if(df2[df2$Category == "Conf4", "crep_evidence"] == "crepuscular"){
      total_evidence <- "crepuscular"
    }
  } 
  return(total_evidence)
}

crep_df <- diel_full_long %>% group_by(Species_name) %>% do(tabulated_crep = tabulateCrep(.)) %>% unnest()

crep_df <- crep_df[!is.na(crep_df$tabulated_crep),]

#alternative method
test <- diel_full_long %>% filter(column %in% c("Conf1", "Conf2", "Conf3", "Conf4", "Conf5")) %>% group_by(Species_name, column) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>% 
  pivot_wider(id_cols = !c(sum_total, sum_crep), names_from = "column", values_from = percent_crep)

test <- test %>% mutate(evidence1 = as.numeric(Conf1 >= 50), evidence2 = as.numeric(Conf2 >= 50), evidence3 = as.numeric(Conf3 >=50), evidence4 = as.numeric(Conf4 >=20), evidence5 = as.numeric(Conf5>=20)) %>%
  mutate(total_evidence = sum(c_across(evidence1:evidence5),na.rm=TRUE), total_sources = sum(as.numeric(!is.na(c_across(evidence1:evidence5))))) %>%
  mutate(tabulated_crep = case_when(total_evidence == 0 ~ "non",
                                    total_evidence/total_sources > 0.5 ~ "crepuscular",
                                    total_evidence/total_sources < 0.5 ~ "non",
                                    #total_evidence/total_sources == 0.5 ~ "tie"))
                                    #to break ties use the higher confidence level
                                    total_sources == 2 & evidence4 == 0 ~ "non",
                                    total_sources == 2 & evidence4 == 1 ~ "crepuscular",
                                    total_sources == 2 & evidence3 == 1 & evidence2 == 0 ~ "crepuscular",
                                    total_sources == 2 & evidence3 == 0 & evidence2 == 1 ~ "non",
                                    total_sources == 2 & evidence3 == 1 & evidence1 == 0 ~ "crepuscular",
                                    total_sources == 2 & evidence3 == 0 & evidence1 == 1 ~ "non",
                                    total_sources == 2 & evidence2 == 1 & evidence1 == 0 ~ "crepuscular",
                                    total_sources == 2 & evidence2 == 0 & evidence1 == 1  ~ "non"))


matches <- crep_df[crep_df$Species_name %in% test$Species_name, "tabulated_crep"] == test[, c("tabulated_crep")]
test[!matches,]

#alternative method: what if I didn't average by category like the day-night pipeline
test2 <- diel_full_long %>% filter(column %in% c("Conf1", "Conf2", "Conf3", "Conf4", "Conf5")) %>% group_by(Species_name) %>% 
  summarize(sum_crep = sum(crepuscular), sum_total = sum(total))  %>% mutate(percent_crep = (sum_crep/sum_total)*100) %>%
  merge(., test[, c(1:6)], by = "Species_name") %>%
  mutate(tabulated_crep = case_when(
    #Conf4 >= 20 ~ "conf4_crepuscular",
                                    percent_crep > 50  ~ "crepuscular",
                                    percent_crep < 50  ~ "non",
                                    percent_crep == 50 ~ "tie" #when there is a tie use higher confidence source as tiebreaker
                                    # percent_crep == 50 & Conf3 >= 50 & Conf2 <= 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf3 <= 50 & Conf2 >= 50 ~ "non",
                                    # percent_crep == 50 & Conf3 >= 50 & Conf4 < 20 ~ "non",
                                    # percent_crep == 50 & Conf2 >= 50 & Conf1 < 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf2 <= 50 & Conf1 >= 50 ~ "non",
                                    # percent_crep == 50 & Conf4 >= 50 & Conf1 <= 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf4 <= 50 & Conf1 >= 50 ~ "non",
                                    # percent_crep == 50 & Conf3 >= 50 & Conf1 <= 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf3 <= 50 & Conf1 >= 50 ~ "non",
                                    # # #if the sources are in the same confidence level, evaluate to crep
                                    # percent_crep == 50 & Conf5 == 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf4 == 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf3 == 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf2 == 50 ~ "crepuscular",
                                    # percent_crep == 50 & Conf1 == 50 ~ "crepuscular"
                                    ))

matches <- crep_df[crep_df$Species_name %in% test2$Species_name, "tabulated_crep"] == test2[, c("tabulated_crep")]
test2[!matches,]
table(test2$tabulated_crep)

final_df <- merge(crep_df, activity_pattern_df, by = "Species_name")
final_df$tabulated_diel <- final_df$tabulated_diel_pattern
for(i in 1:nrow(final_df)){
  if(final_df[i, "tabulated_crep"] == "crepuscular"){
    final_df[i, "tabulated_diel"] <- paste(final_df[i, "tabulated_diel_pattern"], "crepuscular", sep = "/")
  }
}

unique(final_df$tabulated_diel)

final_df <- final_df[, c("Species_name", "tabulated_diel")]

#check that nothing about the data has changed since running it last 
previous_dataset <- read.csv(here("artio_tabulated_full.csv"))
previous_dataset$tips <- str_replace(previous_dataset$Species_name, pattern = " ", replacement = "_")
previous_dataset <- previous_dataset[previous_dataset$tips %in% mam.tree$tip.label, ]

current_dataset <- final_df
current_dataset$tips <- str_replace(current_dataset$Species_name, pattern = " ", replacement = "_")
current_dataset <- current_dataset[current_dataset$tips %in% mam.tree$tip.label, ]
all(previous_dataset == current_dataset)
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

#create the three databases we will use 
#Diel_Pattern includes all 6 possible trait states: di, di/crep, noc, noc/crep, cath, cath/crep
#Max_crep will include 4 trait states and maximize crepuscularity: di, noc, cath and crep (di/crep, noc/crep, cath/crep)
artio_full$max_crep <- artio_full$Diel_Pattern
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "nocturnal/crepuscular", replacement = "crepuscular")
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "diurnal/crepuscular", replacement = "crepuscular")
artio_full$max_crep <- str_replace(artio_full$max_crep, pattern = "cathemeral/crepuscular", replacement = "crepuscular")
#Max_dinoc will include 4 trait states and maximize di and noc, di (including di/crep), noc (including noc/crep), cath, crep (cath/crep)
artio_full$max_dinoc <- artio_full$Diel_Pattern
artio_full$max_dinoc <- str_replace(artio_full$max_dinoc, pattern = "nocturnal/crepuscular", replacement = "nocturnal")
artio_full$max_dinoc <- str_replace(artio_full$max_dinoc, pattern = "diurnal/crepuscular", replacement = "diurnal")
artio_full$max_dinoc <- str_replace(artio_full$max_dinoc, pattern = "cathemeral/crepuscular", replacement = "crepuscular")

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
artio_full <- artio_full %>% select("Species_name", "Order", "Suborder", "Parvorder", "Family", "Diel_Pattern", "max_crep", "max_dinoc", "Confidence", "tips")

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

# Section 5: Concordance between confidence levels (artio/ruminants) ---------------------------------------------
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

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/artio_btw_source_concordance.pdf", width = 7, height = 7, bg = "transparent")
#pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/ruminant_btw_source_concordance.pdf", width = 7, height = 7, bg = "transparent")
plot_countfreq_rum
dev.off()


# Section 6: Concordance within confidence levels artio/ruminants -------------------------
diel_full_long <- read.csv(here("confidence_artio_long.csv"))
#read in the tabulated activity patterns
diel_full <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv"))
#diel_full <- diel_full %>% filter(Suborder == "Ruminantia")
diel_full <- merge(diel_full[, c("Species_name", "Diel_Pattern", "max_crep", "max_dinoc")], diel_full_long[c("Species_name", "column", "value")])

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
  theme_void() +
  scale_x_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  scale_y_discrete(labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.title = element_text(size = 11))

# pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/ruminants_all_conf_levels_confusion_matrix.pdf", width = 4, height = 2)
# confusion_plot_rum 
# dev.off()

#save out combined plots
pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_confusion_matrix.pdf", width = 4, height = 4.3)
(confusion_plot_cet + labs(x = "\n", y = "")) /
  (confusion_plot_rum + theme(axis.title.y = element_blank()))
dev.off()


# Section 7: Sankey pipeline flowchart ------------------------------------------

# #ruminants only
# df <- data.frame(
#   step_8 = c(rep("A. Multiple category D \n source majority?", 206)),
#   step_7 = c(rep("B. Return category D \n (n = 10)", 10), rep("C. Category D + E \n source majority?", 196)),
#   step_6 = c(rep(NA, 10), rep("D. Return category D + E \n (n = 1)", 1), rep("E. Category C + D + E \n source majority?", 195)),
#   step_5 = c(rep(NA, 11), rep("F. Return category C + D + E \n (n = 28)", 28), rep("G. Single category \n D source?", 167)),
#   step_4 = c(rep(NA, 39), rep("H. Return category D source \n (n = 36)", 36), rep("I. Multiple category E \n source majority?", 131)),
#   step_3 = c(rep(NA, 75), rep("J. Return category E (n = 3)",3), rep("K. Multiple category C \n source majority?", 128)),
#   step_2 = c(rep(NA, 78), rep("L. Return category C (n = 64)", 64), rep("M. Single category \n C source?", 64)),
#   step_1 = c(rep(NA, 142), rep("N. Return category C \n source (n = 6)", 6), rep("O. Category A + C + D + E \n source majority?", 58)),
#   step_0 = c(rep(NA, 148), rep("P. Return category A + C + D + E \n (n = 51)",51), rep("Q. Else return \n cathemeral (n = 7)", 7)))

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
pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/rum_sankey_plots.pdf", width =10.25, height = 5, bg = "transparent")
(sankey_rum + coord_flip())
dev.off()

# Section 8: Crepuscularity sankey -------------------------------------------

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
pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/ruminant_crep_flowchart.pdf", height = 3.75, width = 4.3)
sankey_crep_rum + coord_flip()
dev.off()

# Section 9: Maor data comparison ---------------------------------------------

#my data
artio_df <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv")) #235 species with data
artio_df <- artio_df %>% select(Species_name, Diel_Pattern, max_crep, tips)

#Maor dataset
Maor_diel <- read.csv(here("Maor_mam_data.csv")) 
Maor_diel <- Maor_diel[Maor_diel$tips %in% artio_df$tips, ] #154 when filtering for those in my dataframe

unique(Maor_diel$Maor_activity_pattern)

#merge my artiodactyla data with the mammal data
Maor_diel <- merge(Maor_diel, artio_df, by = "tips", all.x = TRUE)  

df <- Maor_diel %>% make_long(Maor_activity_pattern, Diel_Pattern) %>% mutate(node = str_to_title(node), next_node = str_to_title(next_node))

Maor_sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 4, color = 1, fill = "white") +
  #scale_fill_manual(values = custom.colours) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Diel_pattern" = "Maor et al \n (n = 193)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Maor_diel$exact_match <- Maor_diel$Maor_activity_pattern == Maor_diel$Diel_Pattern

#function Max wrote for comparing entries, splits and compares each segment
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

Maor_diel$approx_match <- "Unknown"

for(i in 1:nrow(Maor_diel)){
  Maor_diel[i, "approx_match"] <- compTwo(comp1 = Maor_diel[i, "Maor_activity_pattern"], comp2 =  Maor_diel[i, "Diel_Pattern"])
}

#version with maximum crepuscular and cathemeral classifications
unique(mammals_df$Maor_diel)
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/diurnal/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/diurnal/crepuscular/ultradian", replacement = "cathemeral/crepuscular")
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "diurnal/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/diurnal/crepuscular", replacement = "cathemeral/crepuscular")

Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "diurnal/nocturnal", replacement = "cathemeral")
Maor_diel$Maor_activity_pattern <- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/diurnal", replacement = "cathemeral")
Maor_diel$Maor_activity_pattern<- str_replace(Maor_diel$Maor_activity_pattern, pattern = "cathemeral/nocturnal", replacement = "cathemeral")

Maor_diel <- data.frame(lapply(Maor_diel, function(x) {gsub("cathemeral/crepuscular", "Crepuscular", x)}))
Maor_diel <- Maor_diel %>% mutate(Diel_Pattern = str_to_title(Diel_Pattern), Maor_activity_pattern = str_to_title(Maor_activity_pattern))
Maor_diel <- data.frame(lapply(Maor_diel, function(x) {gsub("/C", " and\n c", x)}))

Maor_sankey2 <-  Maor_diel %>% make_long(Maor_activity_pattern, Diel_Pattern) %>%
  ggplot(., aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.3, color = 1, fill = "white") +
  scale_fill_manual(values = c("#dd8ae7","#EECBAD" , "#FC8D62", "gold", "#66C2A5",  "palegreen")) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Diel_pattern" = "Maor et al \n (n = 193)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Maor_sankey2 

# Section : Bennie et al data comparison ----------------------------------
#my data
artio_df <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv")) #235 species with data
artio_df <- artio_df %>% select(Species_name, Diel_Pattern, max_crep, tips)

#Bennie dataset
Bennie_diel <- read.csv(here("Bennie_mam_data.csv")) #447 species
Bennie_diel <- Bennie_diel[Bennie_diel$tips %in% artio_df$tips, ] #224 sps when filtering for those in my dataframe 

#merge my artiodactyla data with the mammal data
Bennie_diel <- merge(Bennie_diel, artio_df, by = "tips", all.x = TRUE) 
Bennie_diel$exact_match <- Bennie_diel$Bennie_activity_pattern == Bennie_diel$Diel_Pattern

#function Max wrote for comparing entries, splits and compares each segment
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

Bennie_diel$approx_match <- "Unknown"

for(i in 1:nrow(Bennie_diel)){
  Bennie_diel[i, "approx_match"] <- compTwo(comp1 = Bennie_diel[i, "Bennie_activity_pattern"], comp2 =  Bennie_diel[i, "Diel_Pattern"])
}

Bennie_diel <- data.frame(lapply(Bennie_diel, function(x) {gsub("cathemeral/crepuscular", "Crepuscular", x)}))
Bennie_diel <- Bennie_diel %>% mutate(Diel_Pattern = str_to_title(Diel_Pattern), Bennie_activity_pattern = str_to_title(Bennie_activity_pattern))
Bennie_diel <- data.frame(lapply(Bennie_diel, function(x) {gsub("/C", " and\n c", x)}))

Bennie_sankey <- Bennie_diel %>% make_long(Bennie_activity_pattern, Diel_Pattern) %>%
  ggplot(., aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.3, color = 1, fill = "white") + 
  scale_fill_manual(values = c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5", "gold", "palegreen")) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Bennie_activity_pattern" = "Bennie et al \n (n = 224)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Bennie_sankey


# Section 10: Baker et al comparison -----------------------------------------
#my data
artio_df <- read.csv(here("sleepy_artiodactyla_full.csv")) #235 species with data

#Baker et al dataset, a combination of primary data (200sps), the Bennie et al dataset and pantheria
Baker_df <- read.csv(here("Baker_mam_data.csv"))

Baker_df <- Baker_df[Baker_df$tips %in% artio_df$tips, ] #removes 5 sps
# M rooseveltorum nor Sus bucculentus (both extinct) so drop, Bos frontalis and M gouazoupira not in mam tree
# Alces americanus is a subspecies of Alces alces which is already in dataset

Baker_df <- merge(Baker_df, artio_df, by = "tips", all.x = TRUE) #204 sps

Baker_df$exact_match <- Baker_df$Activity_pattern == Baker_df$Diel_Pattern

#function Max wrote for comparing entries, splits and compares each segment
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

Baker_df$approx_match <- "Unknown"

for(i in 1:nrow(Baker_df)){
  Baker_df[i, "approx_match"] <- compTwo(comp1 = Baker_df[i, "Activity_pattern"], comp2 =  Baker_df[i, "Diel_Pattern"])
}

#optional: convert cathemeral/crepuscular species to just crepuscular
Baker_df <- data.frame(lapply(Baker_df, function(x) {gsub("cathemeral/crepuscular", "crepuscular", x)}))
Baker_df <- Baker_df %>% mutate(Diel_Pattern = str_to_title(Diel_Pattern), Activity_pattern = str_to_title(Activity_pattern))
Baker_df <- data.frame(lapply(Baker_df, function(x) {gsub("/C", " and\n c", x)}))

Baker_sankey <-  Baker_df %>% make_long(Activity_pattern, Diel_Pattern) %>%
  ggplot(., aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + 
  geom_sankey_label(size = 3.3, color = 1, fill = "white", width = 0.1) + 
  scale_fill_manual(values = c("#dd8ae7", "#FC8D62", "#66C2A5","#EECBAD", "gold", "palegreen")) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Activity_pattern" = "Existing dataset \n (Baker et al)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Baker_sankey


# Section: Number of matches plot -----------------------------------------
#plot number of matches
table(Baker_df$approx_match)
table(Baker_df$exact_match)
table(Maor_diel$approx_match)
table(Maor_diel$exact_match)
table(Bennie_diel$approx_match)
table(Bennie_diel$exact_match)

match_df <-  rbind(data.frame(table(Baker_df$approx_match)), data.frame(table(Baker_df$exact_match)),
                   data.frame(table(Bennie_diel$approx_match)), data.frame(table(Bennie_diel$exact_match)),
                   data.frame(table(Maor_diel$approx_match)), data.frame(table(Maor_diel$exact_match)))

match_df$dataset <- c(rep("Baker", 4), rep("Bennie", 4), rep("Maor",4))
match_df$match_type <- c(rep(c("Approx", "Approx", "Exact", "Exact"), 3))
match_df$total <- match_df %>% group_by(dataset, match_type) %>% summarize(sum = rep(sum(Freq),2)) %>% pull(sum)
match_df$percent <- match_df$Freq/match_df$total
match_df$dataset_matchtype <- paste(match_df$dataset, match_df$match_type, sep = "_")

matches_plot <- 
  ggplot(match_df, aes(x = dataset_matchtype, y = Freq, fill = Var1)) + geom_col() +
  facet_grid(~dataset, scales = "free_x", space = "free_x", switch = "x") +
  scale_fill_manual(name = "", values = c("blue", "skyblue")) +
  labs(x = "", y = "Number of species") + theme_classic() + 
    scale_x_discrete(labels = c("Baker_Approx" = "Approximate \n match", "Baker_Exact" = "Exact \n match", "Bennie_Approx" = "Approximate \n match", "Bennie_Exact" = "Exact \n match","Maor_Approx" = "Approximate \n match", "Maor_Exact" = "Exact \n match")) +
  theme(strip.placement = "outside", strip.background = element_blank(),panel.spacing.x = unit(0, "pt"),
        legend.position = "none", legend.position.inside = c(0.9,0.9), )

matches_plot2 <- 
  ggplot(match_df, aes(x = dataset_matchtype, y = Freq, fill = Var1)) + geom_col() +
  facet_wrap(~match_type, ncol = 1, scales = "free") +
  scale_fill_manual(name = "", values = c("blue", "skyblue")) +
  labs(x = "", y = "Number of species") + theme_classic() + 
  scale_x_discrete(labels = c("Baker_Approx" = "Approximate \n match", "Baker_Exact" = "Exact \n match", "Bennie_Approx" = "Approximate \n match", "Bennie_Exact" = "Exact \n match","Maor_Approx" = "Approximate \n match", "Maor_Exact" = "Exact \n match")) +
  theme(strip.placement = "outside", strip.background = element_blank(),panel.spacing.x = unit(0, "pt"),
        legend.position = "none", legend.position.inside = c(0.9,0.9), axis.text.x = element_blank())


# Section: Proportion plots -----------------------------------------------

artio_df <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv")) #235 species with data
artio_df <- artio_df %>% select(tips, Diel_Pattern)

#Maor dataset
Maor_diel <- read.csv(here("Maor_mam_data.csv")) 
Maor_diel <- Maor_diel %>% filter(Order %in% c("Artiodactyla", "Cetacea")) %>% select("tips", "Maor_activity_pattern") #172 species
  
#Bennie dataset
Bennie_diel <- read.csv(here("Bennie_mam_data.csv")) 
Bennie_diel <- Bennie_diel %>% filter(Order == "Artiodactyla") %>% select("tips", "Bennie_activity_pattern") #235 sps  

#Baker dataset
Baker_df <- read.csv(here("Baker_mam_data.csv"))
Baker_df <- filter(Baker_df, Order %in% c("Artiodactyla", "Cetacea")) %>% select("tips", "Activity_pattern") #209

mammals_df <- merge(Maor_diel, Bennie_diel, by = "tips", all = TRUE) #256 species
mammals_df <- merge(mammals_df, Baker_df, by = "tips", all = TRUE) #260 species
#merge my artiodactyla data with the mammal data
mammals_df <- merge(mammals_df, artio_df, by = "tips", all.x = TRUE) #265 species

colnames(mammals_df) <- c("tips", "Maor_diel", "Bennie_diel", "Baker_diel", "six_state")

#classify partially cathemeral or crepuscular species as cathemeral or crepuscular
unique(mammals_df$Maor_diel)
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/diurnal/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/diurnal/crepuscular/ultradian", replacement = "cathemeral/crepuscular")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "diurnal/nocturnal/crepuscular", replacement = "cathemeral/crepuscular")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/diurnal/crepuscular", replacement = "cathemeral/crepuscular")

mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "diurnal/nocturnal", replacement = "cathemeral")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/diurnal", replacement = "cathemeral")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "cathemeral/nocturnal", replacement = "cathemeral")

#call cathemeral/crepuscular species crepuscular
mammals_df <- data.frame(lapply(mammals_df, function(x) {gsub("cathemeral/crepuscular", "crepuscular", x)}))

#test <-
proportion_plot <-
  mammals_df %>% 
  pivot_longer(!tips, names_to = "dataset", values_to = "activity_pattern") %>%
  filter(!is.na(activity_pattern)) %>%
  ggplot(., aes(x = factor(dataset, levels = c("Baker_diel", "Bennie_diel", "Maor_diel",  "max_crep")), fill = activity_pattern)) + 
  geom_bar(position = "fill", alpha = 0.75) +
  scale_fill_manual(name = "Activity pattern", values= c("#dd8ae7","#EECBAD", "#FC8D62","gold", "#66C2A5", "palegreen")) +
  labs(y = "Proportion of total species", x = "Clade") + 
  #scale_x_discrete(labels = c("Bennie_diel" = "Bennie et al \n (n = 237)", "max_crep" = "Current \n dataset \n (n = 232)", "Maor_diel" = "Maor et al \n (n =173)", "Baker_diel" = "Baker et al \n (n =210)")) +
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text.x = element_blank(), axis.text.y = element_text(size = 9), panel.grid = element_blank())
proportion_plot


# Section: save out combined plots----------------------------------------------

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_plots0.pdf", width = 8.5, height = 3)
(proportion_plot + matches_plot2)
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_plots1.pdf", width = 8.8, height = 4)
Baker_sankey + Bennie_sankey + Maor_sankey2
dev.off()

(proportion_plot + matches_plot2)/
  (Baker_sankey + Bennie_sankey + Maor_sankey2)
# Section 11: Number of sources in each category-------------------------------------------------------
test_diel_long <- read.csv(here("cetacean_confidence_long_final.csv"))
sources_df <- as.data.frame(table(test_diel_long$column)) %>% mutate(Clade = "Cetacea")

diel_full_long <- read.csv(here("artio_confidence_long_final.csv"))
sources_df <- rbind(sources_df, (as.data.frame(table(diel_full_long$column))) %>% mutate(Clade = "Terrestrial Artiodactyla"))
sources_df

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/source_count_barchart.pdf", height = 3, width = 4)
ggplot(sources_df, aes(x = Var1, y = Freq, fill = Clade)) + geom_col(position = position_dodge()) +
  labs(y = "Count", x = "Data category") + theme_bw() + scale_fill_manual(values = c("#070E8A","#2E9DFF")) +
  scale_x_discrete(labels = c("A", "B", "C", "D", "E")) + 
  theme(panel.grid = element_blank(), legend.position = "bottom")
dev.off()
 
# Section 12: Cetacean and ruminant df with sources -------------------------------------

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
write.csv(artio_full, "C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/artiodactyla_with_sources.csv", row.names = FALSE)
write.csv(artio_sources, "C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/artiodactyla_references.csv", row.names = FALSE)

# Section 13: Save out combined plots -------------------------------------------------
pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_category_confusion_plots.pdf", width = 8.5, height = 3)
plot_countfreq_cet + plot_countfreq_rum
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_plots.pdf", width = 8.5, height = 7, bg = "transparent")
(sankey_cet + coord_flip()) + plot_spacer() + (sankey_rum + coord_flip()) + plot_layout(width = c(5, 0.2, 5))
#grid.arrange(sankey_cet + coord_flip(), plot_spacer(), sankey_rum + coord_flip(), nrow = 1)
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/cet_sankey_plots.pdf", width = 4.25, height = 5, bg = "transparent")
(sankey_cet + coord_flip())
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/rum_sankey_plots.pdf", width = 4.25, height = 5, bg = "transparent")
(sankey_rum + coord_flip())
dev.off()


pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_plots.pdf", width = 8.5, height = 5, bg = "transparent")
(sankey_cet + coord_flip()) + plot_spacer() + (sankey_rum + coord_flip()) + plot_layout(width = c(5, 0.2, 5))
#grid.arrange(sankey_cet + coord_flip(), plot_spacer(), sankey_rum + coord_flip(), nrow = 1)
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_crep_plots.pdf", width = 8.5, height = 3.75, bg = "transparent")
(sankey_crep_cet + coord_flip()) + plot_spacer() + (sankey_crep_cet + coord_flip()) + plot_layout(width = c(5, 0.2, 5))
dev.off()


# pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/combined_sankey_confusion_plots.pdf", width = 8.5, height = 11, bg = "transparent")
# (plot_countfreq_cet + plot_countfreq_rum)/
#   ((sankey_cet + coord_flip()) + (sankey_rum + coord_flip())) + plot_layout(height = c(3, 8))
# dev.off()

