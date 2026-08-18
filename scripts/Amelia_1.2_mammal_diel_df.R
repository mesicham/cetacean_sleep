# Section 1: Bennie dataframe --------

#read in the Bennie diel activity patterns
#from https://doi.org/10.1073/pnas.1216063110 

# fill in taxonomy
Bennie_mam_data <- read_excel(here("Bennie_diel_activity_data.xlsx"))
colnames(Bennie_mam_data) <- "SpeciesBehaviourReference"
Bennie_mam_data$SpeciesBehaviourReference <- str_replace(string = Bennie_mam_data$SpeciesBehaviourReference, pattern = " ", replacement  = "_")
Bennie_mam_data <- separate(Bennie_mam_data, col = SpeciesBehaviourReference, into = c("tips", "max_crep", "Reference"), sep = " ")
Bennie_mam_data$max_crep <- tolower(Bennie_mam_data$max_crep)
Bennie_mam_data$Species_name <- str_replace(string = Bennie_mam_data$tips, pattern = "_", replacement  = " ")
Bennie_mam_data <- Bennie_mam_data[1:4477, c("tips", "max_crep", "Species_name", "Reference") ]

resolved_names <- tnrs_match_names(names = Bennie_mam_data$Species_name, context_name = "Vertebrates", do_approximate_matching = TRUE)
missing_names <- resolved_names[is.na(resolved_names$ott_id), ] #40 names not found
resolved_names <- resolved_names[!is.na(resolved_names$ott_id), ] #returns 4437 names 

get_rank <- function(tax_info, rank_name) {
  lineage <- tax_lineage(tax_info)[[1]]
  values <- lineage$name[lineage$rank == rank_name]
  if (length(values) == 0) return(NA_character_) else return(values[1])
}

#split it up because it takes so long
df1 <- resolved_names[1:1000,] %>% rowwise() %>% mutate(
  tax_info = list(taxonomy_taxon_info(ott_id, include_lineage = TRUE)),
  order = get_rank(tax_info, "order"),
  family = get_rank(tax_info, "family"),
  genus = get_rank(tax_info, "genus")) %>% ungroup() %>% select(-tax_info)

df2 <- resolved_names[1001:2000,] %>% rowwise() %>% mutate(
  tax_info = list(taxonomy_taxon_info(ott_id, include_lineage = TRUE)),
  order = get_rank(tax_info, "order"),
  family = get_rank(tax_info, "family"),
  genus = get_rank(tax_info, "genus")) %>% ungroup() %>% select(-tax_info)

df3 <- resolved_names[2001:3000,] %>% rowwise() %>% mutate(
  tax_info = list(taxonomy_taxon_info(ott_id, include_lineage = TRUE)),
  order = get_rank(tax_info, "order"),
  family = get_rank(tax_info, "family"),
  genus = get_rank(tax_info, "genus")) %>% ungroup() %>% select(-tax_info)

df4 <- resolved_names[3001:4437,] %>% rowwise() %>% mutate(
  tax_info = list(taxonomy_taxon_info(ott_id, include_lineage = TRUE)),
  order = get_rank(tax_info, "order"),
  family = get_rank(tax_info, "family"),
  genus = get_rank(tax_info, "genus")) %>% ungroup() %>% select(-tax_info)

df <- rbind(df1, df2, df3, df4)

df <- df[, c("search_string", "order", "family", "genus")]
df$search_string <- str_to_sentence(df$search_string)
colnames(df) <- c("Species_name","Order", "Family", "Genus")

#fill in missing info
df[df$Family %in% c("Aotidae", "Atelidae", "Cebidae", "Cercopithecidae", "Cheirogaleidae", "Cynocephalidae", "Daubentoniidae", "Galagidae", "Hominidae", "Hylobatidae", "Indriidae", "Lemuridae", "Lepilemuridae", "Lorisidae", "Pitheciidae", "Tarsiidae"), c("Order")] <- "Primates"
df[df$Genus %in% c("Microgale", "Tenrec", "Hemicentetes", "Oryzorictes", "Echinops", "Geogale", "Limnogale", "Setifer"), "Family"] <- "Tenrecidae"
df[df$Genus %in% c("Micropotamogale", "Potamogale"), "Family"] <- "Potamogalidae"
df[df$Family %in% c("Tenrecidae", "Potamogalidae"), "Order"] <- "Afrosoricida"

trait.data <- merge(Bennie_mam_data, df, by = "Species_name", all = TRUE)

#40 species weren't found in the otl and so won't have taxonomic info
trait.data[is.na(trait.data$Genus), "Genus"] <- sub(" .*", "", trait.data[is.na(trait.data$Genus), "Species_name"])

#use existing taxonomic info to fill in those species by matching by genus
#this finds info for all but three
for(i in 1:nrow(trait.data)){
  if(is.na(trait.data[i, "Order"])){
    for(j in 1:nrow(trait.data)){
      if(trait.data[i, "Genus"] == trait.data[j, "Genus"] & !is.na(trait.data[j, "Order"])){
        trait.data[i, "Order"] <- trait.data[j, "Order"]
        trait.data[i, "Family"] <- trait.data[j, "Family"]
        break
      }
    }
  }
}

#species with no match, fill in manually (this is more than we need since setting approximate_match = TRUE found a lot of these already but keeping in case)
trait.data[trait.data$Genus %in% c("Smutsia", "Uromanis", "Phataginus"), "Family"] <- "Manidae"
trait.data[trait.data$Family %in% c("Manidae"), "Order"] <- "Pholidota"
trait.data[trait.data$Genus %in% c("Sphiggurus", "Echinoprocta"), "Family"] <- "Erethizontidae"
trait.data[trait.data$Genus %in% c("Loxodontomys", "Phaiomys"), "Family"] <- "Cricetidae"
trait.data[trait.data$Genus %in% c("Megadendromus"), "Family"] <- "Nesomyidae"
trait.data[trait.data$Family %in% c("Erethizontidae", "Cricetidae", "Nesomyidae"), "Order"] <- "Rodentia"
trait.data[trait.data$Genus %in% c("Pseudalopex", "Alopex"), "Family"] <- "Canidae"
trait.data[trait.data$Family %in% c("Canidae"), "Order"] <- "Carnivora"
trait.data[trait.data$Genus %in% c("Enchisthenes", "Lampronycteris", "Trinycteris"), "Family"] <- "Phyllostomidae"
trait.data[trait.data$Genus %in% c("Lissonycteris"), "Family"] <- "Pteropodidae"
trait.data[trait.data$Genus %in% c("Paracoelops"), "Family"] <- "Hipposideridae"
trait.data[trait.data$Genus %in% c("Vespadelus", "Bauerus"), "Family"] <- "Vespertilionidae"
trait.data[trait.data$Family %in% c("Vespertilionidae", "Phyllostomidae", "Pteropodidae", "Hipposideridae"), "Order"] <- "Chiroptera"
trait.data[trait.data$Genus %in% c("Cebuella"), "Family"] <- "Callitrichidae"
trait.data[trait.data$Genus %in% c("Oreonax"), "Family"] <- "Atelidae"
trait.data[trait.data$Family %in% c("Atelidae"), "Order"] <- "Primates"
trait.data[trait.data$Genus %in% c("Choeropsis"), "Family"] <- "Hippopotamidae"
trait.data[trait.data$Genus %in% c("Nesotragus", "Nilgiritragus"), "Family"] <- "Bovidae"
trait.data[trait.data$Family %in% c("Hippopotamidae" ,"Bovidae"), "Order"] <- "Artiodactyla"
trait.data[trait.data$Genus %in% c("Dactylonax"), "Family"] <- "Petauridae"
trait.data[trait.data$Family %in% c("Petauridae"), "Order"] <- "Diprotodontia"

#this species gets mislabeled as a gobi so fix it now
trait.data[trait.data$tips == "Tadarida_sarasinorum", "Family"] <- "Molossidae"
trait.data[trait.data$tips == "Tadarida_sarasinorum", "Order"] <- "Chiroptera"

colnames(trait.data) <- c("Species_name", "tips", "Bennie_activity_pattern", "Reference", "Order", "Family", "Genus")

#replace alternative taxonomic names to match my dataset, just for artiodactyla
trait.data[trait.data$tips == "Alces_americanus", "tips"] <- "Alces_alces"
trait.data[trait.data$tips == "Capricornis_milneedwardsii", "tips"] <- "Capricornis_sumatraensis"
trait.data[trait.data$tips == "Przewalskium_albirostris", "tips"] <- "Cervus_albirostris"
trait.data[trait.data$tips == "Pseudois_schaeferi", "tips"] <- "Pseudois_nayaur"

#subspecies, don't include unless parent species is not in their dataset
# trait.data[trait.data$tips == "Sus_bucculentus", "tips"] <- "Sus scrofa"
# trait.data[trait.data$tips == "Muntiacus_montanus", "tips"] <- "Muntiacus_muntjak"
# trait.data[trait.data$tips == "Pecari_maximus", "tips"] <- "Pecari_tajacu"

#save out Bennie mammal data, 4477 species
write.csv(trait.data, here("Bennie_mam_data.csv"), row.names = FALSE)

# Section 2: Maor dataframe -----------------------------------------------
#read in the Maor diel activity patterns
#from https://doi.org/10.1038/s41559-017-0366-5 
maor_mam_data <- read_excel(here("Maor_diel_activity_data.xlsx"))
maor_mam_data <- maor_mam_data[17:3403, 1:4]
colnames(maor_mam_data) <- c("Order", "Family", "Species", "Maor_activity_pattern")
maor_mam_data$Maor_activity_pattern <- tolower(maor_mam_data$Maor_activity_pattern)

unique(maor_mam_data$Maor_activity_pattern) 

#remove extinct or inconsistent naming
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal / arrhythmic", "cathemeral/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal or cathemeral", "cathemeral/diurnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal - extinct", "nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal /crepuscular", "crepuscular/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal/crepuscular", "crepuscular/diurnal")

#change to alphabetical order, helps with resolving duplicates later
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal/crepuscular", "crepuscular/diurnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal/crepuscular", "crepuscular/nocturnal")

maor_mam_data$tips <- str_replace(maor_mam_data$Species, pattern = " ", replacement = "_")

# collapse duplicate entries into one line
duplicates1 <- maor_mam_data[duplicated(maor_mam_data$Species),]
#make another dataframe since some sps are repeated twice
duplicates2 <- duplicates1[duplicated(duplicates1$Species),] 
duplicates1 <- duplicates1[!duplicated(duplicates1$Species),]
maor_mam_data <- maor_mam_data[!duplicated(maor_mam_data$tips),]
maor_mam_data <- merge(maor_mam_data, duplicates1[, c("Species", "Maor_activity_pattern")], by='Species', all.x = TRUE, all.y = TRUE)
maor_mam_data <- merge(maor_mam_data, duplicates2[, c("Species", "Maor_activity_pattern")], by='Species', all.x = TRUE, all.y = TRUE)


maor_mam_data$Maor_activity_pattern <- apply(cbind(maor_mam_data$Maor_activity_pattern, 
                                                   maor_mam_data$Maor_activity_pattern.x, 
                                                   maor_mam_data$Maor_activity_pattern.y), 
                                             1, function(x) paste(sort(x), collapse="/"))

unique(maor_mam_data$Maor_activity_pattern)

#remove duplicated identical entries
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal/nocturnal", "nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal/diurnal", "diurnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "cathemeral/cathemeral", "cathemeral")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/crepuscular", "crepuscular")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal/nocturnal", "nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal/diurnal", "diurnal")

maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/diurnal/crepuscular/nocturnal", "crepuscular/diurnal/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/nocturnal/crepuscular/diurnal", "crepuscular/diurnal/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/nocturnal/crepuscular/nocturnal", "crepuscular/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/nocturnal/diurnal/nocturnal", "crepuscular/diurnal/nocturnal")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "cathemeral/nocturnal/cathemeral/nocturnal", "cathemeral/nocturnal")

unique(maor_mam_data$Maor_activity_pattern)

#keep only necessary columns
maor_mam_data <- maor_mam_data %>% select(Order, Family, tips, Maor_activity_pattern)

#change order to match my dataset
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/nocturnal", "nocturnal/crepuscular")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "crepuscular/diurnal", "diurnal/crepuscular")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "diurnal/crepuscular/nocturnal", "diurnal/nocturnal/crepuscular")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "nocturnal/diurnal/crepuscular", "diurnal/nocturnal/crepuscular")
maor_mam_data$Maor_activity_pattern <- str_replace_all(maor_mam_data$Maor_activity_pattern, "cathemeral/nocturnal/diurnal/crepuscular", "cathemeral/diurnal/nocturnal/crepuscular")

#replace alternative taxonomic names to match my dataset, just for artiodactyla
maor_mam_data[maor_mam_data$tips == "Capricornis_milneedwardsii", "tips"] <- "Capricornis_sumatraensis"
maor_mam_data[maor_mam_data$tips == "Hemitragus_hylocrius", "tips"] <- "Nilgiritragus_hylocrius"
maor_mam_data[maor_mam_data$tips == "Hemitragus_jayakari", "tips"] <- "Arabitragus_jayakari"
maor_mam_data[maor_mam_data$tips == "Hexaprotodon_liberiensis", "tips"] <- "Choeropsis_liberiensis"
maor_mam_data[maor_mam_data$tips == "Neotragus_moschatus", "tips"] <- "Nesotragus_moschatus"
maor_mam_data[maor_mam_data$tips == "Pseudois_schaeferi", "tips"] <- "Pseudois_nayaur"
maor_mam_data[maor_mam_data$tips == "Sus_salvanius", "tips"] <- "Porcula_salvania"
maor_mam_data[maor_mam_data$tips == "Taurotragus_oryx", "tips"] <- "Tragelaphus_oryx"

#subspecies, don't include unless parent species is not in their dataset
#maor_mam_data[maor_mam_data$tips == "Alcelaphus_lichtensteinii", "tips"] <- "Alcelaphus_buselaphus"

#save out Maor dataframe, 2416 species
write.csv(maor_mam_data, here("Maor_mam_data.csv"), row.names  = FALSE)


# Section 3: Baker dataframe ----------------------------------------------

#Baker et al dataset, a combination of primary data (200sps), the Bennie et al dataset and pantheria
Baker_df <- read_xlsx(here("Baker_diel_activity_data.xlsx"))
Baker_df <- Baker_df[2: nrow(Baker_df),]
colnames(Baker_df) <- c("tips", "Order", "Corneal_diameter", "Axial_length", "Activity_pattern", "Source")

#fix alternative spellings for just the artiodactyls
Baker_df[Baker_df$tips == "Hemitragus_hylocrius", "tips"] <- "Nilgiritragus_hylocrius"
Baker_df[Baker_df$tips == "Hemitragus_jayakari", "tips"] <- "Arabitragus_jayakari"
Baker_df[Baker_df$tips == "Hexaprotodon_liberiensis", "tips"] <- "Choeropsis_liberiensis"
Baker_df[Baker_df$tips == "Neotragus_moschatus", "tips"] <- "Nesotragus_moschatus"
Baker_df[Baker_df$tips == "Przewalskium_albirostris", "tips"] <- "Cervus_albirostris"
Baker_df[Baker_df$tips == "Pseudois_schaeferi", "tips"] <- "Pseudois_nayaur"
Baker_df[Baker_df$tips == "Rucervus_eldi", "tips"] <- "Rucervus_eldii"
Baker_df[Baker_df$tips == "Saiga_borealis", "tips"] <- "Saiga_tatarica"
Baker_df[Baker_df$tips == "Sus_salvanius", "tips"] <- "Porcula_salvania"
Baker_df[Baker_df$tips == "Taurotragus_derbianus", "tips"] <- "Tragelaphus_derbianus"
Baker_df[Baker_df$tips == "Alces_americanus", "tips"] <- "Alces_alces"
Baker_df[Baker_df$tips == "Mazama_gouazoupira", "tips"] <- "Mazama_gouazoubira"

#don't include subspecies unless parent species is not in the dataset
#Baker_df[Baker_df$tips == "Sus_bucculentus", "tips"] <- "Sus_scrofa"

Baker_df <- Baker_df %>% select(tips, Order, Activity_pattern)

#save out Baker et al dataset, 3,014 species
write.csv(Baker_df, here("Baker_mam_data.csv"), row.names = FALSE)

# Section 4: Maor et al data comparison ---------------------------------------------

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

#function for comparing entries, splits and compares each segment
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
unique(Maor_diel$Maor_activity_pattern)
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
Maor_diel <- data.frame(lapply(Maor_diel, function(x) {gsub("/C", " and\nc", x)}))

Maor_sankey2 <-  Maor_diel %>% make_long(Maor_activity_pattern, Diel_Pattern) %>%
  ggplot(., aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.3, color = 1, fill = "white") +
  scale_fill_manual(values = c("#dd8ae7","#EECBAD" , "#FC8D62", "gold", "#66C2A5",  "palegreen")) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Diel_pattern" = "Maor et al \n (n = 193)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Maor_sankey2 

#concordance by activity pattern
concordance <- as.data.frame(table(Maor_diel$Maor_activity_pattern, Maor_diel$Diel_Pattern))
colnames(concordance) <- c("actual", "predicted", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
colnames(totals_df) <- c("actual", "total")
concordance <- merge(concordance, totals_df, by = "actual")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)

confusion_plot_maor <-
  ggplot(concordance, aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label = paste0(percent, "%")), size = 3) +
  scale_fill_gradient(low = "#F5FBFF", high = "#0070D1") + 
  labs(x = "Maor et al", y = "Mesich et al") + 
  theme_void() +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45 ,
                                                                                                 hjust = 1, vjust = 1), axis.title = element_text(size = 11), axis.title.y = element_blank())
confusion_plot_maor

# Section 5: Bennie et al data comparison ----------------------------------
#my data
artio_df <- read.csv(here("sleepy_artiodactyla_full.csv")) #235 species with data
artio_df <- artio_df %>% select(Species_name, Diel_Pattern, max_crep, tips)

#Bennie dataset
Bennie_diel <- read.csv(here("Bennie_mam_data.csv")) #447 species
Bennie_diel <- Bennie_diel[Bennie_diel$tips %in% artio_df$tips, ] #224 sps when filtering for those in my dataframe 

#merge my artiodactyla data with the mammal data
Bennie_diel <- merge(Bennie_diel, artio_df, by = "tips", all.x = TRUE) 
Bennie_diel$exact_match <- Bennie_diel$Bennie_activity_pattern == Bennie_diel$Diel_Pattern

#function for comparing entries, splits and compares each segment
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
Bennie_diel <- data.frame(lapply(Bennie_diel, function(x) {gsub("/C", " and\nc", x)}))

Bennie_sankey <- Bennie_diel %>% make_long(Bennie_activity_pattern, Diel_Pattern) %>%
  ggplot(., aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.3, color = 1, fill = "white") + 
  scale_fill_manual(values = c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5", "gold", "palegreen")) +
  theme_sankey(base_size = 12) +
  scale_x_discrete(labels = c("Bennie_activity_pattern" = "Bennie et al \n (n = 224)", "Diel_Pattern" = "Current \ndataset"), expand = expansion(0,0.3)) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 13), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Bennie_sankey

#concordance by activity pattern
concordance <- as.data.frame(table(Bennie_diel$Bennie_activity_pattern, Bennie_diel$Diel_Pattern))
colnames(concordance) <- c("actual", "predicted", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
colnames(totals_df) <- c("actual", "total")
concordance <- merge(concordance, totals_df, by = "actual")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)

confusion_plot_bennie <-
  ggplot(concordance, aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label = paste0(percent, "%")), size = 3) +
  scale_fill_gradient(low = "#F5FBFF", high = "#0070D1") + 
  labs(x = "Bennie et al", y = "Mesich et al") + 
  theme_void() +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), axis.title = element_text(size = 11), axis.title.y = element_blank())
confusion_plot_bennie

# Section 6: Baker et al comparison -----------------------------------------
#my data
artio_df <- read.csv(here("sleepy_artiodactyla_full.csv")) #235 species with data

#Baker et al dataset, a combination of primary data (200sps), the Bennie et al dataset and pantheria
Baker_df <- read.csv(here("Baker_mam_data.csv"))

Baker_df <- Baker_df[Baker_df$tips %in% artio_df$tips, ] #removes 5 sps

Baker_df <- merge(Baker_df, artio_df, by = "tips", all.x = TRUE) #204 sps

Baker_df$exact_match <- Baker_df$Activity_pattern == Baker_df$Diel_Pattern

#function for comparing entries, splits and compares each segment
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

#concordance by activity pattern
concordance <- as.data.frame(table(Baker_df$Activity_pattern, Baker_df$Diel_Pattern))
colnames(concordance) <- c("actual", "predicted", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
colnames(totals_df) <- c("actual", "total")
concordance <- merge(concordance, totals_df, by = "actual")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)

confusion_plot_baker <-
  ggplot(concordance, aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label = paste0(percent, "%")), size = 3) +
  scale_fill_gradient(low = "#F5FBFF", high = "#0070D1") + 
  labs(x = "Baker et al", y = "Mesich et al") + 
  theme_void() +
  theme(legend.position = "none", axis.text = element_text(size = 9), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), axis.title = element_text(size = 11), axis.title.y = element_text(angle = 90))
confusion_plot_baker 

# Section 7: Number of matches plot -----------------------------------------
#plot number of matches
match_df <-  rbind(data.frame(table(Baker_df$approx_match)), data.frame(table(Baker_df$exact_match)),
                   data.frame(table(Bennie_diel$approx_match)), data.frame(table(Bennie_diel$exact_match)),
                   data.frame(table(Maor_diel$approx_match)), data.frame(table(Maor_diel$exact_match)))

match_df$dataset <- c(rep("Baker", 4), rep("Bennie", 4), rep("Maor",4))
match_df$match_type <- c(rep(c("Approximate match", "Approximate match", "Exact match", "Exact match"), 3))
match_df$total <- match_df %>% group_by(dataset, match_type) %>% summarize(sum = rep(sum(Freq),2)) %>% pull(sum)
match_df$percent <- match_df$Freq/match_df$total
match_df$dataset_matchtype <- paste(match_df$dataset, match_df$match_type, sep = "_")

matches_plot <- 
  ggplot(match_df, aes(x = dataset_matchtype, y = Freq, fill = Var1)) + geom_col() +
  facet_wrap(~match_type, ncol = 1, scales = "free") +
  scale_fill_manual(name = "Match with current \ndataset", values = c("#070E8A","#2E9DFF"), labels = c("False", "True")) +
  labs(x = "", y = "Number of species") + theme_classic() + 
  scale_x_discrete(labels = c("Baker_Approx" = "Approximate \n match", "Baker_Exact" = "Exact \n match", "Bennie_Approx" = "Approximate \n match", "Bennie_Exact" = "Exact \n match","Maor_Approx" = "Approximate \n match", "Maor_Exact" = "Exact \n match")) +
  theme(strip.placement = "outside", strip.background = element_blank(),panel.spacing.x = unit(0, "pt"),
        legend.position = "bottom", legend.position.inside = c(0.9,0.9), axis.text.x = element_blank())


# Section 8: Proportion plots -----------------------------------------------

artio_df <- read.csv(here("Sleepy_artiodactyla_full.csv")) #235 species with data

#Maor dataset
Maor_diel <- read.csv(here("Maor_mam_data.csv")) 
Maor_diel %>% filter(Order %in% c("Cetacea")) 
cetacean_list <- Maor_diel %>% filter(Order %in% c("Cetacea")) %>% pull(tips)
Maor_diel <- Maor_diel %>% filter(Order %in% c("Artiodactyla", "Cetacea")) %>% select("tips", "Maor_activity_pattern") #172 species

Maor_diel[!Maor_diel$tips %in% artio_df$tips, ]

#Bennie dataset
Bennie_diel <- read.csv(here("Bennie_mam_data.csv")) 
Bennie_diel <- Bennie_diel %>% filter(Order == "Artiodactyla") %>% select("tips", "Bennie_activity_pattern") #235 sps  

Bennie_diel[!Bennie_diel$tips %in% artio_df$tips, ]

#Baker dataset
Baker_df <- read.csv(here("Baker_mam_data.csv"))
Baker_df %>% filter(Order %in% c("Cetacea")) #same species as in Maor
Baker_df <- filter(Baker_df, Order %in% c("Artiodactyla", "Cetacea")) %>% select("tips", "Activity_pattern") #209

Baker_df[!Baker_df$tips %in% artio_df$tips, ]

#filter my dataset
artio_df <- artio_df %>% filter(Parvorder == "non-cetacean" | tips %in% cetacean_list) %>% select(tips, Diel_Pattern) 

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

proportion_plot <-
  mammals_df %>% 
  pivot_longer(!tips, names_to = "dataset", values_to = "activity_pattern") %>%
  filter(!is.na(activity_pattern)) %>%
  ggplot(., aes(x = factor(dataset, levels = c("Baker_diel", "Bennie_diel", "Maor_diel",  "max_crep")), fill = activity_pattern)) + 
  geom_bar(position = "fill", alpha = 0.75) +
  scale_fill_manual(name = "Temporal activity pattern", values= c("#dd8ae7","#EECBAD", "#FC8D62","gold", "#66C2A5", "palegreen"),
                    labels = c("Cathemeral", "Crepuscular", "Diurnal", "Diurnal and crepusuclar", "Nocturnal", "Nocturnal and crepuscular")) +
  labs(y = "Proportion of total species", x = "Clade") + 
  #scale_x_discrete(labels = c("Bennie_diel" = "Bennie et al \n (n = 237)", "max_crep" = "Current \n dataset \n (n = 232)", "Maor_diel" = "Maor et al \n (n =173)", "Baker_diel" = "Baker et al \n (n =210)")) +
  theme_classic() + theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text.x = element_blank(), axis.text.y = element_text(size = 9), panel.grid = element_blank())
proportion_plot


# Section 9: save out combined plots----------------------------------------------

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/combined_sankey_plots0.pdf", width = 8.6, height = 3.5)
(proportion_plot + theme(legend.position = "right")) + (matches_plot + theme(legend.position = "right")) +
  guide_area() +
  plot_layout(guides = 'collect', widths = c(0.9, 0.7, 0.4))
dev.off()

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/combined_sankey_plots1.pdf", width = 8.8, height = 4)
Baker_sankey + Bennie_sankey + Maor_sankey2
dev.off()

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/combined_sankey_plots2.pdf", width = 8.5, height = 3)
confusion_plot_baker + confusion_plot_bennie + confusion_plot_maor + plot_layout(widths = c(0.7, 1, 1.3))
dev.off()
