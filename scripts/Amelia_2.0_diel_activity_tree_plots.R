setwd(here())

source("scripts/fish_sleep_functions.R")
source("scripts/Amelia_functions.R")

# Section 1: Preliminary plots -----------------------
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))

#uncomment whichever clade you want to plot
#clade_name <- "cetaceans_full"
clade_name <- "sleepy_artiodactyla_full"
#clade_name <- "ruminants_full"
#clade_name <- "whippomorpha"

diel_full <- read.csv(here(paste0(clade_name, ".csv")))
#use below to remove NA species
diel_full <- diel_full[!is.na(diel_full$Diel_Pattern), ]

trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#66C2A5","grey")
# custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "gold", "#66C2A5", "palegreen","grey")
# diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "Diel_Pattern")]
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x+1.5, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 3) + 
  scale_fill_manual(values = custom.colours, name = "Temporal activity pattern") +
  geom_tiplab(size = 3, offset = 3.2) 
diel.plot

#reference for tree nodes
#ggtree(trpy_n, layout = "circular") + geom_tiplab() + geom_label(aes(label = node))

# Section 3: Whippomorpha with family labels ----------------------------------------
diel_full <- read.csv(here("whippomorpha.csv"))
diel_full <- diel_full[!is.na(diel_full$max_crep), ]
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

#label major families
families <- trait.data %>% count(Family) %>% filter(n>1) #filter for clades with more than one species or it can't find the MRCA

#Use a for loop to find the nodes for all the families
nodes_list <- list()
for(i in families$Family){
  node_df <- findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 5, taxonomic_level_name = i)
  nodes_list[[i]] <- node_df
  nodes_df <- do.call(rbind, nodes_list)
}

nodes_left <- nodes_df[c("Delphinidae", "Phocoenidae", "Monodontidae", "Ziphiidae"),]
nodes_right <- nodes_df[c("Balaenopteridae", "Balaenidae", "Hippopotamidae"),]

custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")

#change label colours
text_colour <- "white"
  
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep", "Family")]
diel.plot <- diel.plot + 
  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 3) + 
  scale_fill_manual(values = custom.colours, name = "\n Temporal activity pattern", labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  geom_cladelab(node = nodes_right$node_number, label = nodes_right$clade_name, offset=1.5, offset.text=2, barsize=2, fontsize=3, barcolour = "grey50", textcolour = text_colour) +
  geom_cladelab(node = nodes_left$node_number, label = nodes_left$clade_name, offset=1.5, offset.text=2, hjust = 1, barsize=2, fontsize=3, barcolour = "grey50", textcolour = text_colour) +
  geom_cladelab(node = 71, label = "Eschrichtiidae", offset = 3, fontsize = 3, textcolour = text_colour) + 
  geom_cladelab(node = 45, label = "Iniiae", hjust = 1, offset = 3, fontsize = 3, textcolour = text_colour) +
  geom_cladelab(node = 46, label = "Lipotidae", offset = 3, hjust = 1, fontsize = 3, textcolour = text_colour) + 
  geom_cladelab(node = 44, label = "Pontoporiidae", offset = 3, hjust = 1, fontsize = 3, textcolour = text_colour) +
  geom_cladelab(node = 59, label = "Platanistidae", offset =3, hjust = 0.9, fontsize = 3, textcolour = text_colour) +
  geom_cladelab(node = 141, label = "Kogiidae", offset=1.5, offset.text=2, hjust = 0.4, barsize=2, fontsize=3, barcolour = "grey50", textcolour = text_colour) +
  geom_cladelab(node = 60, label = "Physeteridae", offset = 3, fontsize = 3, textcolour = text_colour) +
  geom_cladelab(node = 72, label = "Neobalaenidae", offset = 3,fontsize = 3, textcolour = text_colour) +
  new_scale_fill() + 
  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),] %>% filter(Family %in% c("Eschrichtiidae", "Iniiae", "Lipotidae", "Pontoporiidae", "Platanistidae", "Physeteridae", "Neobalaenidae")), aes(x=x+2.5, y=y), inherit.aes = FALSE, fill = "grey50", width = 1.3, height = 0.6) +
  theme(legend.position = "inside", legend.position.inside = c(0.5,0.4), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) 

diel.plot 

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/whippo_with_families.pdf", width = 7, height = 6.3, bg = "transparent")
diel.plot
dev.off()

# Section 4: Ruminantia with family labels ----------------------------------------
diel_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
diel_full <- diel_full[!is.na(diel_full$max_crep), ] %>% filter(Suborder == "Ruminantia")
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

#label major families
families <- trait.data %>% count(Family) %>% filter(n>1) #filter for clades with more than one species or it can't find the MRCA

#Use a for loop to find the nodes for all the families
nodes_list <- list()
for(i in families$Family){
  node_df <- findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 5, taxonomic_level_name = i)
  nodes_list[[i]] <- node_df
  nodes_df <- do.call(rbind, nodes_list)
}

nodes_left <- nodes_df %>% filter(clade_name == "Bovidae")
nodes_right <- nodes_df %>% filter(clade_name != "Bovidae")

custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep", "Family")]
diel.plot <- diel.plot +
  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 2.5) + 
  scale_fill_manual(values = custom.colours, name = "Temporal activity pattern", labels = c("Cathemeral", "Crepuscular", "Diurnal", "Nocturnal")) +
  geom_cladelab(node = nodes_right$node_number, label = nodes_right$clade_name, offset=1.5, offset.text=2, barsize=2, fontsize=3, barcolour = "grey50", textcolour = "black")+
  geom_cladelab(node = nodes_left$node_number, label = nodes_left$clade_name, offset=1.5, offset.text=2, hjust = 1, barsize=2, fontsize=3, barcolour = "grey50", textcolour = "black") +
  geom_cladelab(node = 190, label = "Antilocapridae", offset=1.5, offset.text=2, barsize=2, fontsize=3, barcolour = "grey50", textcolour = "black") + 
  theme(legend.position = "inside", legend.position.inside = c(0.5,0.4), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) +
  new_scale_fill() + 
  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),] %>% filter(Family == "Antilocapridae"), aes(x=x+2.3, y=y), inherit.aes = FALSE, fill = "grey50", width = 1.0, height = 0.6) 
diel.plot 

# pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/ruminantia_with_families.pdf", width = 8, height = 7, bg = "transparent")
# diel.plot
# dev.off()

# Section 5: Artiodactyla with suborder labels ----------------------------
diel_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
diel_full <- diel_full[!is.na(diel_full$max_crep), ]
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

#label major families
suborders <- trait.data %>% count(Suborder) %>% filter(n>1) #filter for clades with more than one species or it can't find the MRCA

#For loop to find the labels for each suborder
nodes_list <- list()
for(i in suborders$Suborder){
  node_df <- findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 3, taxonomic_level_name = i)
  nodes_list[[i]] <- node_df
  suborder_nodes_df <- do.call(rbind, nodes_list)
}

nodes_left <- suborder_nodes_df[c("Ruminantia"),]
nodes_right <- suborder_nodes_df[c("Whippomorpha", "Tylopoda", "Suina"),]
custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "gold", "#66C2A5", "palegreen","grey")

diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "Diel_Pattern")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = Diel_Pattern), inherit.aes = FALSE, colour = "transparent", width = 3) +
  scale_fill_manual(values = custom.colours, name = "\n Temporal activity pattern", labels = c("Cathemeral", "Cathemeral/crepusuclar", "Diurnal", "Diurnal/crepuscular", "Nocturnal", "Nocturnal/crepuscular")) +
  geom_cladelab(node = nodes_right$node_number, label = nodes_right$clade_name, offset=1.5, offset.text=2, barsize=2, fontsize=4, barcolour = "grey50", textcolour = "black") +
  geom_cladelab(node = nodes_left$node_number, label = nodes_left$clade_name, offset=1.5, offset.text=2, hjust = 1, barsize=2, fontsize=4, barcolour = "grey50", textcolour = "black") +
  theme(legend.position = "inside", legend.position.inside = c(0.5, 0.37), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
diel.plot

# pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/artio_with_suborders.pdf", width = 8.5, height = 8.5, bg = "transparent")
# diel.plot
# dev.off()

#label major families
families <- trait.data %>% count(Family) %>% filter(n>1) #filter for clades with more than one species or it can't find the MRCA

#Use a for loop to find the nodes for all the families
nodes_list <- list()
for(i in families$Family){
  node_df <- findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 5, taxonomic_level_name = i)
  nodes_list[[i]] <- node_df
  nodes_df <- do.call(rbind, nodes_list)
}

nodes_left <- nodes_df[c("Tragulidae", "Giraffidae", "Moschidae", "Cervidae", "Delphinidae"),]
nodes_right <- nodes_df[c("Bovidae", "Camelidae", "Suidae", "Tayassuidae", "Hippopotamidae", "Balaenidae", "Balaenopteridae", "Kogiidae", "Ziphiidae"),]
nodes_extra <- nodes_df[c("Phocoenidae", "Monodontidae"),]
                          
#with family labels, coloured branches by suborder
custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#FFEC10", "#66C2A5", "palegreen")
trpy_n2 <- groupClade(trpy_n, c(suborder_nodes_df$node_number))

diel.plot <- ggtree(trpy_n2, layout = "circular", aes(colour = group), size = 1.2) %<+% trait.data[,c("tips", "Diel_Pattern")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = Diel_Pattern), inherit.aes = FALSE, colour = "transparent", width = 3) +
  scale_fill_manual(values = custom.colours, name = "\n Temporal activity pattern", labels = c("Cathemeral", "Cathemeral and crepusuclar", "Diurnal", "Diurnal and crepuscular", "Nocturnal", "Nocturnal and crepuscular")) +
  scale_colour_manual(values = c("black", "royalblue", "mediumslateblue", "steelblue2", "midnightblue"), name = "Suborder", labels = c("X", "Ruminantia", "Suina", "Tylopoda", "Whippomorpha")) +
  geom_cladelab(node = nodes_extra$node_number, label = nodes_extra$clade_name, offset=1.5, offset.text=2, hjust = 1, barsize=2, fontsize=4, barcolour = "grey50", textcolour = adjustcolor("black", alpha.f = 0.0)) +
  geom_cladelab(node = nodes_right$node_number, label = nodes_right$clade_name, offset=1.5, offset.text=2, barsize=2, fontsize=4, barcolour = "grey50", textcolour = "black") +
  geom_cladelab(node = nodes_left$node_number, label = nodes_left$clade_name, offset=1.5, offset.text=2, hjust = 1, barsize=2, fontsize=4, barcolour = "grey50", textcolour = "black") +
  theme(legend.position = "right", legend.position.inside = c(0.5, 0.37), panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
diel.plot

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/artio_with_families.pdf", width = 8.5, height = 8.5, bg = "transparent")
diel.plot
dev.off()

# Section 10: artio tree with whippo collapsed -----------------------------

diel_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
diel_full <- diel_full[!is.na(diel_full$max_crep), ]
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

#label major families
suborders <- trait.data %>% filter(Suborder != "Whippomorpha") 

#For loop to find the labels for each suborder
nodes_list <- list()
for(i in unique(suborders$Suborder)){
  node_df <- findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 3, taxonomic_level_name = i)
  nodes_list[[i]] <- node_df
  nodes_df <- do.call(rbind, nodes_list)
}

nodes_left <- nodes_df[c("Ruminantia"),]
nodes_right <- nodes_df[c("Tylopoda", "Suina"),]

custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep")]
diel.plot <- scaleClade(diel.plot, 511, .05) %>% collapse(node = 511, 'mixed', fill = "grey40") #whippo is at node 511
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 3) + 
  scale_fill_manual(values = custom.colours, name = "Temporal activity pattern") +
  geom_cladelab(node = nodes_right$node_number, label = nodes_right$clade_name, offset=2, offset.text=2, barsize=2, fontsize=3, barcolour = "grey50", textcolour = "black") +
  geom_cladelab(node = nodes_left$node_number, label = nodes_left$clade_name, offset=2, offset.text=2, hjust = 1, barsize=2, fontsize=3, barcolour = "grey50", textcolour = "white") +
  #geom_cladelabel(node = 511, label = "Whippomorpha", offset = 62, hjust = 0.26, size = 0.5, barsize = 2, fontsize = 3) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
  
diel.plot

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/artio_collapsed.pdf", width = 4.8, height = 4.8, bg = "transparent")
diel.plot
dev.off()

# Section 6: Proportion plots----------------------------

#my primary source data 
artio_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
artio_full <- artio_full[!is.na(artio_full$Diel_Pattern), ]  %>%
  filter(tips %in% mam.tree$tip.label)

mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")

#plot the full order proportions and the suborders in the same barplot
suborders_plot <-
  artio_full %>% mutate(Suborder = "Artiodactyla") %>% rbind(., artio_full) %>%
  ggplot(., aes(x = Suborder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + 
  scale_fill_manual(values = custom.colours) +
  scale_x_discrete(labels = c("Artiodactyla" = "Artiodactyla \n (n = 305)", "Ruminantia" = "Ruminantia \n (n = 203)", "Suina" = "Suina \n (n = 20)", "Tylopoda" = "Tylopoda \n (n = 4)", "Whippomorpha" = "Whippomorpha \n (n = 78)")) +
  labs(y = "Proportion of species", x = "Clade") +
  theme_classic() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text = element_text(size = 9))

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/barplot_percentages.pdf", width = 4.25, height = 2, bg = "transparent")
suborders_plot
dev.off()

pdf("C:/Users/ameli/Documents/R_projects/Amelia_figures/parvorder_barplot_percentages.pdf", width = 2, height = 2, bg = "transparent")
artio_full %>% filter(Parvorder %in% c("Odontoceti", "Mysticeti")) %>%
  ggplot(., aes(x = Parvorder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) +
  scale_fill_manual(values = custom.colours) +
  scale_x_discrete(labels = c("Mysticeti" = "Mysticeti \n (n = 14)", "Odontoceti" = "Odontoceti \n (n = 62)")) +
  labs(y = "Proportion of species", x = "Clade") +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text = element_text(size = 9))
dev.off()
