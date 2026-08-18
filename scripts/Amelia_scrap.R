# Save out all discrete traits plots ---------------------------------------------------------

#### Cetacean diet
trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

#filter for species with activity pattern data
trait.data <- trait.data[!is.na(trait.data$max_crep),]
trait.data <- trait.data[!is.na(trait.data$Diet),]

# #filter for species in the final tree
# trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
# trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)
# 
# custom.colours <- c("#dd8ae7",  "peachpuff2", "#FC8D62", "#66C2A5")
# #custom.colours.2 <- c( "grey90", "grey40", "grey66", "black", "red")
# diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep", "Diet")]
# diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent") + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
# diel.plot <- diel.plot +  new_scale_fill() +  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x +2, y=y, fill = Diet), inherit.aes = FALSE, colour = "transparent") + scale_fill_grey(name = Diet)
# diel.plot <- diel.plot
# diel.plot

test_df <- trait.data[, c("max_crep", "Diet")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 3))) 


####Cetacean habitat
trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

#filter for species with activity pattern data
trait.data <- trait.data[!is.na(trait.data$max_crep),]
trait.data <- trait.data[!is.na(trait.data$Habitat),]

# #filter for species in the final tree
# trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
# trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)
# 
# custom.colours <- c("#dd8ae7",  "peachpuff2", "#FC8D62", "#66C2A5")
# #custom.colours.2 <- c( "grey90", "grey40", "grey66", "black", "red")
# diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep", "Habitat")]
# diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent") + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
# diel.plot <- diel.plot +  new_scale_fill() +  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x +2, y=y, fill = Habitat), inherit.aes = FALSE, colour = "transparent") + scale_fill_grey(name = "Habitat")
# diel.plot <- diel.plot
# diel.plot

test_df <- trait.data[, c("max_crep", "Habitat")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 3))) 

####Cetacean feeding mechanism 

trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

# "Feeding_method" takes data from 4 databases
# "Prey_capture" #less species than feeding method (60 vs 76) but come from one source (Churchill)

#filter for species with activity pattern data
trait.data <- trait.data[!is.na(trait.data$max_crep),]
trait.data <- trait.data[!is.na(trait.data$Feeding_method),]

# #filter for species in the final tree
# trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
# trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)
# 
# custom.colours <- c("#dd8ae7",  "peachpuff2", "#FC8D62", "#66C2A5")
# #custom.colours.2 <- c( "grey90", "grey40", "grey66", "black", "red")
# diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep", "Feeding_method")]
# diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent") + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
# diel.plot <- diel.plot +  new_scale_fill() +  geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x +2, y=y, fill = Feeding_method), inherit.aes = FALSE, colour = "transparent") + scale_fill_grey(name = "Feeding method")
# diel.plot <- diel.plot
# diel.plot

test_df <- trait.data[, c("max_crep", "Feeding_method")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 3))) 


#### Terresterial vs aquatic 

#test to see if aquatic lifestyle is associated with cathemerality
artio_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
artio_full <- artio_full[!is.na(artio_full$max_crep),]
artio_full$enviro <- "aquatic"
for(i in 1:nrow(artio_full)){
  if(artio_full[i, "Parvorder"] == "non-cetacean"){
    artio_full[i, "enviro"] <- "terrestrial"
  }
}

test_df <- artio_full[, c("max_crep", "enviro")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 9)))


#### Habitat and diet terrestrial artiodactyla

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep),]

trait.data.1 <- trait.data.art[!is.na(trait.data.art$TrophicLevel),]
test_df <- trait.data.1[, c("max_crep", "TrophicLevel")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 9)))

trait.data.1 <- trait.data.art[!is.na(trait.data.art$DietBreadth),]
test_df <- trait.data.1[, c("max_crep", "DietBreadth")]
test <- fisher.test(table(test_df))
mosaicplot(table(test_df), color = TRUE, main = paste0("Fischer's exact test: ", "p-value = ", round(test$p.value, 9)))

### Possible multivariate associations

ggplot(trait.data.art, aes(x = max_crep, y = log(AdultBodyMass_g))) + geom_boxplot() + facet_wrap(~TrophicLevel)


# Multivariate analysis ---------------------------------------------------

trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

trait.data <- trait.data %>% filter(!is.na(max_crep) & !is.na(Habitat) & !is.na(Dive_depth_m))

ggplot(trait.data, aes(x = Habitat, y = Dive_depth_m)) + geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = max_crep), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + scale_fill_manual(values=custom.colours) +
  stat_compare_means(label = "p.format", method = "t.test",ref.group = ".all.") + stat_compare_means(label.y = 2000, method = "anova") + facet_wrap(~ max_crep)

ggplot(trait.data, aes(x = max_crep, y = Dive_depth_m)) + geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = Habitat), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + scale_fill_manual(values=custom.colours) +
  stat_compare_means(label = "p.format", method = "t.test",ref.group = ".all.") + stat_compare_means(label.y = 2000, method = "anova") + facet_wrap(~ Habitat)

#two way ANOVA of habitat and diel pattern on dive depth

aggregate(Dive_depth_m ~ Habitat + max_crep, data = trait.data, FUN = mean)

ggplot(trait.data, aes(x = max_crep, y = Dive_depth_m, fill = Habitat)) + geom_boxplot(outlier.shape = NA) + scale_fill_manual(values=custom.colours)

model <- aov(Dive_depth_m ~ Habitat + max_crep, data = trait.data)
summary(model)



#From section 6.1
# Artiodactyla latitude ---------------------------------------------------

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$GR_MidRangeLat_dd), c("tips", "GR_MidRangeLat_dd", "max_crep", "Family", "fam_colours")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ]

#perform the one-way anova
art_model <- aov(GR_MidRangeLat_dd ~ max_crep, data = trait.data.art)
summary(art_model)

#perform the post-hoc tukey test
TukeyHSD(art_model, conf.level = .95)

#perform the phylogenetically corrected one-way anova
art_phylANOVA <- calculatePhylANOVA(trait.data.art, "GR_MidRangeLat_dd")

boxplot_art <- ggplot(trait.data.art, aes(x = max_crep, y = log(GR_MidRangeLat_dd))) +
  geom_boxplot(aes(fill = max_crep), alpha=0.8, outlier.shape = NA) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Mid latitude range") + scale_fill_manual(values=unique(trait.data.art$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.y = 5, method = "anova") + annotate("text", x = 1.15, y = 5.5, label = paste("phylANOVA, p =", art_phylANOVA$Pf))
boxplot_art   

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "mean_latitude", "_boxplots_anova_artio.pdf"), width = 8, height = 7)
boxplot_art
dev.off()


# Artiodactyla max latitude -----------------------------------------------

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$GR_MaxLat_dd), c("tips", "GR_MaxLat_dd", "max_crep", "Family", "fam_colours")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ]

#perform the one-way anova
art_model <- aov(GR_MaxLat_dd ~ max_crep, data = trait.data.art)
summary(art_model)

#perform the post-hoc tukey test
TukeyHSD(art_model, conf.level = .95)

#perform the phylogenetically corrected one-way anova
art_phylANOVA <- calculatePhylANOVA(trait.data.art, "GR_MaxLat_dd")

boxplot_art <- ggplot(trait.data.art, aes(x = max_crep, y = log(GR_MaxLat_dd))) +
  geom_boxplot(aes(fill = max_crep), alpha=0.8, outlier.shape = NA) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "GR_MaxLat_dd") + scale_fill_manual(values=unique(trait.data.art$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.y = 5, method = "anova") + annotate("text", x = 1.15, y = 5.5, label = paste("phylANOVA, p =", art_phylANOVA$Pf))
boxplot_art   

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "max_latitude", "_boxplots_anova_artio.pdf"), width = 8, height = 7)
boxplot_art
dev.off()


# Artiodactyla orbit size -------------------------------------------------

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$Orbit_ratio), c("tips", "Orbit_ratio", "max_crep", "Family", "fam_colours")]

#perform the one-way anova
art_model <- aov(Orbit_ratio ~ max_crep, data = trait.data.art)
summary(art_model)

#perform the post-hoc tukey test
TukeyHSD(art_model, conf.level = .95)

#perform the phylogenetically corrected one-way anova
art_phylANOVA <- calculatePhylANOVA(trait.data.art, "Orbit_ratio")

#cannot calculate the pairwise corrected p-values likley because the cathemeral sample size is too small
#remove cathemeral if artio only and rerun
#trait.data.art <- filter(trait.data.art, max_crep != "cathemeral")
#art_phylANOVA <- calculatePhylANOVA(trait.data.art, "Orbit_ratio")

#add p values manually
library(rstatix)
#for artio
stat.test <- data.frame(group1 = c("crepuscular", "crepuscular", "diurnal"),
                        group2 = c("diurnal", "nocturnal", "nocturnal"),
                        p.adj = c(art_phylANOVA$Pt[2], art_phylANOVA$Pt[3], art_phylANOVA$Pt[6]),
                        y.position = c(0.95, 1.0, 0.994))

stat.test <- stat.test %>% add_x_position(x = "max_crep")

boxplot_art <- ggplot(trait.data.art, aes(x = max_crep, y = Orbit_ratio)) +
  geom_boxplot(aes(fill = max_crep), alpha=0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Corneal diameter: axial length") + scale_fill_manual(values=unique(trait.data.art$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.y = 1.01, method = "anova") + annotate("text", x = 1.15, y = 1.007, label = paste("phylANOVA, p =", art_phylANOVA$Pf)) +
  stat_pvalue_manual(stat.test, label = "p.adj")
boxplot_art

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Orbit_ratio", "_boxplots_anova_artio.pdf"), width = 8, height = 7)
boxplot_art
dev.off()


# Artiodactyla body mass --------------------------------------------------

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$AdultBodyMass_g), c("tips", "AdultBodyMass_g", "max_crep", "Family", "fam_colours")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ]

#perform the one-way anova
art_model <- aov(AdultBodyMass_g ~ max_crep, data = trait.data.art)
summary(art_model)

#perform the post-hoc tukey test
TukeyHSD(art_model, conf.level = .95)

#perform the phylogenetically corrected one-way anova
art_phylANOVA <- calculatePhylANOVA(trait.data.art, "AdultBodyMass_g")

boxplot_art <- ggplot(trait.data.art, aes(x = max_crep, y = log(AdultBodyMass_g))) +
  geom_boxplot(aes(fill = max_crep), alpha=0.8, outlier.shape = NA) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Body mass (g)") + scale_fill_manual(values=unique(trait.data.art$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.y = 14, method = "anova") + annotate("text", x = 1.15, y = 14.5, label = paste("phylANOVA, p =", art_phylANOVA$Pf))
boxplot_art   

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Body_mass", "_boxplots_anova_artio.pdf"), width = 8, height = 7)
boxplot_art
dev.off()




# Cetacean dive data in detail ---------------------------------------------------

dive.data <- read.csv(here("cetacean_dive_depth_all_sources.csv"))
trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

dive.data <- merge(dive.data, trait.data[, -c(14)], by = "tips", all = TRUE)

dive.data %>% ggplot(., aes(x = Diel_Pattern, y = Final_dive_depth)) + geom_boxplot() + geom_point() + facet_wrap(~Parvorder)

#how deep is the deep scattering layer? 
#Wikipedia says they rise to 100m at night and descend to 800-1000m during day
#coastal species also wouldn't be involved in this since they aren't diving that deep

dive.data %>% filter(Habitat %in% c("coastal/pelagic", "pelagic")) %>% filter(!is.na(max_crep)) %>%
  ggplot(., aes(x = max_crep, y = Mean_dive_depth)) + geom_boxplot() +
  geom_point() + facet_wrap(~Parvorder) + stat_compare_means(method = "anova")

dive.data %>% filter(Habitat %in% c("coastal/pelagic", "pelagic")) %>% filter(!is.na(max_crep)) %>%
  ggplot(., aes(x = max_crep, y = Final_dive_depth)) + geom_boxplot() +
  geom_point() + facet_wrap(~Parvorder) + stat_compare_means(method = "anova")


#check for phylogenetic significance

#maximum dive depth
trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))
trait.data <- trait.data[!is.na(trait.data$max_crep),]

trait.data <- filter(trait.data, Habitat %in% c("coastal/pelagic", "pelagic"))

trait.data.od <- filter(trait.data, Parvorder == "Odontoceti")
trait.data.od <- filter(trait.data, Family == "Delphinidae")

trait.data.od <- trait.data.od[!is.na(trait.data.od$Dive_depth_m),]

#perform the phylogenetically corrected one-way anova
odonto_phylANOVA <- calculatePhylANOVA(trait.data.od, "Dive_depth_m")

boxplot_dive <- ggplot(trait.data.od, aes(x = max_crep, y = Dive_depth_m)) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Dive_depth_m") + scale_fill_manual(values=unique(trait.data$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.x = 0.8, label.y = 3400, method = "anova") + 
  annotate("text", x = 1.15, y = 3300, label = paste("phylANOVA, p =", odonto_phylANOVA$Pf))
boxplot_dive

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Maximum_dive_depth", "_boxplots_anova_pelagic_odontocetes.pdf"), width = 8, height = 7)
boxplot_dive
dev.off()

#with mean dive depth 
trait.data.od <- filter(trait.data, Parvorder == "Odontoceti")
trait.data.od <- trait.data.od[!is.na(trait.data.od$Mean_dive_depth_m),]

#perform the phylogenetically corrected one-way anova
odonto_phylANOVA <- calculatePhylANOVA(trait.data.od, "Mean_dive_depth_m")

boxplot_dive <- ggplot(trait.data.od, aes(x = max_crep, y = Mean_dive_depth_m)) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Mean_dive_depth_m") + scale_fill_manual(values=unique(trait.data$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.x = 0.8, label.y = 3400, method = "anova") + 
  annotate("text", x = 1.15, y = 3300, label = paste("phylANOVA, p =", odonto_phylANOVA$Pf))
boxplot_dive

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Mean_dive_depth", "_boxplots_anova_pelagic_odontocetes.pdf"), width = 8, height = 7)
boxplot_dive
dev.off()

#both parvorders on same plot
trait.data.od <- filter(trait.data, Parvorder == "Odontoceti")
trait.data.od <- trait.data.od[!is.na(trait.data.od$Dive_depth_m),]

trait.data.my <- filter(trait.data, Parvorder == "Mysticeti")
trait.data.my <- trait.data.my[!is.na(trait.data.my$Dive_depth_m),]

#perform the phylogenetically corrected one-way anova
odonto_phylANOVA <- calculatePhylANOVA(trait.data.od, "Dive_depth_m")
mystic_phylANOVA <- calculatePhylANOVA(trait.data.my, "Dive_depth_m")

dat_text <- data.frame(label = c(paste("phylANOVA =", mystic_phylANOVA$Pf, sep = " "), paste("phylANOVA =", odonto_phylANOVA$Pf, sep = " ")), Parvorder = c("Mysticeti", "Odontoceti"))

boxplot_dive <- ggplot(trait.data, aes(x = max_crep, y = Dive_depth_m)) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Maximum dive depth (m)") + scale_fill_manual(values=unique(trait.data.1$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.x = 0.8, label.y = 3400, method = "anova") + 
  geom_text(data= dat_text,mapping = aes(x = 1, y = 3300, label = label)) +
  facet_wrap(~Parvorder, scales = "free_x")
boxplot_dive

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Maximum_dive_depth", "_boxplots_anova_pelagic_odontocetes_mysticeties.pdf"), width = 8, height = 7)
boxplot_dive
dev.off()

#both parvorders on same plot for mean
trait.data.od <- filter(trait.data, Parvorder == "Odontoceti")
trait.data.od <- trait.data.od[!is.na(trait.data.od$Mean_dive_depth_m),]

trait.data.my <- filter(trait.data, Parvorder == "Mysticeti")
trait.data.my <- trait.data.my[!is.na(trait.data.my$Mean_dive_depth_m),]

#perform the phylogenetically corrected one-way anova
odonto_phylANOVA <- calculatePhylANOVA(trait.data.od, "Mean_dive_depth_m")
mystic_phylANOVA <- calculatePhylANOVA(trait.data.my, "Mean_dive_depth_m")

dat_text <- data.frame(label = c(paste("phylANOVA =", mystic_phylANOVA$Pf, sep = " "), paste("phylANOVA =", odonto_phylANOVA$Pf, sep = " ")), Parvorder = c("Mysticeti", "Odontoceti"))

boxplot_dive <- ggplot(trait.data, aes(x = max_crep, y = Mean_dive_depth_m)) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Mean dive depth (m)") + scale_fill_manual(values=unique(trait.data.1$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.x = 0.8, label.y = 3400, method = "anova") + 
  geom_text(data= dat_text,mapping = aes(x = 1, y = 3300, label = label)) +
  facet_wrap(~Parvorder, scales = "free_x")
boxplot_dive

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Mean_dive_depth", "_boxplots_anova_pelagic_odontocetes_mysticeties.pdf"), width = 8, height = 7)
boxplot_dive
dev.off()

# Cetacean dive depth parvorder -------------------------------------------

trait.data.od <- filter(trait.data, Parvorder == "Odontoceti")
trait.data.od <- trait.data.od[!is.na(trait.data.od$Dive_depth_m),]

trait.data.my <- filter(trait.data, Parvorder == "Mysticeti")
trait.data.my <- trait.data.my[!is.na(trait.data.my$Dive_depth_m),]

#perform the phylogenetically corrected one-way anova
odonto_phylANOVA <- calculatePhylANOVA(trait.data.od, "Dive_depth_m")
mystic_phylANOVA <- calculatePhylANOVA(trait.data.my, "Dive_depth_m")

dat_text <- data.frame(label = c(paste("phylANOVA =", mystic_phylANOVA$Pf, sep = " "), paste("phylANOVA =", odonto_phylANOVA$Pf, sep = " ")), Parvorder = c("Mysticeti", "Odontoceti"))

boxplot_dive <- ggplot(trait.data.1, aes(x = max_crep, y = Dive_depth_m)) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8) + scale_fill_manual(values = custom.colours) +
  new_scale_fill() + geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = "black", pch = 21) + 
  labs(x = "Temporal activity pattern", y = "Maximum dive depth (m)") + scale_fill_manual(values=unique(trait.data.1$fam_colours))  + 
  theme_minimal() + theme(panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'), panel.border = element_rect(colour = "black", fill = "transparent")) + 
  stat_compare_means(label.x = 0.8, label.y = 3400, method = "anova") + 
  geom_text(data= dat_text,mapping = aes(x = 1, y = 3300, label = label)) +
  facet_wrap(~Parvorder, scales = "free_x")
boxplot_dive
#From section 5.0
# Section 7: Comparison of rate magnitude -------------------------------

#get rates
rates_df1 <- plot1kTransitionRates4state(readRDS(here("august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds")), 5)
rates_df1 <- filter(rates_df1, model == "Bridge_only")
rates_df2 <- plot1kTransitionRates4state(readRDS(here("august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds")), 5)
rates_df2 <- filter(rates_df2, model == "Bridge_only")

#need to compare rates from the same trees 
#each model has 12 rates, filter for one model (ARD), label each tree 
rates_df1$tree_n <- rep(1:1000, each = 10)
#subtract the cetacean rate from the ruminant rate, is it faster (negative number) or slower (positive number)
rates_df2$tree_n <- rep(1:1000, each = 10)

rates_df <- cbind(rates_df1, rates_df2)
colnames(rates_df) <- c("whippo_rates", "model", "solution", "colours", "tree_n", "rumi_rates", "model", "solution", "colours", "tree_n")
rates_df <- rates_df[, c("whippo_rates", "solution", "tree_n", "rumi_rates")]
rates_df$difference <- rates_df$whippo_rates - rates_df$rumi_rates
#difference is negative or small -whippo is much faster, difference is positive or large ruminants have similar rates

ggplot(rates_df, aes(x = solution, y = difference)) + geom_jitter()
ggplot(rates_df, aes(x = whippo_rates, y = rumi_rates, colour = solution)) + geom_point() + facet_wrap(~solution)
ggplot(rates_df, aes(x = whippo_rates, fill = solution)) + geom_histogram() + facet_wrap(~solution)

rates_df %>% group_by(solution) %>% summarize(mean_rates = mean(whippo_rates)) %>% ggplot(., aes(x = mean_rates, y = solution)) + geom_point()
rates_df %>% group_by(solution) %>% summarize(mean_rates = mean(rumi_rates)) %>% ggplot(., aes(x = mean_rates, y = solution)) + geom_point()

rates_df %>% group_by(solution) %>% 
  summarize(mean_whippo_rates = mean(whippo_rates), mean_rumi_rates = mean(rumi_rates)) %>% 
  ggplot(., aes(y = solution)) + geom_point(aes(x=mean_whippo_rates), colour = "blue") +
  geom_point(aes(x = mean_rumi_rates), colour = "red")

#make a forest-ish plot
ggplot(rates_df, aes(y = solution)) + geom_point(aes(x=whippo_rates), colour = "blue") +
  geom_point(aes(x = rumi_rates), colour = "red")


rates_df1 <- plot1kTransitionRates4state(readRDS(here("august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds")), 5)
rates_df1 <- filter(rates_df1, model == "Bridge_only")
rates_df2 <- plot1kTransitionRates4state(readRDS(here("august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds")), 5)
rates_df2 <- filter(rates_df2, model == "Bridge_only")

rates_df1$clade <- "whippomorpha"
rates_df2$clade <- "ruminants"

rates_df <- rbind(rates_df1, rates_df2)

df <- rates_df %>% group_by(clade, solution) %>% summarize(mean_rates = mean(rates), 
                                                           lci = t.test(rates, conf.level = 0.95)$conf.int[1],
                                                           uci = t.test(rates, conf.level = 0.95)$conf.int[2])

ggplot(df, aes(x = mean_rates, y = solution, colour = clade)) + geom_point() + geom_errorbar(aes(y = solution, xmin = lci, xmax =uci),width = 0.4)


# Section 8: Total garbage test ------------------------------------------

filename <- "whippomorpha_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"
model_results <- readRDS(here(paste0(filename, ".rds")))

trait.data <- model_results$ER_model$data
table(trait.data$max_crep)

#since there are four trait states
#likelihood <- ((factorial(n))/(factorial(n1) * factorial(n2) * factorial(n3) *factorial(n4))) * (p1^n1)  * (p2^n2)  * (p3^n3)  * (p4^n4) 

#using the natural log
n1 = 27
n2 = 22
n3 = 7
n4 = 21
n = 77

lnL_garb = n1 * log(n1 / n) + n2 * log(n2 / n) + n3 * log(n3 / n) + n4 * log(n4 / n)
#ln likelihood is -99.926

#compared to the actual likelihood
model_results$bridge_only_model$loglik #-91.0821
model_results$ER_model$loglik #-105.561
model_results$SYM_model$loglik #-97.924
model_results$ARD_model$loglik #-91.025
model_results$CONSYM_model$loglik # -97.46

likelihood_metrics <- max_clade_metrics(readRDS(here(paste0(filename, ".rds"))))
likelihood_metrics <- pivot_wider(likelihood_metrics, names_from = model_metric, values_from = model_value)
likelihood_metrics <- rbind(likelihood_metrics, data.frame(model = "Total garbage", log_likelihoods = lnL_garb, AICc_scores = NA,  AIC_scores = NA))
ggplot(likelihood_metrics, aes(y = log_likelihoods, x = model, fill = model))  + geom_bar(stat = "identity")

#so the log likelihood is similar, but the actual model is more likely (higher log lik)

#for ruminants
filename <- "ruminants_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"
model_results <- readRDS(here(paste0(filename, ".rds")))

trait.data <- model_results$ER_model$data
table(trait.data$max_crep)

n1 = 21
n2 = 125
n3 = 34
n4 = 23
n = 203

lnL_garb = n1 * log(n1 / n) + n2 * log(n2 / n) + n3 * log(n3 / n) + n4 * log(n4 / n)

#the garbage ln likelihood is -219 which is different than our most likely model (-199.78)

model_results$bridge_only_model$loglik #-199.7884
model_results$ER_model$loglik # -230.505
model_results$SYM_model$loglik #-219.0621
model_results$ARD_model$loglik #-199.8458
model_results$CONSYM_model$loglik # -219.0807

likelihood_metrics <- max_clade_metrics(readRDS(here(paste0(filename, ".rds"))))
likelihood_metrics <- pivot_wider(likelihood_metrics, names_from = model_metric, values_from = model_value)
likelihood_metrics <- rbind(likelihood_metrics, data.frame(model = "Total garbage", log_likelihoods = lnL_garb, AICc_scores = NA,  AIC_scores = NA))
ggplot(likelihood_metrics, aes(y = log_likelihoods, x = model, fill = model))  + geom_bar(stat = "identity") + ggtitle(filename)

likelihood_metrics$length <- 100
ggplot(likelihood_metrics, aes(y = log_likelihoods, x = length, fill = model)) +
  geom_line() + ggtitle(filename)

# Section 7: McCurry et al latitude ---------------------------------------
#https://doi.org/10.1093/biolinnean/blac128

McCurry <- read_xlsx("C:\\Users\\ameli\\OneDrive\\Documents\\R_projects\\cetacean_discrete_traits\\McCurry_2023.xlsx")

McCurry <- as.data.frame(McCurry[, c(6, 8:51)])

test <- McCurry %>% pivot_longer(cols = !`Absolute latitude`, names_to = "Species", values_to = "count")

test <- test %>% filter(count > 0)
test$Species <- str_replace_all(test$Species, pattern = "'", replacement = "")
test$Species <- str_replace_all(test$Species, pattern = " ", replacement = "_")
colnames(test) <- c("Absolute_latitude", "Species", "count")

test <- test %>% group_by(Species) %>% summarize(max_lat = max(Absolute_latitude), mean_lat = mean(Absolute_latitude), min_lat = min(Absolute_latitude))
names <- read.csv(here("cetaceans_full.csv"))

names <- names %>% separate(col = tips, into = c("Genus", "Species"), sep = "_")
test <- test %>% separate(col = Species, into = c("Genus", "Species"), sep = "_")

test <- merge(names, test, by = "Species", all.y =TRUE)
#two species have the species name attentuata, glacialis, australis and hectori. Drop the duplicates
latitude_df <- test[-c(5,8,9,11,31), ]

latitude_df %>% filter(!is.na(max_crep)) %>% ggplot(., aes(x = max_crep, y = max_lat)) + geom_boxplot() + stat_compare_means(method = "anova")

latitude_df$tips <- str_replace(latitude_df$Species_name, pattern = " ", replacement = "_")
latitude_df <- latitude_df[, c("tips", "max_lat", "mean_lat", "min_lat")]

#check if species names are spelled correctly
latitude_df[!latitude_df$tips %in% mam.tree$tip.label,]

#save out 
write.csv(latitude_df, here("cetacean_latitude_df.csv"), row.names = FALSE)

# Section 6: Groot et al ignore for now --------------------------------------------------

groot <- read_xlsx("C:\\Users\\ameli\\OneDrive\\Documents\\R_projects\\cetacean_discrete_traits\\Groot_et_al_2023.xlsx")
#this data is coded so need to decode it with the original paper
#contains trait data that is in the other dataframes
#lifespan, length, mass, brain mass, EQ, age to reproduction, group size, gestation, sociality, group foraging, learned foraging, communication



# Section: Random diel plots ----------------------------------------------

#all diel patterns
custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "gold", "#66C2A5", "#A6D854","grey")
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "Diel_Pattern")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x+1.5, y=y, fill = Diel_Pattern), inherit.aes = FALSE, colour = "transparent", width = 3) + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
diel.plot <- diel.plot + theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
diel.plot <- diel.plot + geom_tiplab(size = 3, offset = 3.2) 
diel.plot

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", clade_name, "_six_state_plot_labelled.pdf"), width = 9, height = 8, bg = "transparent")
diel.plot
dev.off() 


cetaceans_full <- read.csv(here("whippomorpha.csv"))
cetaceans_full <- cetaceans_full[!is.na(cetaceans_full$max_crep), ]
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- cetaceans_full[cetaceans_full$tips %in% mam.tree$tip.label,]
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

#add clade labels
findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 4, taxonomic_level_name = "Mysticeti")
findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 4, taxonomic_level_name = "Odontoceti")

custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")
diel.plot <- ggtree(trpy_n, layout = "circular", fill = "transparent") %<+% trait.data[,c("tips", "max_crep")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent") + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
diel.plot <- diel.plot + 
  geom_cladelab(node = 137, label = "Mysticeti", align = TRUE, geom = "label", offset=1, align=TRUE, offset.text=1, barsize=2, fontsize=3, fill = "grey", barcolour = "grey", textcolour = "black")
diel.plot <- diel.plot + 
  geom_cladelab(node = 77, label = "Odontoceti", align = FALSE, geom = "label", offset=1, align=FALSE, offset.text=1, hjust = 1, barsize=2, fontsize=3, fill = "grey", barcolour = "grey", textcolour = "black")
diel.plot

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/max_crep_plot_no_Na_unlabelled_cladelabels.pdf", bg = "transparent")
diel.plot
dev.off()

# Section 7: Mammal tree ----------------------------------------------------

#make plot of bennie et al data with the artiodactyla data replaced with my own
new_mammals <- read.csv(here("Bennie_mam_data.csv")) #data from Bennie et al, 2014, 4732 species
new_mammals <- new_mammals %>% filter(Order != "Artiodactyla") #4492 species (removes 240 artios)
artio_full <- read.csv(here("sleepy_artiodactyla_full.csv")) 
artio_full <- artio_full[!is.na(artio_full$Diel_Pattern), ] #317 species (82 cetaceans, 235 non cetaceans)
artio_full <- artio_full %>% select(Species_name, Order, Family, max_crep)
new_mammals <- new_mammals %>% select(Species_name, Order, Family, max_crep)
diel_full <- rbind(new_mammals, artio_full) #4809 species
diel_full$tips <- str_replace(diel_full$Species_name, pattern = " ", replacement = "_")
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- diel_full[diel_full$tips %in% mam.tree$tip.label,] #should be 4,400 species in tree (other 400 misnamed)
trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)

custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#66C2A5","grey")
diel.plot <- ggtree(trpy_n, layout = "circular") %<+% trait.data[,c("tips", "max_crep")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x+3, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 6) + scale_fill_manual(values = custom.colours, name = "Temporal activity pattern")
diel.plot <- diel.plot + theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
diel.plot

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/clade_name/sleepy_mammals_max_crep_plot_unlabelled.pdf", width = 9, height = 8, bg = "transparent")
diel.plot
dev.off() 

order_list <- trait.data %>% group_by(Order) %>% filter(n()>10)
order_list <- unique(order_list$Order)
node_labels <- lapply(order_list, function(x){findMRCANode2(phylo = trpy_n, trait.data = trait.data, taxonomic_level_col = 2, taxonomic_level_name = x)})
node_labels <- do.call(rbind.data.frame, node_labels)
node_labels$barsize <- 2
node_labels$vjust <- 0.5
node_labels_left <- node_labels[node_labels$clade_name %in% c("Lagomorpha", "Scandentia", "Primates", "Artiodactyla", "Carnivora", "Perissodactyla", "Dermoptera", "Pholidota"),]
#node_labels_left[node_labels_left$clade_name %in% c("Dermoptera", "Pholidota"), "vjust"] <- -2
#node_labels_left[node_labels_left$clade_name %in% c("Perissodactyla"), "vjust"] <- -2
node_labels_right <- node_labels[!node_labels$clade_name %in% c("Lagomorpha", "Scandentia", "Primates", "Artiodactyla", "Carnivora", "Perissodactyla", "Dermoptera", "Pholidota"),]
#node_labels_right[node_labels_right$clade_name %in% c("Proboscidea", "Monotremata"), "vjust"] <- -2
node_labels_right[node_labels_right$clade_name %in% c("Cingulata"), "vjust"] <- 0

diel.plot <- ggtree(trpy_n, layout = "circular", fill = "transparent") %<+% trait.data[,c("tips", "max_crep")]
diel.plot <- diel.plot + geom_tile(data = diel.plot$data[1:length(trpy_n$tip.label),], aes(x=x, y=y, fill = max_crep), inherit.aes = FALSE, colour = "transparent", width = 6) + scale_fill_manual(values = custom.colours)
diel.plot <- diel.plot + geom_cladelab(barsize = 1.5, barcolor = "grey50", node = node_labels_left$node_number, label = node_labels_left$clade_name, hjust = 1, offset = 3, vjust = node_labels_left$vjust, offset.text = 2)
diel.plot <- diel.plot + geom_cladelab(barsize = 1.5, barcolor = "grey50", node = node_labels_right$node_number, label = node_labels_right$clade_name, offset = 3, vjust = node_labels_right$vjust, offset.text = 2)
diel.plot <- diel.plot + theme(legend.position = "none", panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent'))
diel.plot

# pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/mammals_max_crep_plot_cladelabels.pdf", bg = "transparent", width = 10, height = 10)
# diel.plot
# dev.off()




# Comparison to Maor and Bennie artio datasets ----------------------------

#compare my ruminant data to Maor and Bennie datasets
artio_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
artio_full <- filter(artio_full, Family %in% c("Bovidae", "Cervidae", "Antilocapridae", "Giraffidae", "Tragulidae", "Moschidae"))
table(artio_full$max_crep)
table(artio_full$Diel_Pattern)

Bennie_mam_data <- read.csv(here("Bennie_mam_data.csv")) #data from Bennie et al, 2014
Bennie_mam_data <- Bennie_mam_data[Bennie_mam_data$Species_name %in% artio_full$Species_name,]
table(Bennie_mam_data$max_crep)

maor_mam_data <- read.csv(here("Maor_artio_full.csv")) #data from Maor et al, 2017
maor_mam_data <- maor_mam_data[maor_mam_data$tips %in% artio_full$tips,]
maor_mam_data$Diel_pattern <- str_replace(maor_mam_data$Diel_pattern, pattern = c("Cathemeral/Crepuscular"), replacement = c("Crepuscular"))
maor_mam_data$Diel_pattern <- str_replace(maor_mam_data$Diel_pattern, pattern = c("Diurnal/Crepuscular"), replacement = c("Crepuscular"))
maor_mam_data$Diel_pattern <- str_replace(maor_mam_data$Diel_pattern, pattern = c("Nocturnal/Crepuscular"), replacement = c("Crepuscular"))
maor_mam_data$Diel_pattern <- str_replace(maor_mam_data$Diel_pattern, pattern = c("Diurnal/Cathemeral"), replacement = c("Cathemeral"))
maor_mam_data$Diel_pattern <- str_replace(maor_mam_data$Diel_pattern, pattern = c("Nocturnal/Cathemeral"), replacement = c("Cathemeral"))
table(maor_mam_data$Diel_pattern)

# Section 9: Phylogenetic signal lambda -wrong  ----------

#requires a vector of the trait data in the same order as phy$tip.label
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
mam.tree <- keep.tip(mam.tree, tip = trait.data$tips)

#all branches need to have a positive length
#replace branches with length 0 with 1% of the 1% quantile (replace it with a number very close to zero)
mam.tree$edge.length[mam.tree$edge.length == 0] <- quantile(mam.tree$edge.length, 0.1)*0.1

#function phylo.signal isn't working so load in the base code
#rep doesn't do anything?
phylo.signal <- function(trait, phy, rep = 999) {
  if (length(attributes(factor(trait))$levels) == length(trait)) 
    stop("Are you sure this variable is categorical?")
  
  phy <- keep.tip(phy, tip = names(trait))
  
  # calculate likelihood corresponding to maximum likelihood value of lambda
  obs <- fitDiscrete(phy, trait, transform="lambda")
  
  # calculate likelihood of model with no phylogenetic signal
  #null <- fitDiscrete(transform(phylo, "lambda", 0), trait)
  # this wasn't working so I tested the likelihood of the lambda against a tree with traits randomly distributed
  trait.random <- sample(trait)
  names(trait.random) <- names(trait)
  null <- fitDiscrete(phy, trait.random, transform = "lambda")
  
  # calculate the likelihood ratio between the two models
  LLR <- -2*(null$opt$lnL - obs$opt$lnL)
  
  # what is the p value of this likelihood ratio?
  p <- pchisq(LLR, df=1, lower.tail=FALSE)
  
  return(data.frame(row.names=NULL, lambda=obs$opt$lambda, obs=obs$opt$lnL, null=null$opt$lnL, LLR=LLR, p=p))
}

#function to create a vector of trait data, with species in same order as in tree (mam.tree$tip.label)
makeTraitVector <- function(trait.data = trait.data, taxonomic_level = "Order", clade_name = "Primates"){
  trait.data <- trait.data[trait.data[,taxonomic_level] == clade_name, ]
  sps_order <- as.data.frame(mam.tree$tip.label)
  colnames(sps_order) <- "tips"
  sps_order$id <- 1:nrow(sps_order)
  trait.data <- merge(trait.data, sps_order, by = "tips")
  trait.data <- trait.data[order(trait.data$id), ]
  trait <- trait.data$max_crep
  names(trait) <- trait.data$tips
  return(trait)
}

#calculate signal for all mammals
trait.data <- read.csv(here("Bennie_mam_data.csv"))
trait.data$Kingdom <- "Mammals"
trait <- makeTraitVector(trait.data = trait.data, taxonomic_level = "Kingdom", clade_name = "Mammals")
mam_signal <- phylo.signal(trait = trait, phy = mam.tree, rep = 999)
mam_signal$clade <- "Mammals"
row.names(mam_signal) <- "Mammals"

#calculate signal for artiodactyla suborders
trait.data <- read.csv(here("sleepy_artiodactyla_full.csv"))
trait.vector.list <- lapply(unique(trait.data$Suborder), function(x) makeTraitVector(trait = trait.data, taxonomic_level = "Suborder", clade_name = x))
names(trait.vector.list) <- unique(trait.data$Suborder)
phylo.sig.list <- lapply(trait.vector.list, function(x) phylo.signal(trait = x, phy = mam.tree, rep = 999))
suborder_df <- do.call(rbind.data.frame, phylo.sig.list)
suborder_df$clade <- row.names(suborder_df)

#write.csv(suborder_df, here("phylogenetic_signal_artio_suborder.csv"), row.names = FALSE)

#calculate phylogenetic signal for all families with more than 100 species
trait.data <- read.csv(here("Bennie_mam_data.csv"))
trait.data <- trait.data[, c("max_crep", "tips", "Order")]

#to use my artio data instead (or in addition to)
#trait.data <- filter(trait.data, Order != "Artiodactyla")
trait.data.1 <- read.csv(here("sleepy_artiodactyla_full.csv"))
trait.data.1 <- trait.data.1[!is.na(trait.data.1$max_crep), c("max_crep", "tips", "Order")]
trait.data.1$Order <- str_replace(trait.data.1$Order, pattern = "Artiodactyla", replacement = "Amelia_artiodactyla")

trait.data <- rbind(trait.data, trait.data.1) #4459 species

trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,] #4303 mammals in final tree
table(trait.data$Order, trait.data$max_crep)

#filter for orders that have over x number of species
#trait.data <- trait.data %>% group_by(Order) %>% filter(n() > 70)

#filter for species with all four diel categories
trait.data <- trait.data %>% group_by(Order) %>% filter(length(unique(max_crep)) ==4)

trait.vector.list <- lapply(unique(trait.data$Order), function(x) makeTraitVector(trait = trait.data, taxonomic_level = , clade_name = x))
names(trait.vector.list) <- unique(trait.data$Order)

phylo.sig.list <- lapply(trait.vector.list, function(x) phylo.signal(trait = x, phy = mam.tree))
phylo.sig.df <- do.call(rbind.data.frame, phylo.sig.list)
phylo.sig.df$clade <- row.names(phylo.sig.df)

#save out 
#write.csv(phylo.sig.df, here("phylogenetic_signal_mammals.csv"), row.names = FALSE)

#final comparison
phylo_signal_final <- rbind(phylo.sig.df, suborder_df, mam_signal)
write.csv(phylo_signal_final, here("phylogenetic_signal.csv"), row.names = FALSE)

#i think chiroptera only has nocturnal species so the random reordering isn't any different than the actual data on the tree
ggplot(phylo_signal_final, aes(x = clade, y = lambda, fill = log(p))) + geom_bar(stat = "identity") + geom_text(aes(label = round(lambda, digits = 3)), vjust = -0.2)

knitr::kable(phylo_signal_final, format = "html", digits = 3, caption = "Table X") %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("phylosig_table_longer.html")
webshot("phylosig_table_longer.html", file = "C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/phylogenetic_signal.pdf")





# Latitude maps -----------------------------------------------------------
lat.df <- read.csv(here("cetacean_latitude_df.csv"))

cetaceans_full <- read.csv(here("cetaceans_full.csv"))
cetaceans_full <- cetaceans_full[, c("Parvorder", "Family", "Diel_Pattern", "max_crep", "Confidence", "tips")]

lat.df <- merge(cetaceans_full, lat.df, by = "tips", all = TRUE)
lat.df[lat.df == ""] <- NA

lat.df <- lat.df[!is.na(lat.df$max_lat),]
lat.df <- lat.df[!is.na(lat.df$max_crep),]

#what species have the smallest ranges? Largest ranges?
ggplot(lat.df, aes(x = (max_lat - min_lat), y = reorder(tips, (max_lat - min_lat)), fill = max_crep)) +
  geom_col() + facet_wrap(~Parvorder, scales = "free")

ggplot(lat.df, aes(y = (max_lat - min_lat), x = Family)) +
  geom_boxplot() 


range <- read_sf("C:/Users/ameli/Downloads/redlist_species_data_b8eeb8cf-3383-4314-bfb2-55dad2b8fec3/data_0.shp")

unique(range$SCI_NAME)

#filter for species with smallest ranges 
small_range_list <- lat.df %>% filter((max_lat - min_lat) < 30) %>% pull(Species_name)

sps_range <- range %>% filter(SCI_NAME %in% small_range_list)

ggplot(sps_range) +
  geom_sf(aes(fill = SCI_NAME), color = "black")


#filter for all the delphinidae ranges
delphinid_list <- lat.df %>% filter(Family == "Delphinidae") %>% pull(Species_name)
sps_range <- range %>% filter(SCI_NAME %in% delphinid_list)

ggplot(sps_range) +
  geom_sf(aes(fill = SCI_NAME), color = "black")


#filter for species by activity pattern
diel_list <- lat.df %>% filter(max_crep == "diurnal") %>% pull(Species_name)

sps_range <- range %>% filter(SCI_NAME %in% diel_list)

ggplot(sps_range) +
  geom_sf(aes(fill = SCI_NAME), color = "black")


#Do sympatric species show temporal niche partitioning?

lat.df %>% filter(Species_name %in% small_range_list)

##function to take max and min longitude

extractLongitude <-function(species_name){
  
  sps_range <- range %>% filter(SCI_NAME == species_name)
  
  xmin <- extent(sps_range)@xmin
  xmax <- extent(sps_range)@xmax
  
  latitude_list <- c(xmin, xmax)
  return(latitude_list)
}

longitude_list <- lapply(unique(range$SCI_NAME), function(x) extractLongitude(species_name = x))

names(longitude_list) <- unique(range$SCI_NAME)

lon.df <- data.frame(coords = unlist(longitude_list))
lon.df$Species_name <- rownames(lon.df)
lon.df <- lon.df %>% separate(Species_name, into = c("Species_name", "minmax"), sep = "\\.")

lon.df <- pivot_wider(lon.df, names_from = minmax, values_from = coords)
colnames(lon.df) <- c("Species_name", "min_lon", "max_lon")
lon.df$tips <- str_replace(lon.df$Species_name, pattern = " ", replacement = "_")
lon.df$mean_lon <- (lon.df$min_lon + lon.df$max_lon)/2

lon.df$tips <- str_replace(lon.df$Species_name, pattern = " ", replacement = "_")

trait.data <- merge(lat.df, lon.df, by = "tips", all = TRUE)

trait.data <- trait.data[!is.na(trait.data$max_crep),]

#install.packages("amt")
library(amt)







# Section X: Cetacean sleep duration --------------------------------------
url <- 'https://docs.google.com/spreadsheets/d/1F_m52NE1IWQRjfwgwF1yzVV4TaR4MEkKJUr2aDJt6LE/edit?usp=sharing'
duration <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

duration <- duration %>% filter(!is.na(Sleep_duration)) %>% select(tips, Sleep_duration)

trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

trait.data <- merge(trait.data, duration, by = "tips", all = TRUE)

trait.data %>% filter(!is.na(Sleep_duration)) %>%
  ggplot(., aes(x = max_crep, y = Sleep_duration)) + 
  geom_boxplot() + geom_jitter()

trait.data %>% filter(!is.na(Sleep_duration)) %>%
  ggplot(., aes(x = log(Body_mass_kg), y = Sleep_duration)) + 
  geom_point() + geom_smooth(method = "lm")






# Section X: The testing ground -------------------------------------------

#Investigating the Bayestrait packages
library("devtools")
install_github("rgriff23/btw")
library("btw")




#Is there a hidden rate in the artiodactyla transition rates: are cetaceans transitioning faster than ruminants

trait.data <- read.csv(here("Sleepy_artiodactyla_full.csv"))
trait.data <- trait.data[!is.na(trait.data$max_crep), c("tips", "max_crep")]
phylo_trees <- readRDS(here("maxCladeCred_mammal_tree.rds"))

#subset trait data to only include species that are in the tree
trait.data <- trait.data[trait.data$tips %in% phylo_trees$tip.label,]
# this selects a tree that is only the subset with data (mutual exclusive)
phylo_trees <- keep.tip(phylo_trees, tip = trait.data$tips)

hidden_rate_ARD <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 2, model = "ARD", node.states = "marginal")
plotMKmodel(hidden_rate_ARD)

ARD <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 1, model = "ARD", node.states = "marginal")
plotMKmodel(ARD)

#whippo bridge model
trait.data <- read.csv(here("whippomorpha.csv"))
#ruminant bridge model
trait.data <- read.csv(here("ruminants_full.csv"))
trait.data <- trait.data[!is.na(trait.data$max_crep), c("tips", "max_crep")]
phylo_trees <- readRDS(here("maxCladeCred_mammal_tree.rds"))

bridge_only <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 1, rate.mat = matrix(c(0,1,2,3,4,0,5,6,7,8,0,0,10,11,0,0), ncol = 4, nrow = 4), model = "ARD", node.states = "marginal")
bridge_only_HR <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 2, rate.mat = matrix(c(0,1,2,3,4,0,5,6,7,8,0,0,10,11,0,0), ncol = 4, nrow = 4), model = "ARD", node.states = "marginal")
plotMKmodel(bridge_only)
plotMKmodel(brdige_only_HR)

#subset trait data to only include species that are in the tree
trait.data <- trait.data[trait.data$tips %in% phylo_trees$tip.label,]
# this selects a tree that is only the subset with data (mutual exclusive)
phylo_trees <- keep.tip(phylo_trees, tip = trait.data$tips)

bridge_only <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 1, rate.mat = matrix(c(0,1,2,3,4,0,5,6,7,8,0,0,10,11,0,0), ncol = 4, nrow = 4), model = "ARD", node.states = "marginal")

#simmap: plot lineages through time based on max clade cred tree: compare cetaceans and ruminants
trait.data <- read.csv(here("whippomorpha.csv"))
trait.data <- trait.data[!is.na(trait.data$max_crep), c("tips", "max_crep")]
phylo_trees <- readRDS(here("maxCladeCred_mammal_tree.rds"))
#subset trait data to only include species that are in the tree
trait.data <- trait.data[trait.data$tips %in% phylo_trees$tip.label,]
# this selects a tree that is only the subset with data (mutual exclusive)
phylo_trees <- keep.tip(phylo_trees, tip = trait.data$tips)

model_ARD <- corHMM(phy = phylo_trees, data = trait.data, rate.cat = 1, model = "ARD", node.states = "marginal")
  
simmaps <- corHMM::makeSimmap(tree = phylo_trees, data = trait.data, rate.cat = 1, model = model_ARD$solution, nSim = 2, nCores = 1)

plotSimmap(simmaps)


# Concordance for each confidence level -----------------------------------

function to plot the concordance for each of the confidence levels
plotConcordance = function(set_column = "Conf2"){
  diel_full_filtered <- diel_full %>% filter(column == set_column)
  #need to filter for species with more than one entry or else concordance will always be 100%
  mulitple_sources <- diel_full_filtered %>% count(Species_name) %>% filter(n>1)
  diel_full_filtered <- diel_full_filtered[diel_full_filtered$Species_name %in% mulitple_sources$Species_name,]
  concordance <- as.data.frame(table(diel_full_filtered$max_crep, diel_full_filtered$value))
  colnames(concordance) <- c("actual", "predicted", "freq")
  totals_df <- aggregate(concordance$freq, by=list(Category=concordance$actual), FUN=sum)
  colnames(totals_df) <- c("actual", "total")
  concordance <- merge(concordance, totals_df, by = "actual")
  concordance$percent <- round(concordance$freq / concordance$total * 100, 1)
  return(concordance)
}

concordance_list <- lapply(sort(unique(diel_full$column)), function(x){plotConcordance(x)})

use below if
for(i in seq_along(concordance_list)){
  pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "artio", "confidence", i, "_confusion_matrix.pdf"))
  print(ggplot(as.data.frame(concordance_list[i]), aes(actual, predicted, fill = percent)) + geom_tile() + geom_text(aes(label = percent)) +
          scale_fill_gradient(low = "white", high = "dodgerblue") + labs(x = "Actual", y = "Predicted") +
          ggtitle(paste("Confidence level ", i, " concordance")))
  dev.off()
}





#Stochastic mapping with bayesian framework
#tutorial from Liam Revell, 2017 https://blog.phytools.org/2017/11/visualizing-rate-of-change-in-discrete.html
library(phytools)
tree

x






# Section: Transition rate scrap ------------------------------------------

#how do the principal components relate to the data 

ggplot(rates_df1, aes(y = log(rates), x = model_number, colour = solution)) + 
  theme_minimal() +
  geom_point()

#correlations between rates (I've done this before)
rates_wider <- rates_df1 %>% select(rates, solution, model_number) %>%
  pivot_wider(., names_from = solution, values_from = rates)

ggplot(rates_wider, aes(x = log(`Crepuscular -> Cathemeral`), y = log(`Diurnal -> Cathemeral`))) +
  geom_point() + geom_smooth(method = "lm")

#round values to one digit
ggplot(rates_df1, aes(x = log(round(rates)))) + 
  #theme_minimal() +
  geom_density() + facet_wrap(~solution)

#can take the mean because there are no zeros (only values very close to zero)
rates_df1 %>% group_by(solution) %>% summarize(mean_rates = mean(rates), SD_rates = sd(rates)) %>%
  ggplot(., aes(x = solution, y = mean_rates, fill = solution)) + geom_col() + 
  geom_errorbar(aes(ymin = mean_rates, ymax = mean_rates + SD_rates))




# Section: Does cetacean orbit size associate with eye size ---------------

trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))

trait.data %>% filter(!is.na(Orbit_ratio) & !is.na(Mean_dive_depth_m)) %>%
  ggplot(., aes(x = Orbit_ratio, y = Dive_depth_m)) +
  geom_point() + geom_smooth(method = "lm") + stat_poly_eq() +
  facet_wrap(~Family)


# Delphinidae dive depth --------------------------------------------------
trait.data.1 <- trait.data[!is.na(trait.data$Dive_depth_m),]
trait.data.delph <- trait.data.1 %>% filter(Family == "Delphinidae")

phylANOVA <- calculatePhylANOVA(trait.data.delph, "Dive_depth_m")

stat.test <- data.frame(group1 = c("cathemeral", "cathemeral", "cathemeral", "crepuscular", "crepuscular", "diurnal"),
                        group2 = c("crepuscular", "diurnal", "nocturnal", "diurnal", "nocturnal", "nocturnal"),
                        p.adj = c(phylANOVA$Pt[2], phylANOVA$Pt[3], phylANOVA$Pt[4], phylANOVA$Pt[7], phylANOVA$Pt[8], phylANOVA$Pt[12]),
                        y.position = c(7.2, 7.6, 8, 8.4, 8.8, 9.3))

stat.test <- stat.test %>% add_x_position(x = "max_crep")

delph_dive_boxplot <- ggplot(trait.data.delph, aes(x = max_crep, y = log(Dive_depth_m))) +
  geom_boxplot(aes(fill = max_crep), alpha = 0.8, outlier.shape = NA) + 
  scale_fill_manual(values = custom.colours, guide = "none") +
  new_scale_fill() + 
  labs(x = "Temporal activity pattern", y = "Log (maximum dive depth (m))") + 
  geom_jitter(aes(fill = Family), size = 3, width = 0.1, height = 0, colour = 'black', fill = "dodgerblue", pch = 21) +
  annotate("text", x = 1.3, y = 9, label = paste("phylANOVA, p =", phylANOVA$Pf)) +
  boxplot_theme +
  stat_pvalue_manual(stat.test, label = "p.adj") +
  scale_x_discrete(labels = c("cathemeral" = "Cathemeral", "crepuscular" = "Crepuscular", "diurnal" = "Diurnal", "nocturnal" = "Nocturnal")) 

delph_dive_boxplot

# pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/Dive_depth_boxplot_delphinidae.pdf", width = 7, height = 7.5)
# delph_dive_boxplot
# dev.off()



# Section 6: Proportion plots OLD----------------------------

new_mammals <- read.csv(here("Bennie_mam_data.csv")) #data from Bennie et al, 2014, 4477 sps
new_mammals <- new_mammals[!is.na(new_mammals$max_crep), ] 

#add in my primary source data 
artio_full <- read.csv(here("sleepy_artiodactyla_full.csv"))
artio_full <- artio_full[!is.na(artio_full$Diel_Pattern), ]

#we want to compare all mammals, vs all artiodactyla vs cetaceans/ruminants
new_mammals$mammals <- "Mammals"

custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")
mammals_plot <- ggplot(new_mammals, aes(x = mammals, fill = max_crep)) + geom_bar(position = "fill", width = 0.75) + scale_fill_manual(values = custom.colours) + theme_minimal() + theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())
ruminantia_plot <- artio_full %>% filter(Suborder == "Ruminantia") %>% ggplot(., aes(x = Suborder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + scale_fill_manual(values = custom.colours) + theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
whippomorpha_plot <- artio_full %>% filter(Suborder == "Whippomorpha") %>% ggplot(., aes(x = Suborder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + scale_fill_manual(values = custom.colours) + theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())

artiodactyla_plot <- artio_full %>% ggplot(., aes(x = Order, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + scale_fill_manual(values = custom.colours) + theme_minimal() + theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())
#suborders_plot <- 

#plot the full order proportions and the suborders in the same barplot
artio_full %>% mutate(Suborder = "Artiodactyla") %>% rbind(., artio_full) %>%
  ggplot(., aes(x = Suborder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + scale_fill_manual(values = custom.colours) +
  theme_bw() +
  #facet_wrap(~Suborder, nrow = 1, scales = "free") +
  labs(y = "Proportion", x = "Clade")
#theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())

#suborders_plot <- artio_full %>% filter(Suborder %in% c("Whippomorpha", "Ruminantia")) %>% ggplot(., aes(x = Suborder, fill = max_crep)) + geom_bar(position = "fill", width = 0.6) + scale_fill_manual(values = custom.colours) + theme_minimal() #+ theme(legend.position = "none", axis.title.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank())

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/barplot_percentages.pdf", width = 5, height = 2, bg = "transparent")
(artiodactyla_plot + plot_spacer() + plot_spacer() + plot_spacer()) / 
  suborders_plot
dev.off()

mammals_plot <- 
  new_mammals %>% group_by(max_crep) %>% summarize(count = n()) %>%
  ggplot(., aes(x = "", y = count, fill = max_crep)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") + theme_void() + theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank()) + 
  scale_fill_manual(values = custom.colours) + geom_text(aes(label = round((count/4477)*100, digits = 1)), position = position_stack(vjust = 0.5))

ruminantia_plot <- 
  artio_full %>% filter(Suborder == "Ruminantia") %>% 
  group_by(max_crep) %>% summarize(count = n()) %>%
  ggplot(., aes(x = "", y = count, fill = max_crep)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") + theme_void() + theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank()) + 
  scale_fill_manual(values = custom.colours) + geom_text(aes(label = round((count/206)*100, digits = 1)), position = position_stack(vjust = 0.5))

whippomorpha_plot <- 
  artio_full %>% filter(Suborder == "Whippomorpha") %>% 
  group_by(max_crep) %>% summarize(count = n()) %>%
  ggplot(., aes(x = "", y = count, fill = max_crep)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() + theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank()) + 
  scale_fill_manual(values = custom.colours) + geom_text(aes(label = round((count/84)*100, digits = 1)), position = position_stack(vjust = 0.5))

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/piechart_percentages.pdf", width = 10, height = 5, bg = "transparent")
grid.arrange(mammals_plot, ruminantia_plot, whippomorpha_plot, nrow = 1)
dev.off()


# Section : Max crep sankey with labels ----------------------------------
#add the concordance values to the sankey diagram
df <- mammals_df1 %>% make_long(Bennie_diel, Amelia_diel, Maor_diel,)

#with labels
concordance <- as.data.frame(table(mammals_df1$Bennie_diel, mammals_df1$Amelia_diel))
colnames(concordance) <- c("Bennie_data", "Amelia_data", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$Bennie_data), FUN=sum)
colnames(totals_df) <- c("Bennie_data", "total")
concordance <- merge(concordance, totals_df, by = "Bennie_data")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)
concordance$freq_count <- paste0(round(concordance$percent, 2), "%", " ", "(n=", concordance$freq, ")")
Bennie_freq_count <- concordance$freq_count
Bennie_freq_count <- Bennie_freq_count[-5] #remove zero for crep to cath

concordance <- as.data.frame(table(mammals_df1$Maor_diel, mammals_df1$Amelia_diel))
colnames(concordance) <- c("Maor_data", "Amelia_data", "freq")
totals_df <- aggregate(concordance$freq, by=list(Category=concordance$Maor_data), FUN=sum)
colnames(totals_df) <- c("Maor_data", "total")
concordance <- merge(concordance, totals_df, by = "Maor_data")
concordance$percent <- round(concordance$freq / concordance$total * 100, 1)
concordance$freq_count <- paste0(round(concordance$percent, 2), "%", " ", "(n=", concordance$freq, ")")
Maor_freq_count <- concordance$freq_count

sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") + scale_fill_manual(values = c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#66C2A5")) +
  theme_sankey(base_size = 16) + scale_x_discrete(labels = c("Bennie_diel" = "Existing database \n (Bennie et al)", "Amelia_diel" = "Current database \n (Mesich et al)", "Maor_diel" = "Existing database \n (Maor et al)")) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) + labs(y = NULL, x = NULL) + 
  annotate("text", x = 1.27, y = c(-108,-98,-88,-84,-60,-49,-45,-31.5,-4,38,50,64,80,93.5,109),
           label = Bennie_freq_count, size = 3.5, colour = "grey25") + 
  annotate("text", x = 2.73, y = c(-108,-90,-73,-67,-54,-30,-4,4,18,35,63,73,86,95,102,109),
           label = Maor_freq_count, size =3.5, colour = "grey25")
sankey

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Maor_Bennie_sankey_labelled.pdf"), width = 10.5, height = 8)
test
dev.off()


# Section: old crepuscularity pipeline sankey ------------------------------------

df <- data.frame(
  step_5 = c(rep("A. Total species (n = 84) ", 150)),
  step_4 = c(rep("B. No sources, non-crepuscular (n = 76)", 8), rep("C. Level B, C, D sources", 142)),
  step_3 = c(rep(NA, 8),
             rep("D. Category B sources majority (n = 33)", 33),
             rep("E. Category C sources majority (n = 70)", 70),
             rep("F. Category D sources (n = 39)", 39)),
  step_2 = c(rep(NA, 8),
             rep("G. No evidence (n = 23)", 20),
             rep("H. Crepuscular evidence (n = 10)", 13),
             rep("I. No evidence (n = 67)", 48),
             rep("J. Crepuscular evidence (n = 13)", 22),
             rep("K. No evidence (n = 29)", 28),
             rep("L. Crepuscular evidence (n = 10)", 11)),
  step_1 = c(rep(NA, 8),
             rep("M. Crepuscular evidence majority", 20),
             rep("M. Crepuscular evidence majority", 13),
             rep("M. Crepuscular evidence majority", 48),
             rep("M. Crepuscular evidence majority", 22),
             rep("M. Crepuscular evidence majority", 28),
             rep("M. Crepuscular evidence majority", 11)),
  step_0 = c(rep(NA, 8),
             rep("N. Crepuscular", 36),
             rep("O. Non-crepuscular", 88),
             rep("P. Tie", 18))
)


# Section 9: Comparison of artio data to Bennie and Maor data ---------------------------------------------

#my data
artio_df <- read.csv(here("sleepy_artiodactyla_minus_cetaceans.csv")) #235 species with data
#Maor dataset
Maor_diel <- read.csv(here("Maor_artio_full.csv")) #200 species
Maor_diel <- Maor_diel[Maor_diel$tips %in% artio_df$tips, ] #193 when filtering for those in my dataframe
#Bennie dataset
Bennie_diel <- read.csv(here("Bennie_mam_data.csv")) #447 species
Bennie_diel <- Bennie_diel[Bennie_diel$tips %in% artio_df$tips, ] #224 sps when filtering for those in my dataframe 

mammals_df <- merge(Maor_diel, Bennie_diel, by = "tips", all = TRUE) #268 species
#merge my artiodactyla data with the mammal data
mammals_df <- merge(mammals_df, artio_df, by = "tips", all = TRUE) #leaves 276 species
mammals_df <- mammals_df[, c("tips", "Diel_pattern", "max_crep.x", "Diel_Pattern")]
colnames(mammals_df) <- c("tips", "Maor_diel", "Bennie_diel", "Amelia_diel")
mammals_df$Maor_diel <- tolower(mammals_df$Maor_diel)

#classify partially cathemeral species as cathemeral
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "nocturnal/cathemeral", replacement = "cathemeral")
mammals_df$Maor_diel <- str_replace(mammals_df$Maor_diel, pattern = "diurnal/cathemeral", replacement = "cathemeral")

#only keep species that have entries in all three databases, leaves 190 species
mammals_df <- mammals_df[complete.cases(mammals_df[ , c('Bennie_diel', 'Maor_diel', 'Amelia_diel')]), ]

#move my data to centre so its easier to compare my data to both existing datasets
mammals_df <- mammals_df %>% relocate(Maor_diel, .after = last_col())

#change cathemeral/crepuscular species to just crepuscular
mammals_df <- data.frame(lapply(mammals_df, function(x) {gsub("cathemeral/crepuscular", "crepuscular", x)}))
df <- mammals_df %>% make_long(Bennie_diel, Amelia_diel, Maor_diel,)

six_state_sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") + scale_fill_manual(values = c("#dd8ae7", "#EECBAD" ,"#FC8D62", "pink", "#66C2A5", "#A6D854")) +
  theme_sankey(base_size = 16) + scale_x_discrete(labels = c("Bennie_diel" = "Existing database \n (Bennie et al)", "Amelia_diel" = "Current database \n (Mesich et al)", "Maor_diel" = "Existing database \n (Maor et al)")) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

six_state_sankey

#save out to figure folder
# pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Maor_Bennie_sankey_six_state.pdf"))
# six_state_sankey
# dev.off()

#with maximum crepuscular dataset
mammals_df1 <- data.frame(lapply(mammals_df, function(x) {gsub("diurnal/crepuscular", "crepuscular", x)}))
mammals_df1 <- data.frame(lapply(mammals_df1, function(x) {gsub("nocturnal/crepuscular", "crepuscular", x)}))

df <- mammals_df1 %>% make_long(Bennie_diel, Amelia_diel, Maor_diel,)

max_crep_sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") + scale_fill_manual(values = c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#66C2A5")) +
  theme_sankey(base_size = 12) + scale_x_discrete(labels = c("Bennie_diel" = "Existing dataset \n (Bennie et al)", "Amelia_diel" = "Current dataset \n (Mesich et al)", "Maor_diel" = "Existing dataset \n (Maor et al)")) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), axis.text = element_text(size = 11), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

max_crep_sankey


proportion_plot <-   
  mammals_df %>% 
  pivot_longer(!tips, names_to = "dataset", values_to = "activity_pattern") %>%
  mutate(activity_pattern = str_replace(activity_pattern, "diurnal/crepuscular", "diurnal")) %>%
  mutate(activity_pattern = str_replace(activity_pattern, "nocturnal/crepuscular", "nocturnal")) %>%
  ggplot(., aes(x = factor(dataset, levels = c("Bennie_diel", "Amelia_diel", "Maor_diel")), fill = activity_pattern)) + 
  geom_bar(position = "fill", alpha = 0.75) +
  scale_fill_manual(values= c("#dd8ae7","#EECBAD",  "#FC8D62","#66C2A5")) +
  labs(y = "Proportion of species", x = "Clade") + 
  scale_x_discrete(labels = c("Bennie_diel" = "Existing dataset \n (Bennie et al)", "Amelia_diel" = "Current dataset \n (Mesich et al)", "Maor_diel" = "Existing dataset \n (Maor et al)")) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 9))

proportion_plot

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/artiodactyl_proportion_plot.pdf", width = 4.5, height = 3.85)
proportion_plot
dev.off()

#save out to figure folder
pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", "Maor_Bennie_sankey_max_crep.pdf"), width = 4,  height = 4)
max_crep_sankey
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/artiodactyl_proportion_plot.pdf", width = 8.5, height = 3.75)
proportion_plot + max_crep_sankey
dev.off()


# Section 6: Plot rates from 100 most likely models ----------------------------------

filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

rates_df <- plot1kTransitionRates4state(readRDS(here(filename)), 5)

model_selection <- "ARD"

#filter by the model you're plotting
rates_df1 <- rates_df %>% filter(model == model_selection) 

df_full <- plot1kAIC(readRDS(here(filename)), 5)
df_full$model <- factor(df_full$model, levels = c("ER", "SYM", "CONSYM", "ARD", "bridge_only"))

model_selection <- "ARD"

df_full <- df_full %>% filter(model == model_selection)

df_full$model_number <- 1:nrow(df_full)

rates_df1$model_number <- rep(1:1000, each = (nrow(rates_df1)/1000))

#merge by model number

rates_df1 <- merge(rates_df1, df_full[, c("AIC_score", "model_number")], by = "model_number", all = TRUE)

#create list of 100 best models (lowest AIC score)
lowest_100 <- rates_df1 %>% arrange(AIC_score) %>% select(model_number) %>% slice(1:(nrow(rates_df1)/10))

#filter by list
rates_df1 <- rates_df1 %>% filter(model_number %in% unique(lowest_100$model_number))

#plot as usual

#extract just the starting state
rates_df1$start_state <- word(rates_df1$solution, 1)
#rates_df1$start_state <- paste(rates_df1$start_state, "to", sep = " ")
rates_df1$end_state <- word(rates_df1$solution, 3)

rates_df1$start_state <- factor(rates_df1$start_state, levels = c("Cathemeral to", "Diurnal to", "Crepuscular to", "Nocturnal to"))

#ARD colours
rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#3C967E","#A024AE","#AD9680", "#FA4A05")
#bridge colours
#rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#A024AE","#AD9680")

rates_plot <- 
  ggplot(rates_df1, aes(x= end_state, y = log(rates), group = solution, fill = solution, colour = solution)) + 
  geom_quasirandom(alpha = 0.8, width = 0.5, method = "quasirandom") + 
  scale_color_manual(values = rate_colours_end) +
  geom_violin(color = "black", scale = "width", alpha = 0.5) + theme_bw() +
  scale_fill_manual(values = rate_colours_end) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"), legend.position = "none")  +
  labs(x = "\n Transition", y = "Log(transition rate)") + 
  stat_summary(fun=median, geom="point", size=3, colour = "black", alpha = 0.2) +
  facet_wrap(~start_state, scales = "free_x", nrow = 2, ncol = 2)

rates_plot


#which trees overlap in the most likely models?
filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

rates_df <- plot1kTransitionRates4state(readRDS(here(filename)), 5)
rates_df1 <- rates_df %>% filter(model %in% c("ER", "SYM", "ARD")) %>% mutate(model_number = rep(rep(1:1000, each = 12),3))
rates_df2 <- rates_df %>% filter(model %in% c("CONSYM", "Bridge_only")) %>% mutate(model_number = rep(rep(1:1000, each = 10),2))
rates_df <- rbind(rates_df1, rates_df2)

df_full <- plot1kAIC(readRDS(here(filename)), 5)
df_full$model <- factor(df_full$model, levels = c("ER", "SYM", "CONSYM", "ARD", "bridge_only"))
df_full$model_number <- rep(1:1000, 5)

#merge by model number

rates_df <- merge(rates_df, df_full[, c("AIC_score", "model_number")], by = "model_number", all = TRUE)

ggplot(rates_df, aes(x = model_number, y = AIC_score, colour = model)) + geom_point()

df_full %>% group_by(model) %>% filter(AIC_score <200) %>%
  ggplot(., aes(x = model_number)) + geom_histogram(bins = 1000)

#filter for trees that appear in all five models as having the lowest AIC score
best_tree_list <-
  df_full %>%  filter(AIC_score <200) %>% group_by(model_number) %>%
  summarize(count = n()) %>% filter(count == 5) %>% pull(model_number)

#there are 19 trees with an AIC score less than 200 found in all 5 cetacean models
df_full %>% filter(model_number %in% best_tree_list) %>%
  ggplot(., aes(x = model, y = AIC_score)) + geom_boxplot() + geom_point()

rates_df %>% filter(model_number %in% best_tree_list) %>%
  ggplot(., aes(x = solution, y = log(rates), fill = solution)) + geom_point() + geom_violin()

# Section 8: Lineages through time  ---------------------------------------

filename <- "whippomorpha_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"
filename <- "ruminants_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"

model_results <- readRDS(here(paste0(filename, ".rds")))

model <- model_results$bridge_only_model 

trpy_n <- model$phy                 

setwd(here())
source("scripts/fish_sleep_functions.R")

# First, extract the ancestral states from the best fit model
anc_states <- returnAncestralStates(phylo_model = model, phylo_tree = trpy_n, rate.cat = 1, recon = "marg")

# Then, calculate transitions between states (or rate categories if set to true)
anc_states <- calculateStateTransitions(ancestral_states = anc_states, phylo_tree = trpy_n, rate.cat = F)

# Determine transition histories (types of lineages)
#adjust this function so it can accept four trait states
anc_states <- calculateLinTransHist2(ancestral_states = anc_states, phylo_tree = trpy_n)

# Calculate cumsums through time (for ltt plots)
anc_states <- returnCumSums(ancestral_states = anc_states, phylo_tree = trpy_n)

switch.histo <- switchHisto(ancestral_states = anc_states, replace_variable_names = T, backfill = F, states = T)
switch.ratio.types <- switchRatio(ancestral_states = anc_states, phylo_tree = trpy_n, node.age.cutoff = 0.02, use_types = T)
switch.ratio <- switchRatio(ancestral_states = anc_states, phylo_tree = trpy_n, node.age.cutoff = 0.02, use_types = F)

#fix so it has all four trait states
numb_switch_tree <- switchTree(ancestral_states = anc_states, phylo_tree = trpy_n, layout = "circular", replace_variable_names = TRUE)

# Section 9: PCA of transition rates --------------------------------------------------
library(FactoMineR)
#install.packages("factoextra")
library(factoextra)

#filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_artiodactyla_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

#requires the filename, the number of states in the model and the number of Mk models 
#returns a dataframe of the rates from each of the Mk models, for each of the 1k trees
rates_df <- plot1kTransitionRates4state(readRDS(here(filename)), 5)

#filter by the model you're plotting
rates_df1 <- rates_df %>% filter(model == "Bridge_only") 

#convert to needed format, every row is a model, every column is a transition rate
rates_df1$model_number <- rep(1:1000, each = (nrow(rates_df1)/1000))
rates.mx <- rates_df1 %>% select(rates, solution, model_number) %>% 
  pivot_wider(., names_from = solution, values_from = rates) %>%
  select(-model_number) %>%
  as.matrix()

#to get the cluster for each transition instead of each tree, this makes less sense
#rates.mx <- t(rates.mx)

#determine the optimal number of clusters (12 for the 12 transition rates?)
#can try various methods: wss, gap_stat, silhouette

fviz_nbclust(x = rates.mx, FUNcluster =  kmeans, method="wss") +
  theme(text = element_text(size=10))


# Compute k-means with k = 3 (can test alternative values)

# Set a seed for reproducibility.
set.seed(123)

# Generate our k-means analysis
rates.km <- 
  kmeans(scale(rates.mx), # We'll scale our data for this 
         centers = 4, 
         nstart = 25,
         iter.max = 500)

#plot the clusters
fviz_cluster(object = rates.km, # our k-means object
             data = rates.mx, # Our original data needed for PCA to visualize
             ellipse.type = "convex", 
             ggtheme = theme_bw(), 
             geom = "text",
             #repel=TRUE, # Try to avoid overlapping text
             labelsize = 10,
             pointsize = 4,
             main = "K-means clustering of cetacean activity pattern transition rates"
) +
  
  # Set some ggplot theme information
  theme(text = element_text(size=10)) +
  
  # Set the colour and fill scheme to viridis
  scale_fill_viridis_d(begin = 0, end = 0.7) +
  scale_colour_viridis_d(begin = 0, end = 0.7)


# Build a PCA of our RNAseq data with scaling applied
rates_scaled.pca <- FactoMineR::PCA(rates.mx, 
                                    scale.unit = TRUE, 
                                    ncp = 10,
                                    graph = TRUE)


# Visualize the impact of our eigenvalues
fviz_eig(rates_scaled.pca, addlabels = TRUE) + 
  theme(text = element_text(size=10))

# What is the information associated with our original variables
rates.var <- get_pca_var(rates_scaled.pca)

# Compare how our variables contribute and correlate with PC1/PC2
fviz_pca_var(X = rates_scaled.pca, 
             col.var = "contrib", # How will we colour our data/lines
             gradient.cols = c("green", "gold", "red"), 
             labelsize = 6,
             repel = TRUE, # make sure text doesn't overlap
             axes = c(3,4) # Determine which PCs you want to graph
) + 
  theme(text = element_text(size=7))


# Graph our scaled PCA data.
fviz_pca_ind(rates_scaled.pca, 
             #repel = TRUE, # avoid overlapping text points
             labelsize = 2, 
             axes = c(3,4) #chose which principal components 
) + theme(text = element_text(size=6)) # Make our text larger

#extract the individual trees from each cluster
#since the input is the transition rates from each tree (1-1000) numbering the results will give the correct model number
model_clusters <- data.frame(cluster = rates.km$cluster, model_number = 1:1000)

#plot the rates from only these trees

#filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
rates_df <- plot1kTransitionRates4state(readRDS(here(filename)), 5)

model_selection <- "Bridge_only"

#filter by the model you're plotting
rates_df1 <- rates_df %>% filter(model == model_selection) 

#label the model results by the tree it comes from
rates_df1$model_number <- rep(1:1000, each = (nrow(rates_df1)/1000))

#extract the list of tree numbers
cluster_list <- model_clusters %>% filter(cluster == 4) %>% pull(model_number)

#filter for the trees in the PCA cluster
rates_df1 <- rates_df1 %>% filter(model_number %in% cluster_list)

#extract just the starting state
rates_df1$start_state <- word(rates_df1$solution, 1)
rates_df1$end_state <- word(rates_df1$solution, 3)

rates_df1$start_state <- factor(rates_df1$start_state, levels = c("Cathemeral", "Diurnal", "Crepuscular", "Nocturnal"))

#ARD colours
rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#3C967E","#A024AE","#AD9680", "#FA4A05")
#bridge colours
#rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#A024AE","#AD9680")

rates_plot <- 
  ggplot(rates_df1, aes(x= end_state, y = log(rates), group = solution, fill = solution, colour = solution, label = model_number)) + 
  geom_quasirandom(alpha = 0.8, width = 0.5, method = "quasirandom") + 
  #geom_text(colour = "black") +
  scale_color_manual(values = rate_colours_end) +
  geom_violin(color = "black", scale = "width", alpha = 0.5) + theme_bw() +
  scale_fill_manual(values = rate_colours_end) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"), legend.position = "none")  +
  labs(x = "\n Transition", y = "Log(transition rate)") + 
  stat_summary(fun=median, geom="point", size=3, colour = "black", alpha = 0.2) +
  facet_wrap(~start_state, scales = "free_x", nrow = 2, ncol = 2)

rates_plot

#look at the structure of each tree

filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
model_results_all <- readRDS(here(filename))

plotMKmodel(model_results_all$bridge_only_model[439]$UNTITLED)
plotMKmodel(model_results_all$bridge_only_model[997]$UNTITLED)

custom.colours <- c("#dd8ae7", "#EECBAD" ,"#FC8D62", "#66C2A5")

model_results <- model_results_all$bridge_only_model[439]$UNTITLED
model_results$phy$tip.label  <- paste0(substr(model_results$phy$tip.label, 1, 1), "_", sapply(str_split(model_results$phy$tip.label,"_"), `[`, 2))
model_results$data[,c("tips")] <- paste0(substr(model_results$data[,c("tips")], 1, 1), "_", sapply(str_split(model_results$data[,c("tips")],"_"), `[`, 2))
sub_tree <- ggtree(model_results$phy, layout = "rectangular") %<+% model_results$data[,c("tips", "Diel_Pattern")]
sub_tree <- sub_tree + geom_tiplab(size = 2.5, offset = 1) 
sub_tree <- sub_tree + geom_tile(data = sub_tree$data[1:length(model_results$phy$tip.label),], aes(x=x, y=y, fill = Diel_Pattern), inherit.aes = FALSE, colour = "transparent", width = 1) 
sub_tree <- sub_tree + theme(legend.position = "bottom") + scale_fill_manual(name = "Temporal activity pattern", values = custom.colours)
viewClade(sub_tree, 142)

model_results <- model_results_all$bridge_only_model[997]$UNTITLED
model_results$phy$tip.label  <- paste0(substr(model_results$phy$tip.label, 1, 1), "_", sapply(str_split(model_results$phy$tip.label,"_"), `[`, 2))
model_results$data[,c("tips")] <- paste0(substr(model_results$data[,c("tips")], 1, 1), "_", sapply(str_split(model_results$data[,c("tips")],"_"), `[`, 2))
sub_tree2 <- ggtree(model_results$phy, layout = "rectangular") %<+% model_results$data[,c("tips", "Diel_Pattern")]
sub_tree2 <- sub_tree2 + geom_tiplab(size = 2.5, offset = 1)
sub_tree2 <- sub_tree2 + geom_tile(data = sub_tree2$data[1:length(model_results$phy$tip.label),], aes(x=x, y=y, fill = Diel_Pattern), inherit.aes = FALSE, colour = "transparent", width = 1) 
sub_tree2 <- sub_tree2 + theme(legend.position = "none") + scale_fill_manual(name = "Temporal \n activity pattern", values = custom.colours)
viewClade(sub_tree2, 142)

viewClade(sub_tree, 142) + viewClade(sub_tree2, 142)

#save out tree
png("C:\\Users\\ameli\\OneDrive\\Documents\\R_projects\\Amelia_figures\\example_trees_439_997.png", width = 12, height = 5, units = "in", res = 800)
viewClade(sub_tree, 142) + viewClade(sub_tree2, 142)
dev.off()  


lik.anc <- as.data.frame(rbind(model_results$tip.states, model_results$states))
colnames(lik.anc) <- c("cathemeral", "crepuscular", "diurnal", "nocturnal")
ggtree(phylo_tree, layout = "rectangular") + geom_tiplab(size = 1.8) + 
  geom_text(aes(label=node)) 




#associate each of these species and their trait states with its node
lik.anc$node <- c(1:length(phylo_tree$tip.label), (length(phylo_tree$tip.label) + 1):(phylo_tree$Nnode + length(phylo_tree$tip.label)))

#plot the ancestral reconstruction, displaying each of the three trait states (cathemeral, diurnal, nocturnal)
ancestral_plot_di <- ggtree(phylo_tree, layout = "circular") %<+% lik.anc + aes(color = diurnal) + geom_tippoint(aes(color = diurnal), shape = 16, size = 1.5) + scale_color_distiller(palette = "OrRd", direction = 1)  + geom_tiplab(color = "black", size = 3, offset = 0.5) + geom_tippoint(aes(color = diurnal), shape = 16, size = 1.5)
ancestral_plot_di
ancestral_plot_noc <- ggtree(phylo_tree, layout = "circular") %<+% lik.anc + aes(color = nocturnal) + geom_tippoint(aes(color = nocturnal), shape = 16, size = 1.5)+ scale_color_distiller(palette = "GnBu", direction = 1) + geom_tiplab(color = "black", size = 1.5, offset = 0.5) + geom_tippoint(aes(color = nocturnal), shape = 16, size = 1.5)
ancestral_plot_noc
ancestral_plot_cath <- ggtree(phylo_tree, layout = "circular") %<+% lik.anc + aes(color = cathemeral) + geom_tippoint(aes(color = cathemeral), shape = 16, size = 1.5) + scale_color_distiller(palette = "RdPu", direction = 1) + geom_tiplab(color = "black", size = 1.5, offset = 0.5) + geom_tippoint(aes(color = cathemeral), shape = 16, size = 1.5)
ancestral_plot_cath
ancestral_plot_crep <- ggtree(phylo_tree, layout = "circular") %<+% lik.anc + aes(color = crepuscular) + geom_tippoint(aes(color = crepuscular), shape = 16, size = 1.5) + scale_color_distiller(palette = "Greens", direction = 1) + geom_tiplab(color = "black", size = 1.5, offset = 0.5) + geom_tippoint(aes(color = cathemeral), shape = 16, size = 1.5)
ancestral_plot_crep

#as pie charts 
colnames(model_results$data) <- c("tips", "Diel_Pattern")

#to make more clear we can colour the tips separately using geom_tipppoint 
#may have to adjust what trait data column is called in each
base_tree <- ggtree(phylo_tree, layout = "rectangular") + geom_tiplab(size = 2, hjust = -0.1)
base_tree <- base_tree %<+% model_results$data[, c("tips", "Diel_Pattern")]
base_tree <- base_tree + geom_tippoint(aes(color = Diel_Pattern), size = 3) 
base_tree

#make the dataframe of likelihoods at the internal nodes without the tips
lik.anc <- as.data.frame(model_results$states)
lik.anc$node <- c(1:nrow(lik.anc)) + nrow(model_results$data)

#get the pie charts from this database using nodepie
#the number of columns changes depending on how many trait states
pie <- nodepie(lik.anc, 1:(length(lik.anc)-1))

pie_tree <- base_tree + geom_inset(pie, width = .03, height = .03) 
pie_tree 

png("C:\\Users\\ameli\\OneDrive\\Documents\\R_projects\\Amelia_figures\\example_tree_496.png", width = 40, height = 8, units = "cm", res = 800)
pie_tree + theme(legend.position = "none")
dev.off() 

mammal_trees <- read.nexus(here("Cox_mammal_data/Complete_phylogeny.nex"))
trait.data <- model_results_all$bridge_only_model[4]$UNTITLED$data
phylo_trees <- lapply(mammal_trees, function(x) subsetTrees(tree = x, subset_names = trait.data$tips))

cluster_list <- model_clusters %>% filter(cluster == 1) %>% pull(model_number)
subset_trees <- phylo_trees[cluster_list]

ggdensitree(subset_trees, layout = "rectangular")

ggtree(phylo_trees[4]$UNTITLED)
ggtree(model_results_all$bridge_only_model[4]$UNTITLED$phy)

# Section: Baker Proportion plots -----------------------------------------------

Baker_proportion <-   
  Baker_df %>% 
  pivot_longer(c("Activity_pattern", "max_crep"), names_to = "dataset", values_to = "activity_pattern") %>%
  mutate(activity_pattern = str_replace(activity_pattern, "diurnal/crepuscular", "diurnal")) %>%
  mutate(activity_pattern = str_replace(activity_pattern, "nocturnal/crepuscular", "nocturnal")) %>%
  ggplot(., aes(x = factor(dataset, levels = c("Activity_pattern", "max_crep")), fill = activity_pattern)) + 
  geom_bar(position = "fill", alpha = 0.75) +
  scale_fill_manual(values= c("#dd8ae7","#EECBAD", "#FC8D62","#66C2A5")) +
  labs(y = "Proportion of species", x = "Clade") + 
  scale_x_discrete(labels =  c("Activity_pattern" = "Existing dataset \n (Baker et al)", "max_crep" = "Current dataset \n (Mesich et al)")) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.title = element_text(size = 11), axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 9))

Baker_proportion

#save out to figure folder
pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/Baker_sankey_four_state.pdf", width= 5, height = 4)
Baker_sankey
dev.off()

pdf("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/Baker_proportion_plot.pdf", width = 3.5, height = 3.85)
Baker_proportion
dev.off()

# Section: six state sankey -----------------------------------------------

df <- Bennie_diel %>% make_long(max_crep.x, Diel_Pattern) 
Bennie_six_state_sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") + scale_fill_manual(values = c("#dd8ae7", "#EECBAD" ,"#FC8D62", "pink", "#66C2A5", "#A6D854", "grey")) +
  theme_sankey(base_size = 16) + 
  scale_x_discrete(labels = c("max_crep.x" = "Existing database \n (Bennie et al)", "Diel_Pattern" = "Current database \n (Mesich et al)")) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Bennie_six_state_sankey

df <- Maor_diel %>% make_long(Diel_pattern, Diel_Pattern)

Maor_six_state_sankey <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha= 0.5, node.color = 1) + geom_sankey_label(size = 3.5, color = 1, fill = "white") + scale_fill_manual(values = c("#dd8ae7", "#EECBAD" ,"#FC8D62", "pink", "#66C2A5", "#A6D854")) +
  theme_sankey(base_size = 16) + 
  scale_x_discrete(labels = c("Diel_pattern" = "Existing database \n (Maor et al)", "Diel_Pattern" = "Current database \n (Mesich et al)")) +
  theme(legend.position = "none", panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent')) + labs(x = NULL) 

Maor_six_state_sankey

# Section 3: How well do these sources agree (bennie vs maor)? -----------------------------
Bennie_mam_data <- read.csv(here("Bennie_mam_data.csv"))
maor_mam_data <- read.csv(here("Maor_mam_data.csv"))

diel_merge <- merge(Bennie_mam_data, maor_mam_data,by="tips") %>% select("tips", "Bennie_activity_pattern", "Order.x", "Family.x", "Maor_activity_pattern")
colnames(diel_merge) <- c("Species", "Bennie_diel", "Order", "Family", "Maor_diel")

#set default to idk
diel_merge$match <- "Unknown"

for(i in 1:length(diel_merge$Species)){
  if(diel_merge[i, "Bennie_diel"] == diel_merge[i, "Maor_diel"]){
    diel_merge[i, "match"] <- "Yes"
  } else if(diel_merge[i, "Bennie_diel"] %in% c("diurnal/crepuscular", "crepuscular", "cathemeral/crepuscular", "diurnal") & diel_merge[i, "Maor_diel"] %in% c("diurnal/crepuscular", "crepuscular", "cathemeral/crepuscular", "diurnal")){
    diel_merge[i, "match"] <- "Approximate"
  } else if(diel_merge[i, "Bennie_diel"] %in% c("nocturnal/crepuscular", "crepuscular", "cathemeral/crepuscular", "nocturnal")& diel_merge[i, "Maor_diel"] %in% c("nocturnal/crepuscular", "crepuscular", "cathemeral/crepuscular", "nocturnal")){
    diel_merge[i, "match"] <- "Approximate"
  } else if(diel_merge[i, "Bennie_diel"] %in% c("cathemeral", "cathemeral/crepuscular")& diel_merge[i, "Maor_diel"] %in% c("cathemeral", "cathemeral/crepuscular")){
    diel_merge[i, "match"] <- "Approximate"
  } else {
    diel_merge[i, "match"] <- "No"
  }
}

table(diel_merge$match)

#A surprising amount of consistency!
#170/2199 approximate matches, 244/2199 no matches, and 1785/2199 yes matches
#81% match!
diel_table <- diel_merge %>% count(match)
diel_table <- transform(diel_table, percent = (n/sum(diel_table$n)) * 100)

diel_table <- trait.data.all %>% count(Confidence)
diel_table <- transform(diel_table, percent = (n/sum(diel_table$n)) * 100)

ggplot(diel_table, aes(x="", y=n)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme_void()

#which species did not match
mismatch_species <- diel_merge %>% filter(match == "No")
table(mismatch_species$Bennie_diel, mismatch_species$Maor_diel)
#we should clean some of these entries up, could be a formatting error in why they don't match

#plot this on the mammal tree, which species tend to have conflicting data?
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
trait.data <- mismatch_species[,1:2]
trait.data <- trait.data[trait.data$Species %in% mam.tree$tip.label,]
trpy_n_mam <- keep.tip(mam.tree, tip = trait.data$Species)
ggtree(trpy_n_mam, layout = "circular") + geom_tiplab(size = 3)

#seem to be distributed throughout the tree

#get taxonomic information from Maor et al
maor_mam_data <- read_excel(here("Maor_diel_activity_data.xlsx"))
maor_mam_data <- maor_mam_data[17:nrow(maor_mam_data), 1:3]
colnames(maor_mam_data) <- c("Order", "Family", "tips")
maor_mam_data$tips <- str_replace(maor_mam_data$tips, pattern = " ", replacement = "_")

#diel_merge currently has 2,199 species
diel_merge <- merge(diel_merge, maor_mam_data, by = "tips") #now is 3,129 species
diel_merge <- diel_merge[!duplicated(diel_merge$tips),] #remove duplicates, back to 2,199

#save out the final 
colnames(diel_merge) <- c("tips", "Bennie_diel", "Bennie_source", "Maor_diel", "Maor_source", "match", "Order", "Family")
#write.csv(diel_merge, here("sleepy_mammals_old.csv"))

# Section 9: Full table of activity patterns (supplemental) --------------

artio_full <- read.csv(here("sleepy_artiodactyla_full.csv"),)
artio_full <- artio_full[, c("Species_name", "Suborder", "Diel_Pattern")]

#add in NA artio species since they got removed in the previous step
url <- 'https://docs.google.com/spreadsheets/d/1JGC7NZE_S36-IgUWpXBYyl2sgnBHb40DGnwPg2_F40M/edit?gid=562902012#gid=562902012'
diel_full <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
diel_full <- diel_full[is.na(diel_full$Confidence_primary_source), c("Species_name", "Diel_Pattern_primary")]
diel_full[diel_full == ""] <- NA
diel_full$Suborder <- "Ruminantia" #they are all ruminants
colnames(diel_full) <- c("Species_name", "Diel_Pattern", "Suborder")
diel_full <- diel_full %>% relocate(Diel_Pattern, .after = Suborder)

artio_full <- rbind(artio_full, diel_full)
artio_full$Diel_Pattern <- str_to_title(artio_full$Diel_Pattern)

test <- artio_full %>% count(Diel_Pattern)
test1 <- artio_full %>% filter(Suborder == "Whippomorpha") %>% count(Diel_Pattern)
test2 <- artio_full %>% filter(Suborder == "Ruminantia") %>% count(Diel_Pattern)
test3 <- artio_full %>% filter(Suborder == "Suina") %>% count(Diel_Pattern) #there are only 20sps
test4 <- artio_full %>% filter(Suborder == "Tylopoda") %>% count(Diel_Pattern) #there are only 6sps

test <- rbind(test, test1, test2, test3, test4)
test$Clade <- c(rep("All artiodactyla", 7), rep("Whippomorpha", 7), rep("Ruminantia", 7), rep("Suina", 6), rep("Tylopoda", 2))
test[is.na(test)] <- "Unknown"
test <- test %>% pivot_wider(names_from = Diel_Pattern, values_from = n)
test[is.na(test)] <- 0

knitr::kable(test, format = "html", digits = 3, caption = "Table 1") %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("table_final.html")
webshot("table_final.html", file = "C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/Supplemental_diel_pattern_chart.pdf")

#with max crep categories
artio_full$Diel_Pattern <- str_replace(artio_full$Diel_Pattern, pattern = "Diurnal/Crepuscular", replacement = "Crepuscular")
artio_full$Diel_Pattern <- str_replace(artio_full$Diel_Pattern, pattern = "Cathemeral/Crepuscular", replacement = "Crepuscular")
artio_full$Diel_Pattern <- str_replace(artio_full$Diel_Pattern, pattern = "Nocturnal/Crepuscular", replacement = "Crepuscular")

test <- artio_full %>% count(Diel_Pattern)
test1 <- artio_full %>% filter(Suborder == "Whippomorpha") %>% count(Diel_Pattern)
test2 <- artio_full %>% filter(Suborder == "Ruminantia") %>% count(Diel_Pattern)
test3 <- artio_full %>% filter(Suborder == "Suina") %>% count(Diel_Pattern) #there are only 20sps
test4 <- artio_full %>% filter(Suborder == "Tylopoda") %>% count(Diel_Pattern) #there are only 6sps

test <- rbind(test, test1, test2, test3, test4)
test$Clade <- c(rep("All artiodactyla", 5), rep("Whippomorpha", 5), rep("Ruminantia", 5), rep("Suina", 4), rep("Tylopoda", 2))
test[is.na(test)] <- "Unknown"
test <- test %>% pivot_wider(names_from = Diel_Pattern, values_from = n)
test[is.na(test)] <- 0

knitr::kable(test, format = "html", digits = 3, caption = "Table 1") %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("table_final.html")
webshot("table_final.html", file = "C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/Diel_pattern_chart.pdf")

# Section 13: eye mass vs body mass ---------------------------------------
eye_mass <- read_xlsx("C:\\Users\\ameli\\OneDrive\\Documents\\R_projects\\cetacean_discrete_traits\\Burton_2006.xlsx")

eye_mass <- separate(eye_mass, col = SpeciesBodymassBrainmassEyemass, into = c("Genus", "Species", "Body_mass_g", "Brain_mass_g", "Eye_mass_g"), sep = " ")
eye_mass <- eye_mass %>% mutate(Eye_mass_g = as.numeric(Eye_mass_g), 
                                Body_mass_g = as.numeric(Body_mass_g)* 1000, 
                                Brain_mass_g = as.numeric(Brain_mass_g), 
                                relative_eye_mass = Eye_mass_g/Body_mass_g)

eye_mass$tips <- paste(eye_mass$Genus, eye_mass$Species, sep = "_")

#fix species names
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Megaptera_nodosa", replacement = "Megaptera_novaeangliae")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Phocaena_phocaena", replacement = "Phocoena_phocoena")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Cervus_axis", replacement = "Axis_axis")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Gazella_thomsonii", replacement = "Eudorcas_thomsonii")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Rangifer_arcticus", replacement = "Rangifer_tarandus")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Cercopithecus_aethiops", replacement = "Chlorocebus_aethiops")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Sciurus_hudsonicus", replacement = "Tamiasciurus_hudsonicus")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Vulpes_fulvus", replacement = "Vulpes_vulpes")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Thos_mesomelas", replacement = "Canis_mesomelas")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Felis_leo", replacement = "Panthera_leo")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Felis_oregonensis", replacement = "Puma_concolor")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Claviglis_saturatus", replacement = "Graphiurus_murinus") #maybe
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Felis_capensis", replacement = "Leptailurus_serval")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Felis_onca", replacement = "Panthera_onca")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Felis_pardus", replacement = "Panthera_pardus")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Mustela_articus", replacement = "Mustela_erminea") #maybe
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Pecari_angulatus", replacement = "Dicotyles_tajacu")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Tapirella_bairdii", replacement = "Tapirus_bairdii")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Thalarctos_maritimus", replacement = "Ursus_maritimus")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Macaca_rhesus", replacement = "Macaca_mulatta")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Rhinocerus_bicornis", replacement = "Diceros_bicornis")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Equus_caballus", replacement = "Equus_ferus")
eye_mass$tips <- str_replace(eye_mass$tips, pattern = "Citellus_parryii", replacement = "Spermophilus_citellus")

#compare eye size across orders
ggplot(eye_mass, aes(x = Order, y = log(relative_eye_mass), colour = Body_mass_g)) +geom_boxplot() + geom_jitter()

#compare to activity patterns
artio_df <- read.csv(here("Sleepy_artiodactyla_full.csv"))
artio_df <- artio_df[artio_df$tips %in% eye_mass$tips, c("tips", "max_crep")]
#16 of 84 species in artio database

mammals_df <- read.csv(here("Bennie_mam_data.csv"))
mammals_df <- mammals_df[mammals_df$tips %in% eye_mass$tips, c("tips", "max_crep")]
#only 41 of 84 species have activity pattern data from Bennie et al
#remove artios that I have data for (use my data)
mammals_df <- mammals_df[!mammals_df$tips %in% artio_df$tips, ]
#removes 10 artio sps

mammals_df <- rbind(mammals_df, artio_df)

eye_mass <- merge(eye_mass, mammals_df, all = TRUE)
eye_mass[eye_mass$tips == "Homo_sapiens", c("max_crep")] <- "diurnal"
eye_mass[eye_mass$tips == "Peromyscus_sp.", c("max_crep")] <- "nocturnal" #the entire genus is nocturnal

#optional add in missing species, most are domestic 
eye_mass[eye_mass$tips == "Felis_domesticus", c("max_crep")] <- "crepuscular" #https://doi.org/10.1007/s10530-017-1534-x
eye_mass[eye_mass$tips == "Canis_familiaris", c("max_crep")] <- "crepuscular" #https://doi.org/10.1016/j.applanim.2021.105449
eye_mass[eye_mass$tips == "Bos_taurus", c("max_crep")] <- "crepuscular" #https://doi.org/10.1371/journal.pone.0313086
eye_mass[eye_mass$tips == "Capra_hircus", c("max_crep")] <- "crepuscular" #https://doi.org/10.1139/z03-055

eye_mass <- eye_mass[!is.na(eye_mass$max_crep), ]

eye_mass %>% filter(max_crep %in% c("diurnal", "nocturnal")) %>% ggplot(., aes(x = log(Body_mass_g), y = log(Eye_mass_g))) + geom_point(aes(colour = max_crep)) +
  geom_smooth(method="lm", na.rm=T, se=F, formula=y~x, aes(color=max_crep)) 

ggplot(eye_mass, aes(x = log(Body_mass_g), y = log(Eye_mass_g))) + geom_point(aes(colour = max_crep)) +
  geom_smooth(method="lm", na.rm=T, se=F, formula=y~x, aes(color=max_crep)) 

ggplot(eye_mass, aes(x = max_crep, y = log(relative_eye_mass))) +geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour = tips), size = 2) +
  stat_compare_means(method = "anova") # + facet_wrap(~max_crep)

eye_mass%>% filter(Order %in% c("Ungulates (Artiodactyla)", "Cetacea"), max_crep %in% c("diurnal", "nocturnal", "crepusuclar", "cathemeral")) %>%
  ggplot(., aes(x = max_crep, y = log(relative_eye_mass))) +geom_boxplot(outlier.shape = NA) + geom_jitter(aes(colour = Order)) #+ facet_wrap(~Order)

#pinnipeds have no activity pattern data so we can remove them 
eye_mass%>% filter(max_crep %in% c("diurnal", "nocturnal", "crepuscular", "cathemeral")) %>%
  ggplot(., aes(x = max_crep, y = log(relative_eye_mass))) +geom_boxplot(aes(fill = max_crep), outlier.shape = NA) + geom_point(aes(colour = Order), size =2) +
  stat_compare_means(method = "anova") #+facet_wrap(~Order)

eye_mass%>% filter(Order %in% c("Ungulates (Artiodactyla)", "Cetacea"), max_crep %in% c("diurnal", "nocturnal", "crepusuclar", "cathemeral")) %>%
  ggplot(., aes(x = max_crep, y = log(relative_eye_mass))) +geom_boxplot(outlier.shape = NA) + geom_point(aes(colour = tips)) #+ facet_wrap(~Order)

ggplot(eye_mass, aes(x = max_crep, y = log(Eye_mass_g))) +geom_boxplot(outlier.shape = NA, aes(fill = max_crep)) + 
  geom_point(aes(colour = Order), size = 2) +
  stat_compare_means(method = "anova") # + facet_wrap(~max_crep)



# Section: old crepuscularity pipeline ------------------------------------

#if category 3 greater than 50%, category 4 higher than 0% and if category 2 higher than 75% then call this source crepuscular
#if majority of evidence categories call it crepuscular then designate it crepuscular
#category 5 has no crepuscular calls and category 1 has only one, so don't use these to make calls

tabulateCrep = function(x){
  df <- aggregate(x$crepuscular, by = list(Category = x$column), FUN = sum)
  #confidence level 1 and 5 don't have any information on crepuscularity so remove them, they will always be zero
  df <- df[df$Category %in% c("Conf2", "Conf3", "Conf4"), ]
  df2 <- aggregate(x$total, by = list(Category = x$column), FUN = sum)
  
  #if species only has level 1 and/or 5 data then total evidence will always be FALSE (we can't determine crepuscularity)
  if(all(!df2$Category %in% c("Conf2", "Conf3", "Conf4")) == TRUE){
    total_evidence <- "no sources"
    return(total_evidence)
  }
  
  df2 <- df2[df2$Category %in% c("Conf2", "Conf3", "Conf4"), ]
  df2 <- merge(df, df2, by = "Category")
  df2$percentage <- round((df2$x.x/df2$x.y) * 100, digits = 1)
  #define what percentage of each category we want to call a species crepuscular,
  df3 <- data.frame(Category = c("Conf2", "Conf3", "Conf4"), cutoff = c(50, 50, 20))
  df2 <- merge(df2, df3, by = "Category")  #species can have Conf2, Conf3 and/or Conf4 evidence, when merged any missing categories will be dropped
  df2$crep_evidence <- df2$percentage >= df2$cutoff
  df2[df2$crep_evidence == TRUE, "crep_evidence"] <- "crepuscular"
  df2[df2$crep_evidence == FALSE, "crep_evidence"] <- "non"
  unique_percents <- unique(df2$crep_evidence)
  #check if there's a tie, since we only use level 2,3 and 4, a tie will only occur when using two evidence levels 
  if(nrow(df2) == 2){
    if("crepuscular" %in% df2$crep_evidence & "non" %in% df2$crep_evidence){
      #to break ties could look at which has more sources (to avoid making calls off one source)
      #total_evidence <- df2[which.max(df2$x.y), "crep_evidence"]
      #instead could also break ties by looking at the higher confidence level (this will always be the second row but use which to be safe)
      total_evidence <- df2[which(df2$Category == max(df2$Category)), "crep_evidence"]
    } else{
      total_evidence <- unique_percents[which.max(tabulate(match(df2$crep_evidence, unique_percents)))]
    }
  } else {total_evidence <- unique_percents[which.max(tabulate(match(df2$crep_evidence, unique_percents)))]}
  #additional screen: if there is level 4 evidence then evaluate to true 
  if(nrow(filter(df2, Category == "Conf4"))== 1){
    if(df2[df2$Category == "Conf4", "crep_evidence"] == "crepuscular"){
      total_evidence <- "crepuscular"
    }
  } 
  return(total_evidence)
}

crep_df <- diel_long%>% group_by(Species_name) %>% do(tabulated_crep = tabulateCrep(.)) %>% unnest()

