source("scripts/fish_sleep_functions.R")
source("scripts/cetacean_sleep_functions.R")
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))

# Section 1.0: Plot formatting -------------------------------------

trait.data <- read.csv(here("cetacean_ecomorphology_dataset.csv"))
#filter for species with activity pattern data
trait.data <- trait.data[!is.na(trait.data$max_crep),]

#save formatting
custom.colours <- c("#dd8ae7","#EECBAD", "#FC8D62", "#66C2A5")
boxplot_theme <-  theme_minimal() +
  theme(panel.background = element_rect(fill='transparent', colour = "transparent"), 
                        plot.background = element_rect(fill='transparent', color=NA), 
                        legend.background = element_rect(fill='transparent'),
                        legend.position = "none",
                        panel.border = element_rect(colour = "black", fill = "transparent"),
                        axis.text = element_text(size = 9), axis.title = element_text(size = 11))  

#save out the standard plot elements
boxplot_format <- list(geom_boxplot(aes(fill = max_crep), alpha = 0.5, outlier.shape = NA),
                       scale_fill_manual(values = custom.colours, guide = "none"),
                       geom_jitter(aes(fill = max_crep), size = 3, width = 0.1, height = 0, colour = "black", pch = 21, alpha = 0.8), boxplot_theme,
                       scale_x_discrete(labels = c("cathemeral" = "Cathemeral", "crepuscular" = "Crepuscular", "diurnal" = "Diurnal", "nocturnal" = "Nocturnal")))

# Section 2.0: Cetacean orbit ratio ----------------------------------------------------

trait.data.1 <- trait.data[!is.na(trait.data$Orbit_ratio),]

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.1, "Orbit_ratio")

#set the family colours for consistency
#family.colours <- c("grey", "black","dodgerblue", "royalblue3",  "cyan","darkgreen", "springgreen", "darkolivegreen1", "gold", "orange", "firebrick2", "brown", "orchid1", "darkorchid")

#plot out the group means
cet_orbit_boxplot <- ggplot(trait.data.1, aes(x = max_crep, y = Orbit_ratio)) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Relative eye size") + 
  annotate("text", x = 1.4, y = 43, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

cet_orbit_boxplot

# Section 3.0: Cetacean mean latitude --------------------------------------------------

#family.colours <- c("dodgerblue",  "cyan","darkgreen", "springgreen", "darkolivegreen1", "gold", "orange", "firebrick2", "brown", "orchid1", "darkorchid")

#select the variable we're looking at
trait.data.1 <- trait.data[!is.na(trait.data$mean_lat),]
trait.data.1 <- trait.data.1[!is.na(trait.data.1$max_crep),]

trait.data.1 <- filter(trait.data.1, Parvorder == "Odontoceti")

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.1, "mean_lat")

cet_lat_boxplot <- ggplot(trait.data.1, aes(x = max_crep, y = mean_lat)) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Mean latitude range") + 
  annotate("text", x = 1.4, y = 100, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

# Section 4.0: Cetacean max latitude ---------------------------------------------------
#family.colours <- c("dodgerblue",  "cyan","darkgreen", "springgreen", "darkolivegreen1", "gold", "orange", "firebrick2", "brown", "orchid1", "darkorchid")

#select the variable we're looking at
trait.data.1 <- trait.data[!is.na(trait.data$max_lat),]
trait.data.1 <- trait.data.1[!is.na(trait.data.1$max_crep),]

trait.data.1 <- filter(trait.data.1, Parvorder == "Odontoceti")

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.1, "max_lat")

cet_maxlat_boxplot <- ggplot(trait.data.1, aes(x = max_crep, y = max_lat)) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Maximum latitude range") + 
  annotate("text", x = 1.4, y = 100, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

# Section 5.0: Cetacean body mass ------------------------------------------------------

trait.data.1 <- trait.data[!is.na(trait.data$Body_mass_kg),]

#set the family colours for consistency
#family.colours <- c("grey", "black","dodgerblue", "royalblue3",  "cyan","darkgreen", "springgreen", "darkolivegreen1", "gold", "orange", "firebrick2", "brown", "orchid1", "darkorchid")

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.1, "Body_mass_kg")

cet_mass_boxplot <- ggplot(trait.data.1, aes(x = max_crep, y = log(Body_mass_kg))) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Log (body mass (kg))") + 
  annotate("text", x = 1.4, y = 13, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

# Section 6.0: Cetacean dive depth -----------------------------------------------------
#family.colours <- c("grey", "black","dodgerblue", "cyan", "darkgreen","darkolivegreen1", "orange", "firebrick2", "orchid1", "darkorchid")

trait.data.1 <- trait.data[!is.na(trait.data$Dive_depth_m),]

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.1, "Dive_depth_m")

cet_dive_boxplot <- ggplot(trait.data.1, aes(x = max_crep, y = log(Dive_depth_m))) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Log (maximum dive depth (m))") + 
  annotate("text", x = 1.4, y = 9.5, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

cet_dive_boxplot

# Section 7.0: Ruminant orbit size -----------------------------------------------------

#family.colours <- c("dodgerblue", "springgreen",  "gold", "darkorchid")

#load in the data
trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$Orbit_ratio), c("tips", "Orbit_ratio", "max_crep", "Family")]

#filter for just ruminants
trait.data.art <-  filter(trait.data.art, Family %in% c("Bovidae", "Cervidae", "Giraffidae", "Tragulidae"))

#calculate phylogenetic anova
phylANOVA <- calculatePhylANOVA(trait.data.art, "Orbit_ratio")

#cannot calculate the pairwise corrected p-values likely because the cathemeral sample size is too small
#remove cathemeral and rerun
trait.data.art.subset <- filter(trait.data.art, max_crep != "cathemeral")
phylANOVA <- calculatePhylANOVA(trait.data.art.subset, "Orbit_ratio")

#add p values manually
stat.test <- data.frame(group1 = c("crepuscular", "crepuscular", "diurnal"),
                        group2 = c("diurnal", "nocturnal", "nocturnal"),
                        p.adj = c(phylANOVA$Pt[2], phylANOVA$Pt[3], phylANOVA$Pt[6]),
                        y.position = c(0.981, 0.990, 0.999))

stat.test <- stat.test %>% add_x_position(x = "max_crep")
stat.test <- stat.test %>% mutate(xmin = xmin + 1, xmax = xmax + 1)

rum_orbit_boxplot <- ggplot(trait.data.art, aes(x = max_crep, y = Orbit_ratio)) + 
  boxplot_format +
  stat_pvalue_manual(stat.test, label = "p.adj", hjust = 0.7, size = 3) +
  labs(x = "Temporal activity pattern", y = "Relative eye size") + 
  annotate("text", x = 1.4, y = 1, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

rum_orbit_boxplot

# Section 8.0: Ruminant mean latitude -------------------------------------------------------
#family.colours <- c("black","dodgerblue", "springgreen",  "gold", "firebrick2", "darkorchid")

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
#use below for IUCN data
trait.data.art <- trait.data.art[!is.na(trait.data.art$mean_lat), c("tips", "mean_lat", "max_crep", "Family")]
#use below for pantheria data
#trait.data.art <- trait.data.art[!is.na(trait.data.art$GR_MidRangeLat_dd), c("tips", "GR_MidRangeLat_dd", "max_crep", "Family")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ] %>% filter(Family %in% c("Bovidae", "Cervidae", "Antilocapridae", "Giraffidae", "Tragulidae", "Moschidae"))

#perform the phylogenetically corrected one-way anova
#phylANOVA <- calculatePhylANOVA(trait.data.art, "GR_MidRangeLat_dd")
phylANOVA <- calculatePhylANOVA(trait.data.art, "mean_lat")

#add p values manually
stat.test <- data.frame(group1 = c("cathemeral", "cathemeral", "cathemeral", "crepuscular", "crepuscular", "diurnal"),
                        group2 = c("crepuscular", "diurnal", "nocturnal", "diurnal", "nocturnal", "nocturnal"),
                        p.adj = c(phylANOVA$Pt[2], phylANOVA$Pt[3], phylANOVA$Pt[4], phylANOVA$Pt[7], phylANOVA$Pt[8], phylANOVA$Pt[12]),
                        y.position = c(80, 93, 106, 119, 133, 152))

stat.test <- stat.test %>% add_x_position(x = "max_crep")

rum_lat_boxplot <- ggplot(trait.data.art, aes(x = max_crep, y = mean_lat)) + 
  boxplot_format +
  stat_pvalue_manual(stat.test, label = "p.adj", hjust = 0.7, size = 3) +
  labs(x = "Temporal activity pattern", y = "Mean latitude range") + 
  annotate("text", x = 1.4, y = 152, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

rum_lat_boxplot

# Section 9.0: Ruminant max latitude ---------------------------------------------------
#family.colours <- c("black","dodgerblue", "springgreen",  "gold", "firebrick2", "darkorchid")

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
#use below for IUCN data
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_lat), c("tips", "max_lat", "max_crep", "Family")]
#use below for pantheria data
#trait.data.art <- trait.data.art[!is.na(trait.data.art$GR_MaxLat_dd), c("tips", "GR_MaxLat_dd", "max_crep", "Family")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ] %>% filter(Family %in% c("Bovidae", "Cervidae", "Antilocapridae", "Giraffidae", "Tragulidae", "Moschidae"))

#perform the phylogenetically corrected one-way anova
#phylANOVA <- calculatePhylANOVA(trait.data.art, "GR_MaxLat_dd")
phylANOVA <- calculatePhylANOVA(trait.data.art, "max_lat")

#add p values manually
stat.test <- data.frame(group1 = c("cathemeral", "cathemeral", "cathemeral", "crepuscular", "crepuscular", "diurnal"),
                        group2 = c("crepuscular", "diurnal", "nocturnal", "diurnal", "nocturnal", "nocturnal"),
                        p.adj = c(phylANOVA$Pt[2], phylANOVA$Pt[3], phylANOVA$Pt[4], phylANOVA$Pt[7], phylANOVA$Pt[8], phylANOVA$Pt[12]),
                        y.position = c(80, 90, 100, 110, 120, 135))

stat.test <- stat.test %>% add_x_position(x = "max_crep")

rum_max_lat_boxplot <- ggplot(trait.data.art, aes(x = max_crep, y = max_lat)) + 
  boxplot_format +
  stat_pvalue_manual(stat.test, label = "p.adj", hjust = 0.7, size = 3) +
  labs(x = "Temporal activity pattern", y = "Maximum latitude range") + 
  annotate("text", x = 1.4, y = 135, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

rum_max_lat_boxplot

# Section 10.0: Ruminant body mass ------------------------------------------------------
#family.colours <- c("black","dodgerblue", "springgreen",  "gold", "firebrick2", "darkorchid")

trait.data.art <- read.csv(here("artiodactyla_ecomorphology_dataset.csv"))
trait.data.art <- trait.data.art[!is.na(trait.data.art$AdultBodyMass_g), c("tips", "AdultBodyMass_g", "max_crep", "Family", "Diel_Pattern")]
trait.data.art <- trait.data.art[!is.na(trait.data.art$max_crep), ] %>% filter(Family %in% c("Bovidae", "Cervidae", "Antilocapridae", "Giraffidae", "Tragulidae", "Moschidae"))

trait.data.art <- trait.data.art %>% mutate(AdultBodyMass_kg = AdultBodyMass_g/1000)

#perform the phylogenetically corrected one-way anova
phylANOVA <- calculatePhylANOVA(trait.data.art, "AdultBodyMass_kg")

rum_mass_boxplot <- ggplot(trait.data.art, aes(x = max_crep, y = log(AdultBodyMass_kg))) + 
  boxplot_format +
  labs(x = "Temporal activity pattern", y = "Log (body mass (kg))") + 
  annotate("text", x = 1.4, y = 7.5, label = paste("phylANOVA, p =", phylANOVA$Pf)) 

# Section 11.0: Arranging and saving out the plots --------------------------------------

# pdf(here("Figure_folder/ecomorphological_boxplots.pdf"), width = 8.5, height = 7, bg = "transparent")
#(rum_orbit_boxplot/ rum_lat_boxplot) | (cet_orbit_boxplot/cet_lat_boxplot/cet_dive_boxplot) 
# dev.off()

#alternative plot arrangement
rum_boxplots <- (rum_orbit_boxplot + expand_limits(y = 1.005) + theme(axis.title.x = element_text(colour = "white")))/
  (rum_lat_boxplot + expand_limits(y = 160)) 
rum_boxplots

cet_boxplots <- (cet_orbit_boxplot + expand_limits(y = 45) + theme(axis.title.x = element_text(colour = "white")))/
  (cet_lat_boxplot + expand_limits(y = 108) + theme(axis.title.x = element_text(colour = "white"))) / 
  (cet_dive_boxplot + expand_limits(y = 10)) 
cet_boxplots

pdf(here("Figure_folder/rum_ecomorphological_boxplots.pdf"), width = 4.25, height = 6, bg = "transparent")
rum_boxplots
dev.off()

pdf(here("Figure_folder/cet_ecomorphological_boxplots.pdf"), width = 4.25, height = 6, bg = "transparent")
cet_boxplots
dev.off()

