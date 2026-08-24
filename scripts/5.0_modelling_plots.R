# Section 0: Load files and plotting scripts ------------------------------

setwd(here())

#set the filename for maximum clade credibility results
#filename <- "artiodactyla_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models.rds"
#filename <- "whippomorpha_june_2026_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models.rds"
filename <- "ruminants_june_2026_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models.rds"

#set the filename for 1k trees results 
filename_whippo_1k <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

filename_rumi_1k <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename_artio_1k <- "august_artiodactyla_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

#source the plotting scripts
source(here("scripts/fish_sleep_functions.R"))
source(here("scripts/Cetacean_sleep_functions.R"))
source(here("scripts/Plotting_functions.R"))

# Section 1: max_clade_cred likelihood metrics ---------------------------

#returns a dataframe of all three metrics for all models
likelihood_metrics <- max_clade_metrics(readRDS(here(filename)))
likelihood_metrics <- pivot_wider(likelihood_metrics, names_from = model_metric, values_from = model_value)
likelihood_metrics$most_likely <- ""  
likelihood_metrics[which(likelihood_metrics$AIC_scores == min(likelihood_metrics$AIC_scores)), "most_likely"] <- "**"
likelihood_metrics <- likelihood_metrics %>% mutate(delta_AIC = AIC_scores - min(likelihood_metrics$AIC_scores))

knitr::kable(likelihood_metrics, format = "html", digits = 2, caption = filename) %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("likelihood_table.html")
webshot("likelihood_table.html", file = here(paste0("Figure_folder/likelihood_table_", filename, ".png")), vwidth = 992, vheight = 300)

likelihood_metrics <- as.data.frame(likelihood_metrics) %>% 
  mutate(model_comparison = paste0(round(delta_AIC, digits = 2), " (", round(AIC_scores, digits = 2), ")")) %>%
  select(model_comparison, model) %>%
  pivot_wider(names_from = model, values_from = model_comparison) 

colnames(likelihood_metrics) <- c("ER", "SYM", "bridge-SYM", "ARD", "bridge-ARD")

knitr::kable(likelihood_metrics, format = "html", digits = 2, caption = "Model_comparison \n AIC score difference (AIC score)") %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("likelihood_table.html")
webshot("likelihood_table.html", file = here(paste0("Figure_folder/likelihood_table_long_", filename, ".png")), vwidth = 992, vheight = 300)

# Section 2: max_clade_cred rates ---------------------------

model_results <- readRDS(here(filename))

png(here(paste0("Figure_folder/max_clade_cred_rates_", filename, ".png")), width = 30, height = 15, units = "cm", res = 600)

# create a new plotting window and set the plotting area into a 2*2 array
par(mfrow = c(2, 2))
#plotMKmodel(model_results$ER_model)
plotMKmodel(model_results$SYM_model)
plotMKmodel(model_results$ARD_model)
plotMKmodel(model_results$bridge_only)
plotMKmodel(model_results$CONSYM_model)
dev.off()

# Section 3: Plot AIC scores from 1k model results whippo ----------------------

#requires the filename and the number of Mk models (3: ER, SYM, ARD or 4: ER, SYM, ARD, CONARD, 5: ER, SYM, ARD, bridge_only, CONSYM)
#returns a df of the AIC scores for all 1k trees x number of Mk models
df_full <- plot1kAIC(readRDS(here(filename_whippo_1k)), 5)
df_full$model <- factor(df_full$model, levels = c("ER", "SYM", "CONSYM", "ARD", "bridge_only"))

means <- aggregate(AIC_score ~  model, df_full, mean)
means$AIC_score <- round(means$AIC_score, digits = 2)

#calculate the delta AIC (difference between the AIC scores)
means <- means %>% mutate(delta_AIC = round(AIC_score - min(AIC_score), digits = 2))
#allows for the trailing zeros
means$delta_AIC <- formatC(means$delta_AIC, format = "f", digits = 2)

custom.colours <- c("#013873", "#04549F", "#056CCC", "#60B0FB", "#8DC6FC")

#plot and save out - raincloud plot
whippo_plot <-
ggplot(df_full, aes(x = model, y = AIC_score, fill = model)) + 
  scale_fill_manual(values = custom.colours) +
  ggdist::stat_halfeye(alpha = 0.6, adjust = .5, width = .6, justification = -.3, .width = 0, point_colour = NA) +
  geom_boxplot(alpha = 0.2, width = .25, colour = "black" ,outlier.shape = NA) +
  geom_point(aes(color = model), stroke = 1, size = 1, alpha = .2, position = position_jitter(seed = 1, width = .15)) +
  scale_color_manual(values = custom.colours) + 
  geom_text(data = means, aes(label = delta_AIC, y = AIC_score), hjust = -0.35, size = 3) +
  labs(x = "Model", y = "AIC score")  + theme_bw() +
  scale_x_discrete(labels = c("ER", "SYM", "SYM-bridge", "ARD", "ARD-bridge"), expand = expansion(0,0.3)) +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11), legend.position = "none")

whippo_plot

# Section 4: Plot AIC scores from 1k model results ruminants ----------------------

#requires the filename and the number of Mk models (3: ER, SYM, ARD or 4: ER, SYM, ARD, CONARD, 5: ER, SYM, ARD, bridge_only, CONSYM)
#returns a df of the AIC scores for all 1k trees x number of Mk models
df_full <- plot1kAIC(readRDS(here(filename_rumi_1k)), 5)
df_full$model <- factor(df_full$model, levels = c("ER", "SYM", "CONSYM", "ARD", "bridge_only"))

means <- aggregate(AIC_score ~  model, df_full, mean)
means$AIC_score <- round(means$AIC_score, digits = 2)

#calculate the delta AIC (difference between the AIC scores)
means <- means %>% mutate(delta_AIC = round(AIC_score - min(AIC_score), digits = 2))
#allows for the trailing zeros
means$delta_AIC <- formatC(means$delta_AIC, format = "f", digits = 2)

custom.colours <- c("#013873", "#04549F", "#056CCC", "#60B0FB", "#8DC6FC")

#plot and save out - raincloud plot
ruminant_plot <- 
  ggplot(df_full, aes(x = model, y = AIC_score, fill = model)) + 
  scale_fill_manual(values = custom.colours) +
  ggdist::stat_halfeye(alpha = 0.6, adjust = .5, width = .6, justification = -.3, .width = 0, point_colour = NA) +
  geom_boxplot(alpha = 0.2, width = .25, colour = "black",outlier.shape = NA) + 
  geom_point(aes(color = model), stroke = 1, size = 1, alpha = .2, position = position_jitter(seed = 1, width = .15)) +
  scale_color_manual(values = custom.colours) + 
  geom_text(data = means, aes(label = delta_AIC, y = AIC_score), hjust = -0.35, size = 3) +
  labs(x = "Model", y = "AIC score")  + theme_bw() +
  scale_x_discrete(labels = c("ER", "SYM", "SYM-bridge", "ARD", "ARD-bridge"), expand = expansion(0,0.3)) +
  theme(axis.text = element_text(size = 9), axis.title = element_text(size = 11), legend.position = "none") + coord_cartesian(ylim = c(398, 525)) #there is one extreme outlier in the ER models of 564

ruminant_plot

#save out
pdf(here("Figure_folder/model_boxplots.pdf"), width = 3.3, height = 5)
(whippo_plot + theme(axis.title.x = element_text(colour = "white", size = 1), axis.text.x = element_text(angle = 30, vjust = 0.75)))/
  (ruminant_plot + theme(axis.text.x = element_text(angle = 30, vjust = 0.75)))
dev.off()
 
# Section 5: Density plot of transition rates (bridge-ARD) ----------------------------------------

rates_df1 <- plot1kTransitionRates4state(readRDS(here(filename_whippo_1k)), 5)
rates_df2 <- plot1kTransitionRates4state(readRDS(here(filename_rumi_1k)), 5)

ridges_palette_10 <- c(alpha("#AAC7A9", 0.7), alpha("#AAC7A9", 0.4), alpha("#A2A6C6", 0.7), alpha("#A2A6C6", 0.4), alpha("#F5AC88", 0.7), alpha("#F5AC88", 0.4), alpha("#ED8CA5", 0.7), alpha("#ED8CA5", 0.4), alpha("#E6ABCA", 0.7), alpha("#E6ABCA", 0.4))

whippo_bridge_rates_density_ridges <-
  rates_df1 %>% filter(model == "Bridge_only") %>%
  mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = 
           factor(solution, levels = c("Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal",
                                       "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral", 
                                       "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal",
                                       "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral",
                                       "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular" ))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  #scale_fill_viridis_d(option = "C") +
  scale_fill_manual(values = ridges_palette_10) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 1.6))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =9), axis.title.x = element_text(size = 11), axis.title.y = element_blank(),
                     strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = "transparent"), 
                     plot.background = element_rect(fill='transparent', color=NA), panel.grid = element_blank())

rumi_bridge_rates_density_ridges <-
  rates_df2 %>% filter(model == "Bridge_only") %>% mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = 
           factor(solution, levels = c("Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal",
                                       "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral", 
                                       "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal",
                                       "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral",
                                       "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular"))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  scale_fill_manual(values = ridges_palette_10) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 1.75))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.title.y = element_blank(), axis.title = element_text(size = 12),
                     strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = "transparent"), 
                     plot.background = element_rect(fill='transparent', color=NA), panel.grid = element_blank())

#save out
pdf(here("Figure_folder/transition_rates_density_ridges_bridge-ARD.pdf"), width = 3.25, height = 5.5)
(whippo_bridge_rates_density_ridges + theme(axis.title.x = element_text(colour = "white", size = 1))) /
  (rumi_bridge_rates_density_ridges)
dev.off()

# #pdf(here("Figure_folder/model_boxplots.pdf"), width = 6.85, height = 5.5)
# ((whippo_plot + theme(axis.title.x = element_text(colour = "white", size = 1), axis.text.x = element_text(angle = 30, vjust = 0.75))) + whippo_bridge_rates_density_ridges + theme(axis.title.x = element_text(colour = "white", size = 1)))/
#   ((ruminant_plot + theme(axis.text.x = element_text(angle = 30, vjust = 0.75)))+ rumi_bridge_rates_density_ridges)
# dev.off()

# Section 6: Density plot of transition rates (SYM, ARD) ----------------------------------

ridges_palette_12 <- c(alpha("#B1A884", 0.7), alpha("#B1A884", 0.4), alpha("#AAC7A9", 0.7), alpha("#AAC7A9", 0.4), alpha("#A2A6C6", 0.7), alpha("#A2A6C6", 0.4), alpha("#F5AC88", 0.7), alpha("#F5AC88", 0.4), alpha("#ED8CA5", 0.7), alpha("#ED8CA5", 0.4), alpha("#E6ABCA", 0.7), alpha("#E6ABCA", 0.4))

whippo_SYM_rates_density_ridges <-
  rates_df1 %>% filter(model == "SYM") %>%
  mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = factor(solution, levels = c("Nocturnal %->% Diurnal", "Diurnal %->% Nocturnal", "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular",  "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral", "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral",  "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal", "Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal"))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, alpha = 0.5, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  scale_fill_manual(values = ridges_palette_12) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 1.6))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =9), axis.title.x = element_text(size = 11), axis.text.y = element_text(size = 11), axis.title.y = element_blank(), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = NA), plot.background = element_rect(fill='transparent', color=NA))

whippo_ARD_rates_density_ridges <-
  rates_df1 %>% filter(model == "ARD") %>%
  mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = factor(solution, levels = c("Nocturnal %->% Diurnal", "Diurnal %->% Nocturnal", "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular",  "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral", "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral",  "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal", "Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal"))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, alpha = 0.5, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  scale_fill_manual(values = ridges_palette_12) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 1.6))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =9), axis.title.x = element_text(size = 11), axis.title.y = element_blank(), axis.text.y = element_blank(), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = NA), plot.background = element_rect(fill='transparent', color=NA))

rumi_ARD_rates_density_ridges <-
  rates_df2 %>% filter(model == "ARD") %>% 
  mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = factor(solution, levels = c("Nocturnal %->% Diurnal", "Diurnal %->% Nocturnal", "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular",  "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral", "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral",  "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal", "Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal"))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, alpha = 0.5, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  scale_fill_manual(values = ridges_palette_12) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 2.2))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_blank(), axis.title.y = element_blank(), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = NA), plot.background = element_rect(fill='transparent', color=NA))

rumi_SYM_rates_density_ridges <-
  rates_df2 %>% filter(model == "SYM") %>% 
  mutate(solution = str_replace(solution, pattern = "->", replacement = "%->%")) %>%
  mutate(solution = factor(solution, levels = c("Nocturnal %->% Diurnal", "Diurnal %->% Nocturnal", "Crepuscular %->% Cathemeral", "Cathemeral %->% Crepuscular",  "Cathemeral %->% Diurnal", "Diurnal %->% Cathemeral", "Cathemeral %->% Nocturnal", "Nocturnal %->% Cathemeral",  "Diurnal %->% Crepuscular", "Crepuscular %->% Diurnal", "Nocturnal %->% Crepuscular", "Crepuscular %->% Nocturnal"))) %>%
  ggplot(., aes(x = log(rates), y = solution, fill = solution)) + 
  ggridges::geom_density_ridges(bandwidth = 1, scale = 2, show.legend = FALSE, alpha = 0.5, jittered_points = FALSE, point_shape = 21, point_size = 1, point_alpha = 0.2, inherit.aes = TRUE) +
  scale_fill_manual(values = ridges_palette_12) + 
  scale_y_discrete(labels = function(l) parse(text=l), expand = expansion(add = c(0.5, 2.2))) + xlab("Log (transition rates)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size = 11), axis.title.y = element_blank(), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = NA), plot.background = element_rect(fill='transparent', color=NA))


pdf(here("Figure_folder/supplemental_rates_density_ridges_ARD_SYM.pdf"), width = 8.5, height = 7)
(whippo_SYM_rates_density_ridges + whippo_ARD_rates_density_ridges) /
  (rumi_SYM_rates_density_ridges + rumi_ARD_rates_density_ridges) + plot_annotation(tag_levels = 'a')
dev.off() 

