
setwd(here())
source("scripts/fish_sleep_functions.R")
source("scripts/Amelia_functions.R")
source("scripts/Amelia_plotting_functions.R")

# Section 1: max_clade_cred likelihood metrics ---------------------------

#set file name
#filename <- "artiodactyla_finalized_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"
#filename <- "whippomorpha_june_2026_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"
filename <- "ruminants_june_2026_max_clade_cred_four_state_max_crep_traits_ER_SYM_CONSYM_ARD_bridge_only_models"

#returns a dataframe of all three metrics for all models
likelihood_metrics <- max_clade_metrics(readRDS(here(paste0(filename, ".rds"))))
likelihood_metrics <- pivot_wider(likelihood_metrics, names_from = model_metric, values_from = model_value)
likelihood_metrics$most_likely <- ""  
likelihood_metrics[which(likelihood_metrics$AIC_scores == min(likelihood_metrics$AIC_scores)), "most_likely"] <- "**"
likelihood_metrics <- likelihood_metrics %>% mutate(delta_AIC = AIC_scores - min(likelihood_metrics$AIC_scores))

knitr::kable(likelihood_metrics, format = "html", digits = 2, caption = filename) %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("likelihood_table.html")
webshot("likelihood_table.html", file = paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/likelihood_table_", filename, ".png"), vwidth = 992, vheight = 300)

likelihood_metrics <- as.data.frame(likelihood_metrics) %>% 
  mutate(model_comparison = paste0(round(delta_AIC, digits = 2), " (", round(AIC_scores, digits = 2), ")")) %>%
  select(model_comparison, model) %>%
  pivot_wider(names_from = model, values_from = model_comparison) 

colnames(likelihood_metrics) <- c("ER", "SYM", "bridge-SYM", "ARD", "bridge-ARD")

knitr::kable(likelihood_metrics, format = "html", digits = 2, caption = "Model_comparison \n AIC score difference (AIC score)") %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("likelihood_table.html")
webshot("likelihood_table.html", file = paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/likelihood_table_long_", filename, ".png"), vwidth = 992, vheight = 300)

# Section 2: max_clade_cred rates ---------------------------

model_results <- readRDS(here(paste0(filename, ".rds")))

png(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/max_clade_cred_rates_", filename, ".png"), width = 30, height = 15, units = "cm", res = 600)

# create a new plotting window and set the plotting area into a 2*2 array
par(mfrow = c(2, 2))
#plotMKmodel(model_results$ER_model)
plotMKmodel(model_results$SYM_model)
plotMKmodel(model_results$ARD_model)
plotMKmodel(model_results$bridge_only)
plotMKmodel(model_results$CONSYM_model)
dev.off()

# Section 3: Load in and format data --------------------------------------
# model_results1 <- readRDS(here("august_12__whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"))
# model_results2 <- readRDS(here("august_14__whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"))
# model_results1$ER_model <- c(model_results1$ER_model, model_results2$ER_model)
# model_results1$SYM_model <- c(model_results1$SYM_model, model_results2$SYM_model)
# model_results1$ARD_model <- c(model_results1$ARD_model, model_results2$ARD_model)
# model_results1$bridge_only_model <- c(model_results1$bridge_only_model, model_results2$bridge_only_model)
# model_results1$CONSYM_model <- c(model_results1$CONSYM_model, model_results2$CONSYM_model)
# 
# saveRDS(model_results1, here("august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"))

# Section 4: Plot AIC scores from 1k model results whippo ----------------------

filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

#requires the filename and the number of Mk models (3: ER, SYM, ARD or 4: ER, SYM, ARD, CONARD, 5: ER, SYM, ARD, bridge_only, CONSYM)
#returns a df of the AIC scores for all 1k trees x number of Mk models
df_full <- plot1kAIC(readRDS(here(filename)), 5)
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

# Section 5: Plot AIC scores from 1k model results ruminants ----------------------
filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_artiodactyla_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

#requires the filename and the number of Mk models (3: ER, SYM, ARD or 4: ER, SYM, ARD, CONARD, 5: ER, SYM, ARD, bridge_only, CONSYM)
#returns a df of the AIC scores for all 1k trees x number of Mk models
df_full <- plot1kAIC(readRDS(here(filename)), 5)
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

# #save out
# pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/model_boxplots.pdf"), width = 3.3, height = 5)
# (whippo_plot + theme(axis.title.x = element_text(colour = "white", size = 1), axis.text.x = element_text(angle = 30, vjust = 0.75)))/
#   (ruminant_plot + theme(axis.text.x = element_text(angle = 30, vjust = 0.75)))
# dev.off()
 
# Section 6: Plot transition rates from 1k model results ----------------------

filename <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
#filename <- "august_artiodactyla_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

#requires the filename, the number of states in the model and the number of Mk models 
#returns a dataframe of the rates from each of the Mk models, for each of the 1k trees
rates_df <- plot1kTransitionRates4state(readRDS(here(filename)), 5)

model_selection <- "SYM"
#colour by unique transition (solution)
rate_colours_sol <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#AD9680", "#D1B49B","#EECBAD",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
#colour by ending state
rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#3C967E","#A024AE","#AD9680", "#FA4A05")

#if model selection is bridge only, adjust colours
model_selection <- "Bridge_only"
#colour by ending state
rate_colours_end = c("#AD9680","#FA4A05","#3C967E","#A024AE", "#FA4A05","#3C967E", "#A024AE","#AD9680", "#A024AE","#AD9680")

#filter by the model you're plotting
rates_df1 <- rates_df %>% filter(model == model_selection) 

#extract just the starting state
rates_df1$start_state <- word(rates_df1$solution, 1)
rates_df1$start_state <- paste(rates_df1$start_state, "to", sep = " ")
rates_df1$end_state <- word(rates_df1$solution, 3)

rates_df1$start_state <- factor(rates_df1$start_state, levels = c("Cathemeral to", "Diurnal to", "Crepuscular to", "Nocturnal to"))

rates_plot <- 
  ggplot(rates_df1, aes(x= end_state, y = log(rates), group = solution, fill = solution, colour = solution)) + 
  geom_quasirandom(alpha = 0.3, width = 0.5, method = "quasirandom") + 
  scale_color_manual(values = rate_colours_end) +
  geom_violin(color = "black", scale = "width", alpha = 0.5) + theme_bw() +
  scale_fill_manual(values = rate_colours_end) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =9), axis.text.y = element_text(size =9), axis.title = element_text(size = 11), strip.background = element_rect(fill = "grey90"), legend.position = "none")  +
  labs(x = "\n Transition", y = "Log(transition rate)") + 
  stat_summary(fun=median, geom="point", size=3, colour = "black", alpha = 0.2) +
  facet_wrap(~start_state, scales = "free_x", nrow = 1, ncol = 4)

#pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", substr(filename, start = 1, stop = 15), "_square_violin_rate_plot_", model_selection, ".pdf"), width = 7, height = 7)
pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", substr(filename, start = 1, stop = 15), "_violin_rate_plot_", model_selection, ".pdf"), width = 8.5, height = 3)
rates_plot 
dev.off()

#calculate median and mean transition rates
test <- rates_df1 %>% group_by(solution) %>% summarize(mean_rate = mean(rates), median_rate = median(rates))
test$max_clade_rates <- max_clade_rates2(model_results$bridge_only_model) %>% pull(rate)
  
#test %>% pivot_longer(!solution, names_to = "rate_type", values_to = "rates") %>% 
  #ggplot(., aes(x = rate_type, y = rates)) + geom_bar(position = "dodge") + theme_minimal()

#scale these the same way...
knitr::kable(test, format = "html", digits = 2, caption = filename) %>%  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% save_kable("transition_rate_table.html")
webshot("transition_rate_table.html", file = paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/transition_rates_table_", filename, ".png"), vwidth = 992, vheight = 300)

#create a column to identify reciprocal transitions
rates_df1$paired_transitions <- paste(rates_df1$start_state, rates_df1$end_state, sep = " ")
rates_df1$paired_transitions <- sapply(strsplit(rates_df1$paired_transitions, " "), function(x) paste(sort(x), collapse=""))
rates_df1$paired_transitions <- factor(rates_df1$paired_transitions, levels = c("Cathemeral to", "Diurnal to", "Crepuscular to", "Nocturnal to"))


#split violin plot
ggplot(rates_df1, aes(x = paired_transitions, y = log(rates), fill = solution)) +
  geom_split_violin(colour = "black", alpha = 0.5, width = 0.5) +
  theme_bw() +
  scale_fill_manual(values = rate_colours_sol) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"), legend.position = "none")  +
  labs(x = "\n Transition", y = "Log(transition rate)") 

#density plot
density_plot <-
  rates_df1 %>%
  ggplot(., aes(y = log(rates), group = solution, fill = end_state)) +
  geom_density(alpha = 0.4) + theme_void() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.position = "none")  +
  scale_fill_manual(values = c( "#A024AE","#AD9680",  "#FA4A05","#3C967E")) 

#kernel density estimates of rates
pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", filename, "rates_density_plots", model_selection, ".pdf"), width = 2, height = 22)
density_plot  +
  facet_wrap(~start_state + end_state, ncol = 1)
dev.off()


#fix colours
rates_df1$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#AD9680", "#D1B49B","#EECBAD",  "#FA4A05", "#FC8D62","#3C967E", "#66C2A5")

#save out each density plot
for(i in 1:(length(unique(rates_df1$solution)))){
  pdf(paste("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", model_selection, i, "rate_plot.pdf", sep = "_"), width = 3, height = 6)
  print(rates_df1 %>% filter(solution == unique(rates_df1$solution)[i]) %>%
          ggplot(., aes(y = log(rates), group = solution, fill = end_state)) +
          geom_density(alpha = 0.4) + theme_void() +
          theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"),  panel.background = element_rect(fill='transparent', colour = "transparent"), plot.background = element_rect(fill='transparent', color=NA), legend.position = "none")  +
          scale_fill_manual(values = rates_df1$colours))
  dev.off()
}

#combined ggplot and violin plot in column format
rates_column_plot <-
  rates_plot +
  facet_wrap(~start_state, scales = "free_x", nrow = 4, ncol = 1)

density_column_plot <- 
  density_plot + 
  facet_wrap(~start_state, nrow = 4, ncol = 1) +
  labs(x = "\n Density", y = "") +
  #for the ARD ruminant plot only either set scales to be free or limit x axis to 0.3, removes the large number of near zeros skewing the plot
  #xlim(0, 0.4) + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size =10), axis.text.y = element_text(size =10), axis.title = element_text(size = 12), strip.background = element_rect(fill = "grey90"), legend.position = "none") 
  
pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/", substr(filename, start = 1, stop = 15), "_combined_rate_plot_column_trimmed_", model_selection, ".pdf"), width = 7, height = 10)
ggarrange(rates_column_plot, density_column_plot)
dev.off()


#rates histogram
ggplot(rates_df1, aes(x = log(rates), group = solution, fill = solution, colour = solution)) +
  geom_histogram() +
  facet_wrap(~solution) + 
  theme_minimal()


# Section 7: density rate plots ----------------------------------------

filename1 <- "august_whippomorpha_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"
filename2 <- "august_ruminants_four_state_max_crep_traits_ER_SYM_ARD_CONSYM_bridge_only_models.rds"

rates_df1 <- plot1kTransitionRates4state(readRDS(here(filename1)), 5)
rates_df2 <- plot1kTransitionRates4state(readRDS(here(filename2)), 5)

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

pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/density_ridges.pdf"), width = 3.25, height = 5.5)
(whippo_bridge_rates_density_ridges + theme(axis.title.x = element_text(colour = "white", size = 1))) /
  (rumi_bridge_rates_density_ridges)
dev.off()

#save out
pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/model_boxplots.pdf"), width = 6.6, height = 5.5)
#pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/model_boxplots.pdf"), width = 6.85, height = 5.5)
((whippo_plot + theme(axis.title.x = element_text(colour = "white", size = 1), axis.text.x = element_text(angle = 30, vjust = 0.75))) + whippo_bridge_rates_density_ridges + theme(axis.title.x = element_text(colour = "white", size = 1)))/
  ((ruminant_plot + theme(axis.text.x = element_text(angle = 30, vjust = 0.75)))+ rumi_bridge_rates_density_ridges)
dev.off()


# Section: Additional density rate plots ----------------------------------

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


pdf(paste0("C:/Users/ameli/OneDrive/Documents/R_projects/Amelia_figures/supplemental_rates_density_ridges.pdf"), width = 8.5, height = 7)
(whippo_SYM_rates_density_ridges + whippo_ARD_rates_density_ridges) /
  (rumi_SYM_rates_density_ridges + rumi_ARD_rates_density_ridges) + plot_annotation(tag_levels = 'a')
dev.off() 

#what proportion of trees finds a non-zero vs a near zero rate?
test <- rates_df2 %>% filter(model %in% c("ARD", "Bridge_only")) %>%
  mutate(rate_magnitude = case_when(log(rates) > -10 ~ "high rate",
         log(rates) < -10 ~ "low rate")) %>% group_by(solution, rate_magnitude, model) %>%
  summarize(totals = n()) 
  
levels <- test %>% filter(rate_magnitude == "high rate") %>% arrange(desc(totals)) %>% pull(solution)
#test$solution <- factor(test$solution, levels)
ggplot(test, aes(x = solution, rate_magnitude, y = totals, fill = rate_magnitude)) + 
  geom_col() + theme_minimal() + scale_fill_manual(values = c("skyblue", "slateblue")) +
  theme(legend.position = "bottom") +
  facet_wrap(~model, ncol = 1)

