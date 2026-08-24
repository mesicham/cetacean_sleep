setwd(here())
source("scripts/fish_sleep_functions.R")
source("scripts/Cetacean_sleep_functions.R")

#load in mammal tree and cetacean dataframe
mammal_trees <- read.nexus(here("Cox_mammal_data/Complete_phylogeny.nex"))
mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))


# # Function 1: max_clade_cred likelihood metrics --------------------------

max_clade_metrics <- function(model_results = readRDS(here(paste0(filename, ".rds")))) {
  # will take take the max clade cred tree result and plot the likelihood, AIC, and AICc score
  log_likelihoods <- unlist(lapply(model_results, function(x) returnLikelihoods(model = x)))
  likelihoods <- as.data.frame(log_likelihoods)
  likelihoods$modelname <- rownames(likelihoods)
  likelihoods <- separate(likelihoods, col = "modelname", into = c("model", "to_drop"), sep = "[.]", remove = TRUE)
  AICc_scores <- unlist(lapply(model_results, function(x) returnAICc(model = x)))
  likelihoods$AICc_scores <-AICc_scores 
  AIC_scores <- unlist(lapply(model_results, function(x) returnAIC(model = x)))
  likelihoods$AIC_scores <-AIC_scores 
  likelihoods <- likelihoods[,-3]
  likelihoods <- likelihoods %>% pivot_longer(!model, names_to = "model_metric", values_to = "model_value")
  
  return(likelihoods)
}

# # Function 2: Likelihoods from 1k model results (ER, SYM, ARD, bridge_only) -------------------------

plot1kLikelihoods <- function(model_results = readRDS(here(paste0(filename_whippo_1k, ".rds")), number_of_models = 4)) {
  # extract likelihoods
  if(number_of_models == 3){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnLikelihoods(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnLikelihoods(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnLikelihoods(model = x)))
    
    df1 <- data.frame(model = "ER", likelihoods = ER_likelihoods)
    df2 <- data.frame(model = "SYM", likelihoods = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", likelihoods = ARD_likelihoods)
    df_full <- rbind(df1, df2, df3)
  }
  
  if(number_of_models == 4){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnLikelihoods(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnLikelihoods(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnLikelihoods(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnLikelihoods(model = x)))
    
    df1 <- data.frame(model = "ER", likelihoods = ER_likelihoods)
    df2 <- data.frame(model = "SYM", likelihoods = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", likelihoods = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", likelihoods = bridge_only_likelihoods)
    df_full <- rbind(df1, df2, df3, df4)
  }
  
  if(number_of_models == 5){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnLikelihoods(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnLikelihoods(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnLikelihoods(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnLikelihoods(model = x)))
    CONSYM_likelihoods  <- unlist(lapply(model_results$CONSYM_model, function(x) returnLikelihoods(model = x)))
    
    df1 <- data.frame(model = "ER", likelihoods = ER_likelihoods)
    df2 <- data.frame(model = "SYM", likelihoods = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", likelihoods = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", likelihoods = bridge_only_likelihoods)
    df5 <- data.frame(model = "CONSYM", likelihoods = CONSYM_likelihoods)
    df_full <- rbind(df1, df2, df3, df4, df5)
  }
  
  return(df_full)
}

# # Function 3: AIC scores from 1k model results -------------------------
plot1kAIC <- function(model_results = readRDS(here(filename_whippo_1k)), number_of_models = 4) {
  # extract likelihoods
  if(number_of_models == 3){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAIC(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAIC(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAIC(model = x)))
    
    df1 <- data.frame(model = "ER", AIC_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AIC_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AIC_score = ARD_likelihoods)
    df_full <- rbind(df1, df2, df3)
  }
  
  if(number_of_models == 4){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAIC(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAIC(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAIC(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnAIC(model = x)))
    
    df1 <- data.frame(model = "ER", AIC_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AIC_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AIC_score = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", AIC_score = bridge_only_likelihoods)
    df_full <- rbind(df1, df2, df3, df4)
  }
  
  if(number_of_models == 5){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAIC(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAIC(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAIC(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnAIC(model = x)))
    CONSYM_likelihoods  <- unlist(lapply(model_results$CONSYM_model, function(x) returnAIC(model = x)))
    
    df1 <- data.frame(model = "ER", AIC_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AIC_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AIC_score = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", AIC_score = bridge_only_likelihoods)
    df5 <- data.frame(model = "CONSYM", AIC_score = CONSYM_likelihoods)
    df_full <- rbind(df1, df2, df3, df4, df5)
  }
  
  return(df_full)
}

# # Function 4: AICc scores from 1k model results -------------------------
plot1kAICc <- function(model_results = readRDS(here(filename_whippo_1k)), number_of_models = 4) {
  # extract likelihoods
  if(number_of_models == 3){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAICc(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAICc(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAICc(model = x)))
    
    df1 <- data.frame(model = "ER", AICc_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AICc_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AICc_score = ARD_likelihoods)
    df_full <- rbind(df1, df2, df3)
  }
  
  if(number_of_models == 4){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAICc(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAICc(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAICc(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnAICc(model = x)))
    
    df1 <- data.frame(model = "ER", AICc_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AICc_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AICc_score = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", AICc_score = bridge_only_likelihoods)
    df_full <- rbind(df1, df2, df3, df4)
  }
  
  if(number_of_models == 5){
    ER_likelihoods <- unlist(lapply(model_results$ER_model, function(x) returnAICc(model = x)))
    SYM_likelihoods  <- unlist(lapply(model_results$SYM_model, function(x) returnAICc(model = x)))
    ARD_likelihoods  <- unlist(lapply(model_results$ARD_model, function(x) returnAICc(model = x)))
    bridge_only_likelihoods  <- unlist(lapply(model_results$bridge_only_model, function(x) returnAICc(model = x)))
    CONSYM_likelihoods  <- unlist(lapply(model_results$CONSYM_model, function(x) returnAICc(model = x)))
    
    df1 <- data.frame(model = "ER", AICc_score = ER_likelihoods)
    df2 <- data.frame(model = "SYM", AICc_score = SYM_likelihoods)
    df3 <- data.frame(model = "ARD", AICc_score = ARD_likelihoods)
    df4 <- data.frame(model = "bridge_only", AICc_score = bridge_only_likelihoods)
    df5 <- data.frame(model = "CONSYM", AICc_score = CONSYM_likelihoods)
    df_full <- rbind(df1, df2, df3, df4, df5)
  }
  
  return(df_full)
}
  
# # Function 5: Transition rates from 1k model results -------------------------
plot1kTransitionRates <- function(model_results = readRDS(here(filename_whippo_1k)), states_in_model = 6, number_of_models = 3){

  if(number_of_models == 3){
    models_in_file = c("ER","SYM","ARD")
  }

  if(number_of_models == 4){
    models_in_file = c("ER","SYM","ARD","bridge_only")
  }
  
  if(number_of_models == 5){
    models_in_file = c("ER","SYM","ARD","bridge_only", "CONSYM")
  }

  if("ER" %in% models_in_file){
    rates <- unlist(lapply(model_results$ER_model, function(x) returnRates(model = x)))
    ER_rates_df <- as.data.frame(rates)
    ER_rates_df$model <- "ER"
    ER_rates_df <- ER_rates_df[!(is.na(ER_rates_df$rates)),]

    if(states_in_model == 3){
      ER_rates_df$solution <- c("Di -> Cath/crep", "Noc -> Cath/crep", "Cath/crep -> Di", "Noc -> Di", "Cath/crep -> Noc", "Di -> Noc")
      ER_rates_df$colours <- c("deeppink4", "dodgerblue4", "deeppink3", "seagreen4", "dodgerblue2", "seagreen3")
    }

    if(states_in_model == 4){
      ER_rates_df$solution <- c("Crep -> Cath", "Di -> Cath", "Noc -> Cath", "Cath -> Crep", "Di -> Crep", "Noc -> Crep",  "Cath -> Di", "Crep -> Di", "Noc -> Di", "Cath -> Noc", "Crep -> Noc", "Di -> Noc")
      ER_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
    }
    
    if(states_in_model == 5){
      ER_rates_df$solution <- c("Cath -> Di", "Cath -> Di/crep", "Cath -> Noc", "Cath -> Noc/crep",  "Di -> Cath", "Di -> Di/crep", "Di -> Noc", "Di -> Noc/crep", "Di/crep -> Cath", "Di/crep -> Di", "Di/crep -> Noc", "Di/crep -> Noc/crep", "Noc -> Cath", "Noc -> Di", "Noc -> Di/crep", "Noc -> Noc/crep", "Noc/crep -> Cath", "Noc/crep -> Di", "Noc/crep -> Di/crep", "Noc/crep -> Noc" )
      ER_rates_df$colours <- c("#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3", "#a63d13", "#bd5c35", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#e9bb65","#facf80","#176d56","#629884","#82ae9d","#a2c4b6", "#5c8816", "#92b264", "#adc887", "#c9deab")
    }

    if(states_in_model == 6){
      ER_rates_df$solution <- c("Cath/crep -> Cath", "Di -> Cath", "Di/crep -> Cath", "Noc -> Cath", "Noc/crep -> Cath", "Cath -> Cath/crep", "Di -> Cath/crep", "Di/crep -> Cath/crep", "Noc -> Cath/crep", "Noc/crep -> Cath/crep", "Cath -> Di", "Cath/crep -> Di", "Di/crep -> Di", "Noc -> Di", "Noc/crep -> Di", "Cath -> Di/crep", "Cath/crep -> Di/crep", "Di -> Di/crep", "Noc -> Di/crep", "Noc/crep -> Di/crep", "Cath -> Noc", "Cath/crep -> Noc", "Di -> Noc", "Di/crep -> Noc", "Noc/crep -> Noc", "Cath -> Noc/crep", "Cath/crep -> Noc/crep", "Di -> Noc/crep", "Di/crep -> Noc/crep", "Noc -> Noc/crep")
      ER_rates_df$colours <- c("#ac00b6", "#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3","#2a2956", "#383e6f", "#47558a", "#556ca4", "#6385bf",  "#a63d13", "#bd5c35", "#d37a57", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#d7a84a", "#e9bb65","#facf80","#176d56", "#40826d","#629884","#82ae9d","#a2c4b6", "#5c8816", "#779d40", "#92b264", "#adc887", "#c9deab")
    }
  }

  if("SYM" %in% models_in_file){
    rates <- unlist(lapply(model_results$SYM_model, function(x) returnRates(model = x)))
    SYM_rates_df <- as.data.frame(rates)
    SYM_rates_df$model <- "SYM"
    SYM_rates_df <- SYM_rates_df[!(is.na(SYM_rates_df$rates)),]

    if(states_in_model == 3){
      SYM_rates_df$solution <- c("Di -> Cath/crep", "Noc -> Cath/crep", "Cath/crep -> Di", "Noc -> Di", "Cath/crep -> Noc", "Di -> Noc")
      SYM_rates_df$colours <- c("deeppink4", "dodgerblue4", "deeppink3", "seagreen4", "dodgerblue2", "seagreen3")
    }

    if(states_in_model == 4){
      SYM_rates_df$solution <- c("Crep -> Cath", "Di -> Cath", "Noc -> Cath", "Cath -> Crep", "Di -> Crep", "Noc -> Crep",  "Cath -> Di", "Crep -> Di", "Noc -> Di", "Cath -> Noc", "Crep -> Noc", "Di -> Noc")
      SYM_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
    }

    if(states_in_model == 5){
      SYM_rates_df$solution <- c("Cath -> Di", "Cath -> Di/crep", "Cath -> Noc", "Cath -> Noc/crep",  "Di -> Cath", "Di -> Di/crep", "Di -> Noc", "Di -> Noc/crep", "Di/crep -> Cath", "Di/crep -> Di", "Di/crep -> Noc", "Di/crep -> Noc/crep", "Noc -> Cath", "Noc -> Di", "Noc -> Di/crep", "Noc -> Noc/crep", "Noc/crep -> Cath", "Noc/crep -> Di", "Noc/crep -> Di/crep", "Noc/crep -> Noc" )
      SYM_rates_df$colours <- c("#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3", "#a63d13", "#bd5c35", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#e9bb65","#facf80","#176d56","#629884","#82ae9d","#a2c4b6", "#5c8816", "#92b264", "#adc887", "#c9deab")
    }
    
    if(states_in_model == 6){
      SYM_rates_df$solution <- c("Cath/crep -> Cath", "Di -> Cath", "Di/crep -> Cath", "Noc -> Cath", "Noc/crep -> Cath", "Cath -> Cath/crep", "Di -> Cath/crep", "Di/crep -> Cath/crep", "Noc -> Cath/crep", "Noc/crep -> Cath/crep", "Cath -> Di", "Cath/crep -> Di", "Di/crep -> Di", "Noc -> Di", "Noc/crep -> Di", "Cath -> Di/crep", "Cath/crep -> Di/crep", "Di -> Di/crep", "Noc -> Di/crep", "Noc/crep -> Di/crep", "Cath -> Noc", "Cath/crep -> Noc", "Di -> Noc", "Di/crep -> Noc", "Noc/crep -> Noc", "Cath -> Noc/crep", "Cath/crep -> Noc/crep", "Di -> Noc/crep", "Di/crep -> Noc/crep", "Noc -> Noc/crep")
      SYM_rates_df$colours <- c("#ac00b6", "#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3","#2a2956", "#383e6f", "#47558a", "#556ca4", "#6385bf",  "#a63d13", "#bd5c35", "#d37a57", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#d7a84a", "#e9bb65","#facf80","#176d56", "#40826d","#629884","#82ae9d","#a2c4b6", "#5c8816", "#779d40", "#92b264", "#adc887", "#c9deab")
    }
  }

  if("ARD" %in% models_in_file){
    rates <- unlist(lapply(model_results$ARD_model, function(x) returnRates(model = x)))
    ARD_rates_df <- as.data.frame(rates)
    ARD_rates_df$model <- "ARD"
    ARD_rates_df <- ARD_rates_df[!(is.na(ARD_rates_df$rates)),]

    if(states_in_model == 3){
      ARD_rates_df$solution <- c("Di -> Cath/crep", "Noc -> Cath/crep", "Cath/crep -> Di", "Noc -> Di", "Cath/crep -> Noc", "Di -> Noc")
      ARD_rates_df$colours <- c("deeppink4", "dodgerblue4", "deeppink3", "seagreen4", "dodgerblue2", "seagreen3")
    }

    if(states_in_model == 4){
      ARD_rates_df$solution <- c("Crep -> Cath", "Di -> Cath", "Noc -> Cath", "Cath -> Crep", "Di -> Crep", "Noc -> Crep",  "Cath -> Di", "Crep -> Di", "Noc -> Di", "Cath -> Noc", "Crep -> Noc", "Di -> Noc")
      ARD_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
    }
    
    if(states_in_model == 5){
      ARD_rates_df$solution <- c("Cath -> Di", "Cath -> Di/crep", "Cath -> Noc", "Cath -> Noc/crep",  "Di -> Cath", "Di -> Di/crep", "Di -> Noc", "Di -> Noc/crep", "Di/crep -> Cath", "Di/crep -> Di", "Di/crep -> Noc", "Di/crep -> Noc/crep", "Noc -> Cath", "Noc -> Di", "Noc -> Di/crep", "Noc -> Noc/crep", "Noc/crep -> Cath", "Noc/crep -> Di", "Noc/crep -> Di/crep", "Noc/crep -> Noc" )
      ARD_rates_df$colours <- c("#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3", "#a63d13", "#bd5c35", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#e9bb65","#facf80","#176d56","#629884","#82ae9d","#a2c4b6", "#5c8816", "#92b264", "#adc887", "#c9deab")
    }

    if(states_in_model == 6 ){
      ARD_rates_df$solution <- c("Cath/crep -> Cath", "Di -> Cath", "Di/crep -> Cath", "Noc -> Cath", "Noc/crep -> Cath", "Cath -> Cath/crep", "Di -> Cath/crep", "Di/crep -> Cath/crep", "Noc -> Cath/crep", "Noc/crep -> Cath/crep", "Cath -> Di", "Cath/crep -> Di", "Di/crep -> Di", "Noc -> Di", "Noc/crep -> Di", "Cath -> Di/crep", "Cath/crep -> Di/crep", "Di -> Di/crep", "Noc -> Di/crep", "Noc/crep -> Di/crep", "Cath -> Noc", "Cath/crep -> Noc", "Di -> Noc", "Di/crep -> Noc", "Noc/crep -> Noc", "Cath -> Noc/crep", "Cath/crep -> Noc/crep", "Di -> Noc/crep", "Di/crep -> Noc/crep", "Noc -> Noc/crep")
      ARD_rates_df$colours <- c("#ac00b6", "#bb46c2", "#ca6ccd", "#d78fd8", "#e3b0e3","#2a2956", "#383e6f", "#47558a", "#556ca4", "#6385bf",  "#a63d13", "#bd5c35", "#d37a57", "#e79979", "#fbb89d", "#b48204", "#c6952e", "#d7a84a", "#e9bb65","#facf80","#176d56", "#40826d","#629884","#82ae9d","#a2c4b6", "#5c8816", "#779d40", "#92b264", "#adc887", "#c9deab")
    }
  }
  

  if("bridge_only" %in% models_in_file){
    rates <- unlist(lapply(model_results$bridge_only_model, function(x) returnRates(model = x)))
    bridge_only_rates_df <- as.data.frame(rates)
    bridge_only_rates_df$model <- "Bridge_only"
    bridge_only_rates_df <- bridge_only_rates_df[!(is.na(bridge_only_rates_df$rates)),]

    if(states_in_model == 3){
      bridge_only_rates_df$solution <- c("Di -> Cath/crep", "Noc -> Cath/crep", "Cath/crep -> Di", "Cath/crep -> Noc")
      bridge_only_rates_df$colours <- c("deeppink4", "dodgerblue4", "deeppink3", "dodgerblue2")
    }

    if(states_in_model == 4){
      bridge_only_rates_df$solution <- c("Crep -> Cath", "Di -> Cath", "Noc -> Cath", "Cath -> Crep", "Di -> Crep", "Noc -> Crep",  "Cath -> Di", "Crep -> Di", "Cath -> Noc", "Crep -> Noc")
      bridge_only_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62","#3C967E", "#66C2A5")
    }

    # if(number_of_states == 6){
    #   print("six_not_done")
    #   }
  }
  
  if("CONSYM" %in% models_in_file){
    rates <- unlist(lapply(model_results$CONSYM_model, function(x) returnRates(model = x)))
    CONSYM_rates_df <- as.data.frame(rates)
    CONSYM_rates_df$model <- "CONSYM"
    CONSYM_rates_df <- CONSYM_rates_df[!(is.na(CONSYM_rates_df$rates)),]
    
    if(states_in_model == 4){
      CONSYM_rates_df$solution <- c("Crep -> Cath", "Di -> Cath", "Noc -> Cath", "Cath -> Crep", "Di -> Crep", "Noc -> Crep",  "Cath -> Di", "Crep -> Di", "Cath -> Noc", "Crep -> Noc")
      CONSYM_rates_df$colours <-  c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62","#3C967E", "#66C2A5")
    }
  }

  if(number_of_models == 3){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df)
  }

  if(number_of_models == 4){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df, bridge_only_rates_df)
    
  }
  
  if(number_of_models == 5){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df, bridge_only_rates_df, CONSYM_rates_df)
  }

  return(rates_full)
}

# # Function 6: Transition rates from 1k model only for 4 state  --------
plot1kTransitionRates4state <- function(model_results = readRDS(here(filename_whippo_1k)), number_of_models = 5){
  
  if(number_of_models == 3){
    models_in_file = c("ER","SYM","ARD")
  }
  
  if(number_of_models == 4){
    models_in_file = c("ER","SYM","ARD","bridge_only")
  }
  
  if(number_of_models == 5){
    models_in_file = c("ER","SYM","ARD","bridge_only", "CONSYM")
  }
  
  if("ER" %in% models_in_file){
    rates <- unlist(lapply(model_results$ER_model, function(x) returnRates(model = x)))
    ER_rates_df <- as.data.frame(rates)
    ER_rates_df$model <- "ER"
    ER_rates_df <- ER_rates_df[!(is.na(ER_rates_df$rates)),]
    ER_rates_df$solution <- c("Crepuscular -> Cathemeral", "Diurnal -> Cathemeral", "Nocturnal -> Cathemeral", "Cathemeral -> Crepuscular", "Diurnal -> Crepuscular", "Nocturnal -> Crepuscular",  "Cathemeral -> Diurnal", "Crepuscular -> Diurnal", "Nocturnal -> Diurnal", "Cathemeral -> Nocturnal", "Crepuscular -> Nocturnal", "Diurnal -> Nocturnal")
    ER_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#AD9680", "#D1B49B","#EECBAD",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
  }
  
  if("SYM" %in% models_in_file){
    rates <- unlist(lapply(model_results$SYM_model, function(x) returnRates(model = x)))
    SYM_rates_df <- as.data.frame(rates)
    SYM_rates_df$model <- "SYM"
    SYM_rates_df <- SYM_rates_df[!(is.na(SYM_rates_df$rates)),]
    SYM_rates_df$solution <- c("Crepuscular -> Cathemeral", "Diurnal -> Cathemeral", "Nocturnal -> Cathemeral", "Cathemeral -> Crepuscular", "Diurnal -> Crepuscular", "Nocturnal -> Crepuscular",  "Cathemeral -> Diurnal", "Crepuscular -> Diurnal", "Nocturnal -> Diurnal", "Cathemeral -> Nocturnal", "Crepuscular -> Nocturnal", "Diurnal -> Nocturnal")
    SYM_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3","#AD9680", "#D1B49B","#EECBAD", "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
  }
  
  if("ARD" %in% models_in_file){
    rates <- unlist(lapply(model_results$ARD_model, function(x) returnRates(model = x)))
    ARD_rates_df <- as.data.frame(rates)
    ARD_rates_df$model <- "ARD"
    ARD_rates_df <- ARD_rates_df[!(is.na(ARD_rates_df$rates)),]
    ARD_rates_df$solution <- c("Crepuscular -> Cathemeral", "Diurnal -> Cathemeral", "Nocturnal -> Cathemeral", "Cathemeral -> Crepuscular", "Diurnal -> Crepuscular", "Nocturnal -> Crepuscular",  "Cathemeral -> Diurnal", "Crepuscular -> Diurnal", "Nocturnal -> Diurnal", "Cathemeral -> Nocturnal", "Crepuscular -> Nocturnal", "Diurnal -> Nocturnal")
    ARD_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#AD9680", "#D1B49B","#EECBAD",  "#FA4A05", "#FC8D62", "#FECCB9","#3C967E", "#66C2A5","#ABDECE")
  }
  
  if("bridge_only" %in% models_in_file){
    rates <- unlist(lapply(model_results$bridge_only_model, function(x) returnRates(model = x)))
    bridge_only_rates_df <- as.data.frame(rates)
    bridge_only_rates_df$model <- "Bridge_only"
    bridge_only_rates_df <- bridge_only_rates_df[!(is.na(bridge_only_rates_df$rates)),]
    bridge_only_rates_df$solution <- c("Crepuscular -> Cathemeral", "Diurnal -> Cathemeral", "Nocturnal -> Cathemeral", "Cathemeral -> Crepuscular", "Diurnal -> Crepuscular", "Nocturnal -> Crepuscular",  "Cathemeral -> Diurnal", "Crepuscular -> Diurnal", "Cathemeral -> Nocturnal", "Crepuscular -> Nocturnal")
    bridge_only_rates_df$colours <- c( "#A024AE", "#DD8AE7","#EEC4F3", "#AD9680", "#D1B49B","#EECBAD",  "#FA4A05", "#FC8D62","#3C967E", "#66C2A5")
  }
  
  if("CONSYM" %in% models_in_file){
    rates <- unlist(lapply(model_results$CONSYM_model, function(x) returnRates(model = x)))
    CONSYM_rates_df <- as.data.frame(rates)
    CONSYM_rates_df$model <- "CONSYM"
    CONSYM_rates_df <- CONSYM_rates_df[!(is.na(CONSYM_rates_df$rates)),]
    CONSYM_rates_df$solution <- c("Crepuscular -> Cathemeral", "Diurnal -> Cathemeral", "Nocturnal -> Cathemeral", "Cathemeral -> Crepuscular", "Diurnal -> Crepuscular", "Nocturnal -> Crepuscular",  "Cathemeral -> Diurnal", "Crepuscular -> Diurnal", "Cathemeral -> Nocturnal", "Crepuscular -> Nocturnal")
    CONSYM_rates_df$colours <-  c( "#A024AE", "#DD8AE7","#EEC4F3", "#9F7C60", "#BFA895","#D3C3B6",  "#FA4A05", "#FC8D62","#3C967E", "#66C2A5")
    
  if(number_of_models == 3){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df)
  }
  
  if(number_of_models == 4){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df, bridge_only_rates_df)
    
  }
  
  if(number_of_models == 5){
    rates_full <- rbind(ER_rates_df, SYM_rates_df, ARD_rates_df, bridge_only_rates_df, CONSYM_rates_df)
  }
  
  return(rates_full)
  }
  }

# # Function 7: Making split violin splots ----------------------------------

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

    
