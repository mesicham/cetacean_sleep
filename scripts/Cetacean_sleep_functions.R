#first custom function SubsetTrees takes the 1k trees and the subset of mammals (cetaceans, artio)
#returns the 1k subsetted trees

subsetTrees <- function(tree = mammal_trees[[1]], subset_names = cetaceans_full$tips) {
  # will take a tree and keep only those tips that match subset_names
  
  subset_names <- subset_names[subset_names %in% tree$tip.label]
  
  out_tree <- keep.tip(tree, subset_names)
  
  return(out_tree)
  
}

#second custom function returnModels that takes the subsetted tree, the input data (diel patterns +species names) and the model type you want to run (ER, SYM, ARD)
#returns the model results for each of the trees provided. 

returnAceModels <- function(tree = cetacean_trees[[1]], trait.data = trait_data, column = "Diel_Pattern_1", model = "SYM") {
  trait.data <- trait.data[trait.data$tips %in% tree$tip.label,]
  trait.data <- trait.data[!(is.na(trait.data[,column])),]
  tree <- keep.tip(tree, trait.data$tips)
  ace_model <- ace(trait.data[,column], tree, model = model, type = "discrete")
  return(ace_model)
  
}

#third custom function returnAceModels that takes the subsetted tree, the input data (diel patterns +species names) and the model type you want to run (ER, SYM, ARD)
#same as before but for the cor model
#added a conditional so it can take custom rate matrices

returnCorModels <- function(tree = cetacean_trees[[2]], trait.data = cetaceans_full, diel_col = "Diel_Pattern_1", rate.cat = 1, custom.rate.mat = "none", model = "SYM", node.states = "marginal"){
  trait.data <- trait.data[trait.data$tips %in% tree$tip.label,]
  row.names(trait.data) <- trait.data$tips
  trait.data <- trait.data[tree$tip.label, c("tips", diel_col)]
  trait.data <- trait.data[!(is.na(trait.data[, diel_col])),]
  tree <- keep.tip(tree, trait.data$tips)
  
  if(!("none" %in% custom.rate.mat)) {
    cor_model <- corHMM(phy = tree, data = trait.data, rate.cat = rate.cat, rate.mat = custom.rate.mat, model = model, node.states = node.states)
  } else {
    cor_model <- corHMM(phy = tree, data = trait.data, rate.cat = rate.cat, model = model, node.states = node.states)
  }
  
  return(cor_model)
}

#testing to see if this works with and without custom rate matrices
# returnCorModels(phylo_trees[[2]], trait.data, "Diel_Pattern_1", rate.cat = 1, custom.rate.mat = matrix(c(0,0,2,0), nrow = 2, ncol = 2, dimnames = list(c("(1)", "(2)"), c("(1)", "(2)"))), model = "SYM", node.states = "marginal")
# returnCorModels(phylo_trees[[2]], trait.data, "Diel_Pattern_1", rate.cat = 1, custom.rate.mat = "none", model = "SYM", node.states = "marginal")
# returnCorModels(phylo_trees[[2]], trait.data, "Diel_Pattern_1", rate.cat = 1, model = "SYM", node.states = "marginal")

#fourth custom function returnLikelihoods takes model results from returnModels and returns just the likelihoods of those models 

returnLikelihoods <- function(model = cetacean_sim_ace[[1]], return = "loglik"){
  
  return(model[return])
}

#fifth custom  function
#function to extract the transition rates from all models, similar to likelihoods

returnRates <- function(model = cetacean_sim_ace[[1]], return = "solution"){
  
  return(model[return])
}

#sixth function
corhmm_model <- readRDS("~/R_projects/fish_sleep/cetaceans_max_clade_cred_six_state_traits_ER_SYM_ARD_models.rds")
corhmm_model <- corhmm_model$ARD_model
returnAICc <- function(model = corhmm_model, return = "AICc"){
  
  return(model[return])
}

#seventh function
corhmm_model <- readRDS("~/R_projects/fish_sleep/cetaceans_max_clade_cred_six_state_traits_ER_SYM_ARD_models.rds")
corhmm_model <- corhmm_model$ARD_model
returnAIC <- function(model = corhmm_model, return = "AIC"){
  
  return(model[return])
}

#eight function
#create a function that returns the node label for the mrca of a set of species/taxonomic group

#requires a dataframe with columns for species names and their taxonomic level names
#returns a dataframe with the clade name and the mrca node number

findMRCANode <- function(phylo = tr, trait.data = trait.data, taxonomic_level_col = 3){
  nodes_list <- list()
  for(x in 1:nrow(unique(trait.data[,taxonomic_level_col]))){
    i <- unique(trait.data[x,taxonomic_level_col])
    #ensure the species are in the tree you're working with
    trait.data <- trait.data[trait.data$tips %in% phylo$tip.label,]
    #remove any taxonomic levels with only one species (cannot find MRCA for one species)
    trait.data <- trait.data %>% group_by_at(taxonomic_level_col) %>% filter(n()>1)
    #subset the trait data into species belonging to the same taxonomic group
    trait.data.filtered <- trait.data[trait.data[,taxonomic_level_col] == i, ]
    #take the vector of these species names
    tip_vector <- as.vector(trait.data.filtered[, "tips"])
    #find the node number of the MRCA of these species
    MRCA_node <- findMRCA(phylo, tip_vector$tips)
    #create a dataframe
    loop_df <- data.frame(clade_name = i, node_number = MRCA_node)
    #add to list, to extract later (out of for loop)
    nodes_list[[i]] <- loop_df}
  
  nodes_df <- do.call(rbind, nodes_list)
  return(nodes_df)
}
  
 
findMRCANode2 <- function(phylo = tr, trait.data = trait.data, taxonomic_level_col = 3, taxonomic_level_name = "Araneae"){
  nodes_list <- list()
  trait.data <- trait.data %>% group_by_at(taxonomic_level_col) %>% filter(n()>1)
  trait.data.filtered <- trait.data[trait.data[,taxonomic_level_col] == taxonomic_level_name, ]
  tip_vector <- as.vector(trait.data.filtered[, "tips"])
  #find the node number of the MRCA of these species
  MRCA_node <- findMRCA(phylo, tip_vector$tips)
  node_df <- data.frame(clade_name = taxonomic_level_name, node_number = MRCA_node)
  return(node_df)
}

#delta statistic function
#a function that given a df with species names and diel activity data returns the delta statistic for that group

calculateDelta <- function(trait.data = trait.data, taxonomic_group_name = "Cetacea"){
  mam.tree <- readRDS(here("maxCladeCred_mammal_tree.rds"))
  trait.data <- filter(trait.data, Order == taxonomic_group_name)
  trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
  mam.tree <- keep.tip(mam.tree, tip = trait.data$tips)
  mam.tree$edge.length[mam.tree$edge.length == 0] <- quantile(tree$edge.length, 0.1)*0.1
  
  sps_order <- as.data.frame(mam.tree$tip.label)
  colnames(sps_order) <- "tips"
  sps_order$id <- 1:nrow(sps_order)
  trait.data <- merge(trait.data, sps_order, by = "tips")
  trait.data <- trait.data[order(trait.data$id), ]
  trait <- trait.data$Diel_Pattern
  
  source("scripts/Amelia_delta_code.R")
  delta_diel <- delta(trait, mam.tree, 0.1, 0.0589, 1000, 10, 100)
  
  random_delta <- rep(NA,100)
  for (i in 1:100){
    rtrait <- sample(trait)
    random_delta[i] <- delta(rtrait,mam.tree,0.1,0.0589,10000,10,100)
  }
  p_value <- sum(random_delta>delta_diel)/length(random_delta)
  
  result <- data.frame(delta_diel, taxonomic_group_name, p_value)
  return(result)
}

# Function to create trait vector for phylANOVA -------------------------

calculatePhylANOVA <- function(trait.data = trait.data, continuous_trait = "Orbit_ratio"){
  trait.data <- trait.data[trait.data$tips %in% mam.tree$tip.label,]
  trpy_n <- keep.tip(mam.tree, tip = trait.data$tips)
  sps_order <- as.data.frame(trpy_n$tip.label)
  colnames(sps_order) <- "tips"
  sps_order$id <- 1:nrow(sps_order)
  test <- merge(trait.data, sps_order, by = "tips")
  trait.vector <- test[, c(continuous_trait)]
  names(trait.vector) <- test$tips
  #y trait is the continuous (response) variable 
  trait.y <- test[, c(continuous_trait)]
  names(trait.y) <- test$tips
  #x trait is the categorical variable 
  trait.x <- test$max_crep
  names(trait.x) <- test$tips
  phylANOVA <- phytools::phylANOVA(trpy_n, trait.x, trait.y, nsim=1000, posthoc=TRUE, p.adj="holm")
  return(phylANOVA)
}


# Delta code --------------------------------------------------------------

library("ape")
library("expm")

#NENTROPY
#returns the node entropies by calculating sum of the state entropies
#prob: matrix of state probabilities

nentropy <- function(prob) {
  
  k              <- ncol(prob)                       #number of states
  prob[prob>1/k] <- prob[prob>1/k]/(1-k) - 1/(1-k)   #state entropies
  tent           <- apply(prob,1,sum)                #node entropy
  
  #correct absolute 0/1
  tent[tent == 0] <- tent[tent == 0] + runif(1,0,1)/10000
  tent[tent == 1] <- tent[tent == 1] - runif(1,0,1)/10000
  
  return(tent)
}

#FUNCTION FOR BAYESIAN INFERENCES
#bayesian inferences on the node entropies 
#l0: rate parameter of the exponential prior distribution
#se: standard deviation of the proposal distribution 
#a:  alpha parameter (beta likelihood)
#b:  beta paramter (beta likelihood)
#x:  node entropies

lpalpha <- function(a,b,x,l0) {          #log posterior alpha
  N  <- length(x)
  lp <- N*(lgamma(a+b)-lgamma(a)) - a*(l0-sum(log(x)))
  return(lp)
}

lpbeta  <- function(a,b,x,l0) {          #log posterior beta
  N  <- length(x)
  lp <- N*(lgamma(a+b)-lgamma(b)) - b*(l0-sum(log(1-x)))
  return(lp)
}

mhalpha <- function(a,b,x,l0,se) {       #metropolis hastings alpha
  a0 <- a
  a1 <- exp(rnorm(1,log(a0),se))
  
  r  <- min(1, exp(lpalpha(a1,b,x,l0) - lpalpha(a0,b,x,l0) ) )
  
  while (is.na(r) == T) {
    a1 <- exp(rnorm(1,log(a0),se))
    r  <- min(1, exp(lpalpha(a1,b,x,l0) - lpalpha(a0,b,x,l0) ) )
  }
  
  if (runif(1) < r) {
    return(a1) 
  } else {
    return(a0)
  }
}

mhbeta  <- function(a,b,x,l0,se) {      #metropolis hastings beta
  b0 <- b
  b1 <- exp(rnorm(1,log(b0),se))
  
  r  <- min(1, exp(lpbeta(a,b1,x,l0) - lpbeta(a,b0,x,l0) ) )
  
  while (is.na(r) == T) {
    b1 <- exp(rnorm(1,log(b0),se))
    r  <- min(1, exp(lpbeta(a,b1,x,l0) - lpbeta(a,b0,x,l0) ) )
  }  
  
  if (runif(1) < r) {
    return(b1)
  } else {
    return(b0)
  }
}

#MCMC
#Markov chain monte carlo scheme using the conditional posteriors of alpha and beta
#alpha: initial value of alpha
#beta: initial values of beta
#x: node entropies
#sim: number of iterations
#thin: controles the number of saved iterations = sim/thin
#burn: number of iterates to burn

emcmc <- function(alpha,beta,x,l0,se,sim,thin,burn) {
  
  usim <- seq(burn,sim,thin)
  gibbs <- matrix(NA,ncol=2,nrow=length(usim))
  p <- 1
  
  for (i in 1:sim) {
    alpha <- mhalpha(alpha,beta,x,l0,se)
    beta  <- mhbeta(alpha,beta,x,l0,se)
    
    if (i == usim[p]) {
      gibbs[p,] <- c(alpha,beta)
      p <- p+1
    }
  }  
  return(gibbs)
}

#RATE MATRIX FOR TRAIT EVOLUTION. K=2 TO 5
ratematrix <- function(pi,rho){
  
  k <- length(pi)
  
  if (k==2){
    r <- c(pi[1]*0     ,pi[2]*rho[1],
           pi[1]*rho[1],pi[2]*0)
  }
  
  if (k==3){
    r <- c(pi[1]*0     ,pi[2]*rho[1],pi[3]*rho[2],
           pi[1]*rho[1],pi[2]*0     ,pi[3]*rho[3],
           pi[1]*rho[2],pi[2]*rho[3],pi[3]*0 )
  }
  
  if (k==4){
    r <- c(pi[1]*0     ,pi[2]*rho[1],pi[3]*rho[2],pi[4]*rho[3],
           pi[1]*rho[1],pi[2]*0     ,pi[3]*rho[4],pi[4]*rho[5],
           pi[1]*rho[2],pi[2]*rho[4],pi[3]*0     ,pi[4]*rho[6],
           pi[1]*rho[3],pi[2]*rho[5],pi[3]*rho[6],pi[4]*0 )
  }  
  
  if (k==5){
    r <- c(pi[1]*0     ,pi[2]*rho[1],pi[3]*rho[2],pi[4]*rho[3] ,pi[5]*rho[4],
           pi[1]*rho[1],pi[2]*0     ,pi[3]*rho[5],pi[4]*rho[6] ,pi[5]*rho[7],
           pi[1]*rho[2],pi[2]*rho[5],pi[3]*0     ,pi[4]*rho[8] ,pi[5]*rho[9],
           pi[1]*rho[3],pi[2]*rho[6],pi[3]*rho[8],pi[4]*0      ,pi[5]*rho[10],
           pi[1]*rho[4],pi[2]*rho[7],pi[3]*rho[9],pi[4]*rho[10],pi[5]*0)
  }
  
  R <- matrix(r,ncol=k,nrow=k) 
  diag(R) <- -rowSums(R)
  
  return(R)
}

#RTRAIT
#simulates the evolution of a trait in a given tree
# tree: metric-tree
# R: rate matrix
# nstates: number of states

rtrait <- function(tree,R,nstates) {
  
  nspecis <- length(tree$tip.label)
  
  #tree
  edge <- cbind(tree$edge,tree$edge.length)
  
  ancestral <- rep(NA,2*nspecies-1) 
  ancestral[nspecies+1] <- sample(1:nstates,1,prob=pi) 
  
  #rate change
  inode <- nspecies+1
  while (sum(is.na(ancestral)) > 0) {
    
    inode1 <-  edge[which(edge[,1]==inode)[1],2]
    inode2 <-  edge[which(edge[,1]==inode)[2],2]
    bl1 <- edge[which(edge[,1]==inode)[1],3]
    bl2 <- edge[which(edge[,1]==inode)[2],3]
    
    astate <- rep(0,nstates)
    astate[ancestral[inode]] <- 1 
    
    ancestral[inode1] <- sample(1:nstates,1,prob=astate%*%expm(R*bl1))
    ancestral[inode2] <- sample(1:nstates,1,prob=astate%*%expm(R*bl2))
    
    inode <- inode+1
  }
  return(ancestral[1:nspecies])
  
}

#DELTA
#calculate delta statistic
#trait: trait vector 
delta <- function(trait, tree,lambda0,se,sim,thin,burn) {
  
  #returns the likelihood of each trait state at each internal node
  ar <- ace(trait,tree,type="discret",method="ML",model="ARD", marginal = T)$lik.anc
  
  # deletes the complex part whenever it pops up
  if (class(ar[1,1]) == "complex"){
    ar <- Re(ar)
  }
  
  x  <- nentropy(ar) #calculates the entropy of each internal node
  mc1    <- emcmc(rexp(1),rexp(1),x,lambda0,se,sim,thin,burn)
  mc2    <- emcmc(rexp(1),rexp(1),x,lambda0,se,sim,thin,burn)
  mchain <- rbind(mc1,mc2)
  deltaA <- mean(mchain[,2]/mchain[,1])
  
  return(deltaA)
}



# # Example code ------------------------------------------------------------
# 
# #SOME PARAMETERS... 
# lambda0 <- 0.1   #rate parameter of the proposal 
# se      <- 0.5   #standard deviation of the proposal
# sim     <- 10000 #number of iterations
# thin    <- 10    #we kept only each 10th iterate 
# burn    <- 100   #100 iterates are burned-in
# 
# #RANDOM TREE: 
# #same for both examples, only the trait vector varies.
# set.seed(25)
# ns   <- 20        #20 species
# tree <- rtree(ns)
# 
# ##########
# # CASE A # : with phylogenetic signal
# ##########
# trait <- c(2,1,3,1,1,3,1,3,2,1,1,2,2,2,2,1,1,3,1,1)
# 
# #CALCULATE DELTA A
# deltaA <- delta(trait,tree,lambda0,se,sim,thin,burn)
# print(deltaA)
# 
# #DRAW THE TREE...
# par(mfrow=c(1,2))
# tree$tip.label <- rep("",ns)
# plot(tree,main="SCENARIO A")
# ar <- ace(trait,tree,type="discret",method="ML",model="ARD")$lik.anc
# nodelabels(pie = ar, cex = 1,frame="n") 
# mtrait <- matrix(0,ncol=3,nrow=ns)
# for ( i in 1:ns) {
#   mtrait[i,trait[i]] <- 1
# }
# tiplabels(pie=mtrait,cex=0.5)
# 
# 
# 
# ##########
# # CASE B # : no phylogenetic signal
# ##########
# trait <- c(2,3,1,3,3,3,3,2,2,3,1,2,1,2,3,1,2,3,1,2) 
# 
# #CALCULATE DELTA B
# deltaB <-  delta(trait,tree,lambda0,se,sim,thin,burn)
# print(deltaB)
# 
# #DRAW THE TREE...
# ar <- ace(trait,tree,type="discret",method="ML",model="ARD")$lik.anc
# plot(tree,main="SCENARIO B")
# nodelabels(pie = Re(ar), cex = 1) 
# mtrait <- matrix(0,ncol=3,nrow=ns)
# for ( i in 1:ns) {  mtrait[i,trait[i]] <- 1 }
# tiplabels(pie=mtrait,cex=0.5)
# 
# 
# # in scenario A the nodes are informative (ie the reconstruction is more definite)
# # therefore there is low entropy and high phylogenetic signal. DeltaA = 1.3
# # in scenario B the nodes are uninformative and therefore the entropy is high and signal low
# # DeltaB = 1.00
# 
