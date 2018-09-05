
# load the libraries
library(igraph)
#library(qgraph)
#library(gtools)
library(permute)
library(readxl)
library(foreign)
library(xlsx)
library(ggplot2)
library(dplyr)
library(stargazer)
library(reshape)
library(lme4)
#library(gplots)
#library(RColorBrewer)
#library(GMD)
#library(lattice)
#library(graphics)
#library(latticeExtra)
#library(proto)
#library(SparseM)
#library(reshape2)
#load script with functions
setwd("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts")
source("Functions iterated maps.R")
set.seed(65436)
m=1
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession1.xlsx", col_names=F)
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
CORR <- rep(c(NA),100)
COR<- rep(c(NA),100)
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Initial <- c("cooperate","cooperate","cooperate","cooperate","defect","cooperate","defect","defect",
               "defect","defect", "cooperate", "defect","defect", "cooperate","defect", "cooperate",
               "defect", "cooperate", "cooperate", "defect")
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  #ALPHA<-which(Permutations =="cooperate")
  #BETA<- which(Permutations =="defect")
  ALPHA<-c(1,2,3,4,6,11,14,16,18,19)
  BETA<- c(5,7,8,9,10,12,13,15,17,20)
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE1 <-median(COR)
CORSE1 <- median(CORR)

############session 2###############
CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession2.xlsx", col_names=F)
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Initial <- c("cooperate","defect","cooperate","cooperate","defect","cooperate","cooperate","defect","cooperate","cooperate",
               "defect","defect", "defect", "defect", "cooperate","defect", "cooperate",
               "defect", "cooperate",  "defect")
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE2 <-median(COR)
CORSE2 <- median(CORR)


############session 3###############
CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession3.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
Init <-unlist(matobs[1:20,4])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Azul"] <- "cooperate"
  Init[Init == "Amarillo"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE3 <-median(COR)
CORSE3 <- median(CORR)

############session 4###############
CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession4.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
Init <-unlist(matobs[1:20,4])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Azul"] <- "cooperate"
  Init[Init == "Amarillo"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE4 <-median(COR)
CORSE4 <- median(CORR)

#################Session 5#########################

CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession5.xlsx", col_names=F)
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")

for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Initial <- c("cooperate","cooperate","defect","defect",
               "defect","cooperate","cooperate","cooperate",
               "defect","defect","cooperate","cooperate",
               "cooperate","cooperate","defect","defect", 
               "defect","defect", "defect", "cooperate")
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  #ALPHA<-which(Permutations =="cooperate")
  #BETA<- which(Permutations =="defect")
  ALPHA<-c(1,2,6,7,8,11,12,13,14,20)
  BETA<- c(3,4,5,9,10,15,16,17,18,19)
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}

COSE5 <-median(COR)
CORSE5 <- median(CORR)
CORR[CORR < 0.2] <-NA
mean(CORR, na.rm = T)


############session 6###############
CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession6.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Azul"] <- "cooperate"
  Init[Init == "Amarillo"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE6 <-median(COR)
CORSE6 <- median(CORR)

############session 7###############
CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession7.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Azul"] <- "cooperate"
  Init[Init == "Amarillo"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE7 <-median(COR)
CORSE7 <- median(CORR)


CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession7.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Azul"] <- "cooperate"
  Init[Init == "Amarillo"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE7 <-median(COR)
CORSE7 <- median(CORR)


CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession8.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Blue"] <- "cooperate"
  Init[Init == "Yellow"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE8 <-median(COR)
CORSE8 <- median(CORR)

CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession9.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Blue"] <- "cooperate"
  Init[Init == "Yellow"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE9 <-median(COR)
CORSE9 <- median(CORR)


CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession10.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Blue"] <- "cooperate"
  Init[Init == "Yellow"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE10 <-median(COR)
CORSE10 <- median(CORR)


CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession11.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Blue"] <- "cooperate"
  Init[Init == "Yellow"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE11 <-median(COR)
CORSE11 <- median(CORR)

CORR <- rep(c(NA),100)
COR <- rep(c(NA),100)
matobs <- read_excel("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Data/Masterfile/PAsession12.xlsx", col_names=F)
Ini <-unlist(matobs[1:20,4])
matobs <- unlist(matobs[,2])
matobs <- as.numeric(matobs)
load("C:/Users/Joris/Google Drive/Ph.D/PaperMadrid/Scripts/PANgoed2.Rda")
for(q in 1:100){
  iter=20
  
  #if(m <= 5000){g<-sample_gnp(20, 0.25)} else
  #  if(m > 5000 && m < 10000){g <- watts.strogatz.game(1, 20, 2, 0.1)}
  #else{g <- barabasi.game(20,directed = F)}
  #g <- barabasi.game(20,directed = F, m=2)
  
  #g <- watts.strogatz.game(1, 20, 2, 0.1)
  #plot.igraph(g)
  #repeat{
  # g<-sample_gnp(40, 0.1)
  #  COM <-components(g)$no
  # if(COM == 1) break;  
  #}
  #plot.igraph(g)
  #g<-sample_gnp(20, 0.25)
  #g <-sample_gnp(30, 1/7)
  #g<-make_lattice(length = 4, dim = 2, directed = F)
  #g<-make_ring(30)
  #g<-make_star(10, mode= "undirected", center= 2)
  #g<-make_full_graph(30)
  #g<- make_tree(30, mode= "undirected")
  
  #Utility matrix for different groups
  score.matrix <- get.score.matrix(0,1,0.8,0)
  score.matrix2 <- get.score.matrix(0,0.8,1,0)
  
  #choose update rule Function in file: "Functions iterated maps.R"
  #updaterule <- "myopic"
  #updaterule <- "Unconditinalimitation"
  #updaterule <- "myopicprop"
  #updaterule <- "myopicBoS"
  updaterule<- "myopicpropBoS2"
  
  Network <- get.adjacency(g, type= "both", edges=F)
  BB<-get.edgelist(g)
  CC <- BB[,c(2,1)]
  Edgelist <- rbind(BB,CC)
  
  #Clustering algorithm
  fc <- walktrap.community(as.undirected(g))
  
  # n loop for different starting positions
  #for(n in 1){
  n<-1
  #Save network characteristics
  netid <- c(rep(m,nrow(Network)))
  nodeid<- c(1:nrow(Network))+(m-1)*nrow(Network)
  ModG <- c(rep(modularity(fc),nrow(Network)))
  ModL <- fc$modularity
  
  EV <- evcent(g, directed=F)$vector
  Bet <- betweenness(g)
  Clos <- closeness(g)
  Deg <-degree(g)
  TTL <-transitivity(g, type= "local")
  TTG <-transitivity(g, type= "global")
  AVpath <- c(rep(average.path.length(g),nrow(Network)))
  DensN <- rep(graph.density(g), nrow(Network))
  CenN <-rep(centralization.degree(g)$centralization, nrow(Network))
  SegN <-rep(sum(shortest.paths(g) >3)/380, nrow(Network))
  
  #Genrate and randomize different initial configurations
  #Initial <-rep(c("cooperate","defect"), nrow(Network)/2)
  Init <- Ini
  Init[Init == "Blue"] <- "cooperate"
  Init[Init == "Yellow"] <- "defect"
  Initial <- as.vector(Init)
  #SufK <- shuffle(nrow(Network))
  Permutations <- Initial
  ALPHA<-which(Permutations =="cooperate")
  BETA<- which(Permutations =="defect")
  
  ALPHA1<- which(Edgelist[,1] %in% ALPHA)
  BETA1<- which(Edgelist[,1] %in% BETA)
  total <- matrix(c(rep(NA,nrow(Network))), ncol=nrow(Network))
  prop <- Permutations
  prop[prop == "cooperate"] <- 1
  prop[prop == "defect"] <- 0
  prop <- as.numeric(prop)
  prop <- matrix(c(rep(prop, each = iter+1 )), ncol=nrow(Network))
  #Initialize starting probabilities
  #if(updaterule == "myopicpropBoS"){ 
  #  prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  #if(updaterule == "myopicpropBoS2"){ 
  # prop <- matrix(c(rep(0.5, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  #}
  
  #Objects for storage
  action <- matrix(c(rep(0, nrow(Network)*(iter+1 ))), ncol=nrow(Network))
  action[1,] <- Permutations
  total.score.matrix <- matrix(c(rep(NA, nrow(Network)*iter)), nrow=nrow(Network))
  score.array <- array(c(rep(NA, 2*nrow(Edgelist)*iter)), dim= c( nrow(Edgelist),2,iter))
  
  #Computing scores for every iteration
  for(k in 1:iter){
    
    #computing scores for every edge tie
    #Functions in file "Functions iterated maps", should be in directery
    for(j in ALPHA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix)
    }
    for(j in BETA1){
      score.array[j,,k] <- compute.scores(action[k,Edgelist[j,1]],action[k,Edgelist[j,2]],score.matrix2)
    }
    
    #Computing average tie payoff for every node
    for(i in 1:nrow(Network)){
      In <- which(Edgelist[,1]==i)
      SIn <- matrix(c(rep(0,length(In))))
      for(l in 1: length(In)){
        SIn[l] <-  sum(score.array[In[l],1,k])
        
      }
      total.score.matrix[i,k] <- sum(SIn)/length(SIn)
      
    }
    
    #Different update rules to evaluatie utility stratergy
    #Functions in file "Functions iterated maps", should be in directery
    if(updaterule == "myopic"){
      action[k+1,] <- myopic(action[k,], t(total.score.matrix[,k]))
    }else if(updaterule == "myopicBoS"){
      action[k+1,] <- myopicBoS(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "Unconditinalimitation"){
      action[k+1,] <- Unconditinalimitation(action[k,], t(total.score.matrix[,k]))
    } else if(updaterule == "myopicprop"){
      LAP <- myopicprop(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS"){
      LAP <- myopicpropBoS(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    } else if(updaterule == "myopicpropBoS2"){
      LAP <- myopicpropBoS2(action[k,], t(total.score.matrix[,k]), prop[k,])
      action[k+1,] <- LAP[[1]]
      prop[k+1,] <- LAP[[2]]
    }
    else{print("no update rule specified")}
    
  }
  total[1,]<- rowMeans(total.score.matrix)
  
  #Assesing convergence
  Conver <- rep(as.numeric(identical(action[iter,], action[iter+1,])),nrow(Network))
  
  Colour <- action
  Colour[Colour == "cooperate"] <- 1
  Colour[Colour == "defect"] <- 0
  Colour 
  
  matsim <- as.numeric(t(Colour))
  matsim <- matsim[1:400]
  as.vector(t(matsim))
  
  
  Same <- rep(c(NA),400)
  Same2 <- rep(c(NA),400)
  for(i in 1:400){
    Same[i] <-ifelse(matsim[i]==matobs[i],1,-1)
    Same2[i] <-ifelse(matsim[i]==matobs[i],1,0)
  }
  sum(Same)/400
  CORR[q] <- sum(Same2)/400
  COR[q] <- sum(Same)/400
}
COSE12 <-median(COR)
CORSE12 <- median(CORR)

median(c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7,COSE8,COSE9,COSE10,COSE11,COSE12))
median(c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7,CORSE8,CORSE9,CORSE10,CORSE11,CORSE12))
sd(c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7,COSE8,COSE9,COSE10,COSE11,COSE12))
sd(c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7,CORSE8,CORSE9,CORSE10))
sdm <- c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7,COSE8,COSE9,COSE10,COSE11,COSE12)
sdmr <- c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7,CORSE8,CORSE9,CORSE10,CORSE11,CORSE12)

sqrt(sum((sdm - median(sdm))^2))/(length(sdm)-1)
sqrt(sum((sdmr - median(sdmr))^2))/(length(sdm)-1)


median(c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7))
median(c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7))
sd(c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7))
sd(c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7))
sdm <- c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7)
sdmr <- c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7)
sqrt(sum((sdm - median(sdm))^2))/(length(sdm)-1)
sqrt(sum((sdm - median(sdmr))^2))/(length(sdm)-1)
mean(c(COSE1,COSE2,COSE3,COSE4,COSE5,COSE6,COSE7))
mean(c(CORSE1,CORSE2,CORSE3,CORSE4,CORSE5,CORSE6,CORSE7))
