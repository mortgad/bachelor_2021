#author: ngoet
#first version: 5th September 2017
#this version: 29.9.2018

set.seed(12345)
library(base)
library(ca)
library(Hmisc)
library(stringr)
library(austin)
library(methods)
library(pacman)
library(parallel)
pacman::p_load(tm)
pacman::p_load(data.table)

#create incrementer
inc <- function(x)
{
  eval.parent(substitute(x <- x + 1))
}

#set up file
combined_results <- data.frame(id=numeric(),
  theta=numeric(),
  se.theta=numeric(),
  party=numeric(),
  speaker=numeric(),
  session_ref=numeric(),
  session_indicator=numeric(),
  score=numeric(),
  dimensions=numeric(),
  perc.dim1=numeric())

write.table(combined_results, "poisson_model_implementations/full_scaling_estimates_ps.csv", sep = ",", col.names = T, append = F,row.names=F)

apply_wordfish <- function(dir1, dir2, metadat, wcdata){
  
  results     <- wordfish(wfm=wcdata,dir=c(dir1,dir2))
  
  theta_file    <- data.frame(id = results$docs, theta=results$theta,se.theta=results$se.theta,party=metadat$party[rownames(metadat) %in% results$docs],speaker=metadat$speaker[rownames(metadat) %in% results$docs])
  theta_file$session_ref <- rep(unique(metadat$session_ref),nrow(theta_file))
  theta_file$session_indicator <- rep(unique(metadat$session_indicator),nrow(theta_file))
  
  theta_file$theta <- scale(theta_file$theta,center=T,scale=T) # rescale with mean = center, variance = 1
  
  #############################
  #apply CA
  #############################
  ca_results  <- ca(wcdata)
  
  ca_file  <- data.frame(score=ca_results$colcoord[,1])
  
  ca_file$score <- scale(ca_file$score,center=T,scale=T) # rescale with mean = center, variance = 1 

  s <- dim(t(wcdata))[2]

   #check whether the first dimension accounts for more than what we would expect at random
    random_accounts <- 100/(s-1)
    singular_values <- ca_results$sv
    inertia <- singular_values^2
    pct <- 100*inertia/sum(inertia) 
    actual_account <- ca_results
      x <- 0
      for (p in pct){
        if (p > random_accounts){
          inc(x)
        }}

    dimensions <- x
    perc.dim1 <- pct[1]

    ca_file$dimensions <- rep(dimensions,nrow(ca_file))
    ca_file$perc.dim1 <- rep(perc.dim1,nrow(ca_file))

    #############################
    #Combine CA and Wordfish results in one file
    #############################
    
    
    combined_results <- data.frame(id=theta_file$id,theta=theta_file$theta,se.theta=theta_file$se.theta,party=theta_file$party,speaker=theta_file$speaker,session_ref=theta_file$session_ref,session_indicator=theta_file$session_indicator,score=ca_file$score,dimensions=ca_file$dimensions,perc.dim1=ca_file$perc.dim1)

    #appends results to csv
    write.table(combined_results, "poisson_model_implementations/full_scaling_estimates_ps.csv", sep = ",", col.names = F, append = T,row.names=F)
    print(paste("Writing our results for session ",combined_results$session_ref[1],sep=""))
}


fileNames <- Sys.glob("poisson_model_implementations/1.full_scaling/wfms/*.RData")

feed_data <- function(data.path){
  load(data.path)
  
  apply_wordfish(dir1, dir2, metadat, wcdata)
  
  print(paste("Writing out results for session ",metadat$session_ref[1],sep=""))
  
  gc()
}

mclapply(fileNames,
             FUN=function(x) 
               feed_data(x),
             mc.cores=numCores,
             mc.preschedule = T)

log_file <- file(logfile_name, open = "a")

cat(paste("\nfull_scaling_estimates_ps.csv successfully generated at: ",Sys.time(),". (location: poisson_model_implementations/full_scaling_estimates_ps.csv)",sep=""), file = log_file,append = TRUE)
close(log_file)

print(paste("\nfull_scaling_estimates_ps.csv successfully generated at: ",Sys.time(),". (location: poisson_model_implementations/full_scaling_estimates_ps.csv)",sep=""))

