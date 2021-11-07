#=================================
#First version: 5th September 2017
#This version: 29.9.2018
#Author: Niels Goet
#Replication material for "Measuring Polarisation with Text Analysis: Evidence from the UK House of Commons, 1811-2015"
#=================================
set.seed(12345)
library(base)
library(ca)
library(Hmisc)
library(stringr)
library(pacman)
library(parallel)
library(austin)
library(methods)
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
  year=numeric(),
  date=numeric(),
  title=numeric(),
  debate_indicator =numeric(),
score=numeric(),
dimensions=numeric(),
perc.dim1=numeric()
)

# define per_session function
per_debate_wordfish <- function(debate_indicator, dir1, dir2, metadat, wcdata){

    results <- wordfish(wfm=wcdata,dir=c(dir1,dir2))
    
    theta_file    <- data.frame(id = results$docs, theta=results$theta,se.theta=results$se.theta,party=metadat$party[rownames(metadat) %in% results$docs],speaker=metadat$speaker[rownames(metadat) %in% results$docs],year=metadat$year[rownames(metadat) %in% results$docs],date=metadat$date[rownames(metadat) %in% results$docs],title=metadat$title[rownames(metadat) %in% results$docs])
    theta_file$session_ref <- rep(unique(metadat$session_ref),nrow(theta_file))
    
    theta_file$session_indicator <- rep(unique(metadat$session_indicator),nrow(theta_file))
    
    theta_file$debate_indicator <- rep(debate_indicator,nrow(theta_file))
    
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
    
    
    combined_results <- data.frame(id=theta_file$id,theta=theta_file$theta,se.theta=theta_file$se.theta,party=theta_file$party,speaker=theta_file$speaker,session_ref=theta_file$session_ref,session_indicator=theta_file$session_indicator,year=theta_file$year,date=theta_file$date,title=theta_file$title,debate_indicator = theta_file$debate_indicator,score=ca_file$score,dimensions=ca_file$dimensions,perc.dim1=ca_file$perc.dim1)
    
    #appends results to csv
    conn <- file(paste("poisson_model_implementations/debate_level_estimates_ps_process_" , Sys.getpid(),".csv",sep = ''), open='a')
    write.table(combined_results, conn, sep = ",", col.names = F, append = T,row.names=F)
    close(conn)
    
    gc()

} 
    
   


# apply in each session
fileNames <- Sys.glob("poisson_model_implementations/3.wordshoal/wfms_debate_level/*.RData")


#function
feed_data <- function(data.path){
    load(data.path)
            
    per_debate_wordfish(debate_indicator, dir1, dir2, metadat, wcdata)

    print(paste("Writing out results for session ",metadat$session_ref[1], " - debate ",debate_indicator,sep=""))

    gc()
  }

# parallel processing
mclapply(fileNames,
         FUN=function(x) 
           feed_data(x),
         mc.cores = numCores,
         mc.preschedule = T)

# Combine files
fileNames <- Sys.glob("poisson_model_implementations/debate_level_estimates_ps_process_*")

# Read in temp files
files <- lapply(fileNames, function(x) fread(x))

# Combine files
final_file <- do.call(rbind.data.frame,files)
colnames(final_file) <- names(combined_results)
final_file <- final_file[order(final_file$session_indicator),]

# Delete old files
do.call(file.remove, list(fileNames))

# Write out new dataset
write.csv(final_file,"poisson_model_implementations/debate_level_estimates_ps.csv")

#end year-level loop
log_file <- file(logfile_name, open = "a")
cat(paste("\ndebate_level_estimates_ps.csv successfully generated at: ",Sys.time(),". (location: poisson_model_implementations/debate_level_estimates_ps.csv)",sep=""), file = log_file,append = TRUE)
close(log_file)

print(paste("\ndebate_level_estimates_ps.csv successfully generated at: ",Sys.time(),". (location: poisson_model_implementations/debate_level_estimates_ps.csv)",sep=""))


