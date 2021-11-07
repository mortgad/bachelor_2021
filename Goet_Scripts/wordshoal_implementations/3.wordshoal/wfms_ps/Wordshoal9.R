###########################################
### DEFINE WORDFISH ESTIMATION FUNCTION ###
###########################################
set.seed(42)
require(Rcpp)
require(RcppArmadillo)
require(inline)

wordminnowSrc <- '

	// DEFINE INPUTS
	
		Rcpp::NumericMatrix Y(wfm); 
		Rcpp::NumericVector priorvec(priors);
		Rcpp::NumericVector tol(tolerances); 	
		
		double prioralpha = priorvec(0);
		double priorpsi = priorvec(1);
		double priorbeta = priorvec(2);
		double priortheta = priorvec(3);		
		
		int N = Y.nrow();
		int K = Y.ncol();

	// SET INITIAL VALUES
	
		Rcpp::NumericVector alpha(N); 
		Rcpp::NumericVector psi(K); 
		Rcpp::NumericVector beta(K); 
		Rcpp::NumericVector theta(N); 
		
		// Construct Chi-Sq Residuals	
		arma::mat C(Y.begin(),N,K); 
		arma::colvec rsum = sum(C,1);
		arma::rowvec csum = sum(C,0);
		double asum = sum(rsum);		
		for (int i=0; i < N; i++){
			for (int k=0; k < K; k++){
				C(i,k) = (Y(i,k) - rsum(i)*csum(k)/asum)/sqrt(rsum(i)*csum(k)/asum);	
			}
		}
		
		// Singular Value Decomposition of Chi-Sq Residuals
		arma::mat U(N,N);
		arma::vec s(N);
		arma::mat V(K,N);
		svd(U,s,V,C);
	
		// Load initial values
		for (int i=0; i < N; i++) theta(i) = pow(rsum(i)/asum,-0.5) * U(i,0);
		for (int k=0; k < K; k++) beta(k) = 0; // pow(csum(k)/asum,-0.5) * V(k,0);
		for (int i=0; i < N; i++) alpha = log(rsum);
		psi = log(csum/N);
		
		alpha = alpha - log(mean(rsum));
		theta = (theta - mean(theta))/sd(theta);
		
		// Create temporary variables
		Rcpp::NumericMatrix pars(2,1);
		Rcpp::NumericMatrix newpars(2,1);		
		Rcpp::NumericMatrix G(2,1);
		Rcpp::NumericMatrix H(2,2);
		double loglambdaik;
		Rcpp::NumericVector lambdai(K);
		Rcpp::NumericVector lambdak(N);
		double stepsize = 1.0;
		double cc = 0.0;
		int inneriter = 0;
		int outeriter = 0;
		
		double lastlp = -2000000000000.0;
		double lp = -1.0*(sum(0.5 * ((alpha*alpha)/(prioralpha*prioralpha))) + sum(0.5 * ((psi*psi)/(priorpsi* priorpsi))) + sum(0.5 * ((beta*beta)/(priorbeta* priorbeta))) + sum(0.5 * ((theta*theta)/(priortheta*priortheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
	
	// BEGIN WHILE LOOP
	while(((lp - lastlp) > tol(0)) && outeriter < 100){	
		outeriter++;
	
		// UPDATE WORD PARAMETERS
			for (int k=0; k < K; k++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.2;
				while ((cc > tol(1)) && inneriter < 10){
					inneriter++;
					lambdak = exp(alpha + psi(k) + beta(k)*theta);
					G(0,0) = sum(Y(_,k) - lambdak) - psi(k)/(priorpsi*priorpsi);
					G(1,0) = sum(theta*(Y(_,k) - lambdak)) - beta(k)/(priorbeta*priorbeta);
					H(0,0) = -sum(lambdak) - 1/(priorpsi* priorpsi);
					H(1,0) = -sum(theta*lambdak);
					H(0,1) = H(1,0);
					H(1,1) = -sum((theta*theta)*lambdak) - 1/(priorbeta* priorbeta);
					pars(0,0) = psi(k);
					pars(1,0) = beta(k);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					psi(k) = newpars(0,0);
					beta(k) = newpars(1,0);
					cc = max(abs(newpars - pars));
					stepsize = 1.0;
				}	
			}	

		
		// UPDATE DOCUMENT PARAMETERS
			for (int i=0; i < N; i++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.5;
				while ((cc > tol(1)) && inneriter < 10){
					inneriter++;
					lambdai = exp(alpha(i) + psi + beta*theta(i));
					G(0,0) = sum(Y(i,_) - lambdai) - alpha(i)/(prioralpha* prioralpha);
					G(1,0) = sum(beta*(Y(i,_) - lambdai)) - theta(i)/(priortheta* priortheta);		
					H(0,0) = -sum(lambdai) - 1/(prioralpha* prioralpha);
					H(1,0) = -sum(beta*lambdai);
					H(0,1) = H(1,0);
					H(1,1) = -sum((beta* beta)*lambdai) - 1/(priortheta* priortheta);
					pars(0,0) = alpha(i);
					pars(1,0) = theta(i);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					alpha(i) = newpars(0,0);
					theta(i) = newpars(1,0);
					cc = max(abs(newpars - pars));	
					stepsize = 1.0;
				}	
			}
		
		alpha = alpha - mean(alpha);
		theta = (theta - mean(theta))/sd(theta);		
		
		// CHECK LOG-POSTERIOR FOR CONVERGENCE
			lastlp = lp;
			lp = -1.0*(sum(0.5 * ((alpha*alpha)/(prioralpha*prioralpha))) + sum(0.5 * ((psi*psi)/(priorpsi* priorpsi))) + sum(0.5 * ((beta*beta)/(priorbeta* priorbeta))) + sum(0.5 * ((theta*theta)/(priortheta*priortheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
			// Rprintf("%d: %f2\\n",outeriter,lp);

	// END WHILE LOOP		
	} 
		
	// DEFINE OUTPUT	
	
	// return(theta);	
	return Rcpp::List::create(Rcpp::Named("theta") = theta,
                          Rcpp::Named("alpha") = alpha,
                          Rcpp::Named("psi") = psi,
                          Rcpp::Named("beta") = beta);

'

wordminnow <- cxxfunction(signature(
	wfm = "numeric",
	priors = "numeric",
	tolerances = "numeric"
	), wordminnowSrc, plugin = "RcppArmadillo")

###################################
### DEFINE FIRST STAGE FUNCTION ###
###################################

wordseine <- function(speakerWFM.byDebate){

	M <- length(speakerWFM.byDebate)
	
	n.speeches <- 0
	for (j in 1:M){
		if (!is.null(speakerWFM.byDebate[[j]])) n.speeches <- n.speeches + dim(speakerWFM.byDebate[[j]])[1]
		}	
		
	## Scale Each Debate ##
	
	print("Scaling Each Debate...",quote=F)
	
	debate.scaling.results <- matrix(NA,n.speeches,3)
	colnames(debate.scaling.results) <- c("Legislator","Debate","Score")
	wordfish.details <- vector("list", M) 
	
	row.counter <- 0
	for (j in 1:M){
		if (!is.null(speakerWFM.byDebate[[j]])) {
			temp.speeches <- dim(speakerWFM.byDebate[[j]])[1]	
			wordfish.details[[j]] <- wordminnow(as.matrix(speakerWFM.byDebate[[j]]),c(5,5,1,1),c(0.1,1e-3))	
			wordfish.details[[j]]$psi <- as.numeric(wordfish.details[[j]]$psi)
			wordfish.details[[j]]$alpha <- as.numeric(wordfish.details[[j]]$alpha)
			names(wordfish.details[[j]]$beta) <- colnames(speakerWFM.byDebate[[j]])
			names(wordfish.details[[j]]$psi) <- colnames(speakerWFM.byDebate[[j]])
			names(wordfish.details[[j]]$alpha) <- rownames(speakerWFM.byDebate[[j]])
			names(wordfish.details[[j]]$theta) <- rownames(speakerWFM.byDebate[[j]])
			temp.scores <- wordfish.details[[j]]$theta
			if (is.na(temp.scores)[1]) {
				temp.scores <- rep(NA,length(temp.scores))
				print(paste("Scaling failed for debate",j))
				}
			debate.scaling.results[row.counter + 1:temp.speeches,1] <- as.numeric(rownames(speakerWFM.byDebate[[j]]))
			debate.scaling.results[row.counter + 1:temp.speeches,2] <- rep(j,temp.speeches)
			debate.scaling.results[row.counter + 1:temp.speeches,3] <- temp.scores
			row.counter <- row.counter + temp.speeches
		}
		print(paste(j," debates of ",M," scaled (", row.counter," speeches scaled).",sep=""),quote=FALSE)
	}
	
	## Pull Out Legislator Names ##

	debate.scaling.results <- as.data.frame(debate.scaling.results)
	debate.scaling.results$Debate <- as.factor(debate.scaling.results$Debate)
	debate.scaling.results$Legislator <- as.factor(debate.scaling.results$Legislator)
	
	return(list(debate.scaling.results= debate.scaling.results, wordfish.details= wordfish.details))
}






#################################
### DEFINE WORDSHOAL FUNCTION ###
#################################

wordshoal <- function(speakerWFM.byDebate,speakerData,mcmc.burn=1000,mcmc.sample=1000,mcmc.thin=1,party.prior=FALSE){

require(rjags)

## Prepare Party Variable ##	

if (party.prior==TRUE){
	if (is.null(speakerData$party)) stop("To use a party prior, speakerData$party must be specified.")
	party.factor <- as.factor(speakerData$party)
	party <- as.numeric(party.factor)
	G <- nlevels(party.factor)
	}

## DEBATE-LEVEL SCALING ##
tmp <- wordseine(speakerWFM.byDebate)
wordfish.details <- tmp$wordfish.details
debate.scaling.results <- tmp$debate.scaling.results

N <- dim(speakerData)[1]	
	
print("Applying Bayesian Factor Analysis to Debate-Level Scales...",quote=F)	
	
Yflat <- as.numeric(debate.scaling.results$Score)
jVec <- as.integer(as.numeric(debate.scaling.results$Debate))
iVec <- as.integer(as.numeric(debate.scaling.results$Legislator))
S <- length(Yflat)

N <- length(levels(debate.scaling.results$Legislator))
M <- length(levels(debate.scaling.results$Debate))

jagsData <- list(
	Yflat = Yflat,
	jVec = jVec,
	iVec = iVec,
	S = S,
	N = N,
	M = M
)

if (party.prior==TRUE){
	jagsData$G <- G
	jagsData$party <- party
	}

## Factor Analysis on Debate Score Matrix ##


script.noparty.jags <- '
model{

for (s in 1:S){
	Yflat[s] ~ dnorm(alpha[jVec[s]] + beta[jVec[s]]*theta[iVec[s]],tau[iVec[s]])
}
	
for (i in 1:N){
	theta[i] ~ dnorm(0,1)
	tau[i] ~ dgamma(1,1)
	}		

for (j in 1:M){
	alpha[j] ~ dnorm(0,4)
	beta[j] ~ dnorm(0,4)	
}	

}
'

script.party.jags <- '
model{

for (s in 1:S){
	Yflat[s] ~ dnorm(alpha[jVec[s]] + beta[jVec[s]]*theta[iVec[s]],tau[iVec[s]])
}
	
for (i in 1:N){
	theta[i] ~ dnorm(muparty[party[i]],tauparty[party[i]])
	tau[i] ~ dgamma(1,1)
	}		

for (j in 1:M){
	alpha[j] ~ dnorm(0,4)
	beta[j] ~ dnorm(0,4)	
}	

for (g in 1:G){
	muparty[g] ~ dnorm(0,1)
	sigmaparty[g] ~ dunif(0,1)
	tauparty[g] <- pow(sigmaparty[g],-2)	
}

}
'

if (party.prior){
	model.jags <- jags.model(file=textConnection(script.party.jags),
			data=jagsData,
			,n.adapt=mcmc.burn)	
	update(model.jags, mcmc.burn)		
	posterior <- jags.samples(model.jags, 
			c("theta","beta","alpha","tau","muparty","sigmaparty"), 
			n.iter=mcmc.sample, thin = mcmc.thin, progress.bar="text")
} else {
	model.jags <- jags.model(file=textConnection(script.noparty.jags),
			data=jagsData,n.adapt=mcmc.burn)
	update(model.jags, mcmc.burn)		
	posterior <- jags.samples(model.jags, 
			c("theta","beta","alpha","tau"), 
			n.iter=mcmc.sample, thin = mcmc.thin, progress.bar="text")
}	

posterior$theta <- posterior$theta[,,1]
posterior$tau <- posterior$tau[,,1]
posterior$alpha <- posterior$alpha[,,1]
posterior$beta <- posterior$beta[,,1]

rownames(posterior$theta) <- rownames(posterior$tau) <- levels(debate.scaling.results$Legislator)

posterior$debate <- debate.scaling.results

if (party.prior){
	posterior$muparty <- posterior$muparty[,,1]
	posterior$sigmaparty <- posterior$sigmaparty[,,1]	
	}

return(list(posterior=posterior, wordfish.details=wordfish.details))

}




wordshoal2d <- function(speakerWFM.byDebate,speakerData,mcmc.burn=1000,mcmc.sample=1000,mcmc.thin=1){

require(rjags)

## Prepare Party Variable ##	
if (is.null(speakerData$party)) stop("To do 2D scaling, speakerData$party must be specified.")
party.factor <- as.factor(speakerData$party)
party <- as.numeric(party.factor)
G <- nlevels(party.factor)

## DEBATE-LEVEL SCALING ##
tmp <- wordseine(speakerWFM.byDebate)
wordfish.details <- tmp$wordfish.details
debate.scaling.results <- tmp$debate.scaling.results

N <- dim(speakerData)[1]	

print("Applying Bayesian Factor Analysis to Debate-Level Scales...",quote=F)	
	
Yflat <- as.numeric(debate.scaling.results$Score)
jVec <- as.integer(as.numeric(debate.scaling.results$Debate))
iVec <- as.integer(as.numeric(debate.scaling.results$Legislator))
S <- length(Yflat)

N <- length(levels(debate.scaling.results$Legislator))
M <- length(levels(debate.scaling.results$Debate))

jagsData <- list(
	Yflat = Yflat,
	jVec = jVec,
	iVec = iVec,
	party = party,
	S = S,
	N = N,
	M = M,
	G = G
)

## Factor Analysis on Debate Score Matrix ##

script.2d.jags <- '
model{

for (s in 1:S){
	Yflat[s] ~ dnorm(alpha[jVec[s]] 
		+ beta[jVec[s],1]*theta[iVec[s],1] 
		+ beta[jVec[s],2]*theta[iVec[s],2],tau[jVec[s]])
}
	
for (i in 1:N){
	theta[i,1] ~ dnorm(muparty[party[i],1],tauparty[party[i],1])
	theta[i,2] ~ dnorm(muparty[party[i],2],tauparty[party[i],2])	
	}		

for (j in 1:M){
	alpha[j] ~ dnorm(0,4)
	beta[j,1] ~ dnorm(0,4)	
	beta[j,2] ~ dnorm(0,4)		
}	

muparty[1,1] <- 1
muparty[1,2] <- 0
muparty[2,1] <- -1
muparty[2,2] <- 0
for (g in 3:G){
	muparty[g,1] ~ dnorm(0,1)
	muparty[g,2] ~ dnorm(0,1)
}	

for (g in 1:G){	
	sigmaparty[g,1] ~ dunif(0,1)
	sigmaparty[g,2] ~ dunif(0,1)	
	tauparty[g,1] <- pow(sigmaparty[g,1],-2)
	tauparty[g,2] <- pow(sigmaparty[g,2],-2)		
}

for (j in 1:M){
	tau[j] ~ dgamma(1,1)
}

}
'

model.jags <- jags.model(file=textConnection(script.2d.jags)
			,data = jagsData,
			,n.adapt=mcmc.burn)	
update(model.jags, mcmc.burn)		
posterior <- jags.samples(model.jags, 
			c("theta","beta","alpha","muparty","sigmaparty"), 
			n.iter=mcmc.sample, thin = mcmc.thin, progress.bar="text")

posterior$theta <- posterior$theta[,,,1]
posterior$alpha <- posterior$alpha[,,1]
posterior$beta <- posterior$beta[,,,1]
posterior$muparty <- posterior$muparty[,,,1]
posterior$sigmaparty <- posterior$sigmaparty[,,,1]	

rownames(posterior$theta) <- levels(debate.scaling.results$Legislator)

posterior$debate <- debate.scaling.results

return(list(posterior=posterior, wordfish.details=wordfish.details))


}



#########################
### Utility Functions ###
#########################

beta.magnitude <- function(beta,weights=rep(1,length(beta))) sqrt(sum(weights*(beta^2)/sum(weights)))

beta.magnitude.boot <- function(beta,weights=rep(1,length(beta)),replicates=5000,alpha=0.05) {
	estimate <- beta.magnitude(beta,weights)
	boots <- replicate(replicates,beta.magnitude(beta,weights*tabulate(sample(1:length(beta),length(beta),replace=TRUE),length(beta))))
	interval <- 2*estimate - quantile(boots,c(1-alpha/2,alpha/2)) 
	return(list(estimate=estimate,interval=interval))
	}

loading.magnitude <- function(kappa,beta,weights=rep(1,length(beta))) sum(weights*kappa*beta,na.rm=TRUE)/sum(weights,na.rm=TRUE)





