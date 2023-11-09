#### R and JAGS code for estimating LWR-parameters from previous studies for a given species
#### Developed by James Thorson in October 2012, modified by Rainer Froese in November/December 2012 
#### Read in data
File = paste(getwd(),"/",sep="")  # Use current R working directory

library(R2jags)  # Interface with JAGS
runif(1)         # sets random seed  
  
DataFile = "BodyShape_3.csv" # The file BodyShape_3.csv contains over 5000 LWR studies
Genus = "Anguilla"  # generic name, e.g. "Anguilla"
Species = "obscura" # specific name, e.g. "obscura"
Data = read.csv(DataFile, header=TRUE)
DataGS = Data[Data$Genus == Genus & Data$Species == Species,] # select data for Species
Family = DataGS$Family[1] # get Family of species
Family = levels(Family)[as.numeric(Family)] # turn Family from type factor into text
DataFam = Data[Data$Family == Family,]
Bshape = DataGS$Bshape[1] # one of: "eel-like", "elongated", "fusiform", "short & deep"
Bshape = levels(Bshape)[as.numeric(Bshape)] # turn Bshape from type factor into text

# Define data
Keep = which(DataFam$Score>0 & DataFam$Bshape==Bshape) # exclude studies with zero Score or with other body shapes
wts = DataFam$Score[Keep]  # Un-normalized weights (so that Cov is comparable among analyses)
a = DataFam$a[Keep] # vector with estimates of parameter 'a' from selected studies
b = DataFam$b[Keep] # vector with estimates of parameter 'b' from selected studies
GenSpec = paste(DataFam$Genus[Keep],DataFam$Species[Keep]) # get combinations of names
TargetSpec = paste(Genus, Species)

# Relabel GenSpec so that TargetSpec = 1
OtherSpecies = unique(GenSpec[GenSpec != TargetSpec])
GenusSpecies = factor(GenSpec, levels=c(TargetSpec, OtherSpecies))
Nspecies = nlevels(GenusSpecies) # number of species
Nobs = length(a) # number of observations

### Assignment of priors based on available body shape information
# if no body shape is indicated, use pre-estimated priors across all shapes 
prior_mean_log10a = -2.0
prior_sd_log10a = 0.313
prior_tau_log10a = 1/prior_sd_log10a^2
prior_mean_b = 3.04 
prior_sd_b = 0.119
prior_tau_b = 1/prior_sd_b^2
if (Bshape == "eel-like") { # eel-like prior for log(a) and b
prior_mean_log10a = -2.99
prior_sd_log10a = 0.175
prior_tau_log10a = 1/prior_sd_log10a^2
prior_mean_b = 3.06 
prior_sd_b = 0.0896
prior_tau_b = 1/prior_sd_b^2}
if (Bshape == "elongated") { # elongate prior for log(a) and b
prior_mean_log10a = -2.41
prior_sd_log10a = 0.171
prior_tau_log10a = 1/prior_sd_log10a^2
prior_mean_b = 3.12 
prior_sd_b = 0.09
prior_tau_b = 1/prior_sd_b^2}
if (Bshape == "fusiform") { # fusiform prior for log(a) and b
prior_mean_log10a = -1.95
prior_sd_log10a = 0.173
prior_tau_log10a = 1/prior_sd_log10a^2
prior_mean_b = 3.04 
prior_sd_b = 0.0857
prior_tau_b = 1/prior_sd_b^2}
if (Bshape == "short & deep") { # short & deep prior for log(a) and b
prior_mean_log10a = -1.7
prior_sd_log10a = 0.175
prior_tau_log10a = 1/prior_sd_log10a^2
prior_mean_b = 3.01 
prior_sd_b = 0.0905
prior_tau_b = 1/prior_sd_b^2}

# Priors for measurement error (= sigma) based on 5150 studies
# given here as shape mu and rate r, for gamma distribution
SD_rObs_log10a = 6520
SD_muObs_log10a = 25076 
SD_rObs_b = 6808
SD_muObs_b = 37001
# Priors for between species variability (= sigma) based on 5150 studies for 1821 species
SD_rGS_log10a = 1372
SD_muGS_log10a = 7933
SD_rGS_b = 572
SD_muGS_b = 6498


### Define JAGS model 
Model = "
model {               
	#### Process model -- effects of taxonomy
	# given the likelihood distributions and the priors, 
	# create normal posterior distributions for log10a, b, 
	# and for the process error (=between species variability sigmaGS)  
	
	abTrue[1] ~ dnorm(prior_mean_log10a,prior_tau_log10a) 
	abTrue[2] ~ dnorm(prior_mean_b,prior_tau_b) 
	sigmaGSlog10a ~ dgamma( SD_rGS_log10a, SD_muGS_log10a) 
	sigmaGSb ~ dgamma( SD_rGS_b, SD_muGS_b)
	
	# given the posterior distributions and the process errors,
	# establish for every species the expected witin-species 
	# parameter distributions; no correlation roGS between species 
	
	roGS <- 0 
	tauGenusSpecies[1] <- pow(sigmaGSlog10a,-2)
	tauGenusSpecies[2] <- pow(sigmaGSb,-2)
	for(k in 1:Nspecies){
		abGenusSpecies[k,1] ~ dnorm(abTrue[1],tauGenusSpecies[1]) 
		abGenusSpecies[k,2] ~ dnorm(abTrue[2],tauGenusSpecies[2]) 
	}
  
 	### Observation model  
 	## Errors 
 	# given the data and the priors, establish distributions  	
	# for the observation errors sigmaObs 	
	
	sigmaObslog10a ~ dgamma( SD_rObs_log10a, SD_muObs_log10a) 
	sigmaObsb ~ dgamma( SD_rObs_b, SD_muObs_b) 	
    	
	# create inverse covariance matrix, with negative parameter correlation roObs
	roObs ~ dunif(-0.99,0)     
    CovObs[1,1] <- pow(sigmaObslog10a,2)  
	CovObs[2,2] <- pow(sigmaObsb,2) 
	CovObs[1,2] <- roObs * sigmaObslog10a * sigmaObsb 
	CovObs[2,1] <- CovObs[1,2]
	TauObs[1:2,1:2] <- inverse(CovObs[1:2,1:2]) 
	
	## likelihood
	# given the data, the priors and the covariance, 
	# create multivariate likelihood distributions for log10(a) and b 
	
	for(i in 1:N){
		TauObsI[i,1:2,1:2] <- TauObs[1:2,1:2] * pow(Weights[i],2)   # weighted precision
		ab[i,1:2] ~ dmnorm(abGenusSpecies[GenusSpecies[i],1:2],TauObsI[i,1:2,1:2])   
	}
}
"

# Write JAGS model 
cat(Model, file=paste(File,"dmnorm.bug",sep=""))
# JAGS settings
Nchains = 3	# number of MCMC chains to be used in JAGS
Nburnin = 1e4 # number of burn-in iterations, to be discarded; 1e4 = 10000 iterations for burn-in
Niter = 3e4 # number of iterations after burn-in; 3e4 = 30000 iterations
Nthin = 1e1 # subset of iterations to be used for analysis; 1e1 = every 10th iteration 

# Run JAGS: define data to be passed on in DataJags; 
# determine parameters to be returned in Param2Save; 
# call JAGS with function Jags()
DataJags = list(ab=cbind(log10(a),b), N=Nobs, Weights=wts, Nspecies=Nspecies, GenusSpecies=GenusSpecies,
		prior_mean_b=prior_mean_b, prior_tau_b=prior_tau_b, 
		prior_mean_log10a=prior_mean_log10a, prior_tau_log10a=prior_tau_log10a, 
		SD_rObs_log10a=SD_rObs_log10a, SD_muObs_log10a=SD_muObs_log10a,  
		SD_rObs_b=SD_rObs_b, SD_muObs_b=SD_muObs_b, 
		SD_rGS_log10a=SD_rGS_log10a, SD_muGS_log10a=SD_muGS_log10a,
		SD_rGS_b=SD_rGS_b, SD_muGS_b=SD_muGS_b)
Params2Save = c("abTrue","abGenusSpecies","sigmaGSlog10a","sigmaGSb","sigmaObslog10a","sigmaObsb","roObs")
Jags <- jags(model.file=paste(File,"dmnorm.bug",sep=""), working.directory=NULL, data=DataJags, 
		parameters.to.save=Params2Save, n.chains=Nchains, n.thin=Nthin, n.iter=Niter, n.burnin=Nburnin)
Jags$BUGSoutput # contains the results from the JAGS run

# Analyze output for the relatives
abTrue <- Jags$BUGSoutput$sims.list$abTrue
R_mean_log10a  <- mean(abTrue[,1]) # true mean of log10(a)
R_sd_log10a    <- sd(abTrue[,1])   # true SE of log10(a)
R_mean_b       <- mean(abTrue[,2])         # true mean of b
R_sd_b         <- sd(abTrue[,2])           # true SE of b

# Analyze output for the target species
abGenusSpecies <- Jags$BUGSoutput$sims.list$abGenusSpecies
mean_log10a  <- mean(abGenusSpecies[,1,1]) # true mean of log10(a) for the first species= target species
sd_log10a    <- sd(abGenusSpecies[,1,1])   # true SE of log10(a)
mean_b       <- mean(abGenusSpecies[,1,2])         # true mean of b
sd_b         <- sd(abGenusSpecies[,1,2])           # true SE of b
mean_sigma_log10a <- mean(Jags$BUGSoutput$sims.list$sigmaObslog10a) # measurement error of log10(a)
sd_sigma_log10a <- apply(Jags$BUGSoutput$sims.list$sigmaObslog10a, 2, sd)
mean_sigma_b    <- mean(Jags$BUGSoutput$sims.list$sigmaObsb) # measurement error of b
sd_sigma_b		<- apply(Jags$BUGSoutput$sims.list$sigmaObsb, 2, sd)
ro_ab        <- mean(Jags$BUGSoutput$sims.list$roObs) # measurement correlation of log10(a),b

### write results to screen
cat("\n")
cat("Data file =", DataFile,"\n")
cat("Species =", TargetSpec, "\n")
cat("Family =", Family, "\n")
cat("Number of target species observations =", length(GenSpec[GenSpec==TargetSpec]), "\n")
cat("Number of related species =", Nspecies - 1, "\n")
cat("Number of related species observations =", length(a) - length(GenSpec[GenSpec==TargetSpec]), "\n") 
cat("Body shape =", Bshape, "\n")
cat("Prior mean b =", format(prior_mean_b,digits=3),"\n")
cat("Prior SD b =", format(prior_sd_b,digits=3),"\n") 
cat("Prior geom. mean a =", format(10^(prior_mean_log10a),digits=3), "\n")
cat("Prior mean log10(a) =",format(prior_mean_log10a, digits=3),"\n")
cat("Prior SD log10(a) =", format(prior_sd_log10a,digits=3), "\n")  
cat("\n", "Results for the related species", "\n")
cat("True mean of log10(a) =", format(R_mean_log10a,digits=3),"\n") 
cat("True SD of log10(a) and log10(W) =", format(R_sd_log10a,digits=3),"\n")
cat("True geom. mean of a =", format(10^(R_mean_log10a),digits=3),"\n") 
cat("True mean of b =", format(R_mean_b,digits=3),"\n") 
cat("True SD of b =", format(R_sd_b,digits=3),"\n") 
cat("\n", "Results for the target species", "\n")
cat("True mean of log10(a) =", format(mean_log10a,digits=3),"\n") 
cat("True SD of log10(a) and log10(W) =", format(sd_log10a,digits=3),"\n")
cat("True geom. mean of a =", format(10^(mean_log10a),digits=3),"\n") 
cat("95% HDI of a =", format(10^quantile(abGenusSpecies[,1,1],prob=0.025),digits=3),"-",
	format(10^quantile(abGenusSpecies[,1,1],prob=0.975),digits=3),"\n")
cat("True mean of b =", format(mean_b,digits=3),"\n") 
cat("95% HDI of b =", format(quantile(abGenusSpecies[,1,2],prob=0.025),digits=3),"-",
	format(quantile(abGenusSpecies[,1,2],prob=0.975),digits=3),"\n")
cat("True SD of b =", format(sd_b,digits=3),"\n")  
cat("Measurement error (sigma) of log10(a), mean =",format(mean_sigma_log10a,digits=3), ", SD =", 
	format(sd_sigma_log10a,digits=3), "\n")
cat("Measurement error (sigma) of b, mean =", format(mean_sigma_b,digits=3),", SD =", format(sd_sigma_b,digits=3),"\n") 
cat("Measurement correlation of log(a),b =", format(ro_ab,digits=3),"\n")
  
### Create graphs
graphics.off()   # closes open graphic windows from previous sessions
# Plot parameters from studies; 
# use to give less or no weight to outliers, then redo analysis
if (length(a) >2) {
	windows()
	plot(x = b, y = log10(a))
	abline(lm(log10(a) ~ b))
}

# show prior and posterior densities
windows(12,7)
par(mfrow=c(1,2))
# posterior and prior density of log10(a)
 x <- seq((prior_mean_log10a - 4 * prior_sd_log10a),(prior_mean_log10a + 4 * prior_sd_log10a),0.001)
 curve(dnorm(x, mean=mean_log10a, sd=sd_log10a), from = prior_mean_log10a-4*prior_sd_log10a, 
	to = prior_mean_log10a+4*prior_sd_log10a, las=1, main="", xlab=expression("log"[10]*"("~italic(a)~")"), ylab= "Density", frame=F) 
 curve(dnorm(x, mean=prior_mean_log10a, sd=prior_sd_log10a), lty=2, add=T)
 points(mean(log10(DataGS$a)), 0)
 
# posterior and prior density of b
  x <- seq(prior_mean_b - 4 * prior_sd_b, prior_mean_b + 4 * prior_sd_b, 0.001)
  curve(dnorm(x, mean = mean_b, sd = sd_b), from = prior_mean_b - 4 * prior_sd_b, 
	to = prior_mean_b + 4 * prior_sd_b, las=1, main="",xlab=expression(~italic(b)), ylab="Density", frame=F)
  curve(dnorm(x, mean = prior_mean_b, sd = prior_sd_b), lty=2, add=T)
  points(mean(DataGS$b), 0)	
