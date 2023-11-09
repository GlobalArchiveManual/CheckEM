# Get LWR parameters by body-shape
# Original code by James Thorson, modified by Rainer Froese, December 2012

#setwd("C:\\Users\\James Thorson\\Desktop\\UW Hideaway\\FishBase\\Updates\\")
  #setwd("C:\\Users\\James Thorson\\Dropbox\\FishBase\\Updates\\")
File = paste(getwd(),"/",sep="")  # Uses existing R working directory

library(R2jags) # Interface with JAGS
  runif(1)      # sets random seed
Analysis = c("Body-shape","No-body-shape")[1]  # 1=Body-shape analysis;  2=Analysis for all species (i.e. without using body-shape)

DataFile = "BodyShape_3.csv" # Expects headers Bshape, Genus, Species, a, b, Score
  Data = read.csv(DataFile, header=TRUE, skip=0)

# Define data
Keep = which(Data[,'Score']>0)
log10a = log10(Data[Keep,'a'])
b = Data[Keep,'b']
Scores = Data[Keep,'Score']
abObs = cbind(log10a, b)
Weights = Scores # Un-normalized weights (so that Cov is comparable among analyses)
GS = factor(paste(Data[Keep,'Genus'],Data[Keep,'Species']))

# Define groups depending upon goal
if(Analysis=="Body-shape") BS = Data[Keep,'Bshape']
if(Analysis=="No-body-shape") BS = factor(rep("Pooled",length(Keep)))
# Calculate conversion of species to body shape
  GS2BS = apply(table(GS,BS),MARGIN=1,FUN=which.max) 
# Make sure each species has only one body shape 
  range(apply(cbind(BS,GS2BS[GS]),MARGIN=1,diff)) # should be 0 0
# Make indices
nBS = nlevels(BS) # number of body shapes
nGS = nlevels(GS) # number of species
nObs = nrow(Data[Keep,]) # number of observations = studies
R = diag(2)

# Define JAGS model
Model = "
model {
  # Values for each body shape group
  for(iBS in 1:nBS){
    abBS[iBS,1] ~ dnorm(-2,1) # using the overall prior for log10a, with mean = -2 and sd = 1 and tau = 1
    abBS[iBS,2] ~ dnorm(3,4) # using the overall prior for b, with mean = 3 and sd = 0.5 and tau = 4
  }

  # Values for each species
  TauGS[1] ~ dgamma(0.001,0.001)
  TauGS[2] ~ dgamma(0.001,0.001)
  sigmaGS[1] <- pow(TauGS[1],-1/2)
  sigmaGS[2] <- pow(TauGS[2],-1/2)
  roGS <- 0 # prior for correlation set to zero
  for(iGS in 1:nGS){
    abGS[iGS,1] ~ dnorm(abBS[GS2BS[iGS],1],TauGS[1]) 
    abGS[iGS,2] ~ dnorm(abBS[GS2BS[iGS],2],TauGS[2]) 
  }
  # Predictive distribution for species-level parameters -- This is the 'prior' for future species given body-size information
  for(iBS in 1:nBS){
    PredGS[iBS,1] ~ dnorm(abBS[iBS,1],TauGS[1]) #  
    PredGS[iBS,2] ~ dnorm(abBS[iBS,2],TauGS[2]) # 
  }
  
  ##### Values for each observation
  # Generate multivariate 'Precision Matrix' for each observation
  sigmaObs[1] ~ dgamma(0.001,0.001)
  sigmaObs[2] ~ dgamma(0.001,0.001)
  roObs ~ dunif(-0.99,0.99) # uniform prior for correlation, 
  CovObs[1,1] <- pow(sigmaObs[1],2) 
  CovObs[2,2] <- pow(sigmaObs[2],2) 
  CovObs[1,2] <- roObs * sigmaObs[1] * sigmaObs[2] 
  CovObs[2,1] <- CovObs[1,2]
  TauObs[1:2,1:2] <- inverse(CovObs[1:2,1:2])
  # Likelihood of each observation
  for(iObs in 1:nObs){
    TauObsI[iObs,1:2,1:2] <- TauObs[1:2,1:2] * pow(Weights[iObs],2)
    abObs[iObs,1:2] ~ dmnorm(abGS[GS[iObs],1:2],TauObsI[iObs,1:2,1:2])  
  }
}
"

# Write JAGS model
cat(Model, file=paste(File,"dnorm.bug",sep=""))
# JAGS settings
Nchains = 3
Nburnin = 1e4
Niter = 2e4
Nthin = 1e1
# Run JAGS
DataJags = list(abObs=abObs, nObs=nObs, Weights=Weights, nBS=nBS, nGS=nGS, GS=GS, GS2BS=GS2BS)
Params2Save = c("abBS","sigmaObs","roObs","sigmaGS","roGS","PredGS")
Jags <- jags(model.file=paste(File,"dnorm.bug",sep=""), working.directory=NULL, 
	data=DataJags, parameters.to.save=Params2Save, n.chains=Nchains, n.thin=Nthin, 
	n.iter=Niter, n.burnin=Nburnin)
Jags$BUGSoutput # contains the results from the JAGS run
PredGS <- Jags$BUGSoutput$sims.list$PredGS
abBS <- Jags$BUGSoutput$sims.list$abBS
mean_sigmaObs <- apply(Jags$BUGSoutput$sims.list$sigmaObs,2,mean)
sd_sigmaObs <- apply(Jags$BUGSoutput$sims.list$sigmaObs,2,sd)
mean_sigmaGS <- apply(Jags$BUGSoutput$sims.list$sigmaGS,2,mean)
sd_sigmaGS <- apply(Jags$BUGSoutput$sims.list$sigmaGS,2,sd)

cat("--------------------------------","\n")
cat("Data file =", DataFile,"\n")
cat("Number of observations =", nObs,"\n")
cat("Number of species =", nGS,"\n")
cat("Type of analysis =", Analysis,"\n")

if(Analysis=="Body-shape") {
# Analyze output -- EXAMPLE FOR an 'eel-like' species
	mean_log10a  <- mean(PredGS[,1,1]) # true mean of log10(a)
	sd_log10a    <- sd(PredGS[,1,1])   # true SE of log10(a)
	mean_b       <- mean(PredGS[,1,2])         # true mean of b
	sd_b         <- sd(PredGS[,1,2])           # true SE of b
	

cat("Estimated priors for a species with 'eel-like' body shape \n")
cat("Number of observations =",length(BS[BS=="eel-like"]),"\n")
cat("Predictive mean log10(a) =",format(mean_log10a, digits=3),"\n")
cat("Predictive SD log10(a) =", format(sd_log10a,digits=3), "\n")  
cat("Predictive mean b =", format(mean_b,digits=3),"\n")
cat("Predictive SD b =", format(sd_b,digits=3),"\n") 
cat("Predictive mean sigma of log10(a) observations =", format(mean_sigmaObs[1],digits=3),"\n") 	
cat("Predictive sd sigma of log10(a) observations =", format(sd_sigmaObs[1],digits=3),"\n") 	 	
cat("Predictive mean sigma of b observations =", format(mean_sigmaObs[2],digits=3),"\n") 	
cat("Predictive sd sigma of b observations =", format(sd_sigmaObs[2],digits=3),"\n") 	 

cat("Estimated priors for a species with 'elongated' body shape \n")
cat("Number of observations =",length(BS[BS=="elongated"]),"\n")
cat("Predictive mean log10(a) =",format(mean(PredGS[,2,1]), digits=3),"\n")
cat("Predictive SD log10(a) =", format(sd(PredGS[,2,1]),digits=3), "\n")  
cat("Predictive mean b =", format(mean(PredGS[,2,2]),digits=3),"\n")
cat("Predictive SD b =", format(sd(PredGS[,2,2]),digits=3),"\n") 

cat("Estimated priors for a species with 'fusiform' body shape \n")
cat("Number of observations =",length(BS[BS=="fusiform"]),"\n")
cat("Predictive mean log10(a) =",format(mean(PredGS[,3,1]), digits=3),"\n")
cat("Predictive SD log10(a) =", format(sd(PredGS[,3,1]),digits=3), "\n")  
cat("Predictive mean b =", format(mean(PredGS[,3,2]),digits=3),"\n")
cat("Predictive SD b =", format(sd(PredGS[,3,2]),digits=3),"\n") 

cat("Estimated priors for a species with 'short & deep' body shape \n")
cat("Number of observations =",length(BS[BS=="short & deep"]),"\n")
cat("Predictive mean log10(a) =",format(mean(PredGS[,4,1]), digits=3),"\n")
cat("Predictive SD log10(a) =", format(sd(PredGS[,4,1]),digits=3), "\n")  
cat("Predictive mean b =", format(mean(PredGS[,4,2]),digits=3),"\n")
cat("Predictive SD b =", format(sd(PredGS[,4,2]),digits=3),"\n") 

}
if(Analysis=="No-body-shape") {
	mean_log10a   <- mean(PredGS[,1]) # true mean of log10(a)
	sd_log10a     <- sd(PredGS[,1])   # true SE of log10(a)
	mean_b        <- mean(PredGS[,2])         # true mean of b
	sd_b          <- sd(PredGS[,2])           # true SE of b
    mean_sigmaObs <- apply(Jags$BUGSoutput$sims.list$sigmaObs,2,mean)
	sd_sigmaObs <- apply(Jags$BUGSoutput$sims.list$sigmaObs,2,sd)
	mean_sigmaGS <- apply(Jags$BUGSoutput$sims.list$sigmaGS,2,mean)
	sd_sigmaGS <- apply(Jags$BUGSoutput$sims.list$sigmaGS,2,sd)
	
cat("Predictive mean log10(a) =",format(mean_log10a, digits=3),"\n")
cat("Predictive SD log10(a) =", format(sd_log10a,digits=3), "\n")  
cat("Predictive mean b =", format(mean_b,digits=3),"\n")
cat("Predictive SD b =", format(sd_b,digits=3),"\n") 	
cat("Predictive mean sigma of log10(a) observations =", format(mean_sigmaObs[1],digits=3),"\n") 	
cat("Predictive sd sigma of log10(a) observations =", format(sd_sigmaObs[1],digits=3),"\n") 	 	
cat("Predictive mean sigma of b observations =", format(mean_sigmaObs[2],digits=3),"\n") 	
cat("Predictive sd sigma of b observations =", format(sd_sigmaObs[2],digits=3),"\n") 	 	
cat("Predictive mean sigma of log10(a) across species =", format(mean_sigmaGS[1],digits=3),"\n") 	
cat("Predictive sd sigma of log10(a) across species =", format(sd_sigmaGS[1],digits=3),"\n")
cat("Predictive mean sigma of b across species =", format(mean_sigmaGS[2],digits=3),"\n") 	
cat("Predictive sd sigma of b across species =", format(sd_sigmaGS[2],digits=3),"\n")  	
}
