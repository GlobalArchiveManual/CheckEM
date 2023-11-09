## Analysis of weight-at-length data using previous studies (or body shape) as priors 
# Code provided by James Thorston in November 2012, modified by Rainer Froese in January 2013

library(R2jags) # Interface with JAGS
  runif(1)      # sets random seed

# Get LW data
File = paste(getwd(),"/",sep="")  # Use existing R working directory
DataFile = "Scophthalmus_maximus_LW.csv" # expects columns Length, Weight, N 
Data = read.csv(DataFile, header=TRUE, skip=0)

# Define data
log10L = log10(Data[,'Length'])
log10W = log10(Data[,'Weight'])
wts = Data[,'N']

# Get priors for this species from previous studies or body shape
pr_mean_log10a = -1.83
pr_sd_log10a = 0.0688
pr_tau_log10a = 1 / (pr_sd_log10a)^2    
pr_mean_b = 3.04
pr_sd_b = 0.0486
pr_tau_b = 1 / (pr_sd_b)^2     
pr_mean_sigma_log10a = 0.26
pr_sd_sigma_log10a = 0.00319
# calculate parameters for gamma distribution of sigma
pr_r_sigma_log10a <- pr_mean_sigma_log10a^2 / pr_sd_sigma_log10a^2
pr_mu_sigma_log10a <- pr_mean_sigma_log10a / pr_sd_sigma_log10a^2

# Define JAGS model
Model = "
model {
  sigma ~ dgamma(pr_r_sigma_log10a, pr_mu_sigma_log10a)  
  tau <- 1 / sigma^2
  log10a_True ~ dnorm(pr_mean_log10a,pr_tau_log10a) 
  b_True ~ dnorm(pr_mean_b,pr_tau_b) 
  log10Wmean <- sum( log10W ) / Nobs
  for(i in 1:Nobs){
    # Normal
    tau_i[i] <- wts[i] * tau # weighted precision
    log10What[i] <- log10a_True + log10L[i]*b_True
    log10W[i] ~ dnorm(log10What[i],tau_i[i]) # given the observations in ab and the priors in abTrue and Tau, create multivariate normal posteriors for log(a) and b
    scaled_resid_squared_1[i] <- pow( ( log10W[i] - log10What[i] ) * wts[i], 2)
    scaled_resid_squared_0[i] <- pow( ( log10W[i] - log10Wmean ) * wts[i], 2)
  }
}
"

# Write JAGS model
cat(Model, file=paste(File,"dnorm.bug",sep=""))
# JAGS settings
Nchains = 3
Nburnin = 1e4
Niter = 2e4
Nthin = 10
# Run JAGS
DataJags = list(log10L=log10L, log10W=log10W, wts=wts, Nobs=length(log10L), 
	pr_mean_b=pr_mean_b, pr_tau_b=pr_tau_b, pr_mean_log10a=pr_mean_log10a, 
	pr_tau_log10a=pr_tau_log10a, pr_r_sigma_log10a=pr_r_sigma_log10a, 
	pr_mu_sigma_log10a=pr_mu_sigma_log10a)
Jags <- jags(model.file=paste(File,"dnorm.bug",sep=""), working.directory=NULL, 
		data=DataJags, parameters.to.save=c("log10a_True","b_True","sigma",
		"scaled_resid_squared_1","scaled_resid_squared_0"), n.chains=Nchains, 
		n.thin=Nthin, n.iter=Niter, n.burnin=Nburnin)
Jags$BUGSoutput # contains the results from the JAGS run

# Bayesian coefficient of determination
B.r.squared = 1 - mean(Jags$BUGSoutput$sims.list$scaled_resid_squared_1)/
		mean(Jags$BUGSoutput$sims.list$scaled_resid_squared_0)

# Analyze output
mean_log10a  <- mean(Jags$BUGSoutput$sims.list$log10a_True) # true mean of log10(a)
sd_log10a    <- apply(Jags$BUGSoutput$sims.list$log10a_True, 2, sd)   # true SE of log10(a)
mean_b       <- mean(Jags$BUGSoutput$sims.list$b_True)         # true mean of b
sd_b         <- apply(Jags$BUGSoutput$sims.list$b_True, 2, sd)           # true SE of b


# write results to screen
cat("\n")
cat("Data file =", DataFile,"\n")
cat("Number of observations =", length(wts), "\n")
cat("Length range =", min(10^(log10L)), " -", max(10^(log10L)), "\n") 
cat("Weight range =", min(10^(log10W)), " -", max(10^(log10W)), "\n")
regreg <- lm(log10W ~ log10L, weights = wts)
cat("Regular regression a =", format(10^(as.numeric(regreg$coefficients[1])), digits=3), 
	"b =", format(as.numeric(regreg$coefficients[2]), digits=3), 
	", r^2 =", format(summary(regreg)$r.squared, digits=3), "\n") 
cat("Regular regression SE log10(a) =", format(summary(regreg)$coefficients[1,2], digits=3),
	", SE b =", format(summary(regreg)$coefficients[2,2], digits=3), "\n")
cat("Prior mean b =", format(pr_mean_b,digits=3),"\n")
cat("Prior SD b =", format(pr_sd_b,digits=3),"\n")
cat("Prior geom. mean a =", format(10^(pr_mean_log10a),digits=3), "\n")
cat("Prior mean log10(a) =",format(pr_mean_log10a, digits=3),"\n")
cat("Prior SD log10(a) =", format(pr_sd_log10a,digits=2), "\n")
cat("True mean of log10(a) =", format(mean_log10a,digits=3),"\n")
cat("True SD of log10(a) and log10(W) =", format(sd_log10a,digits=3),"\n")
cat("True geom. mean of a =", format(10^(mean_log10a),digits=3),"\n")
cat("95% HDI of a =", format(10^(mean_log10a - 1.96*sd_log10a),digits=3),"-",
		format(10^(mean_log10a + 1.96*sd_log10a),digits=3),"\n")
cat("True mean of b =", format(mean_b,digits=3),"\n")
cat("95% HDI of b =", format(mean_b - 1.96 * sd_b,digits=3),"-",format(mean_b + 1.96 * sd_b,digits=3),"\n")
cat("True SD of b =", format(sd_b,digits=3),"\n")
cat("Bayesian coefficient of determination (r^2) =", format(B.r.squared, digits=3), "\n")

# plot data and fit
plot(x=log10L, y=log10W, xlab="log10(L)", ylab="log10(W)", las=1)
abline(a=mean_log10a, b=mean_b)
