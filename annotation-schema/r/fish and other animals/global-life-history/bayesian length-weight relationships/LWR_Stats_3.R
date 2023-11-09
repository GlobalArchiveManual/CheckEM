# Get stats and graphs for weighted data
require(weights) # package needed for weighted histogram
graphics.off()
DataFile = "BodyShape_3.csv" 
Data = read.csv(DataFile, header=TRUE, skip=0)

# For analayis by body shape, activate next row 
# Data = Data[Data$Bshape=="elongated",] # use one of eel-like, elongated, fusiform, short & deep

# Define data
a = Data$a
b = Data$b
Score = Data$Score
wt = Score/sum(Score) # normalized weights
BS = Data$Bshape

# Get weighted stats
n = length(b)
mean_b = weighted.mean(b, wt)
v_b = sum(wt * (b - mean_b)^2) # wheighted variance around weighted mean b
sd_b = sqrt(v_b)
low_HDI_b = mean_b -1.96 * sd_b
hi_HDI_b = mean_b + 1.96 * sd_b 

log10a = log10(a)
mean_log10a = weighted.mean(log10a, wt)
v_log10a = sum(wt * (log10a - mean_log10a)^2) # variance around weighted mean log10a
sd_log10a = sqrt(v_log10a)
geom_a = 10^(mean_log10a)
low_HDI_a = 10^(mean_log10a -1.96 * sd_log10a)
hi_HDI_a = 10^(mean_log10a + 1.96 * sd_log10a)

# Print results to screen
cat("\n")
cat("Data file =", DataFile,"\n")
cat("Number of observations =", n, "\n") 
cat("Mean of log10(a) =", format(mean_log10a, digits=3),"\n") 
cat("SD of log10(a) =", format(sd_log10a, digits=3),"\n")
cat("Geom. mean of a =", format(geom_a, digits=3),"\n") 
cat("95% HDI of a =", format(low_HDI_a, digits=3),"-",format(hi_HDI_a, digits=3),"\n")
cat("Mean of b =", format(mean_b, digits=3),"\n") 
cat("95% HDI of b =", format(low_HDI_b, digits=3), "-",format(hi_HDI_b, digits=3),"\n")
cat("SD of b =", format(sd_b, digits=3),"\n")   

# Plot weighted histograms with normal curve
# windows()
par(mfrow=c(1,2))
x=seq(2,4,0.01)
wtd.hist(b, xlim=c(2,4), las=1, main="", xlab= "b", freq=F, weight = wt)
curve(dnorm(x, mean = mean_b, sd = sd_b),from=2, to=4, lw=2, add=T) # using weigthed stats
curve(dnorm(x, mean = 3, sd = 0.5),from=2, to=4, lw=1, lty=2, add=T) # show broken line prior for b

x=seq(-4,0,0.01)
wtd.hist(log10a, xlim=c(-4,0), las=1, main="", xlab= "log10 a", freq=F, weight = wt)
curve(dnorm(x, mean = mean_log10a, sd = sd_log10a),from=-4, to=0, lw=2, add=T) # using weighted stats
curve(dnorm(x, mean = -2, sd = 1),from=-4, to=0, lw=1, lty=2, add=T) # show broken line prior for log10a

# Bayesian results across all species, with separation of measurement error sigmaObs
# from parameter and between-species variability (the sds of log10a and b)
B_mean_b 		= 3.04
B_sd_b   		= 0.119
B_mean_log10a	= -2.00
B_sd_log10a		= 0.313
mean_sigmaObs	= 0.245
sd_sigmaObs	 	= 0.00322			

# Plot weighted histograms with priors for parameters
windows(12, 7)
par(mfrow=c(1,2))
x=seq(2,4,0.001)
wtd.hist(b, xlim=c(2,4), ylim=c(0,3.5), las=1, main="", xlab= "b", freq=F, weight = wt)
curve(dnorm(x, mean = B_mean_b, sd = B_sd_b),from=2, to=4, lw=2, lty=3, add=T) # using prior for b
curve(dnorm(x, mean = mean_b, sd = sd_b),from=2, to=4, lw=2, add=T) # using weigthed stats
curve(dnorm(x, mean = 3, sd = 0.5),from=2, to=4, lw=1, lty=2, add=T) # show broken line prior for b

x=seq(-4,0,0.001)
wtd.hist(log10a, xlim=c(-4,0), ylim=c(0,1.4), las=1, main="", xlab= "log10(a)", freq=F, weight = wt)
curve(dnorm(x, mean = B_mean_log10a, sd = B_sd_log10a),from=-4, to=0, lw=2, lty=3, add=T) # using prior for log10a
curve(dnorm(x, mean = mean_log10a, sd = sd_log10a),from=-4, to=0, lw=2, add=T) # using weighted stats
curve(dnorm(x, mean = -2, sd = 1),from=-4, to=0, lw=1, lty=2, add=T) # show broken line prior for log10a

#windows()
#x=seq(-4,0,0.001)
#curve(dnorm(x, mean = -2.41, sd = 0.171),from=-4, to=-1, las=1, main="", xlab= "log10(a)", ylab="Density") # elongated
#curve(dnorm(x, mean = -2.99, sd = 0.175),from=-4, to=-1, add=T) # eel-like
#curve(dnorm(x, mean = -1.95, sd = 0.173),from=-4, to=-1, add=T) # fusiform
#curve(dnorm(x, mean = -1.7, sd = 0.175),from=-4, to=-1, add=T) # short & deep

