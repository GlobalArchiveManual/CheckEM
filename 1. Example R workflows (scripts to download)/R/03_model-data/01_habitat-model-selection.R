###
# Project: Wide-field view stereo-video drop-camera
# Data:    BOSS Habitat data
# Task:    Running model selection
# Author:  Claude Spencer from @beckyfisher/FSSgam 
# Date:    August 2023
##

# Clear the environment ----
rm(list = ls())

# Load libraries ----
library(tidyverse)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(doSNOW)
library(gamm4)
library(RCurl)
library(reshape2)
library(FSSgam)

# Set the study name ----
name <- '2021-2022_SwC_BOSS'

# Bring in and format the data ----
dat <- readRDS(paste0("data/tidy/", name, "_habitat-bathymetry.rds")) %>%
  dplyr::mutate(mbdepth = abs(mbdepth)) %>%                                     # Transform to positive otherwise sqrt(mbdepth) will error
  glimpse()

# Set predictor variables ----
names(dat)
pred.vars <- c("mbdepth","roughness", "detrended", 
               "slope", "TPI", "aspect", "TRI")                         

# Check for correlation of predictor variables ----
# Remove anything highly correlated (>0.95) 
round(cor(dat[ , pred.vars]), 2)

# Remove TRI and slope

# Review of individual predictors for even distribution ----
# Plot of likely transformations
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <-dat[ , i]
  x = as.numeric(unlist(x))
  hist((x))
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}                                                                               # All look pretty OK

# Check to make sure response variables have less than 80% zeros ----
unique.vars = unique(as.character(dat$habitat))
unique.vars.use = character()
for(i in 1:length(unique.vars)){
  temp.dat = dat[which(dat$habitat == unique.vars[i]),]
  if(length(which(temp.dat$habitat == 0)) / nrow(temp.dat) < 0.8){
    unique.vars.use = c(unique.vars.use, unique.vars[i])}
}
unique.vars.use                                                                 # All remain                                                                 # All good  

# Set-up the environment to run model selection ----
outdir    <- ("model out/")                                                     # Set the output directory
use.dat   <- dat[dat$habitat %in% c(unique.vars.use), ]
out.all   <- list()
var.imp   <- list()

# Re-set predictor and response variables ----
pred.vars <- c("mbdepth","roughness", "detrended", 
               "TPI", "aspect")
resp.vars <- unique.vars.use

# Run the full subset model selection process ----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- dat[dat$habitat == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  Model1  <- gam(cbind(number, (total.points.annotated - number)) ~ 
                   s(mbdepth, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  cyclic.vars = c("aspect"),
                                  k = 5,
                                  cov.cutoff = 0.7
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # Plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Save model fits and importance scores ----
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_all.var.imp.csv"))
