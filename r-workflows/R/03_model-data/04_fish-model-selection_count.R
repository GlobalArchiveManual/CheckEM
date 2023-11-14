###
# Project: Parks OMP Ningaloo
# Data:    BOSS & BRUV fish, habitat
# Task:    Modelling fish lengths w/ FSSGAM
# author:  Claude, Brooke, Kingsley
# date:    November 2022
##

# Part 1-FSS modeling----
rm(list=ls())

# Load libraries ----
library(tidyverse)
library(mgcv)
library(gplots)
library(FSSgam)
devtools::load_all("./")

# Set the study name ----
name <- 'example-bruv-workflow'

dat <- readRDS(paste0('1. Example R workflows (scripts to download)/data/tidy/', 
                      name,'_Tidy-count.rds')) %>%
  glimpse()

# Set potential predictors for modeling----
pred.vars <- c("depth", "roughness", "slope", "detrended",
               "tpi", "tri", "aspect", "reef") 

# Check for correlation of predictor variables ----
# Remove anything highly correlated (>0.95) 
round(cor(dat[ , pred.vars]), 2)

# Remove slope due to high correlation with roughness

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

# Re-set predictors for modeling----
pred.vars <- c("depth", "roughness", "detrended",
               "tpi", "tri", "aspect", "reef") 

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(dat$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- dat[which(dat$response == unique.vars[i]), ]
  if(length(which(temp.dat$number == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars   

# Run the full subset model selection----
outdir    <- ("1. Example R workflows (scripts to download)/model output/fish/") # Set the output directory
use.dat <- as.data.frame(dat)
str(use.dat)

out.all = list()
var.imp = list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(dat[which(dat$response == resp.vars[i]),])
  print(resp.vars[i])

  Model1  <- gam(number ~ s(depth, k = 3, bs = 'cr'),
                 family = gaussian(link = "identity"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                               test.fit = Model1,
                               pred.vars.cont = pred.vars,
                               factor.smooth.interactions = NA,
                               k = 3
                               )
  out.list <- fit.model.set(model.set,
                         max.models = 600,
                         parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    png(file = paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(outdir, paste(name, "Count_all-mod-fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(outdir, paste(name, "Count_all-var-imp.csv", sep = "_"), sep = "/"))

