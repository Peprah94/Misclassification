#This function simulates the data for the problem at hand. 
#It returns a list of the unverified data, verified data, 
#classification probabilities and the covariates.

# Set-up for PACKAGES
if (!require(nimble)) install.packages('nimble')
if (!require(MCMCglmm)) install.packages('MCMCglmm')
if (!require(coda)) install.packages('coda')
if (!require(mcmcplots)) install.packages('mcmcplots')
if (!require(MCMCpack)) install.packages('MCMCpack')
if (!require(boot)) install.packages('boot')
if (!require(extraDistr)) install.packages('extraDistr')

library(nimble)
library(MCMCglmm)
library(coda)
library(mcmcplots)
library(MCMCpack)
library(boot)
library(extraDistr)

#source("functions.R")


#Function that simulates data
simrep <- function(n.species, #Number of species
                   n.sites, #Number of sites
                   n.visit, #Number of visits
                   n.cov, #Number of covariates
                   p.tag, #Detection probability
                   beta # Coefficients of the model
                   ){
  
  #Dimensions of data
  cov <- array(NA, dim=c(n.sites, n.cov)) #Covariates
  lambda <- prop <- array(NA, dim=c(n.species,n.sites)) #Occupancy prob
  z <- array(NA, dim=c(n.species, n.visit, n.sites)) #Detect/Non-Detection
  C <- array(NA, dim=c(n.species, n.visit,n.sites)) # Obs detect/non-detect
  Y <- numb <- array(NA, dim=c(n.visit, n.sites)) # Verified species with Machine Learning
  omega <- array(NA, dim= c(n.species, n.species+1)) # Classification Confusion matrix
  
  #check dimensions of beta
  if(dim(beta)[1]!= n.species | dim(beta)[2]!= (n.cov+1)) stop("Dimensions of beta are wrong")
  
  # Random effects for sites
  sites <- seq(1,n.sites,1)
  alpha_sites <- rnorm(n.sites, mean = 0, sd= 1)
  
  #Simulation of covariates
cov <- matrix(runif(n.sites*n.cov, -1,1), nrow = n.sites, ncol = n.cov)
  
  # Linear Predictor the P
  for(spe.tag in 1:n.species){
    for(site.tag in 1:n.sites){
    lambda[spe.tag,site.tag] <- exp(beta[spe.tag,1] + beta[spe.tag,2]*cov[site.tag,1]+ alpha_sites[site.tag])
  }
  }

  # Detection/ Non-detection
  for(site.tag in 1:n.sites){
      prop[ ,site.tag]<- proportions(lambda[1:n.species, site.tag])
  }
  
#Check if the probabilities are correct and sum up to 1
    for(site.tag in n.sites){
      for(spe.tag in 1:n.species){
        if(any(prop[spe.tag, site.tag] <0) | any(prop[spe.tag, site.tag] >1) ) stop("Entry in Probability wrong")
        if(sum(prop[1:n.species,site.tag ]) != 1) stop("Sum of probabilities is not equal to 1")
      }
  }
  
  # Confusion Matrix
  for(site.tag in 1:n.sites){
    for(visit.tag in 1:n.visit){
      omega[1,2] <- 0.05
      omega[1,3] <- 0.13
      omega[2,1] <- 0.15
      omega[2,3] <- 0.1
      omega[3,1] <- 0.09
      omega[3,2] <- 0.18
      for(spe.tag in 1:n.species){
        omega[spe.tag, spe.tag] <- p.tag 
        omega[spe.tag, n.species+1] <- 1 - sum(omega[spe.tag, 1:n.species])  
      }
    }
  }
  
  # Check if the rowSums are 1
  for(spe.tag in 1:n.species){
    for(visit.tag in 1:n.visit){
      for(site.tag in n.sites){
        if(sum(omega[spe.tag,]) != 1) stop("Sum of confusion matrix is not equal to 1")
        if(any(omega[spe.tag,] <0) | any(omega[spe.tag,] >1) ) stop("Entry with Confusion matrix")
      }
    }
  }
  
  # Observed detections/non-detections
  for(site.tag in 1:n.sites){
    for(visit.tag in 1:n.visit){
      C[,visit.tag, site.tag] <- rmultinom(1,1, prop[1:n.species,site.tag])
    }
  }
  

# Which species were present
  for(visit.tag in 1:n.visit){
    for(site.tag in 1:n.sites){
      numb[visit.tag,site.tag] <-which(C[1:n.species,visit.tag,site.tag]==1)
    }
  }
  
  # Simulating the verified data
  for(visit.tag in 1:n.visit){
    for(site.tag in 1:n.sites){
      Y[visit.tag,site.tag] <- rcat(1, prob = omega[numb[visit.tag,site.tag],1:(n.species+1)])
    }
  }
  
  
  # Returning the needed information  
  return(list(C,Y, omega, cov, prop))
}

