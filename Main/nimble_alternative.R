#This sript is the code for the problem written in NIMBLE.

# Writting the nimble code for estimation of parameters.

#Nimble function for indexing
index <- function(y){
  x <- nimC(y)
  return(which(x==1))
}
Rindex <- nimbleRcall(function(y=double(1)){}, Rfun="index", returnType = double(0))

# Nimble function for estimating the proportions
nim_proportion <- function(y){
  x <- nimC(y)
  z <- (x)
  ret <- proportions(z)
  return(ret)
}
nimbleProportion <- nimbleRcall(function(y=double(1)){}, Rfun="nim_proportion", returnType = double(1))

#The code for the MCMC
code <- nimbleCode({

  
  # Priors for beta
  for(spe.tag in 1:n.species){
    for(cov.tag in 1:n.cov){
      beta[spe.tag,cov.tag] ~ dnorm(0, sd=1)
    }
  }
  
  # Random Effects for the model
  sig_sites ~ dgamma(1,1)
  mu_sites ~ dnorm(0, 1)
  for(site.tag in 1:n.sites){
    alpha_sites[site.tag] ~ dnorm(mu_sites, sig_sites)
  }
  
 # Linear Predictor
  for(spe.tag in 1:n.species){
      for(site.tag in 1:n.sites){
        log(lambda[spe.tag,site.tag]) <- beta[spe.tag,1] + beta[spe.tag,2]*cov[site.tag,1]+ alpha_sites[site.tag]
      }
    }

  # Proportion for the multinomial distribution
  for(site.tag in 1:n.sites){
      prop[1:n.species,site.tag] <- nimbleProportion(lambda[1:n.species, site.tag]) 
    }

  
  #Prior for the Confusion Matrix
  for(i in 1:(n.species+1)){
    alpha[i] ~ dexp(1)
  }
  
  # Confusion Matrix for the Species
  for(spe.tag in 1:n.species){
    omega[spe.tag, 1:(n.species+1)] ~ ddirch(alpha[1:(n.species+1)])
  }
  
  # Citizen Science observations
  for(site.tag in 1:n.sites){
    for(visit.tag in 1:n.visit){
      C[1:n.species,visit.tag, site.tag] ~ dmulti(prop[1:n.species,site.tag],1)
    }
  }
  
  # Subsetting index of which species are present
  for(visit.tag in 1:n.visit){
    for(site.tag in 1:n.sites){
      numb[visit.tag,site.tag] <- Rindex(C[1:n.species,visit.tag, site.tag])
    }
  }
  
  # Verified observations from ML classifications
  for(visit.tag in 1:n.visit){
    for(site.tag in 1:n.sites){
      Y[visit.tag,site.tag] ~ dcat(omega[numb[visit.tag,site.tag],1:(n.species+1)])
    }
  }
  
})
