#Estimation of parameters
source("sim_alternative.R")
source("nimble_alternative.R")

#Number of species, sites, visits and covariates for the simulation
n.species = 3; n.sites = 20; n.visit=5; n.cov=2

# Covariate effects for the model
beta <- matrix(c(0.5,-1,0.1,
                 -0.5,0.8,0.3,
                 0.6,-1,0.2),
               nrow = 3,
               ncol = 3)

#Simulation of the data
sim <- simrep(n.species, 
              n.sites, 
              n.visit, 
              n.cov, 
              0.7,
              beta)

# Retrieving data from the simulations
C <- sim[[1]] # Unverified citizen science observation
Y <- sim[[2]] #Verified Citizen science observation
cov <- sim[[4]] #Covariates

#Setting parameters
data <- list(C,Y, cov)
dim_data <- dim(data[[1]])
alpha <- rep(1, (n.species+1))

# Constants for the MCMC model
const <- list(n.sites = dim_data[3], 
              n.species = (dim_data[1]), 
              n.visit=(dim_data[2]), 
              n.cov=n.cov)

# Data for the Model
idm_data <- list(C = data[[1]], 
                 Y = data[[2]], 
                 cov=data[[3]])

# Initials for the model
omega <- matrix(NA, nrow=const$n.species,
                ncol = (const$n.species)+1) 
for(spe.tag in 1:(const$n.species)){
  omega[spe.tag,]<- rdirichlet(1, alpha = rep(1,((const$n.species)+1)))
}
beta<- matrix(rnorm(const$n.species*3,0,1), 
              nrow=n.species, 
              ncol=2)
idm_inits <- function(){list(beta =beta,
                             omega=omega#,
                             #p.tag = p.tag
)
}
initsList <- idm_inits()

#Putting all together for the creating compilation
modelInfo <- list(
  code = code,
  constants = const,
  data = idm_data,
  inits = initsList
)

#Create the model in nimble
mwtc <- nimbleModel(code,data = idm_data, 
                    constants = const, 
                    inits = initsList,
                    dimensions =list(lambda = c(const$n.species, const$n.sites), 
                                     calculate = FALSE))

# Create the model in C
Cmwtc <- compileNimble(mwtc, 
                       showCompilerOutput = FALSE)


mcmcconf <- configureMCMC(Cmwtc, monitors = c("beta","omega"))

Rmcmc <- buildMCMC(mcmcconf, 
                   enableWAIC =FALSE)

# Compile 
cmcmc <- compileNimble(Rmcmc, 
                       project = Cmwtc,
                       resetFunctions = TRUE)

# Run the MCMC
mcmc.out <- runMCMC(cmcmc, 
                    niter = 500000,
                    nchains = 3,
                    nburnin = 250000,
                    inits = initsList,
                    thin =10, 
                    setSeed = TRUE, 
                    samples=TRUE, 
                    samplesAsCodaMCMC = TRUE, 
                    summary = TRUE, 
                    WAIC = FALSE)

#Output from the MCMC
output <- mcmc.out$summary$all.chains
output

# Diagnostics for the model
coda_samples <- mcmc(mcmc.out$samples)
mcmcplot(coda_samples)

#Save the output
save(output, file="output.RData")
