#--------------------------------#
# Parallel computing script
#--------------------------------#

require(parallel)
require(doParallel)
require(foreach)
require(iterators)
require(snow)
require(doSNOW)

# Set number of cores
num_cores <- detectCores() - 1

# Test
nStudies = 4
nPats = c(200,200,200,200) #c(10,25,50,100)
nSim = 30

#(1) - level of non-linearity
#(1a) -- linear
FP1 = c(1,0)
beta1 = c(3,0)
#(1b) -- moderate non-linear 1
FP2 = c(0.5,1)
beta2 = c(4,1)
#(1c) -- moderate non-linear 2
FP3 = c(1,1)
beta3 = c(2,4)
#(1d) -- very non-linear
FP4 = c(0,0)
beta4 = c(-5,-4)

#Lists of FPs, betas, and intercepts to add to the probabilities
FPList = list(FP1, FP2, FP3, FP4)
betaList = list(beta1, beta2, beta3, beta4)
int2s = c(0,0,0,0.2)

#------------------------------------------------------------------------------
# Test doing one sim in "parallel"
cl <- makeCluster(num_cores)
snow::clusterExport(cl, list = c("simulate.ipdma", "simulate.one.study",
                                 "simulate.one.pat", "FPone", "FPeval",
                                 "FPList",
                                 "betaList",
                                 "int2s"))
clusterSetRNGStream(cl, rep(1234,6))
clusterEvalQ(cl,  library(rvest))
registerDoSNOW(cl)
testParallel <-
  parLapply(cl, 1:4, 
          function(i) {
  simulate.ipdma(nStudies = 4, nPats = c(200,200,200,200), times = 1:100, 
                 FPs = FPList[[i]], betas = betaList[[i]], 
                 int1 = arm::logit(0.2), int2 = int2s[i])
})
stopCluster(cl)

#------------------------------------------------------------------------------
# Test doing all sims in foreach
cl <- makeCluster(num_cores)
snow::clusterExport(cl, list = c("simulate.ipdma", "simulate.one.study",
                                 "simulate.one.pat", "FPone", "FPeval",
                                 "FPList",
                                 "betaList",
                                 "int2s"))
clusterSetRNGStream(cl, rep(1234,6))
clusterEvalQ(cl,  library(rvest))
registerDoSNOW(cl)
testParallel <-
  foreach(j = 1:4,
          .verbose = TRUE,
          .packages = c("foreach")) %dopar%
  {
    foreach(i = 1:4,
            .combine = "rbind") %do% { 
    # No random effects
    ran1 <- simulate.ipdma(nStudies = nStudies,
                                nPats = nPats,
                                times = 1:100,
                                FPs = FPList[[i]],
                                betas = betaList[[i]],
                                int1 = arm::logit(0.2),
                                int2 = int2s[i]) %>%
        dplyr::mutate(scen = i, simNum = j, ran = 1)
    # Random intercept - study only
    ran2 <- simulate.ipdma(nStudies = nStudies,
                                nPats = nPats,
                                times = 1:100,
                                FPs = FPList[[i]],
                                betas = betaList[[i]],
                                int1 = arm::logit(0.2),
                                int2 = int2s[i],
                                ranInt.stu = 0.05) %>%
        dplyr::mutate(scen = i, simNum = j, ran = 2)
    # Random intercept - patient and study
    ran3 <- simulate.ipdma(nStudies = nStudies,
                                nPats = nPats,
                                times = 1:100,
                                FPs = FPList[[i]],
                                betas = betaList[[i]],
                                int1 = arm::logit(0.2),
                                int2 = int2s[i],
                                ranInt.stu = 0.05,
                                ranInt.pat = 0.01) %>%
        dplyr::mutate(scen = i, simNum = j, ran = 3)
    toReturn <- ran1 %>% rbind(ran2, ran3)
    return(toReturn)
    }
  }

stopCluster(cl)




