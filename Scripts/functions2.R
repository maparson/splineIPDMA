#--------------------------------#
# Simulation function script
#--------------------------------#

source("./Scripts/fractional.R")

simulate.one.pat <- 
  function(
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(0,1), # Possible outcomes. Binary for now, but extend to vector and continuous later.
    outcomeType = "binary",
    FPs = NULL, # FP powers
    betas = NULL, # FP power coefficients
    ranInt.pat = 0, # Patient-level random intercept? If a value > 0, then that is the sigma.
    ranInt.stuVal = 0, # Study-level random intercept? This is taken as fixed for pat and generated at the study level.
    int1 = 0, # Initial prevalence for simple FPs,
    int2 = 0 # Additional term to force initial prevalence for complicated FPs
  ) {
    nTimes = length(times)
    timesTotal = times[1]:times[length(times)]
    # Pick the fractional polynomial stuff
    if(is.null(FPs)) {
      FPs <- sample(c(-2,-1,-1/2,0,1/2,1,2,3), size = 2, replace = TRUE)
    }
    if(is.null(betas)) {
      betas <- round(rnorm(n=2), 2)
    }
    #- patient-level random effect
    if(ranInt.pat > 0) {
      ranInt.patVal= rnorm(n = 1, mean = 0, sd = sqrt(ranInt.pat))
    } else {ranInt.patVal = 0}
    #- get the matrix of time and linear predictor for the outcome
    probs <- tidyr::tibble(
      timePoint = 1:nTimes,
      timeScale = (1:nTimes)/max(timesTotal),
      linPred = FPeval(FPs = FPs, betas = betas, x = timeScale, int = int1) + ranInt.patVal + ranInt.stuVal
    )
    if (outcomeType == "binary") {
      probs <- probs %>%
        dplyr::mutate(transPred = arm::invlogit(linPred) + int2) 
      simOutcomesTibble = tidyr::tibble(
        timePoint = probs$timePoint,
        timeScale = probs$timeScale,
        outcome = rbinom(nrow(probs), 1, probs$transPred)
        )
      simOutcomes <- simOutcomesTibble %>% dplyr::pull(outcome)
      return(simOutcomes)}}

simulate.one.study <- function(
    nPats = 10,
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    FPs = NULL, # FP powers
    betas = NULL, # FP power coefficients
    ranInt.pat = 0, # Patient-level random intercept? If a value > 0, then that is the sigma.
    ranInt.stu = 0, # Study-level random intercept? This is taken as fixed for pat and generated at the study level.
    int1 = 0, # Initial prevalence for simple FPs,
    int2 = 0 # Additional term to force initial prevalence for complicated FPs
) {
  simStudy = tidyr::tibble(
    patID = numeric(),
    timePoint = numeric(),
    timeScale = numeric(),
    outcome = numeric()
  )
  timesTotal = times[1]:times[length(times)]
  nTimes = length(timesTotal)
  #- study-level random effect
  if(ranInt.stu > 0) {
    ranInt.stuVal= rnorm(n = 1, mean = 0, sd = sqrt(ranInt.stu))
  } else {ranInt.stuVal = 0}
  for (j in 1:nPats) {
    simOutcome = simulate.one.pat(times = times,
                                  FPs = FPs,
                                  betas = betas,
                                  ranInt.pat = ranInt.pat,
                                  ranInt.stuVal = ranInt.stuVal,
                                  int1 = int1, 
                                  int2 = int2)
    # simPatMat = matrix(c(1:timesTotal,simOutcome),
    #                    nrow = nTimes)
    simPatTib = tidyr::tibble(
      patID = rep(j, nTimes),
      timePoint = 1:nTimes,
      timeScale = (1:nTimes)/max(timesTotal),
      outcome = simOutcome
    )
    simStudy = simStudy %>%
      tibble::add_row(simPatTib)
  }
  return(simStudy)
}

simulate.ipdma<- function(
    nStudies = 10, # Number of studies
    nPats = rep(10,10), # Vector of study sizes
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    FPs = NULL, # FP powers
    betas = NULL, # FP power coefficients
    ranInt.pat = 0, # Patient-level random intercept? If a value > 0, then that is the sigma.
    ranInt.stu = 0, # Study-level random intercept? This is taken as fixed for pat and generated at the study level.
    int1 = 0, # Initial prevalence for simple FPs,
    int2 = 0 # Additional term to force initial prevalence for complicated FPs
) {
  simIPDMA = tidyr::tibble(
    studyID = numeric(),
    patID = numeric(),
    timePoint = numeric(),
    timeScale = numeric(),
    outcome = numeric()
  )
  timesTotal = times[1]:times[length(times)]
  nTimes = length(timesTotal)
  for (j in 1:nStudies) {
    nPatsStudy = nPats[j]
    simStudy = simulate.one.study(nPats = nPatsStudy,
                                  times = times,
                                  FPs = FPs,
                                  betas = betas,
                                  ranInt.pat = ranInt.pat,
                                  ranInt.stu = ranInt.stu,
                                  int1 = int1,
                                  int2 = int2)
    # simPatMat = matrix(c(1:timesTotal,simOutcome),
    #                    nrow = nTimes)
    simIPDMAStudy = simStudy %>%
      dplyr::mutate(studyID = j) %>%
      dplyr::select(studyID, patID, everything())
    simIPDMA = simIPDMA %>%
      tibble::add_row(simIPDMAStudy)
  }
  return(simIPDMA)
}

