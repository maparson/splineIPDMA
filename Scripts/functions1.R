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
    betas = NULL # FP power coefficients
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
    probs <- tibble(
      timePoint = 1:nTimes,
      timeScale = (1:nTimes)/max(timesTotal),
      linPred = FPeval(FPs = FPs, betas = betas, x = timeScale)
    )
    if (outcomeType == "binary") {
      probs <- probs %>%
        mutate(transPred = invlogit(linPred))
      simOutcomesTibble = tibble(
        timePoint = probs$timePoint,
        timeScale = probs$timeScale,
        outcome = rbinom(nrow(probs), 1, probs$transPred)
        )
      simOutcomes <- simOutcomesTibble %>% pull(outcome)
      return(simOutcomes)}}

simulate.one.study <- function(
    nPats = 10,
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    FPs = NULL, # FP powers
    betas = NULL # FP power coefficients
) {
  simStudy = tibble(
    patID = numeric(),
    timePoint = numeric(),
    timeScale = numeric(),
    outcome = numeric()
  )
  timesTotal = times[1]:times[length(times)]
  nTimes = length(timesTotal)
  for (j in 1:nPats) {
    simOutcome = simulate.one.pat(times = times,
                                  FPs = FPs,
                                  betas = betas)
    # simPatMat = matrix(c(1:timesTotal,simOutcome),
    #                    nrow = nTimes)
    simPatTib = tibble(
      patID = rep(j, nTimes),
      timePoint = 1:nTimes,
      timeScale = (1:nTimes)/max(timesTotal),
      outcome = simOutcome
    )
    simStudy = simStudy %>%
      add_row(simPatTib)
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
    betas = NULL # FP power coefficients
) {
  simIPDMA = tibble(
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
                                  betas = betas)
    # simPatMat = matrix(c(1:timesTotal,simOutcome),
    #                    nrow = nTimes)
    simIPDMAStudy = simStudy %>%
      mutate(studyID = j) %>%
      dplyr::select(studyID, everything())
    simIPDMA = simIPDMA %>%
      add_row(simIPDMAStudy)
  }
  return(simIPDMA)
}

