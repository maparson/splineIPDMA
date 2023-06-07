#--------------------------------#
# Simulation function script
#--------------------------------#

simulate.one.pat <- 
  function(
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(0,1), # Possible outcomes. Binary for now, but extend to vector and continuous later.
    outcomeType = "binary",
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = c(0.9,0.9), # If binary, prob of staying at same outcome by unit of time by outcome.
    condProbScale = TRUE # If true, scale the probs of changing by initial prevalence so that we don't have a reversion to 0.5
  ) {
    nTimes = length(times)
    timesTotal = times[1]:times[length(times)]
    if (outcomeType == "binary") {
      if(condProbScale == TRUE) {
        condProbOR = initialPrevalence/(1-initialPrevalence)
        condProbWithin[2] <- 1-((1-condProbWithin[2])*condProbOdds)
      }
      simOutcome.init = as.numeric(rbernoulli(n = 1, p = initialPrevalence))
      changeSample = sample(outcome,
                            size = nTimes - 1,
                            prob = c(condProbWithin,
                                     1 - condProbWithin),
                            replace = TRUE)
      change = c(0, changeSample)
      simOutcomesTibble = tibble(
        outcome = rep(simOutcome.init, nTimes),
        timePoint = 1:nTimes,
        change = change) %>%
        mutate(outcome = (simOutcome.init + cumsum(change)) %% 2) # The number of changes is cumsum(change) mod 2
      simOutcomes <- simOutcomesTibble %>% pull(outcome)
      return(simOutcomes)}}

simulate.one.study <- function(
    nPats = 10,
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = 0.9 # If binary, prob of staying at same outcome by unit of time.
) {
  simStudy = tibble(
    patID = numeric(),
    timePoint = numeric(),
    outcome = numeric()
  )
  timesTotal = times[1]:times[length(times)]
  nTimes = length(timesTotal)
  for (j in 1:nPats) {
    simOutcome = simulate.one.pat(times = times,
                                  initialPrevalence = initialPrevalence,
                                  condProbWithin = condProbWithin)
    # simPatMat = matrix(c(1:timesTotal,simOutcome),
    #                    nrow = nTimes)
    simPatTib = tibble(
      patID = rep(j, nTimes),
      timePoint = 1:nTimes,
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
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = 0.9 # If binary, prob of staying at same outcome by unit of time.
) {
  simIPDMA = tibble(
    studyID = numeric(),
    patID = numeric(),
    timePoint = numeric(),
    outcome = numeric()
  )
  timesTotal = times[1]:times[length(times)]
  nTimes = length(timesTotal)
  for (j in 1:nStudies) {
    nPatsStudy = nPats[j]
    simStudy = simulate.one.study(times = times,
                                  initialPrevalence = initialPrevalence,
                                  condProbWithin = condProbWithin,
                                  nPats = nPatsStudy)
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

