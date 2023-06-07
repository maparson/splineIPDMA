#--------------------------------#
# Simulation function script
#--------------------------------#

simulate.one.pat <- 
  function(
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(0,1), # Possible outcomes. Binary for now, but extend to vector and continuous later.
    outcomeType = "binary",
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = c(0.9,0.9) # If binary, prob of staying at same outcome by unit of time by outcome.
  ) {
    nTimes = length(times)
    timesTotal = times[1]:times[length(times)]
    if (outcomeType == "binary") {
      pChange0 = (initialPrevalence/(1-initialPrevalence))*(1-condProbWithin[1])
      pChange1 = ((1-initialPrevalence)/initialPrevalence)*(1-condProbWithin[2])      
      simOutcome.init = as.numeric(rbernoulli(n = 1, p = initialPrevalence))
      simOutcomesTibble = tibble(
        outcome = c(simOutcome.init, rep(0, nTimes - 1)),
        timePoint = 1:nTimes,
        change = rep(0,nTimes)) %>%
        mutate(
          change = case_when(
            timePoint == 1 & outcome == 0 ~ as.numeric(rbernoulli(n = dplyr::n(), p = 1-condProbWithin[1])),
            timePoint == 1 & outcome == 1 ~ as.numeric(rbernoulli(n = dplyr::n(), p = 1-condProbWithin[2])),          
            lag(outcome,1) == 0 ~ as.numeric(rbernoulli(n = dplyr::n(), p = pChange0)),
            lag(outcome,1) == 1 ~ as.numeric(rbernoulli(n = dplyr::n(), p = pChange1))
        )) %>%
        mutate(outcome2 = (simOutcome.init + cumsum(change)) %% 2) # The number of changes is cumsum(change) mod 2
      simOutcomes <- simOutcomesTibble %>% pull(outcome2)
      return(simOutcomes)}}

simulate.one.study <- function(
    nPats = 10,
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = c(0.9, 0.9) # If binary, prob of staying at same outcome by unit of time.
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
    condProbWithin = c(0.9, 0.9) # If binary, prob of staying at same outcome by unit of time.
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

