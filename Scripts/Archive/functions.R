#--------------------------------#
# Simulation function script
#--------------------------------#

simulate.one.pat <- 
  function(
    times = c(1,2), # Time points to simulate data at. Conceptually days.
    outcome = c(1,0), # Possible outcomes. Binary for now, but extend to vector and continuous later. Event (1) comes first.
    outcomeType = "binary",
    initialPrevalence = 0.1, # If binary, initial prevalence of the first outcome
    condProbWithin = 0.9 # If binary, prob of staying at same outcome by unit of time.
  ) {
    nTimes = length(times)
    timesTotal = times[1]:times[length(times)]
    if (outcomeType == "binary") {
      simOutcomes = c()
      for(i in 1:nTimes) {
        if(i == 1) {
          simOutcome.i = sample(outcome,
                                size = 1, 
                                prob = c(initialPrevalence,
                                         1 - initialPrevalence))
          simOutcomes = c(simOutcome.i)
          } 
        if(i != 1) {
          otherOutcome = outcome[outcome != simOutcome.i]
          simOutcome.i = sample(c(simOutcome.i, otherOutcome),
                                size = 1,
                                prob = c(condProbWithin, 
                                         1 - condProbWithin))
          simOutcomes = c(simOutcomes, simOutcome.i)
        }
      }
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

