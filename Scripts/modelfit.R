#--------------------------------#
# Model fit
#--------------------------------#

#Create function for the curves for each scenario
draw_curve <-
  function(x, FP, betas, int2) {
    invlogit(FPeval(
      FPs = FP,
      betas = betas,
      x = x,
      intercept = logit(0.2)
    )) + int2
  }

#Create function to estimate the KLdivergence
KLdiscrete <- function(Qx, Px) {
  vect1 = Qx*log(Qx/Px)
  vect2 = Px*log(Px/Qx)
  sum1 = sum(vect1)
  sum2 = sum(vect2)
  KLd = sum1 + sum2
  return(KLd)
}

#Get the model fits
modFit <- tibble(
  method = character(),
  scen = numeric(),
  ran = numeric(),
  simNum = numeric(),
  KLd = numeric()
)
methodList <- c("meta", "mv", "mvfp", "metafp")
for(meth in methodList) {
  simFit.meth <- get(paste0("simFit.", meth,".2"))
  for (j in 1:4) {
    for (r in 1:3) {
      for (s in 1:nSim) {
        simFit.methjrs <-
          simFit.meth %>% filter(scen == j, ran == r, simNum == s,
                                 studyID == nStudies + 1) # Get the MA results and not study-by-study
        KL.methjrs <- KLdiscrete(
          simFit.methjrs$outcome,
          draw_curve(
            simFit.methjrs$timeScale,
            FP = FPList[[j]],
            betas = betaList[[j]],
            int2 = int2s[[j]]
          )
        )
        modFit.methjrs <- tibble(
          method = meth,
          scen = j,
          ran = r,
          simNum = s,
          KLd = KL.methjrs
        )
        modFit <- modFit %>% bind_rows(modFit.methjrs)
      }
    }
  }
}

#Get the summary of the model fits
modSummary <- modFit %>%
  ungroup() %>%
  group_by(method, scen, ran) %>%
  summarise(KLdm = mean(KLd),
            KLDs = sd(KLd))




