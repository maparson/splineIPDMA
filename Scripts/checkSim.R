#--------------------------------#
# Simulation check script
#--------------------------------#

#- Check the patient-level generation
patMat <- tibble(
  patID = numeric(), 
  timePoint = numeric(), 
  timeScale = numeric(),
  outcome = numeric())
patIntList <- list()
NSims = 1000
FPs = c(-1,2)
betas = c(-1,-1)
for (i in 1:NSims) {
  outcomei <- simulate.one.pat(
    times = 1:100,
    FPs = FPs,
    betas = betas,
    ranInt.pat = 0.5)
  patMati <- tibble(
    patID = i,
    timePoint = 1:100,
    timeScale = timePoint/100,
    outcome = outcomei
  )
  patMat <- patMat %>% add_row(patMati)
  # Check the random intercept variance
  patFiti <- glm(data = patMati,
                 formula = outcome ~ ns(timeScale, 2),
                 family = binomial)
  patIntList[[length(patIntList)+1]] <- sigma(patFiti)
}

#--- regular GLM
patFit1 <- glm(data = patMat,
               formula = outcome ~ ns(timeScale, 2),
               family = binomial)
#--- random fit
patFit1R <- glmer(data = patMat,
                  formula = outcome ~ ns(timeScale, 2) + (1|patID),
                  family = binomial)

plot_model(patFit1, terms = "timeScale [all]", type = "pred") +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FPs, betas = betas, x = x))},
    colour = "black", linetype = "dotted") 



