#--------------------------------#
# Test MVmeta Script
#--------------------------------#

#--- graph the trajectory and CI
nStudies = 3
nPats = c(10,50,100,250,500)[1:nStudies]
FPs = c(1/2,2)
betas = c(-5,5)
testDat = simulate.ipdma(nStudies = nStudies, 
                         nPats = nPats,
                         times = 1:100,
                         FPs = FPs,
                         betas = betas)
tt <- mvspline(dat = testDat)
ttStudies <- tt %>% filter(studyID <= nStudies) # Get only the studies
ttMA <- tt %>% anti_join(ttStudies) # Get only the MA

ggplot() +
  geom_line(data = ttStudies,
            aes(x = timeScale,
                y = outcome,
                group = studyID,
                colour = as.factor(studyID))) +
  geom_ribbon(data = ttStudies,
              aes(x = timeScale,
                  y = outcome,
                  group = studyID,
                  colour = as.factor(studyID),
                  fill = as.factor(studyID),
                  ymin = lower, ymax = upper),
              alpha = 0.1, linetype = "dashed") +
  geom_line(data = ttMA,
            aes(x = timeScale,
                y = outcome)) +
  geom_ribbon(data = ttMA,
              aes(x = timeScale,
                  y = outcome,
                  ymin = lower, ymax = upper),
              alpha = 0.3) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FPs, betas = betas, x = x))},
    colour = "black", linetype = "dotted") +
  ylab("P(Outcome)") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_clean()
