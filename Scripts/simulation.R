#--------------------------------#
# Simulation script
#--------------------------------#

#set.seed(1234)
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

#Graphs of the scenarios
ggsim1 <- 
ggplot() +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP1, betas = beta1, x = x, intercept = logit(0.2)))},
    colour = "black", linetype = "dotted", size = 1.2) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.2)))},
    colour = "green", linetype = "dotted", size = 1.2) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP3, betas = beta3, x = x, intercept = logit(0.2)))},
    colour = "blue", linetype = "dotted", size = 1.2) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP4, betas = beta4, x = x, intercept = logit(0.2)))+0.2},
    colour = "red", linetype = "dotted", size = 1.2) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  ylab("P(Outcome)") +
  xlab("Time") +
  theme_clean(base_size = 15) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())


ggsave(ggsim1, filename = "ggsim.png",  
       device='png', 
       dpi=1400, bg = "white"
)


simDat <- tibble(
  scen = numeric(),
  ran = numeric(),
  simNum = numeric(), 
  studyID = numeric(),
  patID = numeric(),
  timePoint = numeric(),
  timeScale = numeric(),
  outcome = numeric()
)
simFit.meta <- tibble()
simFit.mv <- tibble()
simFit.mvfp <- tibble()
simFit.metafp <- tibble()
for(i in 1:4) {
  for(r in 1:3) {
    for(j in 1:nSim) {
      #--------- Use old data
      testDatj <- simDat %>% filter(scen == i, ran == r, simNum == j)
      
      #--------- Generate new data
      # if(r == 1) { # No random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.2),
      #                             int2 = int2s[i]) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # } else if(r == 2) { # Study-level random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.2),
      #                             int2 = int2s[i],
      #                             ranInt.stu = 0.05) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # } else if(r == 3) { # Study-level random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.2),
      #                             int2 = int2s[i],
      #                             ranInt.stu = 0.05,
      #                             ranInt.pat = 0.01) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # }
      
      #testFitj.meta <- metaspline(dat = testDatj, knotN = 4) %>% mutate(scen = i, simNum = j, ran = r)
      #testFitj.mv <- mvspline(dat = testDatj, knotN = 4) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.mvfp <- mvmeta(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      #testFitj.metafp <- mvspline(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      
      #simDat <- simDat %>% bind_rows(testDatj)
      #simFit.meta <- simFit.meta %>% bind_rows(testFitj.meta)
      #simFit.mv <- simFit.mv %>% bind_rows(testFitj.mv)
      simFit.mvfp <- simFit.mvfp %>% bind_rows(testFitj.mvfp) 
      #simFit.metafp <- simFit.metafp %>% bind_rows(testFitj.metafp) 
    }
  }
}

testDatatt <- simDat %>% filter(scen == 3, simNum == 1, ran == 3)
tt <- metaspline(dat = testDatatt, knotN = 4)
tt <- mvmeta(dat = testDatatt)
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
              alpha = 0.2) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP1, betas = beta1, x = x, intercept = logit(0.2)))},
  #   colour = "darkgrey", linetype = "dotted", size = 1) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.2)))},
  #   colour = "green", linetype = "dotted", size = 1) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP3, betas = beta3, x = x, intercept = logit(0.2)))},
    colour = "brown", linetype = "dotted", size = 1) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP4, betas = beta4, x = x, intercept = logit(0.2)))+0.2},
  #   colour = "red", linetype = "dotted", size = 1) +
  ylab("P(Outcome)") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_clean()

ggsave(goodPlot4, filename = "gootplot4.png",  
       device='png', 
       dpi=1400, bg = "white"
)
