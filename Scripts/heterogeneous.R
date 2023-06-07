#--------------------------------#
# Filter out the data based on study - heterogeneous exposure
#--------------------------------#

#- Heterogneity situation 1: 70% of data for all studies -----------------------------
simFit.meta.2 <- tibble()
simFit.mv.2 <- tibble()
simFit.mvfp.2 <- tibble()
simFit.metafp.2 <- tibble()
simDat <- simDat %>% filter(timePoint %in% 1:70 & studyID == 1 |
                              timePoint %in% 11:80 & studyID == 2 |
                              timePoint %in% 21:90 & studyID == 3 |
                              timePoint %in% 31:100 & studyID == 4)
for(i in 1:4) {
  testDati <- simDat %>% filter(scen == i)
  print(paste0("i = ", i))
  for(r in 1:3) {
    testDatr <- testDati %>% filter(ran == r)
    print(paste0("r = ", r))
    for(j in 1:nSim) {
      print(paste0("j = ", j))
      #--------- Filter data by study
      testDatj <- testDatr %>% filter(simNum == j)
      
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
      
      testFitj.meta.2 <- metaspline(dat = testDatj, knotN = 4) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.mv.2 <- mvspline(dat = testDatj, knotN = 4) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.mvfp.2 <- mvmeta(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.metafp.2 <- mvspline(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      
      #simDat <- simDat %>% bind_rows(testDatj)
      simFit.meta.2 <- simFit.meta.2 %>% bind_rows(testFitj.meta.2)
      simFit.mv.2 <- simFit.mv.2 %>% bind_rows(testFitj.mv.2)
      simFit.mvfp.2 <- simFit.mvfp.2 %>% bind_rows(testFitj.mvfp.2) 
      simFit.metafp.2 <- simFit.metafp.2 %>% bind_rows(testFitj.metafp.2) 
    }
  }
}

#testDattt <- simDat %>% filter(scen == 3, simNum == 4, ran == 3)
tt <- metaspline(dat = testDatj, knotN = 4)
tt <- mvmeta(dat = testDatj)
tt <- mvspline(dat = testDatj, knotN = 4)
tt <- metacurve(dat = testDatj)
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
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP3, betas = beta3, x = x, intercept = logit(0.2)))},
  #   colour = "brown", linetype = "dotted", size = 1) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP4, betas = beta4, x = x, intercept = logit(0.2)))+0.2},
    colour = "red", linetype = "dotted", size = 1) +
  ylab("P(Outcome)") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_clean()



#- Heterogneity situation 1: 40% of data for all studies -----------------------------
simFit.meta.4 <- tibble()
simFit.mv.4 <- tibble()
simFit.mvfp.4 <- tibble()
simFit.metafp.4 <- tibble()
simDat <- simDat %>% filter(timePoint %in% 1:25 & studyID == 1 |
                              timePoint %in% 26:50 & studyID == 2 |
                              timePoint %in% 51:75 & studyID == 3 |
                              timePoint %in% 76:100 & studyID == 4)
for(i in 1:4) {
  testDati <- simDat %>% filter(scen == i)
  print(paste0("i = ", i))
  for(r in 1:3) {
    testDatr <- testDati %>% filter(ran == r)
    print(paste0("r = ", r))
    for(j in 1:nSim) {
      print(paste0("j = ", j))
      #--------- Filter data by study
      testDatj <- testDatr %>% filter(simNum == j)
      
      #--------- Generate new data
      # if(r == 1) { # No random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.4),
      #                             int2 = int2s[i]) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # } else if(r == 2) { # Study-level random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.4),
      #                             int2 = int2s[i],
      #                             ranInt.stu = 0.05) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # } else if(r == 3) { # Study-level random effects
      #   testDatj = simulate.ipdma(nStudies = nStudies,
      #                             nPats = nPats,
      #                             times = 1:100,
      #                             FPs = FPList[[i]],
      #                             betas = betaList[[i]],
      #                             int1 = logit(0.4),
      #                             int2 = int2s[i],
      #                             ranInt.stu = 0.05,
      #                             ranInt.pat = 0.01) %>%
      #     mutate(scen = i, simNum = j, ran = r)
      # }
      
      testFitj.meta.4 <- metaspline(dat = testDatj, knotN = 2) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.mv.4 <- mvspline(dat = testDatj, knotN = 2) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.mvfp.4 <- mvmeta(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      testFitj.metafp.4 <- mvspline(dat = testDatj) %>% mutate(scen = i, simNum = j, ran = r)
      
      #simDat <- simDat %>% bind_rows(testDatj)
      simFit.meta.4 <- simFit.meta.4 %>% bind_rows(testFitj.meta.4)
      simFit.mv.4 <- simFit.mv.4 %>% bind_rows(testFitj.mv.4)
      simFit.mvfp.4 <- simFit.mvfp.4 %>% bind_rows(testFitj.mvfp.4) 
      simFit.metafp.4 <- simFit.metafp.4 %>% bind_rows(testFitj.metafp.4) 
    }
  }
}

#testDattt <- simDat %>% filter(scen == 3, simNum == 4, ran == 3)
tt <- metaspline(dat = testDatj, knotN = 2)
tt <- mvmeta(dat = testDatj)
tt <- mvspline(dat = testDatj, knotN = 2)
tt <- metacurve(dat = testDatj)
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
              alpha = 0.4) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP1, betas = beta1, x = x, intercept = logit(0.4)))},
  #   colour = "darkgrey", linetype = "dotted", size = 1) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.4)))},
  #   colour = "green", linetype = "dotted", size = 1) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP3, betas = beta3, x = x, intercept = logit(0.4)))},
  #   colour = "brown", linetype = "dotted", size = 1) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP4, betas = beta4, x = x, intercept = logit(0.2)))+0.2},
    colour = "red", linetype = "dotted", size = 1.2) +
  ylab("P(Outcome)") +
  xlab("Time") +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_clean()




