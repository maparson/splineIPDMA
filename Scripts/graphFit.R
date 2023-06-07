#--------------------------------#
# Fit graphs script
#--------------------------------#

# Merge the fit datasets together
simFit.metafp.merge <- simFit.metafp %>% mutate(meth = "metafp")
simFit.mvfp.merge <- simFit.mvfp %>% mutate(meth = "mvfp")
simFit.meta.merge <- simFit.meta %>% mutate(meth = "meta") %>% dplyr::select(-starts_with("V"))
simFit.mv.merge <- simFit.mv %>% mutate(meth = "mv") %>% dplyr::select(-starts_with("V"))

simFit <- 
  simFit.meta.merge %>%
  bind_rows(simFit.mv.merge) %>%
  bind_rows(simFit.metafp.merge) %>%
  bind_rows(simFit.mvfp.merge) %>%
  mutate(
    trueOutcome = 
      case_when(
        scen == 1 ~ invlogit(FPeval(FPs = FP1, betas = beta1, x = timeScale, intercept = logit(0.2))),
        scen == 2 ~ invlogit(FPeval(FPs = FP2, betas = beta2, x = timeScale, intercept = logit(0.2))),
        scen == 3 ~ invlogit(FPeval(FPs = FP3, betas = beta3, x = timeScale, intercept = logit(0.2))),
        scen == 4 ~ invlogit(FPeval(FPs = FP4, betas = beta4, x = timeScale, intercept = logit(0.2)))+0.2
      ),
    difOutcome = outcome - trueOutcome,
    difPred = logit(outcome) - logit(trueOutcome))

# Faceted plot of fits by simulation and method
ggplot(data = simFit %>% filter(ran == 3, studyID == nStudies+1)) +
  geom_line(aes(x = timeScale,
                y = outcome,
                group = simNum),
            alpha = 0.2) +
  # geom_ribbon(aes(x = timeScale,
  #                 y = outcome,
  #                 group = simNum,
  #                 ymin = lower, ymax = upper),
  #             alpha = 0.1, linetype = "dashed") +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.2)))},
  #   colour = "red", linetype = "dotted", size = 1) +
  ylab("P(Outcome)") +
  xlab("Time") +
  facet_wrap(~meth+scen) +
  theme_clean()

# Faceted plot of difference from truth in outcome by simulation and method
ggplot(data = simFit %>% filter(ran == 3, studyID == nStudies+1)) +
  geom_line(aes(x = timeScale,
                y = difOutcome,
                group = simNum),
            alpha = 0.21) +
  geom_smooth(aes(x = timeScale,
                  y  = difOutcome)) +
  geom_hline(yintercept = 0, colour = "black") +
  # geom_ribbon(aes(x = timeScale,
  #                 y = outcome,
  #                 group = simNum,
  #                 ymin = lower, ymax = upper),
  #             alpha = 0.1, linetype = "dashed") +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.2)))},
  #   colour = "red", linetype = "dotted", size = 1) +
  ylab("Difference in P(Outcome)") +
  xlab("Time") +
  facet_wrap(~meth+scen) +
  theme_clean()

# Faceted plot of difference from truth in predictor by simulation and method
ggplot(data = simFit %>% filter(ran == 3, studyID == nStudies+1)) +
  geom_line(aes(x = timeScale,
                y = difPred,
                group = simNum),
            alpha = 0.21) +
  geom_smooth(aes(x = timeScale,
                  y  = difPred)) +
  geom_hline(yintercept = 0, colour = "black") +
  # geom_ribbon(aes(x = timeScale,
  #                 y = outcome,
  #                 group = simNum,
  #                 ymin = lower, ymax = upper),
  #             alpha = 0.1, linetype = "dashed") +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FP2, betas = beta2, x = x, intercept = logit(0.2)))},
  #   colour = "red", linetype = "dotted", size = 1) +
  ylab("Difference in predictor") +
  xlab("Time") +
  facet_wrap(~meth+scen) +
  theme_clean()

