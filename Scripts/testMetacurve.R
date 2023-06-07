#--------------------------------#
# Test Metaspline Script
#--------------------------------#
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(12345)
#--- randomise the FP and beta selection
 FPs <- sample(c(-2,-1,-1/2,0,1/2,1,2,3), size = 2, replace = TRUE)
 betas <- round(rnorm(n=2), 2)
#--- graph the trajectory and CI
nStudies = 2
nPats = c(10,50,50,250,500)[1:nStudies]
FPs = c(1/2,2)
betas = c(-0.5,2)
testDat = simulate.ipdma(nStudies = nStudies, 
                         nPats = nPats,
                         times = 1:100,
                         FPs = FPs,
                         betas = betas,
                         ranInt.pat = 0.5,
                         ranInt.stu = 0.5)
tt <- metaspline(dat = testDat, ranInt = FALSE)
ttStudies <- tt %>% filter(studyID <= nStudies) # Get only the studies
ttMA <- tt %>% anti_join(ttStudies) # Get only the MA

ggexample <- ggplot() +
  geom_line(data = ttStudies,
            aes(x = timeScale,
                y = outcome,
                group = studyID,
                colour = as.factor(studyID)), size = 1) +
  geom_ribbon(data = ttStudies,
              aes(x = timeScale,
                  y = outcome,
                  group = studyID,
                  colour = as.factor(studyID),
                  fill = as.factor(studyID),
                  ymin = lower, ymax = upper),
              alpha = 0.08, linetype = "dashed") +
  geom_line(data = ttMA,
            aes(x = timeScale,
                y = outcome), colour = "black", size = 1.3) +
  geom_ribbon(data = ttMA,
              aes(x = timeScale,
                  y = outcome,
                  ymin = lower, ymax = upper),
              alpha = 0.3, colour = "black") +
   geom_function(fun = function(x) {invlogit(
     FPeval(FPs = FPs, betas = betas, x = x))},
     colour = cbPalette[6], linetype = "dashed", size = 1.5) +
  ylab("P(Outcome)") +
  xlab("X") +
  scale_fill_manual(values = cbPalette[c(2,4)]) + 
  scale_colour_manual(values = cbPalette[c(2,4)]) +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_minimal(base_size = 16)


ggsave(ggexample, filename = "ExampleGraph2.png",  device='png', dpi=1000)


