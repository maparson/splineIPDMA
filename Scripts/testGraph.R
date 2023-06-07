#--------------------------------#
# Test Graph Script
#--------------------------------#

testDat = simulate.ipdma(nStudies = 3, 
                         nPats = c(10,100,50),
                         times = 1:100,
                         FPs = c(1/2,1),
                         betas = c(0.005,-0.005))

ggplot(data = testDat) +
  geom_smooth(aes(group = studyID, x = timePoint, y = outcome), 
              se = FALSE,
              method = "loess") +
  geom_smooth(aes(x = timePoint, y = outcome),
              se = FALSE,
              method = "loess",
              colour = "black") +
  #scale_x_continuous(breaks = 1:100) +
  xlab("Day") +
  ylab("Prevalence")
