#--------------------------------#
# SSC Results
#--------------------------------#

SSCRes1 <- read_csv("./Data/modfitSummary250patients.csv") %>% mutate(hetero = 1)
SSCRes2 <- read_csv("./Data/modfitSummary250patients2.csv") %>% mutate(hetero = 2)
SSCRes3 <- read_csv("./Data/modfitSummary250patients3.csv") %>% mutate(hetero = 3)

SSCRes <- bind_rows(SSCRes1, SSCRes2, SSCRes3) %>%
  dplyr::select(method, scen, ran, hetero, KLdm, KLDs) %>%
  mutate(scen = case_when(
    scen == 1 ~ "Linear",
    scen == 2 ~ "Mod. non-linear 1",
    scen == 3 ~ "Mod. non-linear 2",
    scen == 4 ~ "Non-linear")
  )

SSCRes$heteroL <- factor(SSCRes$hetero,
                         levels = 1:3,
                         labels = c("Full overlap", "Some overlap", "Little overlap"))

SSCRes$ranL <- factor(SSCRes$ran,
                     levels = 1:3,
                     labels = c("None","Study", "Study & pt."))
ggplot(data = SSCRes,
       aes(x = as.factor(method),
           colour = as.factor(method))) +
  geom_boxplot(aes(y = KLdm)) + 
  facet_grid(scen~ranL) +
  scale_y_log10()

ggplot(data = modFit,
       aes(x = as.factor(method),
           colour = as.factor(method))) +
  geom_violin(aes(y = KLd)) +
  facet_grid(scen~ran)



KLine <- 
ggplot(data = SSCRes,
       aes(x = as.factor(method),
           #colour = as.factor(method),
           group = as.factor(heteroL),
           colour = as.factor(heteroL))) +
  #geom_point(aes(y = KLdm)) + 
  geom_line(aes(y = KLdm),
            size = 1.2) +
  facet_grid(ranL~scen) +
  scale_y_log10() +
  theme_clean(base_size = 15) +
  ylab("KL divergence") +
  xlab("Method") + 
  scale_x_discrete(
    breaks = c("meta", "metafp", "mv", "mvfp"),
    labels = c(
      "metaspline", "metacurve", "mvspline",
      "mvcurve"),
    guide = guide_axis(n.dodge = 2)) +
  ggh4x::facet_nested("Random effects" + ranL ~ "Trajectory" + scen) +
  guides(colour=guide_legend(title="Study-level overlap"))+
  scale_colour_manual(values = c("#00BA38", "#619CFF", "#F8766D"))+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())


ggsave(KLine, filename = "KLine.png",  
       device='png', 
       dpi=1400, bg = "white"
)



#------------------------------------


datPlot <- simDat %>% filter(simNum == 2, ran == 1, scen == 4)
tttMs <- metaspline(dat = datPlot, knotN = 4)
tttMc <- metacurve(dat = datPlot)
tttVs <- mvspline(dat = datPlot, knotN = 4)
tttVc <- mvmeta(dat = datPlot)
#ttt <- finalMat


# Get the unscaled months & add study names
tttMs <- tttMs %>%
  mutate(meth = "metaspline")
tttMc <- tttMc %>%
  mutate(meth = "metacurve")
tttVs <- tttVs %>%
  mutate(meth = "mvspline")
tttVc <- tttVc %>%
  mutate(meth = "mvmeta")


TTT <- tttMs %>% bind_rows(tttMc, tttVs, tttVc)


ttStudies1 <- TTT %>% filter(studyID <= 4) # Get only the studies
ttMA1 <- TTT %>% anti_join(ttStudies1) %>% dplyr::select(-starts_with("V"))# Get only the MA 

#goodPlot4 <- 
  ggplot() +
  geom_line(data = ttMA1,
            aes(x = timeScale,
                y = outcome,
                group = as.factor(meth),
                colour = as.factor(meth)), size = 1.2) +
  geom_ribbon(data = ttMA1,
              aes(x = timeScale,
                  y = outcome, group = as.factor(meth), fill = as.factor(meth),
                  ymin = lower, ymax = upper),
              alpha = 0.5, linetype = 0) +
  ylab("Estimated P(Y=1)") +
  xlab("X") +
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  theme_clean(base_size = 15) +
    ylim(c(0,0.8)) +
  geom_function(fun = function(x) {invlogit(
    FPeval(FPs = FP4, betas = beta4, x = x, intercept = logit(0.2)))+0.2},
    colour = "black", linetype = "dotted", size = 1.2) +
  facet_wrap(~meth) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())

ggsave(goodPlot4, filename = "goodPlot4.png",  
       device='png', 
       dpi=1400, bg = "white"
)

