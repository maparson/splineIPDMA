#--------------------------------#
# Real data example
#--------------------------------#

cbPalette <- c("peru", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "lavenderblush4")

#- Load in the raw data
datPregnancyRaw <- read_csv("./Data/PregnancyData.csv")

#- Change the data to meet the formats for my functions
datPregnancy <- datPregnancyRaw %>%
  #filter(stID != 5) %>%
  group_by(stID) %>%
  mutate(stID2 = cur_group_id()) %>% ungroup() %>%
  dplyr::select(stID2, patID, trimonth, diag, STUDY_AUTHOR_YEAR) %>%
  rename(studyID = stID2,
         patID = patID,
         timePoint = trimonth,
         outcome = diag) %>%
  mutate(timeScale = as.numeric(scale(timePoint))) %>%
  mutate(Months = (timeScale *3.504341) + 7.712934)


ttt1 <- metaspline(dat = datPregnancy)
ttt2 <- metacurve(dat = datPregnancy)
#ttt <- finalMat


# Get the unscaled months & add study names
ttt1 <- ttt1 %>%
  mutate(Months = (timeScale *3.504341) + 7.712934) %>% 
  left_join(datPregnancy %>% dplyr::select(STUDY_AUTHOR_YEAR, studyID) %>% distinct())
ttt2 <- ttt2 %>%
  mutate(Months = (timeScale *3.504341) + 7.712934) %>% 
  left_join(datPregnancy %>% dplyr::select(STUDY_AUTHOR_YEAR, studyID) %>% distinct())

ttStudies1 <- ttt1 %>% filter(studyID <= 9) # Get only the studies
ttMA1 <- ttt1 %>% anti_join(ttStudies1) %>% dplyr::select(-starts_with("V"))# Get only the MA 

ttStudies2 <- ttt2 %>% filter(studyID <= 9) # Get only the studies
ttMA2 <- ttt2 %>% anti_join(ttStudies2) %>% dplyr::select(-starts_with("V"))# Get only the MA 


gg1 <- 
  ggplot() +
  geom_line(data = ttStudies1,
            aes(x = Months,
                y = outcome,
                group = STUDY_AUTHOR_YEAR,
                colour = as.factor(STUDY_AUTHOR_YEAR)),
            size = 0.7) +
  geom_ribbon(data = ttStudies1,
              aes(x = Months,
                  y = outcome,
                  group = STUDY_AUTHOR_YEAR,
                  colour = as.factor(STUDY_AUTHOR_YEAR),
                  fill = as.factor(STUDY_AUTHOR_YEAR),
                  ymin = lower, ymax = upper),
              alpha = 0.1, linetype = "dashed") +
  geom_line(data = ttMA1,
            aes(x = Months,
                y = outcome), size = 1.3) +
  geom_ribbon(data = ttMA1,
              aes(x = Months,
                  y = outcome,
                  ymin = lower, ymax = upper),
              alpha = 0.5) +
  geom_vline(xintercept = 10, 
             linetype = "dotted") +
  annotate(
    "text", label = "Pregnancy",
    x = 8.5, y = 0.42, size = 4, 
  ) +
  annotate(
    "text", label = "Post-partum",
    x = 11.7, y = 0.42, size = 4, 
  ) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FPs, betas = betas, x = x))},
  #   colour = "black", linetype = "dotted") +
  ylab("P(Depression)") +
  xlab("Months after pregnancy start") +
  ylim(c(0,0.48)) +
    scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  ggtitle("Metaspline") +
  theme_clean(base_size = 15) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())


gg2 <- 
  ggplot() +
  geom_line(data = ttStudies2,
            aes(x = Months,
                y = outcome,
                group = STUDY_AUTHOR_YEAR,
                colour = as.factor(STUDY_AUTHOR_YEAR)),
            size = 0.7) +
  geom_ribbon(data = ttStudies2,
              aes(x = Months,
                  y = outcome,
                  group = STUDY_AUTHOR_YEAR,
                  colour = as.factor(STUDY_AUTHOR_YEAR),
                  fill = as.factor(STUDY_AUTHOR_YEAR),
                  ymin = lower, ymax = upper),
              alpha = 0.1, linetype = "dashed") +
  geom_line(data = ttMA2,
            aes(x = Months,
                y = outcome), size = 1.3) +
  geom_ribbon(data = ttMA2,
              aes(x = Months,
                  y = outcome,
                  ymin = lower, ymax = upper),
              alpha = 0.5) +
  geom_vline(xintercept = 10, 
             linetype = "dotted") +
  annotate(
    "text", label = "Pregnancy",
    x = 8.5, y = 0.42, size = 4, 
  ) +
  annotate(
    "text", label = "Post-partum",
    x = 11.7, y = 0.42, size = 4, 
  ) +
  # geom_function(fun = function(x) {invlogit(
  #   FPeval(FPs = FPs, betas = betas, x = x))},
  #   colour = "black", linetype = "dotted") +
  ylab("P(Depression)") +
  xlab("Months after pregnancy start") +
  ylim(c(0,0.48)) +
  scale_fill_manual(values = cbPalette) + 
  scale_colour_manual(values = cbPalette) +
  guides(colour = guide_legend(title = "Study ID"),
         fill = guide_legend(title = "Study ID")) +
  ggtitle("Metacurve") +
  theme_clean(base_size = 15) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())

# 
# 
pp1 <- ggpubr::ggarrange(gg1+ theme(
                     plot.margin = margin(l=0,)),
                   gg2+ theme(axis.title.y = element_blank(),
                              plot.margin = margin(r = 0)),
                  align = "hv", nrow = 1)

ggsave(pp1, filename = "test3.png",  
       device='png', 
       dpi=1400, bg = "white"
       )

#-------------------------------------------------------------------------
# LOESS graph

mot1 <- 
ggplot(data = datPregnancy,
       aes(x = Months,
           y = outcome)) +
  geom_smooth(
    aes(group = as.factor(studyID),
        colour = as.factor(studyID)),
    method = "loess", span = 1, se = FALSE) +
  #geom_smooth(method = "loess", span = 1, se = FALSE, colour = "black", linetype = "dashed") +
  scale_colour_manual(values = cbPalette) +
  geom_vline(xintercept = 10, 
             linetype = "dotted") +
  annotate(
    "text", label = "Pregnancy",
    x = 8.5, y = 0.42, size = 4, 
  ) +
  annotate(
    "text", label = "Post-partum",
    x = 11.7, y = 0.42, size = 4, 
  ) +  
  theme_clean(base_size = 15) +
  guides(colour = guide_legend(title = "Study ID")) +
  ylab("% w/ MDD or MDE") +
  xlab("Months after pregnancy start") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())

ggsave(mot1, filename = "motivatingExample1.png",  
       device='png', 
       dpi=1400, bg = "white")
  




# LOESS graph version 2 with line

mot2 <- 
  ggplot(data = datPregnancy,
         aes(x = Months,
             y = outcome)) +
  geom_smooth(
    aes(group = as.factor(studyID),
        colour = as.factor(studyID)),
    method = "loess", span = 1, se = FALSE) +
  geom_smooth(method = "loess", span = 1, se = FALSE, colour = "black", linetype = "dashed") +
  scale_colour_manual(values = cbPalette) +
  geom_vline(xintercept = 10, 
             linetype = "dotted") +
  annotate(
    "text", label = "Pregnancy",
    x = 8.5, y = 0.42, size = 4, 
  ) +
  annotate(
    "text", label = "Post-partum",
    x = 11.7, y = 0.42, size = 4, 
  ) +  
  theme_clean(base_size = 15) +
  guides(colour = guide_legend(title = "Study ID")) +
  ylab("% w/ MDD or MDE") +
  xlab("Months after pregnancy start") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill = 'transparent'),
        panel.border = element_blank())

ggsave(mot2, filename = "motivatingExample2.png",  
       device='png', 
       dpi=1400, bg = "white")













