#------------------------#
# Pregnancy trajectories Data Load Initial
#------------------------#

rm(list = ls())
source("./Scripts/packages.R")

#------------------------------------------------------------------------------#
#-- Load dat.rawa -----------------------------------------------------------------#
dat.raw <- read.csv("./Data/Raw Data/EPDS_trajectory_2021-11-09.csv")
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- Clean dat.rawa ----------------------------------------------------------------#
#---- Collapse the MDD/MDE stuff ----------------------------------------------#
#------ NA the 99s ------------------------------------------------------------#
dat.raw1 <- dat.raw %>% 
  as_tibble() %>%
  mutate(MDD_DICH_DSM = if_else(MDD_DICH_DSM == 99, as.integer(NA), MDD_DICH_DSM),
         MDD_DICH_ICD = if_else(MDD_DICH_ICD == 99, as.integer(NA), MDD_DICH_ICD),
         MDE_DICH_DSM = if_else(MDE_DICH_DSM == 99, as.integer(NA), MDE_DICH_DSM),
         MDE_DICH_ICD = if_else(MDE_DICH_ICD == 99, as.integer(NA), MDE_DICH_ICD)) %>%
  #mutate(diag = coalesce(MDE_DICH_ICD, MDE_DICH_DSM, MDD_DICH_DSM, MDD_DICH_ICD)) %>% # This misses some people with MDE_DICH_DSM == 1 & MDD_DICH_DSM == 0
  group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD, DATA_POINT) %>%
  mutate(diag = case_when(
    sum(MDE_DICH_ICD, MDE_DICH_DSM, MDD_DICH_DSM, MDD_DICH_ICD, na.rm = TRUE) > 0 ~ 1,
    TRUE ~ 0)) %>%
  # mutate(diagDSM = coalesce(MDD_DICH_DSM, MDE_DICH_DSM),
  #        diagICD = coalesce(MDD_DICH_ICD, MDE_DICH_ICD),
  #        diag = if_else(diagDSM == 1 | diagICD == 1, 1, 0)) %>%
  #------ Get the week midpoints ------------------------------------------------#
  mutate(
    week_midpoint = case_when( 
      str_detect(TIME_RANGE, "Between 12 and 16 weeks pregnant") ~ (16-12)/2 + 12,
      str_detect(TIME_RANGE, "Between 22 and 26 weeks pregnant") ~ (26-22)/2 + 22,
      str_detect(TIME_RANGE, "Between 32 and 36 weeks pregnant") ~ (36-32)/2 + 32,
      str_detect(TIME_RANGE, "Between 35 and 39 weeks pregnant") ~ (39-35)/2 + 35,
      str_detect(TIME_RANGE, "Between 4 and 6 weeks postpartum") ~ (6-4)/2 + 6 + 40, # Assume pregnancy is 40 weeks
      str_detect(TIME_RANGE, "Between 8 and 34 weeks pregnant") ~ (34-8)/2 + 8,
      GEST_TRIMESTER == 1 ~ 13/2,
      GEST_TRIMESTER == 2 ~ (29-13)/2 + 13,
      GEST_TRIMESTER == 3 ~ (40-29)/2 + 29,
      POST_QUARTILE == 1 ~ 13/2,
      POST_QUARTILE == 2 ~ (26-13)/2 + 13,
      POST_QUARTILE == 3 ~ (39-26)/2 + 26,
      POST_QUARTILE == 4 ~ (52-39)/2 + 39
    )) %>% 
  #------ Get the derived trimester/quartile ------------------------------------#
  mutate(trimquart = case_when( #old trimquart that did not take length of trim/quarters into account
    GEST_TRIMESTER == 1 ~ 1,
    GEST_TRIMESTER == 2 ~ 2, 
    GEST_TRIMESTER == 3 ~ 3,
    POST_QUARTILE == 1 ~ 4,
    POST_QUARTILE == 2 ~ 5,
    POST_QUARTILE == 3 ~ 6,
    POST_QUARTILE == 4 ~ 7,
    week_midpoint >= 0 & week_midpoint < 13 ~ 1,
    week_midpoint >=13 & week_midpoint < 29 ~ 2,
    week_midpoint >=19 & week_midpoint < 40 ~ 3,
    week_midpoint >=40 & week_midpoint < 40+13 ~ 4,
    week_midpoint >=40+13 & week_midpoint < 40+26 ~ 5,
    week_midpoint >=40+26 & week_midpoint < 40+39 ~ 6,
    week_midpoint >=40+39 & week_midpoint < 99 ~ 7,
  )) %>%
  #------ NA the time points ----------------------------------------------------#
  mutate(GEST_WEEK = if_else(GEST_WEEK %in% c(99,888,999), as.integer(NA), GEST_WEEK),
         POST_WEEK = if_else(POST_WEEK %in% c(99,888,999), as.numeric(NA), POST_WEEK)) %>%  
  mutate(POST_WEEK2 = POST_WEEK + 40) %>% # Assume pregnancy is 40 weeks
  #------ Coalesce the derived weeks --------------------------------------------#
  mutate(week_deriv = coalesce(GEST_WEEK, POST_WEEK2, week_midpoint)) %>%
  #------------------------------------------------------------------------------#
  #---- DATA CHANGES ------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  # mutate(
  #   #------ Change Yonkers 28 week to third trimester -----------------------------# TOOK THIS OUT FOR NOW
  #   # trimquart = case_when( 
  #   #   GEST_TRIMESTER == 2 & str_detect(STUDY_AUTHOR_YEAR, "Yonker") == TRUE & GEST_WEEK == 28 ~ 3,
  #   #   TRUE ~ trimquart),
  #   #------ filter out Roomruangwong week 1 post birth ---------------------------#
  #   filter_out = case_when(
  #     str_detect(STUDY_AUTHOR_YEAR, "Roomruangwong") & POST_WEEK == 1 ~ 1,
#     TRUE ~ 0)#,
#   # #------ Change Felice trimester 3 to 2 for ease of smoothing ---------------------------#
#   # trimquart = case_when(
#   #   str_detect(STUDY_AUTHOR_YEAR, "Fel") & GEST_TRIMESTER == 3 ~ 2,
#   #   TRUE ~ trimquart)
# ) %>% 
# filter(filter_out != 1) %>%
# select(-filter_out) %>%
#------ Get the change indicators ---------------------------------------------#
arrange(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD, DATA_POINT) %>%
  group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
  mutate(
    n_timepoints = dplyr::n(),
    prev_diag = lag(diag, 1),
    first_timepoint = min(trimquart),
    min_DATA_POINT = min(DATA_POINT),
    min_trimquart = min(trimquart),
    diag_max = max(diag, na.rm = TRUE),
    diag_min = min(diag, na.rm = TRUE),
    any_change = as.numeric(diag_max != diag_min)) %>%
  # - get patient ID
  group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
  mutate(patID = cur_group_id(),
         patIDfact = as.factor(patID)) %>% ungroup()


#---- Get the diagnosis letter string -----------------------------------------# MOVED THIS OUT SINCE IT DOESNT MATTER
dat.raw2 <- dat.raw1 #%>%
#   select(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD, min_trimquart,
#          week_deriv, n_timepoints,
#          trimquart, diag, diag_max, diag_min, any_change, STAT_WEIGHT, patID) %>%
#   group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
#   complete(trimquart = 1:7) %>%
#   mutate(
#     letter_NA = case_when(
#       is.na(diag) ~ as.character(NA),
#       diag == 1 ~ "D",
#       diag == 0 ~ "W"),
#     letter = case_when(
#       is.na(diag) ~ "N",
#       diag == 1 ~ "D",
#       diag == 0 ~ "W"),
#     last_letter = case_when(
#       trimquart == min_trimquart ~ as.character(NA),
#       is.na(letter_NA) ~ as.character(NA),
#       TRUE ~ coalesce(lag(letter_NA,1),
#                       lag(letter_NA,2),
#                       lag(letter_NA,3),
#                       lag(letter_NA,4),
#                       lag(letter_NA,5),
#                       lag(letter_NA,6),
#                       lag(letter_NA,7))),
#     change = case_when(
#       trimquart == min_trimquart ~ -1,
#       is.na(letter) ~ as.numeric(NA),
#       letter != last_letter ~ 1,
#       letter == last_letter ~ 0),
#     diagstring = case_when(
#       trimquart == 7 ~ paste0(lag(letter,6),
#                               lag(letter,5),
#                               lag(letter,4),
#                               lag(letter,3),
#                               lag(letter,2),
#                               lag(letter,1),
#                               lag(letter,0)),
#       TRUE ~ as.character(NA)),
#     n_points = dplyr::n()) %>% # check that there are 7 timepoints per person
#   group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
#   fill(diagstring, .direction = "up")
dat.raw3 <- dat.raw2 #%>%
#   filter(is.na(letter_NA) == FALSE) %>%
#   mutate(
#     dep_new = if_else(
#       n_timepoints > 1 & last_letter == "W" & letter == "D", 
#       1, 0),
#     dep_old = if_else(
#       n_timepoints > 1 & last_letter == "D" & letter == "D", 
#       1, 0),
#     dep_1st = if_else(
#       is.na(last_letter) & letter == "D", 
#       1, 0),
#     wel_new = if_else(
#       n_timepoints > 1 & last_letter == "D" & letter == "W", 
#       1, 0),
#     wel_old = if_else(
#       n_timepoints > 1 & last_letter == "W" & letter == "W", 
#       1, 0),
#     wel_1st = if_else(
#       is.na(last_letter) & letter == "W", 
#       1, 0))

#---- Generate the new trimquart variables ------------------------------------#
dat.raw4 <- dat.raw3  %>% 
  mutate(trimquart_weeks = case_when( 
    trimquart == 1 ~ (13- 0)/2,
    trimquart == 2 ~ (29-13)/2 + 13,
    trimquart == 3 ~ (40-29)/2 + 29,
    trimquart == 4 ~ (13- 0)/2 + 40,
    trimquart == 5 ~ (26-13)/2 + 40 + 13,
    trimquart == 6 ~ (39-26)/2 + 40 + 29,
    trimquart == 7 ~ (52-39)/2 + 40 + 39,
  )) %>% 
  mutate(trimonth = trimquart_weeks/4)


#---- Generate the trimquart strings ------------------------------------#
dat.rawTrimquart <- dat.raw3 %>% 
  group_by(patID, STUDY_AUTHOR_YEAR) %>%
  group_by(patID) %>%
  mutate(DATPOINT = row_number()) %>%
  group_by(patID, trimquart) %>%
  mutate(diag = max(diag), DATPOINT = min(DATPOINT)) %>% # Use this to merge the ones with multiple obs per trimquart
  distinct(STUDY_AUTHOR_YEAR, patID, trimquart, DATPOINT) %>%
  pivot_wider(id_cols = c(STUDY_AUTHOR_YEAR, patID), names_from = DATPOINT, values_from = trimquart, names_prefix = "o") %>% ungroup() %>%
  mutate(across(c(3,4,5,6), ~ substr(paste0(.),1,1))) %>%
  mutate(trimquartString = str_remove_all(paste0(o1,o2,o3,o4), "N"),
         trimquartStringN = paste0(o1,o2,o3,o4)) %>%
  mutate(tq1 = as.numeric(str_detect(trimquartString, "1")),
         tq2 = as.numeric(str_detect(trimquartString, "2")),
         tq3 = as.numeric(str_detect(trimquartString, "3")),
         tq4 = as.numeric(str_detect(trimquartString, "4")),
         tq5 = as.numeric(str_detect(trimquartString, "5")),
         tq6 = as.numeric(str_detect(trimquartString, "6")),
         tq7 = as.numeric(str_detect(trimquartString, "7")))
#---- merge this back into the dat.rawa
dat.raw5 <- dat.raw4 %>%
  left_join(dat.rawTrimquart)

#---- Get the patients with more than one timepoint only ----------------------#
dat.raw_longit <- dat.raw5 %>% filter(n_timepoints > 1) %>%
  #---- and add in centered & scaled week variable
  ungroup() %>%
  mutate(week_scale = scale(week_deriv)) %>%
  #---- and add in completely unique ID for each person (even between studies) MOVED EARLIER TO dat.raw1
  # group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
  # mutate(patID = cur_group_id(),
  #        patIDfact = as.factor(patID)) %>% ungroup() %>%
  #---- and add in a study ID
  group_by(STUDY_AUTHOR_YEAR) %>%
  mutate(stID = cur_group_id(),
         stIDfact = as.factor(stID)) %>% ungroup() %>%
  #------ Get the observation number by patient ---------------------------------#
  ungroup() %>% group_by(stID, patID) %>%
  mutate(obsNum = seq_along(patID))


#---- Get the same dat.rawaset as dat.raw_longit but including people with only one point 
dat.raw_all <- dat.raw5 %>% filter(n_timepoints > 0) %>%
  #---- and add in centered & scaled week variable
  ungroup() %>%
  mutate(week_scale = scale(week_deriv)) %>%
  #---- and add in completely unique ID for each person (even between studies)
  group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
  mutate(patID = cur_group_id(),
         patIDfact = as.factor(patID)) %>% ungroup() %>%
  #---- and add in a study ID
  group_by(STUDY_AUTHOR_YEAR) %>%
  mutate(stID = cur_group_id(),
         stIDfact = as.factor(stID)) %>% ungroup() %>%
  #------ Get the observation number by patient ---------------------------------#
  ungroup() %>% group_by(stID, patID) %>%
  mutate(obsNum = seq_along(patID))

#-------- Remove attributes from week_scale -------------------------------------#
attributes(dat.raw_longit$week_scale) <- NULL
attributes(dat.raw_all$week_scale) <- NULL

#---- Get patient-level dat.rawa for trimquarts
dat.raw_participant <- dat.raw_all %>%
  select(colnames(dat.rawTrimquart)) %>%
  distinct() %>%
  mutate(n_obs = nchar(trimquartString))





#---- Generate the trimquart strings FOR DEPRESSED PEOPLE ONLY
dat.rawTrimquartD <- dat.raw3 %>% 
  filter(diag == 1) %>%
  group_by(patID, STUDY_AUTHOR_YEAR) %>%
  mutate(DATPOINT = row_number()) %>%
  distinct(STUDY_AUTHOR_YEAR, patID, trimquart, DATPOINT) %>%
  pivot_wider(id_cols = c(STUDY_AUTHOR_YEAR, patID), names_from = DATPOINT, values_from = trimquart, names_prefix = "o") %>% ungroup() %>%
  mutate(across(c(3,4,5,6), ~ substr(paste0(.),1,1))) %>%
  mutate(trimquartString = str_remove_all(paste0(o1,o2,o3,o4), "N"),
         trimquartStringN = paste0(o1,o2,o3,o4)) %>%
  mutate(tq1 = as.numeric(str_detect(trimquartString, "1")),
         tq2 = as.numeric(str_detect(trimquartString, "2")),
         tq3 = as.numeric(str_detect(trimquartString, "3")),
         tq4 = as.numeric(str_detect(trimquartString, "4")),
         tq5 = as.numeric(str_detect(trimquartString, "5")),
         tq6 = as.numeric(str_detect(trimquartString, "6")),
         tq7 = as.numeric(str_detect(trimquartString, "7")))
#---- merge this back into the dat.rawa
dat.raw5D <- dat.raw4 %>%
  left_join(dat.rawTrimquartD)
#---- Get the same dat.rawaset as dat.raw_longit but including people with only one point 
dat.raw_allD <- dat.raw5D %>% filter(n_timepoints > 0) %>%
  #---- and add in centered & scaled week variable
  ungroup() %>%
  mutate(week_scale = scale(week_deriv)) %>%
  #---- and add in completely unique ID for each person (even between studies)
  group_by(STUDY_AUTHOR_YEAR, PATIENT_ID_DEPRESSD) %>%
  mutate(patID = cur_group_id(),
         patIDfact = as.factor(patID)) %>% ungroup() %>%
  #---- and add in a study ID
  group_by(STUDY_AUTHOR_YEAR) %>%
  mutate(stID = cur_group_id(),
         stIDfact = as.factor(stID)) %>% ungroup() %>%
  #------ Get the observation number by patient ---------------------------------#
  ungroup() %>% group_by(stID, patID) %>%
  mutate(obsNum = seq_along(patID))
#---- Get patient-level dat.rawa for trimquarts
dat.raw_participantD <- dat.raw_allD %>%
  select(colnames(dat.rawTrimquartD), -diag) %>%
  distinct() %>%
  mutate(n_obs = nchar(trimquartString))




#-------------------------- SAVE THE IMPORTANT DATA -----------------------------#
write_csv(dat.raw_longit, "./Data/PregnancyData.csv")
rm(list = ls())
