legal_problems <- c(
  "A1", "A2", "A3", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "D1", "D2", "D3", "D4", "D5", "D6", "E1",
  "E2", "E3", "F1", "F2", "G1", "G2", "G3", "H1", "H2", "H3", "I1", "J1", "J2", "J3", "J4", "K1", "K2", "K3",
  "L1", "L2"
)
legprob_bin <- paste0("AJP_", legal_problems, "_bin")
legprob_sev <- paste0("AJP_", legal_problems, "_sev")


A2J.df <- master_data_gpp %>%
  mutate(across(all_of(legprob_bin), ~ as.integer(. == 1))) %>%
  mutate(
    legprob = as.integer(rowSums(across(all_of(legprob_bin)), na.rm = TRUE) > 0)
    ) %>%
  filter(legprob == 1) %>%
  mutate(
    across(all_of(legprob_sev), ~ if_else(.>3, 1, 0))
  ) %>%
  mutate(
    legprob_sev = as.integer(rowSums(across(all_of(legprob_sev)), na.rm = TRUE) > 0)
  ) %>%
  filter(legprob_sev == 1)


justice_gap <- A2J.df %>%
  mutate(
    DIM1 =
      case_when(
        AJE_infosource == 1 ~ 1,     # were able to access info --> not in gap
        AJE_infosource == 2 ~ 0     # were not able to access info --> gap
      )
      ,
    AJD_noadvice_reason =
      case_when(
        AJD_noadvice_reason < 98 ~ NA_real_,
        T ~ AJD_noadvice_reason
      ),
    DIM2 = 
      case_when(
        # 1. Able to access advice from a formal source --> not in gap
        (AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_8 == 1) & (AJD_inst_advice == 1) ~ 1, 
        # 2. Did not access advice because problem was not important --> not in gap
        (AJD_noadvice_reason == 1 | AJD_noadvice_reason == 3) & (AJD_inst_advice == 2) ~ 1,
        # 3. Accessed advice from a friend, religious org or other --> in the justice gap depending on reason
        (AJD_adviser_1 == 1 | AJD_adviser_7 == 1 | AJD_adviser_9 == 1) & (AJD_inst_advice == 1) ~ 0,
        # 4. If reason for not seeking advice falls into these categories --> in the justice gap
        (AJD_noadvice_reason %in% c(2, 4, 5, 6, 7, 8, 9, 10)) & (AJD_inst_advice == 2)~ 0, 
        T ~ NA_real_
      ),
    # timeliness
    DIM3_1 = 
      case_when(
        # 1. Problem resolved in one year or less --> not in justice gap
        AJR_solvingtime < 13 & (AJR_state_noresol == 3 | AJR_state_noresol == 4 | AJR_state_resol == 3 | AJR_state_resol == 4) ~ 1,
        # 2. Problem solved in more than one year --> in justice gap
        AJR_solvingtime > 12 & (AJR_state_noresol == 3 | AJR_state_noresol == 4 | AJR_state_resol == 3 | AJR_state_resol == 4) ~ 0,
        # 3. Problem ongoing --> NA
        AJR_state_noresol == 1 | AJR_state_noresol == 2 | AJR_state_resol == 1 | AJR_state_resol == 2 ~ NA_real_,
        T ~ NA_real_
      ),
    # cost
    DIM3_2 = 
      case_when(
        # 1. If you incurred costs, but they were easy to pay --> not in gap
        (AJR_state_noresol == 3 | AJR_state_noresol == 4 | AJR_state_resol == 3 | AJR_state_resol == 4) & (AJR_costdiff == 1 | AJR_costdiff == 2) & AJR_solvingcosts == 1 ~ 1,
        # 2. If you incurred costs that were difficult to pay --> in the justice gap
        (AJR_state_noresol == 3 | AJR_state_noresol == 4 | AJR_state_resol == 3 | AJR_state_resol == 4) & (AJR_costdiff == 3 | AJR_costdiff == 4) & AJR_solvingcosts == 1 ~ 0,
        # 3. You did not incur costs -- not in justice gap
        AJR_solvingcosts == 2 ~ 1,
        T ~ NA_real_
      ),
    # fairness
    DIM3_3 = 
      case_when(
        # 1. Process was fair --> not in gap
        AJR_fair == 1 ~ 1,
        # 2. Process was not fair --> in gap
        AJR_fair == 2 ~ 0,
        T ~ NA_real_
      ),
    DIM4 =
      case_when(
        # 1. Problem fully resolved --> not in gap
        AJR_state_noresol == 4 | AJR_state_resol == 4 ~ 1,
        # 2. Problem persists --> in gap
        AJR_state_noresol == 3 | AJR_state_resol == 3 ~ 0,
        # 3. Otherwise --> NA
        AJR_state_noresol == 1 | AJR_state_resol == 1 | AJR_state_noresol == 2 | AJR_state_resol == 2 ~ NA_real_,
        T ~ NA_real_,
      )
  ) %>%
  mutate(
    DIM3 = rowMeans(across(c(DIM3_1,  DIM3_2,  DIM3_3)), na.rm = T),
    DIM3 = ifelse((DIM3 == NA), NA_real_, DIM3)
  )

justice_score <- justice_gap %>%
  mutate(
    score_JG = rowMeans(across(c(DIM1,  DIM2,  DIM3, DIM4)), na.rm = T)
  ) %>%
  # select(DIM1, DIM2, DIM3, DIM4, score_JG)
  mutate(
    gap = case_when(
    score_JG <= 0.67 ~ 1,             # less than 2/3rds of justice needs met --> in gap
    score_JG > 0.67 ~ 0,              # more than 2/3rds of justice needs met --> not in gap
    T ~  NA_real_                     # every other case is not in gap
    
  )) %>%
  select(
    country_name_ltn, DIM1, DIM2, DIM3, DIM4, score_JG, gap
  )

jg_country <- justice_score %>%
  group_by(country_name_ltn) %>%
  summarise(
    gap_score = mean(gap, na.rm = T)
  ) %>%
  select(country_name_ltn, gap_score)


# We count the 98 and 99 as missing value. 
# We are grouping by country
# We want to add the variable q22 for representation and also we want to include religious or community leader as good representation
# Timeliness and cost should filter as well for the people which problem is solved.
# We are not including 98 and 99 in any estimation


