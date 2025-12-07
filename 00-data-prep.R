#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.5.0 ---------------------------------- Revised: August 07, 2025-#

# 1) Load packages

pacman::p_load(haven, dplyr, tidyr, magrittr, forcats, ggplot2, psych, xtable)

# 2) Load data

data_raw <- read_sav("data/data-raw/CCES21_BGU_OUTPUT.sav")

# 3) Data cleaning

### Keep only necessary variables
data_raw %<>%
  dplyr::select(caseid, teamweight, birthyr, educ, race, CC21_330a, pid3, pid7, gender4, 
         BGU_Trumpapproval, CC21_315a, BGU_group,
         BGU_control1, BGU_control2, BGU_control3, BGU_control4, BGU_control5,
         BGU_control6, BGU_control7, BGU_control8, BGU_control9, BGU_Trumplib1,
         BGU_Trumplib2, BGU_Trumplib3, BGU_Trumplib4, BGU_Trumplib5, BGU_Trumplib6,
         BGU_Trumplib7, BGU_Trumplib8, BGU_Trumplib9, BGU_Trumpcon1, BGU_Trumpcon2,
         BGU_Trumpcon3, BGU_Trumpcon4, BGU_Trumpcon5, BGU_Trumpcon6, BGU_Trumpcon7,
         BGU_Trumpcon8, BGU_Trumpcon9, BGU_friendlib1, BGU_friendlib2, BGU_friendlib3,
         BGU_friendlib4, BGU_friendlib5, BGU_friendlib6, BGU_friendlib7, BGU_friendlib8,
         BGU_friendlib9, BGU_friendcon1, BGU_friendcon2, BGU_friendcon3, BGU_friendcon4,
         BGU_friendcon5, BGU_friendcon6, BGU_friendcon7, BGU_friendcon8, BGU_friendcon9,
         BGU_conf1,BGU_conf2,BGU_conf3,BGU_conf4,BGU_conf5,BGU_conf6,
         BGU_knowledge1,BGU_knowledge2,BGU_knowledge3,BGU_knowledge4,BGU_knowledge5,
         BGU_knowledge6, BGU_discussion1, BGU_discussion2, BGU_discussion3,
         BGU_discussion4, BGU_discussion5, BGU_discussion6, BGU_discussion7,
         BGU_discussion8, BGU_discussion9, ideo5, pid7, newsint, faminc_new)

### NAs replacement
# Some of the varaibles have `__NA__` instead of normal NA, so it's necessary to
# change that. 

data_raw %<>%
  mutate_if(is.character, ~ ifelse(. == "__NA__", NA, .)) 

data_raw %<>%
  mutate(BGU_group = as.numeric(as.character(BGU_group)),
         race = as.numeric(as.character(race)),
         gender4 = as.numeric(as.character(gender4)),
         BGU_Trumpapproval = as.numeric(as.character(BGU_Trumpapproval)),
         CC21_330a = as.numeric(as.character(CC21_330a)),
         pid3 = as.numeric(as.character(pid3)),
         faminc_new = as.numeric(as.character(faminc_new)),
         educ = as.numeric(as.character(educ)),
         BGU_conf1 = as.numeric(as.character(BGU_conf1)),
         BGU_conf2 = as.numeric(as.character(BGU_conf2)),
         BGU_conf3 = as.numeric(as.character(BGU_conf3)),
         BGU_conf4 = as.numeric(as.character(BGU_conf4)),
         BGU_conf5 = as.numeric(as.character(BGU_conf5)),
         BGU_conf6 = as.numeric(as.character(BGU_conf6)),
         BGU_knowledge1 = as.numeric(as.character(BGU_knowledge1)),
         BGU_knowledge2 = as.numeric(as.character(BGU_knowledge2)),
         BGU_knowledge3 = as.numeric(as.character(BGU_knowledge3)),
         BGU_knowledge4 = as.numeric(as.character(BGU_knowledge4)),
         BGU_knowledge5 = as.numeric(as.character(BGU_knowledge5)),
         BGU_knowledge6 = as.numeric(as.character(BGU_knowledge6)),
         CC21_330a = as.numeric(as.character(CC21_330a)), #ideology
         ideo5 = as.numeric(as.character(ideo5)), # ideology 5 points
         pid7 = as.numeric(as.character(pid7)), #party id 7
         newsint = as.numeric(as.character(newsint))) #political interest

# 4) Creation of indicators

### Political knowledge - counts the correct answers per item

data_raw %<>%
  mutate(knowledge = as.integer(BGU_knowledge1 == 1) + # Constitution
           as.integer(BGU_knowledge2 == 1) + # Deficit
           as.integer(BGU_knowledge3 == 6) + # Term
           as.integer(BGU_knowledge4 == 1) + # Spending
           as.integer(BGU_knowledge5 == 1) + # Nomination
           as.integer(BGU_knowledge6 == 3)) # Veto

data_raw$knowledge <- as.numeric(data_raw$knowledge)

table(data_raw$knowledge)

### Realism

# - Categories: 
#   1 more than once a week
#   2 Once a week
#   3 Once or twice a month
#   4 A few times a year
#   5 Seldom
#   6 Never

table(data_raw$BGU_discussion1)

data_raw$BGU_discussion1[data_raw$BGU_discussion1 == 9] <- NA
data_raw$BGU_discussion2[data_raw$BGU_discussion2 == 9] <- NA
data_raw$BGU_discussion3[data_raw$BGU_discussion3 == 9] <- NA
data_raw$BGU_discussion4[data_raw$BGU_discussion4 == 9] <- NA
data_raw$BGU_discussion5[data_raw$BGU_discussion5 == 9] <- NA
data_raw$BGU_discussion6[data_raw$BGU_discussion6 == 9] <- NA
data_raw$BGU_discussion7[data_raw$BGU_discussion7 == 9] <- NA
data_raw$BGU_discussion8[data_raw$BGU_discussion8 == 9] <- NA
data_raw$BGU_discussion9[data_raw$BGU_discussion9 == 9] <- NA


# - Recode the variable so lower values indicate they never talk with a friend
# - and higher values that they talk often

data_raw %<>%
  mutate(across(starts_with("BGU_discussion"), ~7 - ., .names = "inverted_{.col}")) %>% 
  rowwise() %>%
  mutate(pol_dis_friend = round(mean(c_across(starts_with("inverted_")), na.rm = TRUE), 2)) %>%
  ungroup()

table(data_raw$inverted_BGU_discussion1)

table(data_raw$pol_dis_friend)

### Social conformism

# Please indicate the extent to which you agree or disagree with the following statement:

#- BGU_conf1 It’s best for everyone if people try to fit in instead of acting in unusual ways.
#- BGU_conf2 People should be encouraged to express themselves in unique and possibly unusual ways. 
#- BGU_conf3 Obeying the rules and fitting in are signs of a strong and healthy society.
#- BGU_conf4 People who continually emphasize the need for unity will only limit creativity and hurt our society.
#- BGU_conf5 We should admire people who go their own way without worrying about what others think.
#- BGU_conf6 People need to learn to fit in and get along with others.

#1   Strongly agree
#2   Agree
#3   Neither agree nor disagree
#4   Disagree
#5   Strongly disagree
#9   Don't know

# - Given that some statements are phrased in a way that measure non-conformism 
# - or individualism, we need to change that so all the statements range from 
# - low levels of conformism to high levels of conformism.

# - Questions that need to be reordered: BGU_conf2, BGU_conf4, and BGU_conf5

# - Additionally, neither agree nor disagree and don't know will be mixed in the 
# - same one. New categories will be:
#1. Strongly disagree (low social conformism) - 4 and 5
#2. Don't know/neither agree or disagree - 9 and 3
#3. Agree/strongly agree (high social conformism) - 2 and 1

data_raw %<>%
  mutate(
    BGU_conf1_rec = recode(BGU_conf1, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1), #disagree/strongly disagree (low SC)
    
    BGU_conf2_rec = recode(BGU_conf2, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf3_rec = recode(BGU_conf3, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1), #disagree/strongly disagree (low SC)
    
    BGU_conf4_rec = recode(BGU_conf4, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf5_rec = recode(BGU_conf5, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf6_rec = recode(BGU_conf6, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1))  #disagree/strongly disagree (low SC)

table(data_raw$BGU_conf6) 
table(data_raw$BGU_conf6_rec) #validation

### Test for Cronbach's alpha

items_sci_full <- data_raw %>% 
  dplyr::select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf4_rec, BGU_conf5_rec, BGU_conf6_rec)

print(alpha(items_sci_full, check.keys=TRUE))

items_sci_reduced <- data_raw %>% 
  dplyr::select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf5_rec, BGU_conf6_rec)

print(alpha(items_sci_reduced, check.keys=TRUE))


####4 and 5 item  removed, reliability of the index is better without it

data_raw %<>%
  rowwise() %>%
  mutate(SCI = mean(c_across(c(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, 
                               BGU_conf4_rec, BGU_conf5_rec, BGU_conf6_rec)), 
                    na.rm = TRUE)) %>%
  ungroup()

table(data_raw$SCI)

# 5) Creation of long data format

data_long <- data_raw %>%
  pivot_longer(
    cols = c(
      BGU_control1, BGU_control2, BGU_control3, BGU_control4, BGU_control5,
      BGU_control6, BGU_control7, BGU_control8, BGU_control9, BGU_Trumplib1,
      BGU_Trumplib2, BGU_Trumplib3, BGU_Trumplib4, BGU_Trumplib5, BGU_Trumplib6,
      BGU_Trumplib7, BGU_Trumplib8, BGU_Trumplib9, BGU_Trumpcon1, BGU_Trumpcon2,
      BGU_Trumpcon3, BGU_Trumpcon4, BGU_Trumpcon5, BGU_Trumpcon6, BGU_Trumpcon7,
      BGU_Trumpcon8, BGU_Trumpcon9, BGU_friendlib1, BGU_friendlib2, BGU_friendlib3,
      BGU_friendlib4, BGU_friendlib5, BGU_friendlib6, BGU_friendlib7, BGU_friendlib8,
      BGU_friendlib9, BGU_friendcon1, BGU_friendcon2, BGU_friendcon3, BGU_friendcon4,
      BGU_friendcon5, BGU_friendcon6, BGU_friendcon7, BGU_friendcon8, BGU_friendcon9),
    names_to = "policy_positions_raw",
    values_to = "policy_opinion") # shows support oppose 

data_long <- data_long[!is.na(data_long$policy_opinion), ] # remove empty rows

data_long %<>% 
  rename(treatment_group = BGU_group) # create treatment groups


### Policy Positions

# - Create a new policy position variable that stores all the 9 policy positions where:
# - 1 minimum wage 2 taxes 3 abortion 4 immigration 5 guns 6 health care 7 background checks
# - 8 climate change 9 planned parenthood

data_long %<>%
  mutate(policy_issue = as.numeric(sub(".*([0-9]+)$", "\\1", policy_positions_raw)))

# Modify policy_opinion
# - Following Barber & Pope (2023), they code the policy positions as following:
# - 1 support 0 don't know -1 oppose
# - let's first see how the original variable looks like

table(data_long$policy_opinion)
#1    2    9 
#5162 2882  956 

# 1 (supports) stay the same. 2 (oppose) becomes -1, and 9 (don't know) becomes 0.
data_long %<>%
  mutate(policy_opinion = case_when(
    policy_opinion == 1 ~ 1,    # Support stays 1
    policy_opinion == 2 ~ -1,   # Oppose becomes -1
    policy_opinion == 9 ~ 0     # Don't know becomes 0
  ))

# Reverse the coding for abortion and guns questions where supporting is the conservative position
data_long %<>%
  mutate(policy_opinion = case_when(
    policy_issue %in% c(3, 5) ~ policy_opinion * -1,  # Reverse for abortion and guns
    TRUE ~ policy_opinion  # Keep original coding for all other issues
  ))

# Check results
table(data_long$policy_opinion)

# Check means by issue
data_long %>%
  group_by(policy_issue) %>%
  summarize(
    mean_opinion = mean(policy_opinion, na.rm = TRUE),
    n = n()
  )

### Create experimental treatment conditions based on treatment_group
table(data_long$treatment_group)

data_long %<>%
  mutate(control = if_else(treatment_group == 1, 1, 0),
         libtrump = if_else(treatment_group == 2, 1, 0),
         contrump = if_else(treatment_group == 3, 1, 0),
         libfriend = if_else(treatment_group == 4, 1, 0),
         confriend = if_else(treatment_group == 5, 1, 0))

data_long %<>%
  mutate(treatment_group = as.factor(treatment_group),
         treatment_group = factor(treatment_group, levels = c(1, 2, 3, 4, 5), 
                            labels = c("control", "TL", "TC", "CFL", "CFC")))

data_long$treatment_status <- ifelse(data_long$treatment_group == "control", 0, 1)

### Double-check consistency
table(data_long$control, data_long$treatment_group)
table(data_long$confriend, data_long$treatment_group)
table(data_raw$BGU_control2, exclude = NULL) #usually around 800 NAs
table(data_raw$BGU_friendcon5, exclude = NULL)
# Given that there's usually 800 NAS per position, and there are 5 experimental treatments,
# it's expected that there will be 4000 NAs per policy position

### Construct/modify additional variables

#1. Age

data_long$age <- 2021 - data_long$birthyr

#2. Race = white

table(data_long$race)

data_long$race_white <- ifelse(data_long$race == 1, 1, 0)

table(data_long$race_white)

#3. Gender = male

data_long$male <- ifelse(data_long$gender4 == 1, 1, 0)

table(data_long$male)

#4. Trump approval

data_long$trump_approve <- ifelse(data_long$BGU_Trumpapproval %in% c(4, 5), 1, #disapprove/strongly disapprove
                                  ifelse(data_long$BGU_Trumpapproval %in% c(3, 6), 2,  #NAND, DK
                                         ifelse(data_long$BGU_Trumpapproval %in% c(1, 2), 3, NA))) # approves/strongly approves
data_long$trump_approve_num <- ifelse(data_long$BGU_Trumpapproval %in% c(4, 5), 1, #disapprove/strongly disapprove
                                  ifelse(data_long$BGU_Trumpapproval %in% c(3, 6), 2,  #NAND, DK
                                         ifelse(data_long$BGU_Trumpapproval %in% c(1, 2), 3, NA))) # approves/strongly approves

table(data_long$trump_approve)

#  Levels for trump approval
data_long$trump_approve <- factor(data_long$trump_approve, 
                                  levels = c(1, 2, 3),
                                  labels = c("Disapprove", "Neither", "Approve"))

 #4. Ideology 5 point scale

table(data_long$ideo5)

data_long$ideo5[data_long$ideo5 == 6] <- NA

#6. 7 point Party ID

table(data_long$pid7)

data_long$pid7[data_long$pid7 == 8] <- NA

#7. Income

table(data_long$faminc_new)

data_long$faminc_new[data_long$faminc_new == 97] <- NA

#8. Political interest

table(data_long$newsint)

data_long$newsint[data_long$newsint == 7] <- NA

#9. Binary party associations

table(data_long$pid3)

data_long$democrat <- ifelse(data_long$pid3 == 1, 1, 0)
data_long$republican <- ifelse(data_long$pid3 == 2, 1, 0)
data_long$independent <- ifelse(data_long$pid3 == 3, 1, 0)

table(data_long$republican)

#10. Three-category party id

data_long %<>%
  mutate(party_id = case_when(
    pid3 == 1 ~ "Democrat",
    pid3 == 2 ~ "Republican",
    pid3 %in% 3:5 ~ "Independent/Other"))


# Save in both RDS and csv formats

saveRDS(data_long, file = "data/derived-data/data_long.rds")

write.csv(data_long, file = "data/derived-data/data_long.csv", row.names = FALSE)
