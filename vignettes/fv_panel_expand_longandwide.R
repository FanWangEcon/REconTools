## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load Library
rm(list = ls(all.names = TRUE))
library(dplyr)
library(tidyr)
library(broom)
# library(tidymodels)
library(REconTools)

# Select Cebu Only
df_hw_cebu <- df_hgt_wgt %>% filter(S.country == 'Cebu' & svymthRound <= 24 & svymthRound > 0)
str(df_hw_cebu)

# To Save Processing Time, only Expand Panel for Individuals with low ID numbers
df_hw_cebu <- df_hw_cebu %>% filter(indi.id <= 50)

# Count Unique
svr_unique_identifier = 'indi.id'
df_uniques_vil <- ff_summ_count_unique_by_groups(
  df_hw_cebu, ar_svr_group=c('S.country', 'vil.id'), svr_unique_identifier)
print(df_uniques_vil, n=20)
df_uniques_mth <- ff_summ_count_unique_by_groups(
  df_hw_cebu, ar_svr_group=c('S.country', 'svymthRound'),svr_unique_identifier)
print(df_uniques_mth, n=20)
df_uniques_mth <- ff_summ_count_unique_by_groups(
  df_hw_cebu, ar_svr_group=c('S.country'),svr_unique_identifier)
print(df_uniques_mth, n=20)

## -----------------------------------------------------------------------------
# Create Additional Categorical Variables, ever 6 months for averaging
df_hw_cebu = df_hw_cebu %>% mutate(mth6 = recode(svymthRound,
                                    `0`="m00t06", `2`="m00t06", `4`="m00t06", `6`="m00t06",
                                    `8`="m08t12", `10`="m08t12", `12`="m08t12",
                                    `14`="m14t18", `16`="m14t18", `18`="m14t18",
                                    `20`="m20t24", `22`="m20t24", `24`="m20t24"))

# Create Additional Categorical Variables, ever 8 months for averaging
df_hw_cebu = df_hw_cebu %>% mutate(mth8 = recode(svymthRound,
                                    `0`="m00t08", `2`="m00t08", `4`="m00t08", `6`="m00t08", `8`="m00t08",
                                    `10`="m10t16", `12`="m10t16", `14`="m10t16", `16`="m10t16",
                                    `18`="m18t24", `20`="m18t24", `22`="m18t24", `24`="m18t24"))

# Create Additional Categorical Variables, ever 10 months for averaging
df_hw_cebu = df_hw_cebu %>% mutate(mth10 = recode(svymthRound,
                                    `0`="m00t08", `2`="m00t08", `4`="m00t08",
                                     `6`="m00t08",  `8`="m00t08", `10`="m10t16", `12`="m10t16", `14`="m10t16",
                                    `16`="m10t16", `18`="m18t24", `20`="m18t24", `22`="m18t24", `24`="m18t24"))

# Re-order variables so that months come earlier
df_hw_cebu = df_hw_cebu %>% select(indi.id, svymthRound, mth6, mth8, mth10, everything())


## -----------------------------------------------------------------------------

# Generate New Variable

# Variables to do groupings by
svr_id_t <- 'svymthRound'
svr_id_i <- 'indi.id'
svr_data <- 'cal'
bl_gen_newgrp <- FALSE

# Select vars to keep for spreading
ls_svr_mth_keep <- c(svr_id_i, svr_id_t, svr_data)
df_hw_cebu_mth_2spread <- df_hw_cebu %>% select(!!!syms(ls_svr_mth_keep))

# Spread
df_hw_cebu_mth_wide <- df_hw_cebu_mth_2spread %>% spread(!!sym(svr_id_t), !!sym(svr_data), sep = "")
str(df_hw_cebu_mth_wide)

# Merge Back, now dataframe is both wide and long
df_hw_cebu_mth_widelong <- df_hw_cebu %>% left_join(df_hw_cebu_mth_wide)

## -----------------------------------------------------------------------------

# Generate New Variable

# Variables to do groupings by
svr_id_t <- 'mth6'
svr_id_i <- 'indi.id'
svr_data <- 'cal'
bl_gen_newgrp <- FALSE

# Select vars to keep for spreading
ls_svr_6mth_keep <- c(svr_id_i, svr_id_t, svr_data)
df_hw_cebu_6mth_2spread <- df_hw_cebu %>% select(!!!syms(ls_svr_6mth_keep))

# Aggregate
svr_data_mean <- paste(svr_data,svr_id_t,'mean',sep='_')
df_hw_cebu_6mth_2spread <- df_hw_cebu_6mth_2spread %>%
  group_by(!!sym(svr_id_i), !!sym(svr_id_t)) %>%
  summarise(!!sym(svr_data_mean) := mean(!!sym(svr_data)))

# Spread
df_hw_cebu_6mth_wide <- df_hw_cebu_6mth_2spread %>% spread(!!sym(svr_id_t), !!sym(svr_data_mean), sep = "")

# Merge Back, now dataframe is both wide and long
df_hw_cebu_widelong <- df_hw_cebu %>%
                          left_join(df_hw_cebu_mth_wide) %>%
                          left_join(df_hw_cebu_6mth_wide)

## Testing Regression Linear Including RHS Lag Input interaction with time


## -----------------------------------------------------------------------------
attach(df_hw_cebu_widelong)
vf_months <- model.matrix(~factor(svymthRound))
vf_mth6 <- model.matrix(~factor(mth6))

## -----------------------------------------------------------------------------
# Regression Model:
# h_t = a_0*t + a_1*1{t}*N_t + a_2*1{t}*N_{t-1}
rs_test = lm(hgt ~
               svymthRound +
               vf_months[,9:12]:svymthRound18 +
               vf_months[,10:12]:svymthRound20 +
               vf_months[,11:12]:svymthRound22 +
               vf_months[,12]:svymthRound24 ,
             data=df_hw_cebu_widelong)
rsm_test = summary(rs_test)
rsm_test =

rs_test = lm(hgt ~
               svymthRound +
               log(svymthRound) +
               1/(svymthRound) +
               vf_months[,1:12]:svymthRound2 +
               vf_months[,2:12]:svymthRound4 +
               vf_months[,2:12]:svymthRound6 +
               vf_months[,4:12]:svymthRound8 +
               vf_months[,5:12]:svymthRound10 +
               vf_months[,6:12]:svymthRound12 +
               vf_months[,7:12]:svymthRound14 +
               vf_months[,8:12]:svymthRound16 +
               vf_months[,9:12]:svymthRound18 +
               vf_months[,10:12]:svymthRound20 +
               vf_months[,11:12]:svymthRound22 +
               vf_months[,12]:svymthRound24 ,
             data=df_hw_cebu_widelong)
rsm_test = summary(rs_test)
#rsm_test

## -----------------------------------------------------------------------------
rs_test = lm(hgt ~
             factor(indi.id):svymthRound +
             factor(indi.id):log(svymthRound) +
             factor(indi.id):1/(svymthRound) +
             vf_months[,1:12]:svymthRound2 +
             vf_months[,2:12]:svymthRound4 +
             vf_months[,3:12]:svymthRound6 +
             vf_months[,4:12]:svymthRound8 +
             vf_months[,5:12]:svymthRound10 +
             vf_months[,6:12]:svymthRound12 +
             vf_months[,7:12]:svymthRound14 +
             vf_months[,8:12]:svymthRound16 +
             vf_months[,9:12]:svymthRound18 +
             vf_months[,10:12]:svymthRound20 +
             vf_months[,11:12]:svymthRound22 +
             vf_months[,12]:svymthRound24 ,
           data=df_hw_cebu_widelong)
rsm_coef = tidy(rs_test)
# vf_months[, 11:12]factor(svymthRound)24:svymthRound22
rsm_coef_inputs = rsm_coef %>% filter(grepl('vf_months', term))
rsm_coef_inputs

## -----------------------------------------------------------------------------
rs_test_mth4 = lm(hgt ~
             factor(indi.id):svymthRound +
             factor(indi.id):log(svymthRound) +
             factor(indi.id):1/(svymthRound) +
             vf_mth6[,1:4]:mth6m00t06 +
             vf_mth6[,2:4]:mth6m08t12 +
             vf_mth6[,3:4]:mth6m14t18 +
               vf_mth6[,4]:mth6m20t24 ,
           data=df_hw_cebu_widelong)
rsm_coef_mth4 = tidy(rs_test_mth4)
rsm_coef_inputs = rsm_coef_mth4 %>% filter(grepl('vf_mth6', term))
rsm_coef_inputs

