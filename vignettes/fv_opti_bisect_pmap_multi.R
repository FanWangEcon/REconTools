## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadlib, echo = T, results = 'hide', message=F, warning=F----------------
# rm(list = ls(all.names = TRUE))
library(REconTools)
library(tibble)
library(tidyr)
library(purrr)
library(dplyr)
library(knitr)
library(ggplot2)
library(kableExtra)

## ----setup--------------------------------------------------------------------
# Parameters
fl_rho = 0.20
svr_id_var = 'INDI_ID'

# it_child_count = N, the number of children
it_N_child_cnt = 9
# it_heter_param = Q, number of parameters that are heterogeneous across children
it_Q_hetpa_cnt = 2

# P fixed parameters, nN is N dimensional, nP is P dimensional
ar_nN_A = seq(-2, 2, length.out = it_N_child_cnt)
ar_nN_alpha = seq(0.1, 0.9, length.out = it_N_child_cnt)
ar_nP_A_alpha = c(ar_nN_A, ar_nN_alpha)

# N by Q varying parameters
mt_nN_by_nQ_A_alpha = cbind(ar_nN_A, ar_nN_alpha)

# Choice Grid for nutritional feasible choices for each
fl_N_agg = 100
fl_N_min = 0

# Mesh Expand
tb_states_choices <- as_tibble(mt_nN_by_nQ_A_alpha) %>% rowid_to_column(var=svr_id_var)
ar_st_col_names = c(svr_id_var,'fl_A', 'fl_alpha')
tb_states_choices <- tb_states_choices %>% rename_all(~c(ar_st_col_names))

# display
summary(tb_states_choices)
kable(tb_states_choices) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## -----------------------------------------------------------------------------
# Define Implicit Function
ffi_nonlin_dplyrdo <- function(fl_A, fl_alpha, fl_N, ar_A, ar_alpha, fl_N_agg, fl_rho){
  # scalar value that are row-specific, in dataframe already: *fl_A*, *fl_alpha*, *fl_N*
  # array and scalars not in dataframe, common all rows: *ar_A*, *ar_alpha*, *fl_N_agg*, *fl_rho*

  # Test Parameters
  # ar_A = ar_nN_A
  # ar_alpha = ar_nN_alpha
  # fl_N = 100
  # fl_rho = -1
  # fl_N_q = 10

  # Apply Function
  ar_p1_s1 = exp((fl_A - ar_A)*fl_rho)
  ar_p1_s2 = (fl_alpha/ar_alpha)
  ar_p1_s3 = (1/(ar_alpha*fl_rho - 1))
  ar_p1 = (ar_p1_s1*ar_p1_s2)^ar_p1_s3
  ar_p2 = fl_N^((fl_alpha*fl_rho-1)/(ar_alpha*fl_rho-1))
  ar_overall = ar_p1*ar_p2
  fl_overall = fl_N_agg - sum(ar_overall)

  return(fl_overall)
}

## -----------------------------------------------------------------------------

# common prefix to make reshaping easier
st_bisec_prefix <- 'bisec_'
svr_a_lst <- paste0(st_bisec_prefix, 'a_0')
svr_b_lst <- paste0(st_bisec_prefix, 'b_0')
svr_fa_lst <- paste0(st_bisec_prefix, 'fa_0')
svr_fb_lst <- paste0(st_bisec_prefix, 'fb_0')

# Add initial a and b
tb_states_choices_bisec <- tb_states_choices %>%
                            mutate(!!sym(svr_a_lst) := fl_N_min, !!sym(svr_b_lst) := fl_N_agg)

# Evaluate function f(a_0) and f(b_0)
tb_states_choices_bisec <- tb_states_choices_bisec %>% rowwise() %>%
                            mutate(!!sym(svr_fa_lst) := ffi_nonlin_dplyrdo(fl_A, fl_alpha, !!sym(svr_a_lst),
                                                                          ar_nN_A, ar_nN_alpha,
                                                                          fl_N_agg, fl_rho),
                                   !!sym(svr_fb_lst) := ffi_nonlin_dplyrdo(fl_A, fl_alpha, !!sym(svr_b_lst),
                                                                          ar_nN_A, ar_nN_alpha,
                                                                          fl_N_agg, fl_rho))
# Summarize
dim(tb_states_choices_bisec)
summary(tb_states_choices_bisec)

## -----------------------------------------------------------------------------

# fl_tol = float tolerance criteria
# it_tol = number of interations to allow at most
fl_tol <- 10^-2
it_tol <- 100

# fl_p_dist2zr = distance to zero to initalize
fl_p_dist2zr <- 1000
it_cur <- 0
while (it_cur <= it_tol && fl_p_dist2zr >= fl_tol ) {

  it_cur <- it_cur + 1

  # New Variables
  svr_a_cur <- paste0(st_bisec_prefix, 'a_', it_cur)
  svr_b_cur <- paste0(st_bisec_prefix, 'b_', it_cur)
  svr_fa_cur <- paste0(st_bisec_prefix, 'fa_', it_cur)
  svr_fb_cur <- paste0(st_bisec_prefix, 'fb_', it_cur)

  # Evaluate function f(a_0) and f(b_0)
  # 1. generate p
  # 2. generate f_p
  # 3. generate f_p*f_a
  tb_states_choices_bisec <- tb_states_choices_bisec %>% rowwise() %>%
                              mutate(p = ((!!sym(svr_a_lst) + !!sym(svr_b_lst))/2)) %>%
                              mutate(f_p = ffi_nonlin_dplyrdo(fl_A, fl_alpha, p,
                                                              ar_nN_A, ar_nN_alpha,
                                                              fl_N_agg, fl_rho)) %>%
                              mutate(f_p_t_f_a = f_p*!!sym(svr_fa_lst))
  # fl_p_dist2zr = sum(abs(p))
  fl_p_dist2zr <- mean(abs(tb_states_choices_bisec %>% pull(f_p)))

  # Update a and b
  tb_states_choices_bisec <- tb_states_choices_bisec %>%
                              mutate(!!sym(svr_a_cur) :=
                                       case_when(f_p_t_f_a < 0 ~ !!sym(svr_a_lst),
                                                 TRUE ~ p)) %>%
                              mutate(!!sym(svr_b_cur) :=
                                       case_when(f_p_t_f_a < 0 ~ p,
                                                 TRUE ~ !!sym(svr_b_lst)))
  # Update f(a) and f(b)
  tb_states_choices_bisec <- tb_states_choices_bisec %>%
                              mutate(!!sym(svr_fa_cur) :=
                                       case_when(f_p_t_f_a < 0 ~ !!sym(svr_fa_lst),
                                                 TRUE ~ f_p)) %>%
                              mutate(!!sym(svr_fb_cur) :=
                                       case_when(f_p_t_f_a < 0 ~ f_p,
                                                 TRUE ~ !!sym(svr_fb_lst)))
  # Save from last
  svr_a_lst <- svr_a_cur
  svr_b_lst <- svr_b_cur
  svr_fa_lst <- svr_fa_cur
  svr_fb_lst <- svr_fb_cur

  # Summar current round
  print(paste0('it_cur:', it_cur, ', fl_p_dist2zr:', fl_p_dist2zr))
  summary(tb_states_choices_bisec %>% select(one_of(svr_a_cur, svr_b_cur, svr_fa_cur, svr_fb_cur)))
}

## ----reshape solution from wide to very long----------------------------------
# New variables
svr_bisect_iter <- 'biseciter'
svr_abfafb_long_name <- 'varname'
svr_number_col <- 'value'
svr_id_bisect_iter <- paste0(svr_id_var, '_bisect_ier')

# Pivot wide to very long
tb_states_choices_bisec_long <- tb_states_choices_bisec %>%
  pivot_longer(
    cols = starts_with(st_bisec_prefix),
    names_to = c(svr_abfafb_long_name, svr_bisect_iter),
    names_pattern = paste0(st_bisec_prefix, "(.*)_(.*)"),
    values_to = svr_number_col
  )

# Print
summary(tb_states_choices_bisec_long)
head(tb_states_choices_bisec_long %>% select(-one_of('p','f_p','f_p_t_f_a')), 30)
tail(tb_states_choices_bisec_long %>% select(-one_of('p','f_p','f_p_t_f_a')), 30)

# Pivot wide to very long to a little wide
tb_states_choices_bisec_wider <- tb_states_choices_bisec_long %>%
  pivot_wider(
    names_from = !!sym(svr_abfafb_long_name),
    values_from = svr_number_col
  )

# Print
summary(tb_states_choices_bisec_wider)
head(tb_states_choices_bisec_wider %>% select(-one_of('p','f_p','f_p_t_f_a')), 30)
tail(tb_states_choices_bisec_wider %>% select(-one_of('p','f_p','f_p_t_f_a')), 30)

## ----reshape solution for graphing--------------------------------------------
# Graph results
lineplot <- tb_states_choices_bisec_long %>%
    mutate(!!sym(svr_bisect_iter) := as.numeric(!!sym(svr_bisect_iter))) %>%
    filter(!!sym(svr_abfafb_long_name) %in% c('a', 'b')) %>%
    ggplot(aes(x=!!sym(svr_bisect_iter), y=!!sym(svr_number_col),
               colour=!!sym(svr_abfafb_long_name),
               linetype=!!sym(svr_abfafb_long_name),
               shape=!!sym(svr_abfafb_long_name))) +
        facet_wrap( ~ INDI_ID) +
        geom_line() +
        geom_point() +
        labs(title = 'Bisection Iteration over individuals Until Convergence',
             x = 'Bisection Iteration',
             y = 'a (left side point) and b (right side point) values',
             caption = 'DPLYR concurrent bisection nonlinear multple individuals') +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(lineplot)

## ----define function----------------------------------------------------------
fv_opti_bisect_pmap_multi_test <- function(df, fc_withroot,
                                           fl_lower_x, fl_upper_x,
                                           ls_svr_df_in_func,
                                           svr_root_x = 'x',
                                           it_iter_tol = 50, fl_zero_tol = 10^-5,
                                           bl_keep_iter = TRUE,
                                           st_bisec_prefix = 'bisec_',
                                           st_lower_x = 'a', st_lower_fx = 'fa',
                                           st_upper_x = 'b', st_upper_fx = 'fb') {


# A. common prefix to make reshaping easier
svr_a_lst <- paste0(st_bisec_prefix, st_lower_x, '_0')
svr_b_lst <- paste0(st_bisec_prefix, st_upper_x, '_0')
svr_fa_lst <- paste0(st_bisec_prefix, st_lower_fx, '_0')
svr_fb_lst <- paste0(st_bisec_prefix, st_upper_fx, '_0')
svr_fxvr_name <- paste0('f', svr_root_x)
ls_pmap_vars <- unique(c(ls_svr_df_in_func, svr_root_x))

# B. Add initial a and b
df_bisec <- df %>% mutate(!!sym(svr_a_lst) := fl_lower_x, !!sym(svr_b_lst) := fl_upper_x)

# C. Evaluate function f(a_0) and f(b_0)
# 1. set x = a_0
# 2. evaluate f(a_0)
# 3. set x = b_0
# 4. evaluate f(b_0)
df_bisec <- df_bisec %>% mutate(!!sym(svr_root_x) := !!sym(svr_a_lst))
df_bisec <- df_bisec %>% mutate(
                !!sym(svr_fa_lst) :=
                  unlist(
                    pmap(df_bisec %>% select(ls_pmap_vars), fc_withroot)
                    )
                )
df_bisec <- df_bisec %>% mutate(!!sym(svr_root_x) := !!sym(svr_b_lst))
df_bisec <- df_bisec %>% mutate(
                !!sym(svr_fb_lst) :=
                  unlist(
                    pmap(df_bisec %>% select(ls_pmap_vars), fc_withroot)
                    )
                )

# D. Iteration Convergence Criteria
# fl_p_dist2zr = distance to zero to initalize
fl_p_dist2zr <- 1000
it_cur <- 0
while (it_cur <= it_iter_tol && fl_p_dist2zr >= fl_zero_tol ) {

  it_cur <- it_cur + 1

  # New Variables
  svr_a_cur <- paste0(st_bisec_prefix, st_lower_x, '_', it_cur)
  svr_b_cur <- paste0(st_bisec_prefix, st_upper_x, '_', it_cur)
  svr_fa_cur <- paste0(st_bisec_prefix, st_lower_fx, '_', it_cur)
  svr_fb_cur <- paste0(st_bisec_prefix, st_upper_fx, '_', it_cur)

  # Evaluate function f(a_0) and f(b_0)
  # 1. generate p
  # 2. generate f_p
  # 3. generate f_p*f_a
  df_bisec <- df_bisec %>% mutate(!!sym(svr_root_x) := ((!!sym(svr_a_lst) + !!sym(svr_b_lst))/2))
  df_bisec <- df_bisec %>% mutate(
                  !!sym(svr_fxvr_name) :=
                    unlist(
                      pmap(df_bisec %>% select(ls_pmap_vars), fc_withroot)
                      )
                  ) %>%
                  mutate(f_p_t_f_a = !!sym(svr_fxvr_name)*!!sym(svr_fa_lst))

  # fl_p_dist2zr = sum(abs(p))
  fl_p_dist2zr <- mean(abs(df_bisec %>% pull(!!sym(svr_fxvr_name))))

  # Update a and b
  df_bisec <- df_bisec %>%
                  mutate(!!sym(svr_a_cur) :=
                           case_when(f_p_t_f_a < 0 ~ !!sym(svr_a_lst),
                                     TRUE ~ !!sym(svr_root_x))) %>%
                  mutate(!!sym(svr_b_cur) :=
                           case_when(f_p_t_f_a < 0 ~ !!sym(svr_root_x),
                                     TRUE ~ !!sym(svr_b_lst)))

  # Update f(a) and f(b)
  df_bisec <- df_bisec %>%
                  mutate(!!sym(svr_fa_cur) :=
                           case_when(f_p_t_f_a < 0 ~ !!sym(svr_fa_lst),
                                     TRUE ~ !!sym(svr_fxvr_name))) %>%
                  mutate(!!sym(svr_fb_cur) :=
                           case_when(f_p_t_f_a < 0 ~ !!sym(svr_fxvr_name),
                                     TRUE ~ !!sym(svr_fb_lst)))

  # Drop past record possibly
  if(!bl_keep_iter) {
    df_bisec <- df_bisec %>% select(-one_of(c(svr_a_lst, svr_b_lst, svr_fa_lst, svr_fb_lst)))
  }

  # Save from last
  svr_a_lst <- svr_a_cur
  svr_b_lst <- svr_b_cur
  svr_fa_lst <- svr_fa_cur
  svr_fb_lst <- svr_fb_cur

  # Summar current round
  message(paste0('it_cur:', it_cur, ', fl_p_dist2zr:', fl_p_dist2zr))
}


# return
return(df_bisec)

}

## -----------------------------------------------------------------------------
# Define Inputs
df <- tb_states_choices
fc_withroot <- function(fl_A, fl_alpha, x){
  # Function has four inputs hardcoded in: ar_nN_A, ar_nN_alpha, fl_rho, fl_N_agg
  # Also renamed fl_N to x, indicating this is the x we are shifting to look for zero.
  ar_p1_s1 = exp((fl_A - ar_nN_A)*fl_rho)
  ar_p1_s2 = (fl_alpha/ar_nN_alpha)
  ar_p1_s3 = (1/(ar_nN_alpha*fl_rho - 1))
  ar_p1 = (ar_p1_s1*ar_p1_s2)^ar_p1_s3
  ar_p2 = x^((fl_alpha*fl_rho-1)/(ar_nN_alpha*fl_rho-1))
  ar_overall = ar_p1*ar_p2
  fl_overall = fl_N_agg - sum(ar_overall)
  return(fl_overall)
}
fl_lower_x <- 0
fl_upper_x <- 200
ls_svr_df_in_func <- c('fl_A', 'fl_alpha')
svr_root_x <- 'x'
fl_zero_tol = 10^-6

## -----------------------------------------------------------------------------
ar_intercept = seq(-10, -1, length.out = it_N_child_cnt)
ar_slope = seq(0.1, 1, length.out = it_N_child_cnt)
df_lines <- as_tibble(cbind(ar_intercept, ar_slope)) %>% rowid_to_column(var='ID')
ar_st_col_names = c('ID','fl_int', 'fl_slope')
df_lines <- df_lines %>% rename_all(~c(ar_st_col_names))
fc_withroot_line <- function(fl_int, fl_slope, x){
  return(fl_int + fl_slope*x)
}
fl_lower_x_line <- 0
fl_upper_x_line <- 100000
ls_svr_df_in_func_line <- c('fl_int', 'fl_slope')
svr_root_x_line <- 'x'
fl_zero_tol = 10^-6

## -----------------------------------------------------------------------------
df_bisec <- fv_opti_bisect_pmap_multi_test(df, fc_withroot, fl_lower_x,fl_upper_x,
                                           ls_svr_df_in_func, svr_root_x, bl_keep_iter = TRUE)

# Do individual inputs sum up to total allocations available
fl_allocated <- sum(df_bisec %>% pull(svr_root_x))
if (fl_N_agg == fl_allocated){
  message('Total Input Available = Sum of Optimal Individual Allocations')
} else {
  cat('fl_N_agg =', fl_N_agg, ', but, sum(df_bisec %>% pull(svr_root_x))=', fl_allocated)
}

# Show all
df_bisec

## -----------------------------------------------------------------------------
df_bisec <- suppressMessages(fv_opti_bisect_pmap_multi_test(df, fc_withroot, fl_lower_x, fl_upper_x,
                                                            ls_svr_df_in_func, svr_root_x, bl_keep_iter = FALSE))
df_bisec %>% select(-one_of('f_p_t_f_a'))

## -----------------------------------------------------------------------------
df_bisec <- fv_opti_bisect_pmap_multi_test(df_lines, fc_withroot_line,
                                           fl_lower_x_line, fl_upper_x_line,
                                           ls_svr_df_in_func_line, svr_root_x_line, bl_keep_iter = FALSE)
df_bisec %>% select(-one_of('f_p_t_f_a'))

## -----------------------------------------------------------------------------
# Load Package and Data
# ls_opti_alpha_A <- PrjOptiAlloc::ffy_opt_dtgch_cbem4()
# df_esti <- ls_opti_alpha_A$df_esti
df_esti <- df_opt_dtgch_cbem4

# Select only some individuals to speed up test
df_esti <- df_esti %>% filter(indi.id <= 10)

# Select only A and Alpha and Index
# %>% mutate(A_log =(A_log))
df_dtgch_cbem4_bisec <- df_esti %>% select(indi.id, A_log, alpha_log)
ar_st_col_names = c('INDI_ID', 'fl_A', 'fl_alpha')
df <- df_dtgch_cbem4_bisec %>% rename_all(~c(ar_st_col_names))

## -----------------------------------------------------------------------------
# hard-coded parameters
ar_nN_A <- df %>% pull(fl_A)
ar_nN_alpha <- df %>% pull(fl_alpha)
# Function again updating the hard-coded parameters
fc_withroot <- function(fl_A, fl_alpha, x){
  # Function has four inputs hardcoded in: ar_nN_A, ar_nN_alpha, fl_rho, fl_N_agg
  # Also renamed fl_N to x, indicating this is the x we are shifting to look for zero.
  ar_p1_s1 = exp((fl_A - ar_nN_A)*fl_rho)
  ar_p1_s2 = (fl_alpha/ar_nN_alpha)
  ar_p1_s3 = (1/(ar_nN_alpha*fl_rho - 1))
  ar_p1 = (ar_p1_s1*ar_p1_s2)^ar_p1_s3
  ar_p2 = x^((fl_alpha*fl_rho-1)/(ar_nN_alpha*fl_rho-1))
  ar_overall = ar_p1*ar_p2
  fl_overall = fl_N_agg - sum(ar_overall)
  return(fl_overall)
}

## -----------------------------------------------------------------------------
# Bisect and Solve
ls_svr_df_in_func <- c('fl_A', 'fl_alpha')
svr_root_x <- 'x'
df_bisec_dtgch_cbem4 <- fv_opti_bisect_pmap_multi_test(df, fc_withroot,
                                                       0, fl_upper_x,
                                                       ls_svr_df_in_func, svr_root_x,
                                                       bl_keep_iter = FALSE)

# Do individual inputs sum up to total allocations available
fl_allocated <- sum(df_bisec_dtgch_cbem4 %>% pull(svr_root_x))
if (fl_N_agg == fl_allocated){
  message('Total Input Available = Sum of Optimal Individual Allocations')
  cat('fl_N_agg =', fl_N_agg, ', and, sum(df_bisec_dtgch_cbem4 %>% pull(svr_root_x))=', fl_allocated)
} else {
  cat('fl_N_agg =', fl_N_agg, ', but, sum(df_bisec_dtgch_cbem4 %>% pull(svr_root_x))=', fl_allocated)
}

df_bisec_dtgch_cbem4

