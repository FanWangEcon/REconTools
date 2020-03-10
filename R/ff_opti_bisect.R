ff_opti_bisect_pmap_multi <- function(df, fc_withroot,
                                           fl_lower_x, fl_upper_x,
                                           ls_svr_df_in_func,
                                           svr_root_x = 'x',
                                           it_iter_tol = 50, fl_zero_tol = 10^-5,
                                           bl_keep_iter = TRUE,
                                           st_bisec_prefix = 'bisec_',
                                           st_lower_x = 'a', st_lower_fx = 'fa',
                                           st_upper_x = 'b', st_upper_fx = 'fb') {

#' Dataframe rows are individuals, find root for each individual, given cts diff monotonic function with root.
#'
#' @description
#' This is only for strictly monotonically functions that have unique crossing at zero. There are potentially three
#' types of inputs relevant for the bisection root evaluation. Values in each row are parameters for the same nonlinear
#' function, we want to find roots for N nonlinear functions defined by each row. First type of input are these row specific
#' variable values. Second type of inputs are scalars or arrays that are fixed over all rows. Third type of inputs are values
#' that are shifting over bisection iterations. The implementation here assumes that we have lower and upper bound values
#' that are common across all individauls (rows), and that garantee opposing signs.
#'
#' @param df dataframe containing all row/individual specific variable information, will append bisection results to datafram
#' @param fc_withroot function with root, the function should have hard-coded in scalars and arrays that
#' would not change over iterations and would not change across individuals
#' @param fl_lower_x float value of common lower bound
#' @param fl_upper_x float value of common upper bound, opposing sign
#' @param ls_svr_df_in_func list of string names variables in df that are inputs for fc_withroot.
#' @param svr_root_x string the x variable name that appears n fc_withroot.
#' @param it_iter_tol integer how many maximum iterations to allow for bisection at most
#' @param fl_zero_tol float at what gap to zero will algorithm stop
#' @param bl_keep_iter whether to keep all iteration results as data columns
#' @param st_bisec_prefix string prefix for all bisection iteration etc results variables
#' @param st_lower_x string variable name component for lower bound x
#' @param st_lower_fx string variable name component for lower bound x evaluated at function
#' @param st_upper_x string variable name component for upper bound x
#' @param st_upper_fx string variable name component for upper bound x evaluated at function
#' @return dataframe containing bisection root for each individual/row
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#' @references
#' \url{https://fanwangecon.github.io/REconTools/reference/ff_opti_bisect_pmap_multi.html}
#' \url{https://fanwangecon.github.io/REconTools/articles/fv_opti_bisect_pmap_multi.html}
#' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_opti_bisect.R}    
#' @export
#' @import tibble tidyr purrr dplyr
#' @examples
#' library(dplyr)
#' library(tibble)
#' it_N_child_cnt <- 9
#' ar_intercept = seq(-10, -1, length.out = it_N_child_cnt)
#' ar_slope = seq(0.1, 1, length.out = it_N_child_cnt)
#' df_lines <- as_tibble(cbind(ar_intercept, ar_slope)) %>% rowid_to_column(var='ID')
#' ar_st_col_names = c('ID','fl_int', 'fl_slope')
#' df_lines <- df_lines %>% rename_all(~c(ar_st_col_names))
#' fc_withroot_line <- function(fl_int, fl_slope, x){
#'   return(fl_int + fl_slope*x)
#' }
#' fl_lower_x_line <- 0
#' fl_upper_x_line <- 100000
#' ls_svr_df_in_func_line <- c('fl_int', 'fl_slope')
#' svr_root_x_line <- 'x'
#' fl_zero_tol = 10^-6
#' df_bisec <- ff_opti_bisect_pmap_multi(df_lines, fc_withroot_line,
#'                                            fl_lower_x_line, fl_upper_x_line,
#'                                            ls_svr_df_in_func_line, svr_root_x_line, bl_keep_iter = FALSE)
#' df_bisec %>% select(-one_of('f_p_t_f_a'))


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
