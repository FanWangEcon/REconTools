ff_panel_expand_longandwide <- function(df, svr_id_t, svr_id_i, svr_data) {
    #' Append Group Mean Lagged Values as Additional Panel Variables
    #'
    #' @description
    #' There is a variable recorded for a panel, and a variable with date info, and another with obs info
    #' Compute averages over sub-groups of dates for each variable, with different ways of specifying date subgroups. average(svr_data) over svr
    #' Reshape data so each date is a variable for selected subset of key variables
    #' Merge results from 2 back to main, so that each indi/date observation has as variables all lagged and forward information as additional variables. Append not n lag m forward, but full history as additional variables
    #' Doing this allows for lagged intereaction that are time specific in an arbitrary way.
    #'
    #' @param svr_id_t string time variable name
    #' @param svr_id_i string individual ID name
    #' @param svr_data string variable name
    #' @return a long panel frame with wide expansion of group mean lagged vars
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_panel_expand_longandwide.html}
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_panel_expand_longandwide.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_panel_expand.R}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' library(dplyr)
    #' df_hw_cebu <- df_hgt_wgt %>% filter(S.country == 'Cebu' & svymthRound <= 24 & svymthRound > 0)
    #' df_hw_cebu <- df_hw_cebu %>% mutate(mth6 = recode(svymthRound,
    #'                                `0`="m00t06", `2`="m00t06", `4`="m00t06", `6`="m00t06",
    #'                                `8`="m08t12", `10`="m08t12", `12`="m08t12",
    #'                                `14`="m14t18", `16`="m14t18", `18`="m14t18",
    #'                                `20`="m20t24", `22`="m20t24", `24`="m20t24"))
    #' df_longandwide <- ff_panel_expand_longandwide(df_hw_cebu,
    #'                                          svr_id_t = 'mth6',
    #'                                          svr_id_i = 'indi.id',
    #'                                          svr_data = 'cal')
    #' print(df_longandwide %>% select(indi.id, svymthRound, mth6, cal, matches('mth6'), everything()), n=100)

    # Select vars to keep for spreading
    ls_svr_keep <- c(svr_id_i, svr_id_t, svr_data)
    df_widespread <- df %>% select(!!!syms(ls_svr_keep))

    # Aggregate
    svr_data_mean <- paste(svr_data,svr_id_t,'mean',sep='_')
    df_widespread <- df_widespread %>%
      group_by(!!sym(svr_id_i), !!sym(svr_id_t)) %>%
      summarise(!!sym(svr_data_mean) := mean(!!sym(svr_data)))

    # Spread
    df_wide <- df_widespread %>% spread(!!sym(svr_id_t), !!sym(svr_data_mean), sep = paste0('_', svr_data, '_'))

    # Merge Back, now dataframe is both wide and long
    df_widelong <- df %>% left_join(df_wide)

    # return
    return(df_widelong)

}

ff_panel_expand_longrosterwide <- function(df, svr_id_t, svr_id_i, st_idcol_prefix='id_') {
    #' long Panel Record Date of Attendance each ID, Expand to Wide of Cumulative Attendance by Dates
    #'
    #' @description
    #' INPUT MATRIX: There are $N$ students in class, but only a subset of them attend class each day.
    #' If student $id_i$ is in class on day $Q$, the teacher records on a sheet the date
    #' and the student ID. So if the student has been in class 10 times, the teacher has
    #' ten rows of recorded data for the student with two columns: column one is the student
    #' ID, and column two is the date on which the student was in class. Suppose there were
    #' 50 students, who on average attended exactly 10 classes each during the semester,
    #' this means we have $10 \cdot 50$ rows of data, with differing numbers of
    #' rows for each student. This is the input matrix for this function here.
    #'
    #' OUTPUT MATRIX: Now we want to generate a new dataframe, where each row is a date, and each column
    #' is a student. The values in the new dataframe shows, at the $Q^{th}$ day, how many
    #' classes student $i$ has attended so far. The following results is also in a REconTools
    #' Function. This is shown as df outputed by this function generated below.
    #'
    #' This function is useful beyond the roster example. It is used in the optimal allocation
    #' problem as well. There are individual recipients of allocation, and each can receive some Q
    #' units of allocations. Given total resources available, what is the sequence in which allocation
    #' should be given to each. The input dataframe has two columns, the ID of each individual, and
    #' the queue rank for a particular allocation for this individual. Expanding to wide gives us
    #' a new df where each row is each additional unit of aggregate allocation available, and each
    #' column is a different individual. The values says at the current level of overall resources
    #' how many units of resources go to each of the individual.
    #'
    #' @param svr_id_t string time variable name
    #' @param svr_id_i string individual ID name
    #' @param st_idcol_prefix string prefix for wide id
    #' @return a list of two dataframes
    #' \itemize{
    #'   \item df_roster_wide - a wide dataframe rows are unique dates, columns are individuals,
    #'   cells are 1 if attended that day
    #'   \item df_roster_wide_cumu - a wide dataframe rows are unique dates, columns are individuals,
    #'   cells show cumulative attendance
    #' }

    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_panel_expand_longrosterwide.html}
    #' \url{https://fanwangecon.github.io/R4Econ/panel/widelong/fs_pivotwider.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_panel_expand.R}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' library(dplyr)
    #' library(tidyr)
    #' library(tibble)
    #' # Generate Input Data Structure
    #' # Define
    #' it_N <- 3
    #' it_M <- 5
    #' svr_id <- 'student_id'
    #'
    #' # from : support/rand/fs_rand_draws.Rmd
    #' set.seed(222)
    #' df_panel_attend_date <- as_tibble(matrix(it_M, nrow=it_N, ncol=1)) %>%
    #'   rowid_to_column(var = svr_id) %>%
    #'   uncount(V1) %>%
    #'   group_by(!!sym(svr_id)) %>% mutate(date = row_number()) %>%
    #'   ungroup() %>% mutate(in_class = case_when(rnorm(n(),mean=0,sd=1) < 0 ~ 1, TRUE ~ 0)) %>%
    #'   filter(in_class == 1) %>% select(!!sym(svr_id), date) %>%
    #'   rename(date_in_class = date)
    #'
    #' print(df_panel_attend_date)
    #'
    #' # Parameters
    #' df <- df_panel_attend_date
    #' svr_id_i <- 'student_id'
    #' svr_id_t <- 'date_in_class'
    #' st_idcol_prefix <- 'sid_'
    #'
    #' # Invoke Function
    #' ls_df_rosterwide <- ff_panel_expand_longrosterwide(df, svr_id_t, svr_id_i, st_idcol_prefix)
    #' df_roster_wide_func <- ls_df_rosterwide$df_roster_wide
    #' df_roster_wide_cumu_func <- ls_df_rosterwide$df_roster_wide_cumu
    #'
    #' # Print
    #' print(df_roster_wide_func)
    #' print(df_roster_wide_cumu_func)

    # Unique IDs
    ar_unique_ids <- sort(unique(df %>% pull(!!sym(svr_id_i))))


    # Generate cumulative enrollment counts by date
    # Sort and rename
    # rename see: https://fanwangecon.github.io/R4Econ/support/tibble/fs_tib_basics.html
    df_roster_wide <- df %>% mutate(attended = 1) %>%
      pivot_wider(names_from = svr_id_i,
                  values_from = attended)  %>%
      arrange(!!sym(svr_id_t)) %>%
      rename_at(vars(num_range('',ar_unique_ids))
                , list(~paste0(st_idcol_prefix, . , ''))
      )

    # Cumulative sum results
    df_roster_wide_cumu <- df_roster_wide %>%
      mutate_at(vars(contains(st_idcol_prefix)), list(~replace_na(., 0))) %>%
      mutate_at(vars(contains(st_idcol_prefix)), list(~cumsum(.)))

    # return
    return(list(df_roster_wide=df_roster_wide,
                df_roster_wide_cumu=df_roster_wide_cumu))

}
