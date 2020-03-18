ff_panel_cumsum_grouplast <- function(df, svr_id, svr_x, svr_y, svr_cumsumtop = 'y_movingavg_lastestscores', stat='mean', quick=TRUE, verbose=FALSE) {
    #' Cumulative stat value from each group from the latest date of availability. Sum or Average
    #'
    #' @description
    #' svr_id are individuals, observed at multiple points; svr_x is date of observation;
    #' svr_y is some value observed at each time point. Imagine these are SAT scores for individuals.
    #' we want to calculate the sum/avg etc cumulatively at each date for all individuals, but only using their last score.
    #'
    #' @param df dataframe
    #' @param svr_id string name of individual id variable
    #' @param svr_x string name of the ranking variable
    #' @param svr_y string name of the value variable
    #' @param svr_cumsumtop string variable name that stores the cumulative summed variable
    #' @param stat string type of statistics to compute, mean, sum, max, min, median
    #' @param quick boolean faster algorithm without repeating calculation, quick should be used, slow was initial bad algorithm, results identical
    #' @param verbose boolean when quick is FALSE, could verbose which generates slower results
    #' @return a dataframe with the cumulative summed/averaged etc column up to each row
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_panel_cumsum_grouplast.html}
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_panel_cumsum_grouplast.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_panel_cumsum.R}
    #' @export
    #' @import dplyr tidyr tibble rlang
    #' @examples
    #' library(tidyr)
    #' library(dplyr)
    #' library(tibble)
    #' set.seed(12345)
    #' it_N <- 5
    #' ar_it_M <- sample(1:10, it_N, replace = TRUE)
    #' ar_it_M_ID <- sample(1:it_N, it_N, replace = FALSE)
    #' tb_combine <- as_tibble(cbind(ar_it_M, ar_it_M_ID)) %>% rowid_to_column(var = "id")
    #' tb_long <- tb_combine %>% uncount(ar_it_M)
    #' tb_long <- tb_long %>% add_column(xrand = runif(dim(tb_long)[1])) %>% arrange(xrand) %>% mutate(x = row_number())
    #' tb_long <- tb_long %>% arrange(id, x) %>% group_by(id) %>% mutate(rank_l = row_number())
    #' df <- tb_long %>% select(id, x) %>% add_column(y = runif(dim(tb_long)[1])) %>% arrange(id,x) %>% group_by(id) %>% mutate(y = cumsum(y))
    #' print(df)
    #' quick <- TRUE
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmean_lastestscore', stat='mean', quick=quick)
    #' quick <- FALSE
    #' verbose <- TRUE
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmean_lastestscore', stat='mean', quick=quick, verbose=verbose)
    #' verbose <- FALSE
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmean_lastestscore', stat='mean', quick=quick, verbose=verbose)
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmedian_lastestscore', stat='median')
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingsum_lastestscore', stat='sum')
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmin_lastestscore', stat='min')
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y',
    #'                           svr_cumsumtop = 'y_movingmax_lastestscore', stat='max')

    tb_data <- df %>% arrange(!!sym(svr_x))
    tb_data_cum_sum_top <- tb_data %>% mutate(!!sym(svr_cumsumtop) := 0)
    ar_latest_indi <- rep(NA, 1, length(unique(tb_data[[svr_id]])))

    # This is the quicker algorithm that saves a lot of time hopefully
    if (quick) {

        if (stat == 'sum'){
          for (row_ctr in seq(1, dim(tb_data)[1])) {
              ar_latest_indi[tb_data[[svr_id]][row_ctr]] <- tb_data[[svr_y]][row_ctr]
              fl_cum_sum_top_cur <- sum(ar_latest_indi, na.rm = TRUE)
              tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
          }
        } else if (stat == 'mean'){
          for (row_ctr in seq(1, dim(tb_data)[1])) {
              ar_latest_indi[tb_data[[svr_id]][row_ctr]] <- tb_data[[svr_y]][row_ctr]
              fl_cum_sum_top_cur <- mean(ar_latest_indi, na.rm = TRUE)
              tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
          }
        } else if (stat == 'median'){
          for (row_ctr in seq(1, dim(tb_data)[1])) {
              ar_latest_indi[tb_data[[svr_id]][row_ctr]] <- tb_data[[svr_y]][row_ctr]
              fl_cum_sum_top_cur <- median(ar_latest_indi, na.rm = TRUE)
              tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
          }
        } else if (stat == 'max'){
          for (row_ctr in seq(1, dim(tb_data)[1])) {
              ar_latest_indi[tb_data[[svr_id]][row_ctr]] <- tb_data[[svr_y]][row_ctr]
              fl_cum_sum_top_cur <- max(ar_latest_indi, na.rm = TRUE)
              tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
          }
        } else if (stat == 'min'){
          for (row_ctr in seq(1, dim(tb_data)[1])) {
              ar_latest_indi[tb_data[[svr_id]][row_ctr]] <- tb_data[[svr_y]][row_ctr]
              fl_cum_sum_top_cur <- min(ar_latest_indi, na.rm = TRUE)
              tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
          }
        }

    } else {

        for (row_ctr in seq(1, dim(tb_data)[1])) {
          # The operations below have a lot of repetition, at each x, only 1 piece of information is added, but redoing all calculations

          if (verbose) {
            # select up to current row sort and group
            tb_data_up2row <- tb_data[1:row_ctr,] %>% arrange(!!sym(svr_id), !!sym(svr_x)) %>% group_by(!!sym(svr_id))

            # Obtain last element sorted by x for each group, and resort by x
            tb_data_up2row <- tb_data_up2row %>% slice(n()) %>% arrange(!!sym(svr_x)) %>% ungroup()

            # cumulative sum of the highest element of each group below row_ctr
            # Statistics, sum, mean, etc are not within individual, but across individual
            # within individual, always simply taking the last individual with slice(n())
            if (stat == 'sum'){
              fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = sum(!!sym(svr_y))) %>% pull(y_sum_top_up2row)
            } else if (stat == 'mean'){
              fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = mean(!!sym(svr_y))) %>% pull(y_sum_top_up2row)
            } else if (stat == 'median'){
              fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = median(!!sym(svr_y))) %>% pull(y_sum_top_up2row)
            } else if (stat == 'max'){
              fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = max(!!sym(svr_y))) %>% pull(y_sum_top_up2row)
            } else if (stat == 'min'){
              fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = min(!!sym(svr_y))) %>% pull(y_sum_top_up2row)
            } else {
              warning('function stat not available')
            }
          } else {
            func = paste0(stat, '(y)')
            fl_cum_sum_top_cur <- tb_data[1:row_ctr,] %>% arrange(!!sym(svr_id), !!sym(svr_x)) %>%
                group_by(!!sym(svr_id)) %>% slice(n()) %>% ungroup() %>%
                summarize(y_sum_top_up2row = !!parse_expr(func)) %>% pull()
          }

          # Store results
          tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
      }

    }


    # return
    return(tb_data_cum_sum_top)
}
