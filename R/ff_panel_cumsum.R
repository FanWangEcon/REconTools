ff_panel_cumsum_grouplast <- function(df, svr_id, svr_x, svr_y, svr_cumsumtop = 'y_movingavg_lastestscores', stat='mean') {
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
    #' @return a dataframe with the cumulative summed/averaged etc column up to each row
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_panel_cumsum_grouplast.html}
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_panel_cumsum_grouplast.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_panel_cumsum.R}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' library(tidyr)
    #' library(dplyr)
    #' library(tibble)
    #' it_N <- 5
    #' ar_it_M <- sample(1:10, it_N, replace = TRUE)
    #' ar_it_M_ID <- sample(1:it_N, it_N, replace = FALSE)
    #' tb_combine <- as_tibble(cbind(ar_it_M, ar_it_M_ID)) %>% rowid_to_column(var = "id")
    #' tb_long <- tb_combine %>% uncount(ar_it_M)
    #' tb_long <- tb_long %>% add_column(xrand = runif(dim(tb_long)[1])) %>% arrange(xrand) %>% mutate(x = row_number())
    #' tb_long <- tb_long %>% arrange(id, x) %>% group_by(id) %>% mutate(rank_l = row_number())
    #' df <- tb_long %>% select(id, x) %>% add_column(y = runif(dim(tb_long)[1])) %>% arrange(id,x) %>% group_by(id) %>% mutate(y = cumsum(y))
    #' print(df)
    #' ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingavg_lastestscore', stat='mean')

    tb_data <- df %>% arrange(x)
    tb_data_cum_sum_top <- tb_data %>% mutate(!!sym(svr_cumsumtop) := 0)

    for (row_ctr in seq(1, dim(tb_data)[1])) {

        # select up to current row sort and group
        tb_data_up2row <- tb_data[1:row_ctr,] %>% arrange(id, x) %>% group_by(id)

        # Obtain last element sorted by x for each group, and resort by x
        tb_data_up2row <- tb_data_up2row %>% slice(n()) %>% arrange(x) %>% ungroup()

        # cumulative sum of the highest element of each group below row_ctr
        if (stat == 'sum'){
          fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = sum(y)) %>% pull(y_sum_top_up2row)
        } else if (stat == 'mean'){
          fl_cum_sum_top_cur <- tb_data_up2row %>% summarize(y_sum_top_up2row = mean(y)) %>% pull(y_sum_top_up2row)
        } else {
          warning('function stat not available')
        }

        # Store results
        tb_data_cum_sum_top[row_ctr, svr_cumsumtop] <- fl_cum_sum_top_cur
    }

    # return
    return(tb_data_cum_sum_top)
}
