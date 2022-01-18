## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load Library
rm(list = ls(all.names = TRUE))
# library(tidyverse)
# library(tidymodels)
# library(rlang)

library(tibble)
library(dplyr)
library(tidyr)

library(REconTools)

library(knitr)
library(kableExtra)

## -----------------------------------------------------------------------------
# Generate X vector
set.seed(12345)

# Number of N
it_N <- 5
# M values for each i
ar_it_M <- sample(1:10, it_N, replace = TRUE)
ar_it_M_ID <- sample(1:it_N, it_N, replace = FALSE)
# Generate dataframe
tb_combine <- as_tibble(cbind(ar_it_M, ar_it_M_ID)) %>% rowid_to_column(var = "id")

# Generate X Vector
tb_long <- tb_combine %>% uncount(ar_it_M)
tb_long <- tb_long %>% add_column(xrand = runif(dim(tb_long)[1])) %>% arrange(xrand) %>% mutate(x = row_number())

# Generate within Group Rank
tb_long <- tb_long %>% arrange(id, x) %>% group_by(id) %>% mutate(rank_l = row_number())

# Select Core
tb_data <- tb_long %>% select(id, x) %>% add_column(y = runif(dim(tb_long)[1])) %>%
              arrange(id,x) %>% group_by(id) %>% mutate(y = cumsum(y))

# Display
kable(tb_long) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
kable(tb_data ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## -----------------------------------------------------------------------------

tb_data <- tb_data %>% arrange(x)
tb_data_cum_sum_top <- tb_data %>% mutate(cum_sum_top = 0,
                                          cum_mean_top = 0,
                                          cum_median_top = 0)

for (row_ctr in seq(1, dim(tb_data)[1])) {

    # select up to current row sort and group
    tb_data_up2row <- tb_data[1:row_ctr,] %>% arrange(id, x) %>% group_by(id)

    # Obtain last element sorted by x for each group, and resort by x
    tb_data_up2row <- tb_data_up2row %>% slice(n()) %>% arrange(x)

    # cumulative sum of the highest element of each group below row_ctr
    fl_cum_sum_top_cur <- tb_data_up2row %>% ungroup() %>%
        summarize(y_sum_top_up2row = sum(y)) %>% pull(y_sum_top_up2row)
    fl_cum_mean_top_cur <- tb_data_up2row %>% ungroup() %>%
        summarize(y_mean_top_up2row = mean(y)) %>% pull(y_mean_top_up2row)
    fl_cum_median_top_cur <- tb_data_up2row %>% ungroup() %>%
        summarize(y_median_top_up2row = median(y)) %>% pull(y_median_top_up2row)

    # Store results
    tb_data_cum_sum_top[row_ctr, 'cum_sum_top'] <- fl_cum_sum_top_cur
    tb_data_cum_sum_top[row_ctr, 'cum_mean_top'] <- fl_cum_mean_top_cur
    tb_data_cum_sum_top[row_ctr, 'cum_median_top'] <- fl_cum_median_top_cur

    # Display
    if (row_ctr %% 10 == 0) {
      cat('row_ctr:', row_ctr, '\n')
      print(tb_data_up2row)
    }
}

# Display Final
kable(tb_data_cum_sum_top) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## -----------------------------------------------------------------------------

tb_data <- tb_data %>% arrange(x)
tb_data_cum_sum_top <- tb_data %>% mutate(cum_sum_top = 0,
                                          cum_mean_top = 0,
                                          cum_median_top = 0)
ar_latest_indi <- rep(NA, 1, it_N)

for (row_ctr in seq(1, dim(tb_data)[1])) {

    # current rank, what is the ID of the person at this rank
    it_id_row <- tb_data[['id']][row_ctr]

    # update the overall individual array with highest, latest value
    ar_latest_indi[it_id_row] <- tb_data[['y']][row_ctr]

    # Compute sum.
    fl_cum_sum_top_cur <- sum(ar_latest_indi, na.rm = TRUE)
    fl_cum_mean_top_cur <- mean(ar_latest_indi, na.rm = TRUE)
    fl_cum_median_top_cur <- median(ar_latest_indi, na.rm = TRUE)

    # Store results
    tb_data_cum_sum_top[row_ctr, 'cum_sum_top'] <- fl_cum_sum_top_cur
    tb_data_cum_sum_top[row_ctr, 'cum_mean_top'] <- fl_cum_mean_top_cur
    tb_data_cum_sum_top[row_ctr, 'cum_median_top'] <- fl_cum_median_top_cur

    # Display
    if (row_ctr %% 10 == 0) {
      cat('row_ctr:', row_ctr, '\n')
      print(tb_data_up2row)
    }
}

# Display Final
kable(tb_data_cum_sum_top) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

## -----------------------------------------------------------------------------
ff_panel_cumsum_grouplast(tb_data, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_cumsum', stat='sum')

## -----------------------------------------------------------------------------
ff_panel_cumsum_grouplast(tb_data, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingaverage_mean', stat='mean')

## ---- large dataframe testing-------------------------------------------------
# Initialize
set.seed(67890)

it_loop <- 3
it_N_min <- 5
it_N_max <- 100
it_N_max <- 30
it_max_M_min <- 10
it_max_M_max <- 100
it_max_M_max <- 30

ar_it_N <- floor(seq(it_N_min, it_N_max, length.out=it_loop))
ar_it_max_M <- floor(seq(it_max_M_min, it_max_M_max, length.out=it_loop))

for (it_ctr in seq(1, it_loop)) {

  # Set df size
  it_N <- ar_it_N[it_ctr]
  it_max_M <- ar_it_max_M[it_ctr]

  # Generate Panel Frame
  df_start_time <- Sys.time()
  ar_it_M <- sample(1:it_max_M, it_N, replace = TRUE)
  ar_it_M_ID <- sample(1:it_N, it_N, replace = FALSE)
  tb_combine <- as_tibble(cbind(ar_it_M, ar_it_M_ID)) %>% rowid_to_column(var = "id")
  tb_long <- tb_combine %>% uncount(ar_it_M)
  tb_long <- tb_long %>% add_column(xrand = runif(dim(tb_long)[1])) %>% arrange(xrand) %>% mutate(x = row_number())
  tb_long <- tb_long %>% arrange(id, x) %>% group_by(id) %>% mutate(rank_l = row_number())
  df <- tb_long %>% select(id, x) %>% add_column(y = runif(dim(tb_long)[1])) %>% arrange(id,x) %>% group_by(id) %>% mutate(y = cumsum(y))
  df_end_time <- Sys.time()

  # Timing Test FAST
  start_time_fast <- Sys.time()
  tb_data_cum_sum_top <- ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingsum_lastestscore', stat='sum', quick=TRUE)
  end_time_fast <- Sys.time()

  # Timing Test SLOW VERBOSE TRUE
  start_time_slow <- Sys.time()
  tb_data_cum_sum_top <- ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingsum_lastestscore', stat='sum', quick=FALSE, verbose = TRUE)
  end_time_slow <- Sys.time()

  # Timing Test SLOW VERBOSE FALSE
  start_time_slow_verbose <- Sys.time()
  tb_data_cum_sum_top <- ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingsum_lastestscore', stat='sum', quick=FALSE, verbose = FALSE)
  end_time_slow_verbose <- Sys.time()

  cat('it_ctr:', it_ctr, ', it_N:', it_N, ', it_max_N:', it_max_M, '\n')
  print(paste0('Df Generation Took:', df_end_time - df_start_time))
  print(paste0('Moving Stat Took QUICK True:', end_time_fast - start_time_fast))
  print(paste0('Moving Stat Took SLOW VERBOSE True:', end_time_slow - start_time_slow))
  print(paste0('Moving Stat Took SLOW VERBOSE False:', end_time_slow_verbose - start_time_slow_verbose))
}


## -----------------------------------------------------------------------------
# Initialize
set.seed(67890)

it_loop <- 5
it_N_min <- 5
it_N_max <- 50
it_max_M_min <- 10
it_max_M_max <- 20

ar_it_N <- floor(seq(it_N_min, it_N_max, length.out=it_loop))
ar_it_max_M <- floor(seq(it_max_M_min, it_max_M_max, length.out=it_loop))

for (it_ctr in seq(1, it_loop)) {

  # Set df size
  it_N <- ar_it_N[it_ctr]
  it_max_M <- ar_it_max_M[it_ctr]

  # Generate Panel Frame
  df_start_time <- Sys.time()
  ar_it_M <- sample(1:it_max_M, it_N, replace = TRUE)
  ar_it_M_ID <- sample(1:it_N, it_N, replace = FALSE)
  tb_combine <- as_tibble(cbind(ar_it_M, ar_it_M_ID)) %>% rowid_to_column(var = "id")
  tb_long <- tb_combine %>% uncount(ar_it_M)
  tb_long <- tb_long %>% add_column(xrand = runif(dim(tb_long)[1])) %>% arrange(xrand) %>% mutate(x = row_number())
  tb_long <- tb_long %>% arrange(id, x) %>% group_by(id) %>% mutate(rank_l = row_number())
  df <- tb_long %>% select(id, x) %>% add_column(y = runif(dim(tb_long)[1])) %>% arrange(id,x) %>% group_by(id) %>% mutate(y = cumsum(y))
  df_end_time <- Sys.time()

  # Timing Test FAST
  start_time_fast <- Sys.time()
  tb_data_cum_sum_top <- ff_panel_cumsum_grouplast(df, svr_id='id', svr_x='x', svr_y='y', svr_cumsumtop = 'y_movingsum_lastestscore', stat='sum', quick=TRUE)
  end_time_fast <- Sys.time()

  cat('it_ctr:', it_ctr, ', it_N:', it_N, ', it_max_N:', it_max_M, '\n')
  print(paste0('Df Generation Took:', df_end_time - df_start_time))
  print(paste0('Moving Stat Took QUICK True:', end_time_fast - start_time_fast))
}

