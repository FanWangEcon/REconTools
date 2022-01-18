## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Load Library
rm(list = ls())
# Formula, directly implement the GINI formula Following Step 4 above
fv_dist_gini_vector_pos_test <- function(ar_pos) {
  # Check length and given warning
  it_n <- length(ar_pos)
  if (it_n <= 100)  warning('Data vector has n=',it_n,', max-inequality/max-gini=',(it_n-1)/(it_n + 1))
  # Sort
  ar_pos <- sort(ar_pos)
  # formula implement
  fl_gini <- 1 - ((2/(it_n+1)) * sum(cumsum(ar_pos))*(sum(ar_pos))^(-1))
  return(fl_gini)
}

## -----------------------------------------------------------------------------
# Example Arrays of data
ar_equal_n1 = c(1)
ar_ineql_n1 = c(100)

ar_equal_n2 = c(1,1)
ar_ineql_alittle_n2 = c(1,2)
ar_ineql_somewht_n2 = c(1,2^3)
ar_ineql_alotine_n2 = c(1,2^5)
ar_ineql_veryvry_n2 = c(1,2^8)
ar_ineql_mostmst_n2 = c(1,2^13)

ar_equal_n10 = c(2,2,2,2,2,2, 2, 2, 2, 2)
ar_ineql_some_n10 = c(1,2,3,5,8,13,21,34,55,89)
ar_ineql_very_n10 = c(1,2^2,3^2,5^2,8^2,13^2,21^2,34^2,55^2,89^2)
ar_ineql_extr_n10 = c(1,2^2,3^3,5^4,8^5,13^6,21^7,34^8,55^9,89^10)

# Uniform draw testing
ar_unif_n1000 = runif(1000, min=0, max=1)

# Normal draw testing
ar_norm_lowsd_n1000 = rnorm(1000, mean=100, sd =1)
ar_norm_lowsd_n1000[ar_norm_lowsd_n1000<0] = 0
ar_norm_highsd_n1000 = rnorm(1000, mean=100, sd =20)
ar_norm_highsd_n1000[ar_norm_highsd_n1000<0] = 0

# Beta draw testing
ar_beta_mostrich_n1000 = rbeta(1000, 5, 1)
ar_beta_mostpoor_n1000 = rbeta(1000, 1, 5)
ar_beta_manyrichmanypoor_nomiddle_n1000 = rbeta(1000, 0.5, 0.5)

## ---- eval=TRUE, echo=FALSE---------------------------------------------------
# Hard-Code Small N Dist Tests
cat('\nSmall N=1 Hard-Code\n')
cat('ar_equal_n1:', fv_dist_gini_vector_pos_test(ar_equal_n1), '\n')
cat('ar_ineql_n1:', fv_dist_gini_vector_pos_test(ar_ineql_n1), '\n')

cat('\nSmall N=2 Hard-Code, converge to 1/3, see formula above\n')
cat('ar_ineql_alittle_n2:', fv_dist_gini_vector_pos_test(ar_ineql_alittle_n2), '\n')
cat('ar_ineql_somewht_n2:', fv_dist_gini_vector_pos_test(ar_ineql_somewht_n2), '\n')
cat('ar_ineql_alotine_n2:', fv_dist_gini_vector_pos_test(ar_ineql_alotine_n2), '\n')
cat('ar_ineql_veryvry_n2:', fv_dist_gini_vector_pos_test(ar_ineql_veryvry_n2), '\n')

cat('\nSmall N=10 Hard-Code, convege to 9/11=0.8181, see formula above\n')
cat('ar_equal_n10:', fv_dist_gini_vector_pos_test(ar_equal_n10), '\n')
cat('ar_ineql_some_n10:', fv_dist_gini_vector_pos_test(ar_ineql_some_n10), '\n')
cat('ar_ineql_very_n10:', fv_dist_gini_vector_pos_test(ar_ineql_very_n10), '\n')
cat('ar_ineql_extr_n10:', fv_dist_gini_vector_pos_test(ar_ineql_extr_n10), '\n')

# Uniform Dist Tests
cat('\nUNIFORM Distribution\n')
cat('ar_unif_n1000:', fv_dist_gini_vector_pos_test(ar_unif_n1000), '\n')

# Normal Dist Tests
cat('\nNORMAL Distribution\n')
cat('ar_norm_lowsd_n1000:', fv_dist_gini_vector_pos_test(ar_norm_lowsd_n1000), '\n')
cat('ar_norm_highsd_n1000:', fv_dist_gini_vector_pos_test(ar_norm_highsd_n1000), '\n')

# Beta Dist Tests
cat('\nBETA Distribution\n')
cat('ar_beta_mostpoor_n1000:', fv_dist_gini_vector_pos_test(ar_beta_mostpoor_n1000), '\n')
cat('ar_beta_manyrichmanypoor_nomiddle_n1000:', fv_dist_gini_vector_pos_test(ar_beta_manyrichmanypoor_nomiddle_n1000), '\n')
cat('ar_beta_mostrich_n1000:', fv_dist_gini_vector_pos_test(ar_beta_mostrich_n1000), '\n')

# Example when it does not work
ar_unif_n1000_NEGATIVE = runif(1000, min=-1, max=1)
cat('\n\nSHOULD/DOES NOT WORK TEST\n ar_unif_n1000_NEGATIVE:', fv_dist_gini_vector_pos_test(ar_unif_n1000_NEGATIVE), '\n')

## -----------------------------------------------------------------------------
# array
ar_x <- seq(1, 100, length.out = 30)
# prob array
ar_prob_x_unif <- rep.int(1, length(ar_x))/sum(rep.int(1, length(ar_x)))
# prob higher at lower values
ar_prob_x_lowval_highwgt <- rev(cumsum(ar_prob_x_unif))/sum(cumsum(ar_prob_x_unif))
# prob higher at lower values
ar_prob_x_highval_highwgt <- (cumsum(ar_prob_x_unif))/sum(cumsum(ar_prob_x_unif))
# show
print(cbind(ar_x, ar_prob_x_unif, ar_prob_x_lowval_highwgt, ar_prob_x_highval_highwgt))

## -----------------------------------------------------------------------------
library(REconTools)
ff_dist_gini_vector_pos(ar_x)
ff_dist_gini_random_var(ar_x, ar_prob_x_unif)
ff_dist_gini_random_var(ar_x, ar_prob_x_lowval_highwgt)
ff_dist_gini_random_var(ar_x, ar_prob_x_highval_highwgt)

