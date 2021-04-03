ff_dist_gini_vector_pos <- function(ar_pos) {
    #' Compute gini coefficient between 0 (equality) and (n-1)/(n+1) (inequality) where n is vector length.
    #'
    #' @description
    #' See vignette for details, but very simple one line algorithm that computes gini for a pos or zero data vector.
    #' This does not work with negative values. Additionally, this is not for a sample, the vector is the population.
    #' Also note while the highest equality is 0, the greatest inequality only asymptotes to 1, but equals to (n-1)/(n+1).
    #'
    #' @param ar_pos array of numeric values that are all non-negative
    #' @return a scalar value of the gini coefficient
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_dist_gini_vector_pos.html}
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_dist_gini_vector_pos.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_dist_gini.R}
    #' @export
    #' @examples
    #' ar_equal_n2 = c(1,1)
    #' ar_ineql_alittle_n2 = c(1,2)
    #' ar_ineql_somewht_n2 = c(1,2^3)
    #' ar_ineql_alotine_n2 = c(1,2^5)
    #' ar_ineql_veryvry_n2 = c(1,2^8)
    #' ar_ineql_mostmst_n2 = c(1,2^13)
    #' # Beta draw testing
    #' ar_beta_mostrich_n1000 = rbeta(1000, 5, 1)
    #' ar_beta_mostpoor_n1000 = rbeta(1000, 1, 5)
    #' ar_beta_manyrichmanypoor_nomiddle_n1000 = rbeta(1000, 0.5, 0.5)
    #' ff_dist_gini_vector_pos(ar_equal_n2)
    #' ff_dist_gini_vector_pos(ar_ineql_alittle_n2)
    #' ff_dist_gini_vector_pos(ar_ineql_somewht_n2)
    #' ff_dist_gini_vector_pos(ar_ineql_alotine_n2)
    #' ff_dist_gini_vector_pos(ar_ineql_mostmst_n2)
    #' ff_dist_gini_vector_pos(ar_beta_mostrich_n1000)
    #' ff_dist_gini_vector_pos(ar_beta_mostpoor_n1000)
    #' ff_dist_gini_vector_pos(ar_beta_manyrichmanypoor_nomiddle_n1000)

    # message('see REconTools for formula: DIST GINI--Compute Gini Inequality Coefficient Given Data Vector (One Variable)')
    # Check length and given warning
    it_n <- length(ar_pos)
    if (it_n <= 100)  warning('Data vector has only n=',it_n,', max-inequality/min-gini=',(it_n - 1)/(it_n + 1))
    # Sort
    ar_pos <- sort(ar_pos)
    # formula implement
    fl_gini <- 1 - ((2/(it_n+1)) * sum(cumsum(ar_pos))*(sum(ar_pos))^(-1))
    return(fl_gini)
}

# Combining the code from above
ff_dist_gini_random_var <- function(ar_x, ar_prob_of_x, bl_sort_sum=TRUE) {
    #' Compute gini coefficient between 0 (equality) and 1 based on a discrete random variable.
    #'
    #' @description
    #' Suppose there are N outcomes from x1, to xn, each with P(x_i) of happening. What is the
    #' gini index/coefficient that summarizes the level of inequality of the population that is
    #' described by this discrete random variable?
    #'
    #' @param ar_x array of sclaar values x1, x2, ..., xn-1, xn, sorted.
    #' @param ar_prob_of_x array of probability mass that sums to one for sequentially each
    #' element of ar_x.
    #' @param bl_sort_sum boolean if true then finds the unique values of ar_x, sort it, and sum
    #' up the probabilities contained in ar_prob_of_x for each one of the unique values in ar_x
    #' @return a scalar value of the gini coefficient
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_dist_gini_vector_pos.html}
    #' \url{https://fanwangecon.github.io/R4Econ/math/func_ineq/htmlpdfr/fs_gini_disc.html}
    #' @export
    #' @examples
    #' # already sorted example
    #' for (fl_binom_success_prob in seq(0.0001,0.9999,length.out=10)) {
    #'     ar_x <- seq(0, 100, by=1)
    #'     ar_prob_of_x <- dbinom(ar_x, 100, fl_binom_success_prob)
    #'     fl_gini_index <- ff_dist_gini_random_var(ar_x, ar_prob_of_x, bl_sort_sum=FALSE)
    #'     st_print <- paste0('binom p(success)=', fl_binom_success_prob ,
    #'                        ', the fl_gini_index=', fl_gini_index)
    #'     print(st_print)
    #' }
    #' # Example 2a, unsorted
    #' set.seed(123)
    #' ar_x <- c(1,2,3,5,2,1,3)
    #' ar_prob_of_x <- runif(length(ar_x))
    #' ar_prob_of_x <- ar_prob_of_x/sum(ar_prob_of_x)
    #' ff_dist_gini_random_var(ar_x, ar_prob_of_x, bl_sort_sum=TRUE)
    #' # Example 2b, sorted same
    #' ar_x_sort <- c(1,2,3,5)
    #' ar_prob_of_x_sort <- c(0,0,0,0)
    #' ar_prob_of_x_sort[1] <- ar_prob_of_x[1] + ar_prob_of_x[6]
    #' ar_prob_of_x_sort[2] <- ar_prob_of_x[2] + ar_prob_of_x[5]
    #' ar_prob_of_x_sort[3] <- ar_prob_of_x[3] + ar_prob_of_x[7]
    #' ar_prob_of_x_sort[4] <- ar_prob_of_x[4]
    #' ff_dist_gini_random_var(ar_x_sort, ar_prob_of_x_sort, bl_sort_sum=FALSE)
    #' ff_dist_gini_random_var(ar_x_sort, ar_prob_of_x_sort, bl_sort_sum=TRUE)
    #'

    # 0. sort and unique and sum
    if (bl_sort_sum) {
        # step 1, sort
        ls_sorted_res <- sort(ar_x, decreasing = FALSE, index.return=TRUE)
        # step 2, unique sorted
        ar_x <- unique(ls_sorted_res$x)
        # step 3, mass for each unique
        mt_prob_unique <- aggregate(ar_prob_of_x[ls_sorted_res$ix], by=list(ls_sorted_res$x), FUN=sum)
        ar_prob_of_x <- mt_prob_unique$x
    }

    # 1. to normalize, get mean (binomial so mean is p*N=50)
    fl_mean <- sum(ar_x*ar_prob_of_x);
    # 2. get cumulative mean at each point
    ar_mean_cumsum <- cumsum(ar_x*ar_prob_of_x);
    # 3. Share of wealth (income etc) accounted for up to this sorted type
    ar_height <- ar_mean_cumsum/fl_mean;
    # 4. The total area, is the each height times each width summed up
    fl_area_drm <- sum(ar_prob_of_x*ar_height);
    # 5. area below 45 degree line might not be 1/2, depends on discretness
    fl_area_below45 <- sum(ar_prob_of_x*(cumsum(ar_prob_of_x)/sum(ar_prob_of_x)))
    # 6. Gini is the distance between
    fl_gini_index <- (fl_area_below45-fl_area_drm)/fl_area_below45

    return(fl_gini_index)
}
