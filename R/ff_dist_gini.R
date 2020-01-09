ff_dist_gini_vector_pos <- function(ar_pos) {
    #' Compute gini coefficient between 1 (equality) and 2/(n+1) (inequality) where n is vector length.
    #'
    #' @description
    #' See vignette for details, but very simple one line algorithm that computes gini for a pos or zero data vector.
    #' This does not work with negative values. Additionally, this is not for a sample, the vector is the population.
    #' Also note while the highest equality is 1, the greatest inequality only asymptotes to 0, but equals to 2/(n+1).
    #'
    #' @param ar_pos array of numeric values that are all non-negative
    #' @return a scalar value of the gini coefficient
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_dist_gini_vector_pos.html}
    #' \url{https://fanwangecon.github.io/REconTools/articles/fv_dist_gini_vector_pos.html}
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

    message('see REconTools for formula: DIST GINI--Compute Gini Inequality Coefficient Given Data Vector (One Variable)')
    # Check length and given warning
    it_n <- length(ar_pos)
    if (it_n <= 100)  warning('Data vector has only n=',it_n,', max-inequality/min-gini=',2/(it_n + 1))
    # Sort
    ar_pos <- sort(ar_pos)
    # formula implement
    fl_gini <- (2/(it_n+1)) * sum(cumsum(ar_pos))*(sum(ar_pos))^(-1)
    return(fl_gini)
}
