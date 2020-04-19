ff_dist_integrate_normal <- function(fl_eps_mean=0, fl_eps_sd = 1,
                                     it_eps = 10) {
    #' Trapezoidal rule style discretized normal random variable.
    #'
    #' @description
    #' Supose epsilon follows the normal distribution. Draw it_eps number of
    #' normal shocks that best approximates the normal distribution.
    #' Outputs a number of statistics to check to quality of approximation
    #'
    #' @param fl_eps_mean float mean of the normal distribution
    #' @param fl_eps_sd float sd of the normal distribution
    #' @param it_eps integer number of discretized points to return
    #' @return a list of normal draw arrays and checks
    #' \itemize{
    #'   \item ar_eps_val - An array of shock draws as Discrete Random Variable
    #'   \item ar_eps_prb - The probability mass for each discrete point
    #'   \item fl_cdf_total_approx - Approximated aggregate probability
    #'   \item fl_mean_approx - Approximated mean given draws array
    #'   \item fl_sd_approx - Approximated standard deviation given draws array
    #' }
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_dist_integrate_normal.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_dist_integrate.R}
    #' @export
    #' @examples
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=3)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=5)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=7)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=9)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=11)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=13)
    #' ff_dist_integrate_normal(fl_eps_mean=60, fl_eps_sd = 20, it_eps=15)

    fl_cdf_min = 0.0001
    fl_cdf_max = 0.9999
    fl_eps_min <- qnorm(fl_cdf_min, mean = fl_eps_mean, sd = fl_eps_sd)
    fl_eps_max <- qnorm(fl_cdf_max, mean = fl_eps_mean, sd = fl_eps_sd)
    fl_gap <- (fl_eps_max-fl_eps_min)/(it_eps)
    ar_eps_bounds <- seq(fl_eps_min, fl_eps_max, by=fl_gap)

    ar_eps_val <- (tail(ar_eps_bounds, -1) + head(ar_eps_bounds, -1))/2
    ar_eps_prb <- dnorm(ar_eps_val, mean = fl_eps_mean, sd = fl_eps_sd)*fl_gap

    fl_cdf_total_approx <- sum(ar_eps_prb)
    fl_mean_approx <- sum(ar_eps_val*ar_eps_prb)
    fl_sd_approx <- sqrt(sum((ar_eps_val-fl_mean_approx)^2*ar_eps_prb))

  return(list(ar_eps_val=ar_eps_val,
              ar_eps_prb=ar_eps_prb,
              fl_cdf_total_approx=fl_cdf_total_approx,
              fl_mean_approx=fl_mean_approx,
              fl_sd_approx=fl_sd_approx))
}
