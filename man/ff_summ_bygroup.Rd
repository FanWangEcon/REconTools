% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_summ_bygroup.R
\name{ff_summ_bygroup}
\alias{ff_summ_bygroup}
\title{Summarize one variable in a dataset, by another categorical variable}
\usage{
ff_summ_bygroup(
  df,
  vars.group,
  var.numeric,
  str.stats.group = "main",
  ar.perc = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99),
  str.stats.specify = NULL,
  boo.overall.stats = TRUE
)
}
\arguments{
\item{df}{dataframe input dataframe of interest}

\item{vars.group}{list of strings containing grouping variables, could be gender and age groups for example}

\item{var.numeric}{string variable name of continuous quantitative variable to summarize}

\item{str.stats.group}{string what type of statistics to consider see line 31 and below}

\item{ar.perc}{array of percentiles to calculate, only calculated if str.stats.group = 'mainperc'}
}
\value{
a list of various variables
\itemize{
  \item df_table_grp_stats - A dataframe where each row is a combination of categories, and columns are categories and statistics
  \item df_row_grp_stats - A single row with all statistics
  \item df_overall_stats - A dataframe with non-grouped overall summaries
  \item df_row_stats_all - A named list of all statistics generated
}
}
\description{
Generate distributional and other statistics for a particular continuous variable,
categorized by some discrete variables. Wage by gender for example.
}
\examples{
data(mtcars)
df_mtcars <- mtcars
df <- df_mtcars
vars.group <- c('am', 'vs')
var.numeric <- 'mpg'
str.stats.group <- 'allperc'
ar.perc <- c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
ls_summ_by_group <- ff_summ_bygroup(df, vars.group, var.numeric, str.stats.group, ar.perc)
df_table_grp_stats <- ls_summ_by_group$df_table_grp_stats
df_row_grp_stats <- ls_summ_by_group$df_row_grp_stats
df_overall_stats <- ls_summ_by_group$df_overall_stats
df_row_stats_all <- ls_summ_by_group$df_row_stats_all
print(df_table_grp_stats)
print(df_row_grp_stats)
print(df_overall_stats)
print(df_row_stats_all)
}
\references{
\url{https://fanwangecon.github.io/REconTools/reference/ff_summ_bygroup.html}
\url{https://fanwangecon.github.io/REconTools/articles/fv_summ_bygroup.html}
\url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_summ_bygroup.R}
}
\author{
Fan Wang, \url{http://fanwangecon.github.io}
}
