ff_summ_bygroup <- function(df, vars.group, var.numeric, str.stats.group = 'main',
                                      str.stats.specify = NULL, boo.overall.stats = TRUE){
    #' Summarize one variable in a dataset, by another categorical variable
    #'
    #' @description
    #' Generate distributional and other statistics for a particular continuous variable,
    #' categorized by some discrete variables. Wage by gender for example.
    #'
    #' @param df dataframe input dataframe of interest
    #' @param vars.group list of strings containing grouping variables, could be gender and age groups for example
    #' @param var.numeric string variable name of continuous quantitative variable to summarize
    #' @param str.stats.group string what type of statistics to consider see line 31 and below
    #' @return a list of various variables
    #' \itemize{
    #'   \item df_table_grp_stats - A dataframe where each row is a combination of categories, and columns are categories and statistics
    #'   \item df_row_grp_stats - A single row with all statistics
    #'   \item df_overall_stats - A dataframe with non-grouped overall summaries
    #'   \item df_row_stats_all - A named list of all statistics generated
    #' }
    #' @author Fan Wang, \url{http://fanwangecon.github.io}
    #' @references
    #' \url{https://fanwangecon.github.io/REconTools/reference/ff_summ_bygroup.html}
    #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_summ_bygroup.R}
    #' @export
    #' @import dplyr tidyr tibble
    #' @examples
    #' data(mtcars)
    #' df_mtcars <- mtcars
    #' df <- df_mtcars
    #' vars.group <- c('am', 'vs')
    #' var.numeric <- 'mpg'
    #' str.stats.group <- 'all'
    #' ls_summ_by_group <- ff_summ_bygroup(df, vars.group, var.numeric, str.stats.group)
    #' df_table_grp_stats <- ls_summ_by_group$df_table_grp_stats
    #' df_row_grp_stats <- ls_summ_by_group$df_row_grp_stats
    #' df_overall_stats <- ls_summ_by_group$df_overall_stats
    #' df_row_stats_all <- ls_summ_by_group$df_row_stats_all
    #' print(df_table_grp_stats)
    #' print(df_row_grp_stats)
    #' print(df_overall_stats)
    #' print(df_row_stats_all)

    # List of statistics
    # https://rdrr.io/cran/dplyr/man/summarise.html
    strs.center <- c('mean', 'median')
    strs.spread <- c('sd', 'IQR', 'mad')
    strs.range <- c('min', 'max')
    strs.pos <- c('first', 'last')
    strs.count <- c('n_distinct')

    # Grouping of Statistics
    if (missing(str.stats.specify)) {
        if (str.stats.group == 'main') {
            strs.all <- c('mean', 'min', 'max', 'sd')
        }
        if (str.stats.group == 'all') {
            strs.all <- c(strs.center, strs.spread, strs.range, strs.pos, strs.count)
        }
    } else {
        strs.all <- str.stats.specify
    }

    # Start Transform
    df <- df %>% drop_na() %>% mutate(!!(var.numeric) := as.numeric(!!sym(var.numeric)))

    # Overall Statistics
    if (boo.overall.stats) {
        df.overall.stats <- df %>% summarize_at(vars(var.numeric), funs(!!!strs.all))
        if (length(strs.all) == 1) {
            # give it a name, otherwise if only one stat, name of stat not saved
            df.overall.stats <- df.overall.stats %>% rename(!!strs.all := !!sym(var.numeric))
        }
        names(df.overall.stats) <- paste0(var.numeric, '.', names(df.overall.stats))
    }

    # Group Sort
    df.select <- df %>%
                  group_by(!!!syms(vars.group)) %>%
                  arrange(!!!syms(c(vars.group, var.numeric)))

    # Table of Statistics
    df.table.grp.stats <- df.select %>% summarize_at(vars(var.numeric), funs(!!!strs.all))

    # Add Stat Name
    if (length(strs.all) == 1) {
        # give it a name, otherwise if only one stat, name of stat not saved
        df.table.grp.stats <- df.table.grp.stats %>% rename(!!strs.all := !!sym(var.numeric))
    }


    # Row of Statistics
    str.vars.group.combine <- paste0(vars.group, collapse='_')
    if (length(vars.group) == 1) {
        df.row.grp.stats <- df.table.grp.stats %>%
                mutate(!!(str.vars.group.combine) := paste0(var.numeric, '.',
                                               vars.group, '.g',
                                               (!!!syms(vars.group)))) %>%
                gather(variable, value, -one_of(vars.group)) %>%
                unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
                spread(str.vars.group.combine, value)
    } else {
        df.row.grp.stats <- df.table.grp.stats %>%
                                mutate(vars.groups.combine := paste0(paste0(vars.group, collapse='.')),
                                       !!(str.vars.group.combine) := paste0(interaction(!!!(syms(vars.group))))) %>%
                                mutate(!!(str.vars.group.combine) := paste0(var.numeric, '.', vars.groups.combine, '.',
                                                                           (!!sym(str.vars.group.combine)))) %>%
                                ungroup() %>%
                                select(-vars.groups.combine, -one_of(vars.group)) %>%
                gather(variable, value, -one_of(str.vars.group.combine))  %>%
                unite(str.vars.group.combine, c(str.vars.group.combine, 'variable')) %>%
                spread(str.vars.group.combine, value)
    }

    # Clean up name strings
    names(df.table.grp.stats) <- gsub(x = names(df.table.grp.stats),pattern = "_", replacement = "\\.")
    names(df.row.grp.stats) <- gsub(x = names(df.row.grp.stats),pattern = "_", replacement = "\\.")

    # Return
    list.return <- list(df_table_grp_stats = df.table.grp.stats, df_row_grp_stats = df.row.grp.stats)

    # Overall Statistics, without grouping
    if (boo.overall.stats) {
        df.row.stats.all <- c(df.row.grp.stats, df.overall.stats)
        list.return <- append(list.return, list(df_overall_stats = df.overall.stats,
                                                df_row_stats_all = df.row.stats.all))
    }

    # Return
    return(list.return)
}
