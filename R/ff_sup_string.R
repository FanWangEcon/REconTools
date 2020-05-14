# Some string helper functions
ff_sup_lst2str <- function(ls_list, st_desc, bl_print=TRUE) {
  #' This function converts a list of strings to a single string for easier printing
  #'
  #' @description
  #' Collapases list of string to a single string with some concatenation. Also prints
  #' out the lst_list code itself for easier identification of what is been printed
  #'
  #' @param ls_list named or unamed list of strings or numerical values
  #' @param st_desc if this is provided, will use this as the string description in print, otherwise
  #' will print out the ls_list code itself
  #' @param bl_print if to print out results
  #' @return a string that collapses a list with description up front
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/R4Econ/amto/list/htmlpdfr/fs_lst_basics.html}
  #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_sup_string.R}
  #' @export
  #' @examples
  #' # Define Lists
  #' ls_num <- list(1,2,3)
  #' ls_str <- list('1','2','3')
  #' ls_num_str <- list(1,2,'3')
  #'
  #' # Named Lists
  #' ar_st_names <- c('e1','e2','e3')
  #' ls_num_str_named <- ls_num_str
  #' names(ls_num_str_named) <- ar_st_names
  #'
  #' # Add Element to Named List
  #' ls_num_str_named$e4 <- 'this is added'
  #'
  #' ff_sup_lst2str(ls_num)
  #' ff_sup_lst2str(ls_str)
  #' ff_sup_lst2str(ls_num_str)
  #' ff_sup_lst2str(ls_num_str_named)
  #'
  #' ff_sup_lst2str(ls_num[2:3])
  #' ff_sup_lst2str(ls_str[2:3])
  #' ff_sup_lst2str(ls_num_str[2:4])
  #' ff_sup_lst2str(ls_num_str_named[c('e2','e3','e4')])
  #'

  # string desc
  if(missing(st_desc)){
    st_desc <- deparse(substitute(ls_list))
  }

  # create string
  st_string_from_list = paste0(paste0(st_desc, ':'),
                               paste(names(ls_list), ls_list, sep="=", collapse=";" ))

  # print and return
  if (bl_print){
    print(st_string_from_list)
  }
  return(st_string_from_list)
}
