ff_sup_clean_rmd <- function(ar_spt_root = c('C:/Users/fan/R4Econ/amto',
                                             'C:/Users/fan/R4Econ/math'),
                             ar_spn_skip = c('matrix', 'tibble'),
                             st_folder_pdf = '/htmlpdfr/',
                             st_folder_html = '/htmlpdfr/',
                             st_folder_R = '/htmlpdfr/',
                             bl_gen_if_git_old = FALSE,
                             bl_recursive = TRUE,
                             bl_verbose = TRUE,
                             bl_test = FALSE) {
  #' This function cleans rmd: creates subfolder with pdf, html and R
  #'
  #' @description
  #' knit rmd normally generates pdf and html in the same folder. This deletes those files.
  #' The file generates a new subfolder by default called htmlpdfr that stores the knit
  #' pdf and html files, along with curled R file.
  #'
  #' The RMD files should not clear all at the top. That would lead to deleting the rest of
  #' the string paths to be searched over.
  #'
  #' @param ls_spt_root array a string array of path roots in which to search for Rmd files to knit
  #' @param ar_spn_skip array a string array of names, if path found contains any of the string
  #' in the array, will skip.
  #' @param st_folder_pdf string subfolder where to store the pdf file, if just empty, store in rmd folder
  #' @param st_folder_html string subfolder where to store the html file, if just empty, store in rmd folder
  #' @param st_folder_R string subfolder where to store the R file, if just empty, store in rmd folder
  #' @param bl_gen_if_git_old boolean if true then even if RMD files do not have git status change,
  #' still update pdf and html files.
  #' @param bl_recursive boolean if to search in folders recursively
  #' @param bl_test boolean if testing, meaning do not generate pdf html, just see which files are been
  #' considered included, searched and found
  #' @return a list of string paths of files generated
  #' \itemize{
  #'   \item ls_spt_pdf_generated - a list of pdf file names
  #'   \item ls_spt_html_generated - a list of html file names
  #'   \item ls_spt_R_generated - a list of R file names generated
  #' }#'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/REconTools/reference/ff_sup_clean_rmd.html}
  #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_sup_inout.R}
  #' @export
  #' @examples
  #' ff_sup_clean_rmd()
  #'

  # Get Path
  ls_sfls  <- list.files(path=ar_spt_root,
                         recursive=bl_recursive,
                         pattern='.Rmd',
                         full.names=T)
  if (ar_spn_skip != '') {
    ls_sfls <- ls_sfls[!grepl(paste(ar_spn_skip, collapse = "|"), ls_sfls)]
  }

  ls_spt_pdf_generated <- c('')
  ls_spt_html_generated <- c('')
  ls_spt_R_generated <- c('')

  # print
  for (spt_file in ls_sfls) {
    # 1. store pdf and html files in a subfolder
    # 2. main folder keeps only Rmd file
    # 3. delete tex and other files

    # Check if the RMD file has been modified or is new, if neither, do not generate pdf html
    spg_check_git_status <- paste0('git status -s ', spt_file)
    st_git_status <- toString(system(spg_check_git_status, intern=TRUE))
    bl_modified <- grepl(' M ', st_git_status, fixed=TRUE)
    bl_anewfile <- grepl('?? ', st_git_status, fixed=TRUE)
    bl_nochange <- (st_git_status == "")

    if (bl_modified == 1) {
      message(paste0('MODIFIED: ', spt_file))
    } else if (bl_anewfile == 1) {
      message(paste0('A NEW FL: ', spt_file))
    } else {
      message(paste0('NO CHNGE: ', spt_file))
    }

    if ((bl_modified + bl_anewfile == 1) |
        (bl_nochange & bl_gen_if_git_old)) {

      if (bl_verbose) message(paste0('spt_file:',spt_file))
      st_fullpath_noname <- dirname(spt_file)
      st_fullpath_nosufx <- sub('\\.Rmd$', '', spt_file)
      st_file_wno_suffix <- sub('\\.Rmd$', '', basename(spt_file))
      if (bl_verbose) message(paste0('st_fullpath_noname:', st_fullpath_noname))
      if (bl_verbose) message(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
      if (bl_verbose) message(paste0('st_file_wno_suffix:', st_file_wno_suffix))

      spth_pdf <- paste0(st_fullpath_noname, st_folder_pdf)
      spth_html <- paste0(st_fullpath_noname, st_folder_html)
      sfle_R <- paste0(st_fullpath_noname, st_folder_R, st_file_wno_suffix)
      if (bl_verbose) message(spth_pdf_html)

      sfl_nht <- paste0(st_fullpath_nosufx, '.nb.html')
      sfl_tex <- paste0(st_fullpath_nosufx, '.tex')
      sfl_pdf <- paste0(st_fullpath_nosufx, '.pdf')
      sfl_htm <- paste0(st_fullpath_nosufx, '.html')
      sfl_Rla <- paste0(st_fullpath_nosufx, '.R')
      sfl_log <- paste0(st_fullpath_nosufx, '.log')

      sfl_sub_nht <- paste0(sfle_pdf_html, '.nb.html')
      sfl_sub_tex <- paste0(sfle_pdf_html, '.tex')

      if (!bl_test){
        if (grepl('_main', spt_file)) {

          # try(file.remove(paste0(st_fullpath_nosufx, '.pdf')))
          # try(file.remove(paste0(st_fullpath_nosufx, '.html')))

        } else {

          # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header = "C:/Users/fan/R4Econ/preamble.tex"))', output_dir = spth_pdf_html)
          # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header))', output_dir = spth_pdf_html)

          if (bl_verbose) message(paste0('spt_file:',spth_pdf, ', PDF started'))
          rmarkdown::render(spt_file, output_format='pdf_document', output_dir = spth_pdf)
          if (bl_verbose) message(paste0('spt_file:',spth_pdf, ', PDF finished'))

          if (bl_verbose) message(paste0('spt_file:',spth_html, ', HTML started.'))
          rmarkdown::render(spt_file, output_format='html_document', output_dir = spth_html)
          if (bl_verbose) message(paste0('spt_file:',spth_html, ', HTML finished.'))

          if (bl_verbose) message(paste0('purl_to:', paste0(sfle_R, ".R")))
          knitr::purl(spt_file, output=paste0(sfle_R, ".R"), documentation = 1)

          ls_spt_pdf_generated <-
            c(ls_spt_pdf_generated, paste0(spth_pdf, st_file_wno_suffix, '.pdf'))
          ls_spt_html_generated <-
            c(ls_spt_html_generated, paste0(spth_html, st_file_wno_suffix, '.html'))
          ls_spt_R_generated <-
            c(ls_spt_R_generated, paste0(sfle_R, '.R'))

        }

        try(file.remove(sfl_nht))
        try(file.remove(sfl_tex))
        try(file.remove(sfl_pdf))
        try(file.remove(sfl_htm))
        try(file.remove(sfl_Rla))
        try(file.remove(sfl_log))

        try(file.remove(sfl_sub_nht))
        try(file.remove(sfl_sub_tex))
      }

    }

  }

  if (bl_verbose) {
    message('Generated pdf files:')
    message(print(sapply(ls_spt_pdf_generated, print)))
    message('Generated html files:')
    message(print(sapply(ls_spt_html_generated, print)))
    message('Generated R files:')
    message(print(sapply(ls_spt_R_generated, print)))
  }

  ls_spt_pdf_generated <- tail(ls_spt_pdf_generated, -1)
  ls_spt_html_generated <- tail(ls_spt_html_generated, -1)
  ls_spt_R_generated <- tail(ls_spt_R_generated, -1)

  return(list(ls_spt_pdf_generated=ls_spt_pdf_generated,
              ls_spt_html_generated=ls_spt_html_generated,
              ls_spt_R_generated=ls_spt_R_generated))
}
