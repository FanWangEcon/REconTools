ff_sup_clean_rmd <- function(ar_spt_root, ar_spn_skip,
                             st_file_pattern='.Rmd',
                             st_git_pattern='.Rmd',
                             st_folder_pdf = '/htmlpdfr/',
                             st_folder_html = '/htmlpdfr/',
                             st_folder_R = '/htmlpdfr/',
                             bl_gen_if_git_old = FALSE,
                             bl_recursive = TRUE,
                             bl_verbose = TRUE,
                             bl_test = TRUE,
                             it_hierachy_lower_rmd = 0,
                             it_hierachy_shift=2,
                             it_toc_depth=3,
                             ls_bool_convert=list(bl_pdf=TRUE, bl_html=TRUE, bl_R=TRUE),
                             ls_bool_remove=list(bl_remove_html=TRUE)) {
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
  #' @param st_file_pattern string name of file suffix to search over
  #' @param st_git_pattern string name of file suffix based on which to consider if updating. If this is
  #' difference from st_file_pattern, use the file base with this suffix to detect if file has been changed. This
  #' is needed to detect if mlx files have changed for rmd files, where rmd files are not git tracked.
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
  #' @param it_hierachy_lower_rmd integer promote saved hierrachy for rmd files and resave.
  #' @param it_hierachy_shift int modification level of promotion from bookdown hierachy
  #' to own file hierarchy for individual files
  #' @param it_toc_depth rmd own file outputs toc levels to show
  #' @param ls_bool_convert list of booleans to generate pdf, html and or R file. Generate only HTML for example.
  #' considered included, searched and found
  #' @param ls_bool_remove list of booleans contorling if removing certain files in the same directory where the rmd file is
  #' @return a list of string paths of files generated
  #' \itemize{
  #'   \item ls_spt_pdf_generated - a list of pdf file names
  #'   \item ls_spt_html_generated - a list of html file names
  #'   \item ls_spt_R_generated - a list of R file names generated
  #' }#'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/R4Econ/development/inout/fs_rmd_pdf_html.html}
  #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_sup_inout.R}
  #' @export
  #' @examples
  #' ar_spt_root = c('C:/Users/fan/R4Econ/amto/array/', 'C:/Users/fan/R4Econ/math/integration')
  #' ar_spt_root = c('C:/Users/fan/R4Econ/math/integration')
  #' ar_spt_root = c('C:/Users/fan/R4Econ/development/inout')
  #' ar_spn_skip <- c('matrix', 'tibble', '_main', '_mod')
  #' ff_sup_clean_rmd(ar_spt_root, ar_spn_skip)
  #' # ff_sup_clean_rmd(ar_spt_root, ar_spn_skip, bl_test = FALSE, bl_gen_if_git_old = TRUE)
  #'

  # Get Path
  ls_sfls  <- list.files(path=ar_spt_root,
                         recursive=bl_recursive,
                         pattern=st_file_pattern,
                         full.names=T)
  if(!missing(ar_spn_skip)) {
    ls_sfls <- ls_sfls[!grepl(paste(ar_spn_skip, collapse = "|"), ls_sfls)]
  }

  ls_spt_pdf_generated <- c('')
  ls_spt_html_generated <- c('')
  ls_spt_R_generated <- c('')

  # print
  message(paste0('Search and Check'))
  for (spt_file in ls_sfls) {
    # 1. store pdf and html files in a subfolder
    # 2. main folder keeps only Rmd file
    # 3. delete tex and other files

    # Get Current File Path, Assume to be git, check status after
    st_fullpath_noname <- dirname(spt_file)
    st_fullpath_nosufx <- sub(paste0('\\', st_file_pattern, '$'), '', spt_file)
    st_file_wno_suffix <- sub(paste0('\\', st_file_pattern, '$'), '', basename(spt_file))
    # need to do setwd, need to check status within git repo
    setwd(st_fullpath_noname)

    # Check if the RMD file has been modified or is new, if neither, do not generate pdf html
    if (st_file_pattern == st_git_pattern) {
      spt_file_git = spt_file
      st_git_status <- ''
    } else {
      spt_file_git = paste0(st_fullpath_nosufx, st_git_pattern)
      st_git_status <- paste0('(F=', st_file_pattern, ', S=', st_git_pattern, ') ')
    }
    ls_file_status = ff_sup_git_status_check(spt_file_git)
    bl_anewfile = ls_file_status$bl_anewfile
    bl_modified = ls_file_status$bl_modified
    bl_nochange = ls_file_status$bl_nochange
    st_git_status = paste0(st_git_status, ls_file_status$st_git_status)
    if (bl_gen_if_git_old) {
      st_git_status <- paste0('FORCE UPDATE, ', st_git_status)
    }
    message(st_git_status)

    # Convert Files
    if ((bl_modified + bl_anewfile == 1) |
        (bl_nochange & bl_gen_if_git_old)) {

      # Print Path
      if (bl_verbose) message(paste0('spt_file:',spt_file))
      if (bl_verbose) message(paste0('st_fullpath_noname:', st_fullpath_noname))
      if (bl_verbose) message(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
      if (bl_verbose) message(paste0('st_file_wno_suffix:', st_file_wno_suffix))

      # Generate New Paths and File Names
      spth_pdf <- paste0(st_fullpath_noname, st_folder_pdf)
      sname_pdf <- paste0(st_fullpath_noname, st_folder_pdf, st_file_wno_suffix)
      spth_html <- paste0(st_fullpath_noname, st_folder_html)
      sname_html <- paste0(st_fullpath_noname, st_folder_html, st_file_wno_suffix)
      if (bl_verbose) message(spth_html)

      sfle_R <- paste0(st_fullpath_noname, st_folder_R, st_file_wno_suffix)

      sfl_nht <- paste0(st_fullpath_nosufx, '.nb.html')
      sfl_tex <- paste0(st_fullpath_nosufx, '.tex')
      sfl_pdf <- paste0(st_fullpath_nosufx, '.pdf')
      sfl_htm <- paste0(st_fullpath_nosufx, '.html')
      sfl_Rla <- paste0(st_fullpath_nosufx, '.R')
      sfl_log <- paste0(st_fullpath_nosufx, '.log')

      sfl_sub_tex <- paste0(sname_pdf, '.tex')
      sfl_sub_nht <- paste0(sname_html, '.nb.html')

      # Convert to PDF, HTML etc if Not Testing
      if (!bl_test){

        if (grepl('_main', spt_file)) {

          # try(file.remove(paste0(st_fullpath_nosufx, '.pdf')))
          # try(file.remove(paste0(st_fullpath_nosufx, '.html')))

        } else {

          # Step 1, Path Names
          spn_modtex <- paste0(st_fullpath_noname, '/', st_file_wno_suffix, '_mod.Rmd')

          # Step 2a, Generate New Tex and Modify RMD for rising Hierachy and TOC
          # READ
          fileConn_rd <- file(spt_file, "r")
          st_file_read <- readLines(fileConn_rd)

          # Write to new
          fileConn_sr <- file(spn_modtex)
          st_file_read_mod <- ff_sup_clean_rmd_mod(st_file_read,
                                                   it_hierachy_shift=it_hierachy_shift,
                                                   it_toc_depth=it_toc_depth)
          writeLines(st_file_read_mod, fileConn_sr)

          # Close
          close(fileConn_rd)
          close(fileConn_sr)

          # Step 2b, Update RMD file If Hierarchy needs reduction
          if (it_hierachy_lower_rmd > 0) {
            st_file_read_chg <- ff_sup_lower_hierarchy(st_file_read, it_hierachy_lower_rmd=it_hierachy_lower_rmd)
            fileConn_wr <- file(spt_file)
            writeLines(st_file_read_chg, fileConn_wr)
            close(fileConn_wr)
          }

          # Step 4, File Conversions
          # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header = "C:/Users/fan/R4Econ/preamble.tex"))', output_dir = spth_pdf_html)
          # rmarkdown::render(spt_file, output_format='pdf_document(includes = includes(in_header))', output_dir = spth_pdf_html)

          if (ls_bool_convert$bl_pdf) {
            if (bl_verbose) message(paste0('spth_pdf:',spth_pdf, ', PDF started'))
            rmarkdown::render(spn_modtex, output_format='pdf_document',
                              output_dir = spth_pdf, output_file = st_file_wno_suffix)
            if (bl_verbose) message(paste0('spth_pdf:',spth_pdf, ', PDF finished'))
            ls_spt_pdf_generated <-
              c(ls_spt_pdf_generated, paste0(spth_pdf, st_file_wno_suffix, '.pdf'))
          }

          if (ls_bool_convert$bl_html) {
            if (bl_verbose) message(paste0('spth_html:',spth_html, ', HTML started.'))
            rmarkdown::render(spn_modtex, output_format='html_document',
                              output_dir = spth_html, output_file = st_file_wno_suffix)
            if (bl_verbose) message(paste0('spth_html:',spth_html, ', HTML finished.'))
            ls_spt_html_generated <-
              c(ls_spt_html_generated, paste0(spth_html, st_file_wno_suffix, '.html'))
          }

          if (ls_bool_convert$bl_R) {
            if (bl_verbose) message(paste0('purl_to:', paste0(sfle_R, ".R")))
            knitr::purl(spn_modtex, output=paste0(sfle_R, ".R"), documentation = 1)
            ls_spt_R_generated <-
              c(ls_spt_R_generated, paste0(sfle_R, '.R'))
          }

          # Step 5, Delete mod temp file and rename original back to original
          try(file.remove(spn_modtex))
          # file.rename(spn_texrename, spt_file)
        }

        try(file.remove(sfl_nht))
        try(file.remove(sfl_tex))
        try(file.remove(sfl_pdf))
        if (ls_bool_remove$bl_remove_html){
          # do not remove for matlab directories
          try(file.remove(sfl_htm))
        }
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


ff_sup_clean_mlx <- function(st_prj_root,
                             ar_spt_root, ar_spn_skip,
                             st_pattern='.mlx',
                             st_folder_pdf = 'htmlpdfm/',
                             st_folder_html = 'htmlpdfm/',
                             st_folder_m = 'htmlpdfm/',
                             st_folder_html_m = '',
                             st_folder_rmd = '',
                             bl_gen_if_git_old = FALSE,
                             bl_recursive = TRUE,
                             bl_verbose = TRUE,
                             bl_test = TRUE,
                             it_hierachy_shift=2, it_toc_depth=3) {
  #' Convert mlx files to tex and pdf
  #'
  #' @description
  #' If a mlx file has been edited, in math4econ for example, check it against git and
  #' convert to tex and pdf using command line matlab. Then the function wraps \strong{ff_sup_clean_rmd}
  #'
  #' @param st_prj_root string project root for image folder movement
  #' @param ls_spt_root array a string array of path roots in which to search for Rmd files to knit
  #' @param ar_spn_skip array a string array of names, if path found contains any of the string
  #' @param st_pattern string search pattern suffix
  #' in the array, will skip.
  #' @param st_folder_pdf string subfolder where to store the pdf file, if just empty, store in rmd folder
  #' @param st_folder_html string subfolder where to store the html file, if just empty, store in rmd folder
  #' @param st_folder_R string subfolder where to store the R file, if just empty, store in rmd folder
  #' @param bl_gen_if_git_old boolean if true then even if RMD files do not have git status change,
  #' still update pdf and html files.
  #' @param bl_recursive boolean if to search in folders recursively
  #' @param bl_test boolean if testing, meaning do not generate pdf html, just see which files are been
  #' @param it_hierachy_shift rmd modification level of promotion from bookdown hierachy
  #' to own file hierarchy
  #' @param it_toc_depth rmd own file outputs toc levels to show
  #' considered included, searched and found
  #' @return a list of string paths of files generated
  #' \itemize{
  #'   \item ls_spt_pdf_generated - a list of pdf file names
  #'   \item ls_spt_html_generated - a list of html file names
  #'   \item ls_spt_R_generated - a list of R file names generated
  #' }#'
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @references
  #' \url{https://fanwangecon.github.io/R4Econ/development/inout/fs_rmd_pdf_html.html}
  #' \url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_sup_inout.R}
  #' @export
  #' @examples
  #' ar_spt_root = c('C:/Users/fan/Math4Econ/math/matrix_application')
  #' ar_spn_skip <- c('matrix', 'tibble', '_main', '_mod')
  #' ff_sup_clean_mlx(ar_spt_root, ar_spn_skip)
  #'

  # Get Path
  ls_sfls  <- list.files(path=ar_spt_root,
                         recursive=bl_recursive,
                         pattern=st_pattern,
                         full.names=T)
  if(!missing(ar_spn_skip)) {
    ls_sfls <- ls_sfls[!grepl(paste(ar_spn_skip, collapse = "|"), ls_sfls)]
  }

  ls_spt_pdf_generated <- c('')
  ls_spt_html_m_generated <- c('')
  ls_spt_m_generated <- c('')

  # Generate RMD from MLX
  message(paste0('Search and Check'))
  for (spt_file in ls_sfls) {
    # 1. store pdf and html files in a subfolder
    # 2. main folder keeps only Rmd file
    # 3. delete tex and other files

    # Get Current File Path, Assume to be git, check status after
    st_fullpath_noname <- dirname(spt_file)
    st_fullpath_nosufx <- sub(paste0('\\',st_pattern,'$'), '', spt_file)
    st_file_wno_suffix <- sub(paste0('\\',st_pattern,'$'), '', basename(spt_file))
    # need to do setwd, need to check status within git repo
    setwd(st_fullpath_noname)

    # Check if the RMD file has been modified or is new, if neither, do not generate pdf html
    ls_file_status = ff_sup_git_status_check(spt_file)
    bl_anewfile = ls_file_status$bl_anewfile
    bl_modified = ls_file_status$bl_modified
    bl_nochange = ls_file_status$bl_nochange
    st_git_status = ls_file_status$st_git_status
    if (bl_gen_if_git_old) {
      st_git_status <- paste0('FORCE UPDATE, ', st_git_status)
    }
    message(st_git_status)


    # Modify and process
    if ((bl_modified + bl_anewfile == 1) |
        (bl_nochange & bl_gen_if_git_old)) {

      # Print Path
      if (bl_verbose) message(paste0('spt_file:',spt_file))
      if (bl_verbose) message(paste0('st_fullpath_noname:', st_fullpath_noname))
      if (bl_verbose) message(paste0('st_fullpath_nosufx:', st_fullpath_nosufx))
      if (bl_verbose) message(paste0('st_file_wno_suffix:', st_file_wno_suffix))

      # Generate New Paths and File Names
      spth_pdf <- paste0(st_fullpath_noname, st_folder_pdf)
      sname_pdf <- paste0(st_fullpath_noname, st_folder_pdf, st_file_wno_suffix)
      spth_html <- paste0(st_fullpath_noname, st_folder_html)
      sname_html <- paste0(st_fullpath_noname, st_folder_html, st_file_wno_suffix)
      spth_html_m <- paste0(st_fullpath_noname, st_folder_html_m)
      sname_html_m <- paste0(st_fullpath_noname, st_folder_html_m, st_file_wno_suffix)
      if (bl_verbose) message(spth_html)

      sfle_R <- paste0(st_fullpath_noname, st_folder_m, st_file_wno_suffix)

      sfl_nht <- paste0(st_fullpath_nosufx, '.nb.html')
      sfl_tex <- paste0(st_fullpath_nosufx, '.tex')
      sfl_pdf <- paste0(st_fullpath_nosufx, '.pdf')
      sfl_htm <- paste0(st_fullpath_nosufx, '.html')
      sfl_Rla <- paste0(st_fullpath_nosufx, '.R')
      sfl_log <- paste0(st_fullpath_nosufx, '.log')

      sfl_sub_tex <- paste0(sname_pdf, '.tex')
      sfl_sub_nht <- paste0(sname_html, '.nb.html')


      # Convert to PDF, HTML etc if Not Testing
      if (!bl_test){

        if (grepl('_main', spt_file)) {

          # try(file.remove(paste0(st_fullpath_nosufx, '.pdf')))
          # try(file.remove(paste0(st_fullpath_nosufx, '.html')))

        } else {

          # 0. remove tex and md files, which are intermediaries
          setwd(st_fullpath_noname)
          try(file.remove(paste0(st_folder_pdf, st_file_wno_suffix, '.pdf')))
          try(file.remove(paste0(st_folder_html_m, st_file_wno_suffix, '.html')))
          try(file.remove(paste0(st_folder_m, st_file_wno_suffix, '.m')))
          try(file.remove(paste0(st_folder_rmd, st_file_wno_suffix, '.Rmd')))

          # 1. Convert MLX to M PDF
          dir.create(file.path(st_fullpath_noname, st_folder_pdf))
          spg_pdf_convert <- paste0("matlab -batch ",
                                    "\"matlab.internal.liveeditor.openAndConvert(",
                                    "'",st_file_wno_suffix, ".mlx',",
                                    "'",st_folder_pdf, st_file_wno_suffix, ".pdf');",
                                    "exit\"")
          message(paste0('\n',spg_pdf_convert))
          st_system_spg_pdf_convert <- toString(system(spg_pdf_convert, intern=TRUE))
          message(st_system_spg_pdf_convert)
          ls_spt_pdf_generated <-
            c(ls_spt_pdf_generated, paste0(st_folder_pdf, st_file_wno_suffix, '.pdf'))

          # 2. Convert MLX to M HTML
          dir.create(file.path(st_fullpath_noname, st_folder_html_m))
          spg_mhtml_convert <- paste0("matlab -batch ",
                                      "\"matlab.internal.liveeditor.openAndConvert(",
                                      "'",st_file_wno_suffix, ".mlx',",
                                      "'",st_folder_html_m, st_file_wno_suffix, ".html');",
                                      "exit\"")
          message(paste0('\n',spg_mhtml_convert))
          st_system_spg_mhtml_convert <- toString(system(spg_mhtml_convert, intern=TRUE))
          message(st_system_spg_mhtml_convert)
          ls_spt_html_m_generated <-
            c(ls_spt_html_m_generated, paste0(st_folder_html_m, st_file_wno_suffix, '.html'))

          # 3. Convert MLX to M
          dir.create(file.path(st_fullpath_noname, st_folder_m))
          spg_m_convert <- paste0("matlab -batch ",
                                  "\"matlab.internal.liveeditor.openAndConvert(",
                                  "'",st_file_wno_suffix, ".mlx',",
                                  "'",st_folder_m, st_file_wno_suffix, ".m');",
                                  "exit\"")
          message(paste0('\n',spg_m_convert))
          st_system_spg_m_convert <- toString(system(spg_m_convert, intern=TRUE))
          message(st_system_spg_m_convert)
          ls_spt_m_generated <-
            c(ls_spt_m_generated, paste0(st_folder_m, st_file_wno_suffix, '.m'))

          # 4, generate RMD (mlx to tex to md to rmd)
          dir.create(file.path(st_fullpath_noname, st_folder_rmd))
          # 4a. Convert MLX to TEX (in html folder)
          spg_m_convert <- paste0("matlab -batch ",
                                  "\"matlab.internal.liveeditor.openAndConvert(",
                                  "'",st_file_wno_suffix, ".mlx',",
                                  "'",st_folder_rmd, st_file_wno_suffix, ".tex');",
                                  "exit\"")
          message(paste0('\n',spg_m_convert))
          st_system_spg_m_convert <- toString(system(spg_m_convert, intern=TRUE))
          message(st_system_spg_m_convert)
          # 4b. Convert python tex to md (in html folder)
          # python -c "from pyfan.util.rmd.mattexmd import fp_mlxtex2md; fp_mlxtex2md(spt_root='C:/Users/fan/Math4Econ/matrix_application/', ls_srt_subfolders=None, st_rglob_tex='twogoods.tex', verbose=True)"
          spg_tex2md <- paste0("python -c ",
                               "\"from pyfan.util.rmd.mattexmd import fp_mlxtex2md;",
                               "fp_mlxtex2md(",
                               "spt_root='", st_fullpath_noname, st_folder_rmd, "',",
                               "ls_srt_subfolders=None,",
                               "st_rglob_tex='",st_file_wno_suffix, ".tex');",
                               "\"")
          message(paste0('\n',spg_tex2md))
          st_system_spg_tex2md_convert <- toString(system(spg_tex2md, intern=TRUE))
          message(st_system_spg_tex2md_convert)
          # 4c. Convert python md to rmd (in html folder)
          # python -c "from pyfan.util.rmd.mattexmd import fp_md2rmd; fp_md2rmd(spt_root='C:/Users/fan/Math4Econ/matrix_application/', ls_srt_subfolders=None, snm_folder_yml='preamble.yml', st_rglob_md='twogoods.md', verbose=True)"
          spg_md2rmd <- paste0("python -c ",
                               "\"from pyfan.util.rmd.mattexmd import fp_md2rmd;",
                               "fp_md2rmd(",
                               "spt_root='", st_fullpath_noname, st_folder_rmd, "',",
                               "ls_srt_subfolders=None,",
                               "snm_folder_yml='preamble.yml',",
                               "st_rglob_md='",st_file_wno_suffix, ".md');",
                               "\"")
          message(paste0('\n',spg_md2rmd))
          st_system_spg_md2rmd_convert <- toString(system(spg_md2rmd, intern=TRUE))
          message(st_system_spg_md2rmd_convert)

          # 7. Move Image Folders
          dir.create(file.path(st_fullpath_noname, 'img'))
          dir.create(file.path(st_prj_root, 'img'))
          spg_imgmove <- paste0("python -c ",
                               "\"from pyfan.util.path.movefiles import fp_agg_move_subfiles;",
                               "fp_agg_move_subfiles(",
                               "spt_root_src='", st_fullpath_noname, "/',",
                               "st_srt_srh='", st_file_wno_suffix, "_images',",
                               "st_fle_srh='*.png',",
                               "srt_agg='img',",
                               "ls_srt_dest=['",st_fullpath_noname,"/','",st_prj_root,"/'],",
                               "bl_test=False,bl_delete_src=True);",
                               "\"")
          message(paste0('\n',spg_imgmove))
          st_system_spg_imgmove_convert <- toString(system(spg_imgmove, intern=TRUE))
          message(st_system_spg_imgmove_convert)


          # 6. remove tex and md files, which are intermediaries
          setwd(st_fullpath_noname)
          try(file.remove(paste0(st_folder_rmd, st_file_wno_suffix, '.tex')))
          try(file.remove(paste0(st_folder_rmd, 'matlab.sty')))
          try(file.remove(paste0(st_folder_rmd, st_file_wno_suffix, '.md')))
          # old m file
          try(file.remove(paste0(st_folder_rmd, st_file_wno_suffix, '_m.m')))

        }
      }
    }
  }

  # # Generate HTML based on RMD, already have MLX based PDF, and don't need curl to R
  # ls_stp_generated <-
  #   ff_sup_clean_rmd(ar_spt_root=ar_spt_root,
  #                    ar_spn_skip=ar_spn_skip,
  #                    st_folder_pdf = paste0('/',  st_folder_pdf),
  #                    st_folder_html = paste0('/', st_folder_html),
  #                    st_folder_R = paste0('/', st_folder_m),
  #                    bl_gen_if_git_old = bl_gen_if_git_old,
  #                    bl_recursive = bl_recursive,
  #                    bl_verbose = bl_verbose,
  #                    bl_test = bl_test,
  #                    it_hierachy_shift=it_hierachy_shift,
  #                    it_toc_depth=it_toc_depth,
  #                    ls_bool_convert=list(bl_pdf=FALSE, bl_html=TRUE, bl_R=FALSE),
  #                    ls_bool_remove=list(bl_remove_html=FALSE))
  # ls_spt_pdf_generated=ls_stp_generated$ls_spt_pdf_generated
  # ls_spt_html_generated=ls_stp_generated$ls_spt_html_generated
  # ls_spt_R_generated=ls_stp_generated$ls_spt_R_generated

  # Return
  return(list(ls_spt_html_m_generated=ls_spt_html_m_generated,
              ls_spt_m_generated=ls_spt_m_generated))
}

ff_sup_git_status_check <- function(spt_file){
  #' Check if local file is different from git version on windows
  #'
  #' @description
  #' A file could be new, or cold be modified, identify this by using the git status
  #' command and interpreting the output.
  #'
  #' @param spt_file path to file with file name
  #' @return a named list of bools and strings
  #' \itemize{
  #'   \item it_status - 1 is new, 2 is modified, 3 no change
  #'   \item st_git_status - string print out of file status
  #'   \item bl_modified - bool 1 if modified
  #'   \item bl_anewfile - bool 1 if new
  #'   \item bl_nochange - bool 1 if no change
  #' }
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #' @export
  #' @examples
  #' spt_file = 'C:/Users/fan/Math4Econ/README.md'
  #' ff_sup_git_status_check(spt_file)
  #'

  # 1. store pdf and html files in a subfolder
  # 2. main folder keeps only Rmd file
  # 3. delete tex and other files

  # Get Current File Path, Assume to be git, check status after
  st_fullpath_noname <- dirname(spt_file)
  # need to do setwd, need to check status within git repo
  setwd(st_fullpath_noname)

  # Check if the RMD file has been modified or is new, if neither, do not generate pdf html
  spg_check_git_status <- paste0('git status -s ', spt_file)
  st_git_status <- toString(system(spg_check_git_status, intern=TRUE))
  bl_modified <- grepl(' M ', st_git_status, fixed=TRUE)
  bl_anewfile <- grepl('?? ', st_git_status, fixed=TRUE)
  bl_nochange <- (st_git_status == "")

  # Return String and Status
  it_status = 3
  if (bl_modified == 1) {
    it_status = 1
    st_message_prefix <- 'MODIFIED: '
  } else if (bl_anewfile == 1) {
    it_status = 2
    st_message_prefix <- 'NEW FILE: '
  } else {
    it_status = 3
    st_message_prefix <- 'NO CHNGE: '
  }

  # output message string
  st_git_status <- paste0(st_message_prefix, spt_file)

  return(list(it_status=it_status, st_git_status=st_git_status,
              bl_modified=bl_modified, bl_anewfile=bl_anewfile, bl_nochange=bl_nochange))
}


ff_sup_clean_rmd_mod <- function(st_file, it_hierachy_shift=2, it_toc_depth=3) {
  #' Modify R4Econ (and others) Rmd files, add Numbered Sections for HTML, and higher Hierarchy
  #'
  #' @description
  #' Modify R4Econ Rmd files, add Numbered Sections for HTML, and higher Hierarchy. Was
  #' not able to, for HTML file, use pandoc arguments to add in TOC. And Need to have two hierarchies
  #' for RMD's individual specific pdf and html files, and the joint bookdown file combining all.
  #'
  #' @param st_file string texts contents from a R4Econ RMD file.
  #' @param it_hierachy_shift rmd modification level of promotion from bookdown hierachy
  #' to own file hierarchy
  #' @param it_toc_depth rmd own file outputs toc levels to show
  #' @return modified texts
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #'

  # Add in (1) TOC (2) Numbering (3) Menu Left Float
  st_search <- "html_document:"
  st_replace <- paste0("html_document:\n",
                       "    toc: true\n",
                       "    number_sections: true\n",
                       "    toc_float:\n",
                       "      collapsed: false\n",
                       "      smooth_scroll: false\n",
                       "      toc_depth: ", it_toc_depth)
  st_file_updated <- gsub(x = st_file,
                          pattern = st_search,
                          replacement = st_replace)

  # Update Hierarchy (Each file Own PDF and HTML Hierarchy Higher)
  # RMD originals start with triple pound as a part of larger bookdown structure
  if (it_hierachy_shift == 1) {
    st_file_updated <- gsub(x = st_file_updated, pattern = '## ', replacement = '# ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '### ', replacement = '## ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '#### ', replacement = '### ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '##### ', replacement = '#### ')
  } else if (it_hierachy_shift == 2) {
    st_file_updated <- gsub(x = st_file_updated, pattern = '### ', replacement = '# ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '#### ', replacement = '## ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '##### ', replacement = '### ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '###### ', replacement = '#### ')
  } else if (it_hierachy_shift == 3) {
    st_file_updated <- gsub(x = st_file_updated, pattern = '#### ', replacement = '# ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '##### ', replacement = '## ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '###### ', replacement = '### ')
    st_file_updated <- gsub(x = st_file_updated, pattern = '####### ', replacement = '#### ')
  }

  # Return
  return(st_file_updated)
}


ff_sup_lower_hierarchy <- function(st_file, it_hierachy_lower_rmd=1) {
  #' Increase the RMD Hierarchy of Existing Files
  #'
  #' @description
  #' Modify Rmd files by increasing pound sign counts. This is needed for the RMD files
  #' converted from mlx, where I can only set file top hierachy to 2 pounds, but should be
  #' three for M4Econ
  #'
  #' @param st_file string texts contents from a R4Econ RMD file.
  #' @param it_hierachy_lower_rmd integer how many pound tiers to promote hierarchy by
  #' @return modified texts
  #' @author Fan Wang, \url{http://fanwangecon.github.io}
  #'

  if (it_hierachy_lower_rmd == 1) {
    st_file_updated <- gsub(x = st_file, pattern = '# ', replacement = '## ')
  } else if (it_hierachy_lower_rmd == 2) {
    st_file_updated <- gsub(x = st_file, pattern = '# ', replacement = '### ')
  }

  # Return
  return(st_file_updated)
}
