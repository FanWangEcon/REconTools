% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ff_sup_inout.R
\name{ff_sup_clean_mlx}
\alias{ff_sup_clean_mlx}
\title{Convert mlx files to tex and pdf}
\usage{
ff_sup_clean_mlx(
  st_prj_root,
  ar_spt_root,
  ar_spn_skip,
  st_pattern = ".mlx",
  st_folder_pdf = "htmlpdfm/",
  st_folder_html = "htmlpdfm/",
  st_folder_m = "htmlpdfm/",
  st_folder_html_m = "",
  st_folder_rmd = "",
  bl_gen_if_git_old = FALSE,
  bl_recursive = TRUE,
  bl_verbose = TRUE,
  bl_test = TRUE,
  it_hierachy_shift = 2,
  it_toc_depth = 3,
  st_conda_env = "wk_pyfan"
)
}
\arguments{
\item{st_prj_root}{string project root for image folder movement}

\item{ar_spt_root}{array a string array of path roots in which to search for Rmd files to knit}

\item{ar_spn_skip}{array a string array of names, if path found contains any of the string}

\item{st_pattern}{string search pattern suffix
in the array, will skip.}

\item{st_folder_pdf}{string subfolder where to store the pdf file, if just empty, store in rmd folder}

\item{st_folder_html}{string subfolder where to store the html file, if just empty, store in rmd folder}

\item{bl_gen_if_git_old}{boolean if true then even if RMD files do not have git status change,
still update pdf and html files.}

\item{bl_recursive}{boolean if to search in folders recursively}

\item{bl_test}{boolean if testing, meaning do not generate pdf html, just see which files are been}

\item{it_hierachy_shift}{rmd modification level of promotion from bookdown hierachy
to own file hierarchy}

\item{it_toc_depth}{rmd own file outputs toc levels to show
considered included, searched and found}

\item{st_conda_env}{string variable of conda env name}

\item{st_folder_R}{string subfolder where to store the R file, if just empty, store in rmd folder}
}
\value{
a list of string paths of files generated
\itemize{
  \item ls_spt_pdf_generated - a list of pdf file names
  \item ls_spt_html_generated - a list of html file names
  \item ls_spt_R_generated - a list of R file names generated
}#'
}
\description{
If a mlx file has been edited, in math4econ for example, check it against git and
convert to tex and pdf using command line matlab. Then the function wraps \strong{ff_sup_clean_rmd}
}
\examples{
ar_spt_root = c('C:/Users/fan/Math4Econ/math/matrix_application')
ar_spn_skip <- c('matrix', 'tibble', '_main', '_mod')
ff_sup_clean_mlx(ar_spt_root, ar_spn_skip)

}
\references{
\url{https://fanwangecon.github.io/R4Econ/development/inout/fs_rmd_pdf_html.html}
\url{https://github.com/FanWangEcon/REconTools/blob/master/R/ff_sup_inout.R}
}
\author{
Fan Wang, \url{http://fanwangecon.github.io}
}
