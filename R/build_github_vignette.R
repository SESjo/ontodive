#' Build Vignette for Github
#'
#' This function aims at processing vignettes (building, organizing) before pushing on Github so that user can download a package with pre-compiled vignettes. Using the website argument allow to build the associated website.
#'
#' To make sure pre-compiled vignettes are enclosed within the package downloaded from Github, vignettes are (i) compiled with tools::buildVignettes, (ii) then copy-paste to `inst\\doc`. To build the website, this function first converts README.Rmd file to README.md file, and then calls pkgdown::build_site() since this function only take into account *.md.
#'
#' @param website A boolean to compile the website associated with weanlingNES package
#'
#' @return A `\\docs` folder containing the website and a `inst\\doc` folder containing the vignettes
#'
#' @export
#'
#' @references
#' \href{https://community.rstudio.com/t/how-to-add-vignette-html-or-r-files-to-a-github-rep/45905/7}{https://community.rstudio.com/t/how-to-add-vignette-html-or-r-files-to-a-github-rep/45905/7}
#'
#' @examples
#' \dontrun{
#' # compile vignettes
#' build_github_vignette()
#' }
build_github_vignette <- function(website = TRUE) {
  if (website == TRUE){
    # compute readme as *.md
    rmarkdown::render("README.Rmd", rmarkdown::md_document())
    # compute website
    pkgdown::build_site()
  }
  # build vignettes
  tools::buildVignettes(dir = ".", tangle = TRUE)
  # create the proper folders
  dir.create("inst/doc")
  # copy the proper files
  file.copy(dir(
    "vignettes",
    full.names = TRUE,
    pattern = c("*.html|*.Rmd|*.R")
  ),
  "inst/doc",
  overwrite = TRUE
  )
}
