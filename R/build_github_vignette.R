#' Build Vignette for Github
#'
#' This function aims at processing vignettes (building, organizing) before pushing on Github so that user can download a package with vignettes
#'
#' @return
#' @export
#' @references
#' \code{\link[community.rstudio.com/t/how-to-add-vignette-html-or-r-files-to-a-github-rep/45905/7]{https://community.rstudio.com/t/how-to-add-vignette-html-or-r-files-to-a-github-rep/45905/7}}
#'
#' @examples build_github_vignette()
build_github_vignette <- function() {
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
