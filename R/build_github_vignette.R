#' @title Build Vignettes for Github
#'
#' @description  This function aims at processing vignette (building, organizing)
#' before pushing on Github so that user can download a package with
#' pre-compiled vignette. Using the website argument allow to build the
#' associated website.
#'
#' @details To make sure pre-compiled vignette are enclosed within the package
#' downloaded from Github, vignette are (\emph{i}) compiled with \code{tools::buildVignettes},
#' (\emph{ii}) then copy-paste to \code{inst\\doc}. To build the website, this function
#' first converts \code{README.Rmd} file to \code{README.md} file, and then calls
#' \code{pkgdown::build_site()} since this function only take into account \code{*.md}.
#'
#' @rawNamespace import(dplyr, except = c(between))
#' @import pkgdown
#'
#' @param article The name or the path to the \code{.Rmd} file to be compiled
#' @param website A boolean to compile the website associated with the article
#' @param vignette A boolean to compile the vignette associated with the article
#' @param clean Logical to remove cache and temporary files
#'
#' @return A \code{\\docs} folder containing the website and a \code{inst\\doc} folder containing the vignette
#'
#' @export
#'
#' @references
#' \href{https://community.rstudio.com/t/how-to-add-article-html-or-r-files-to-a-github-rep/45905/7}{https://community.rstudio.com/t/how-to-add-article-html-or-r-files-to-a-github-rep/45905/7}
#'
#' @examples
#' \dontrun{
#' # compile all articles into vignette and website
#' build_github_vignette()
#' }
build_github_vignette <- function(article = NULL,
                                  website = NULL,
                                  vignette = NULL,
                                  clean = FALSE) {
  # to avoid warnings when checking the package
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  . <- NULL

  # if "article" is not only the name of a file
  # (e.g with extension ".Rmd" or path "/")
  if (grepl(".Rmd|/", article) && !is.null(article)) {
    # split the character chain with "/"
    article <- strsplit(article, "/") %>%
      # select the last element of the list
      dplyr::last(.) %>%
      # select the last item of this last element
      dplyr::last(.) %>%
      # remove ".Rmd"
      sub(".Rmd", "", .)
  }

  # if nothing is specified then build vignette and website
  if (is.null(website) & is.null(vignette)) {
    website <- TRUE
    vignette <- TRUE
  }

  # check if article argument is null
  if (is.null(article)) {
    # we rebuild the all documentation (vignette + website)
    if (!is.null(website) && website == TRUE) {
      # if clean
      if (clean) {
        pkgdown::clean_site()
      }
      # update favicon
      pkgdown::build_favicons(pkg = ".", overwrite = TRUE)
      # "re"initiate a website
      pkgdown::init_site()
      # compute readme as *.md
      rmarkdown::render("README.Rmd", rmarkdown::md_document())
      # compute website
      pkgdown::build_site()
      # copy the external files
      file.copy(
        dir(
          "./vignettes/",
          recursive = TRUE,
          full.names = TRUE,
          pattern = c("*.mp4$")
        ),
        "./docs/articles",
        overwrite = TRUE
      )
      # make sure image/gif generated within rmarkdown would be found on website
      system("sed -i 's+../docs+..+g' ./docs/articles/*.html")
    }
    if (!is.null(vignette) && vignette == TRUE) {
      # if clean
      if (clean) {
        unlink(
          dir("./vignettes/",
            full.names = T,
            pattern = "*_cache"
          ),
          recursive = TRUE
        )
      }
      # build vignette
      tools::buildVignettes(
        dir = ".",
        tangle = TRUE
      )
      # create the proper folders
      dir.create("inst/doc")
      # copy the proper files
      file.copy(
        dir(
          "./vignettes",
          recursive = TRUE,
          full.names = TRUE,
          pattern = c("*.html$|*.Rmd$|*.R$|*.mp4$")
        ),
        "./inst/doc/",
        overwrite = TRUE
      )
      # make sure image/gif generated within rmarkdown would be found on website
      system("sed -i 's+../docs+..+g' ./docs/articles/*.html")
    }
  } else {
    if (!is.null(website) && website == TRUE) {
      # update the article
      pkgdown::build_article(article)
      # make sure image/gif generated within rmarkdown would be found on website
      system("sed -i 's+../docs+..+g' ./docs/articles/*.html")
    }

    if (!is.null(vignette) && vignette == TRUE) {
      # if clean
      if (clean) {
        unlink(paste0("./vignettes/", article, "_cache"), recursive = TRUE)
      }
      # update the vignette
      tools::buildVignette(
        file = paste0("./vignettes/", article, ".Rmd"),
        dir = "./vignettes/",
        tangle = TRUE
      )
      # if it does not exist
      if (!dir.exists("./inst/doc")) {
        # create the doc folder
        dir.create("./inst/doc")
      }
      # copy the proper file
      file.copy(
        dir(
          "./vignettes",
          recursive = TRUE,
          full.names = TRUE,
          pattern = c(paste0(
            article, ".html$|",
            article, ".Rmd$|",
            article, "*.R$"
          ))
        ),
        "./inst/doc/",
        overwrite = TRUE
      )
      # copy the external files
      file.copy(
        dir(
          "./vignettes",
          recursive = TRUE,
          full.names = TRUE,
          pattern = c("*.mp4$")
        ),
        "./inst/doc/",
        overwrite = TRUE
      )
      # make sure image/gif generated within rmarkdown would be found on website
      system("sed -i 's+../docs+..+g' ./docs/articles/*.html")
    }
  }
}
