#' Retrieve elements' name of a list within apply function
#'
#' This small function should be used within an apply function when retrieving the element's name is needed.
#'
#' @return The name of the element considered
#'
#' @references <https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun>
#'
#' @examples A = list(name_1 = NULL, name_2 = NULL)
#' sapply(A,function(x) .n())
.n <- function() {
  env <- parent.frame(2)
  names(c(env$X, env$.x))[env$i[]]
}
