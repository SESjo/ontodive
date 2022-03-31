#' @title Retrieve elements' name of a list within apply function
#'
#' @description This small function should be used within an `apply` function when retrieving the element's name is needed.
#'
#' @return The name of the element considered
#'
#' @export
#'
#' @references
#' \href{https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun}{https://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun}
#'
#' @examples
#' # list creation
#' A <- list(name_1 = NULL, name_2 = NULL)
#'
#' # print name of each element using .n()
#' sapply(A, function(x) .n())
.n <- function() {
  env <- parent.frame(2)
  names(c(env$X, env$.x))[env$i[]]
}
