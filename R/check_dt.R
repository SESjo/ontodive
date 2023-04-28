#' @title Check if an object is a \code{data.table}
#'
#' @description Allow to quickly check if an object is a \code{data.table}
#'
#' @details This function checks if the class \code{data.table} is among the considered object's classes.
#'
#' @param dt The object to check
#'
#' @return A boolean:
#'
#' * TRUE the object is a \code{data.table}
#'
#' * FALSE the object is not a \code{data.table}
#'
#' @export
#'
#' @importFrom data.table data.table
#'
#' @examples
#' # load data.table
#' library(data.table)
#'
#' # data.table creation
#' dt <- data.table()
#'
#' # check if `dt` is a data.table
#' check_dt(dt)
check_dt <- function(dt) {
  # return
  return(any(class(dt) == "data.table"))
}
