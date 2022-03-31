#' @title Check if an object is a `data.table`
#'
#' @description Allow to quickly check if an object is a `data.table`
#'
#' @details This function checks if the class `data.table` is among the considered object's classes.
#'
#' @param dt The object to check
#'
#' @return A boolean:
#'
#' * TRUE the object is a data.table
#'
#' * FALSE the object is not a data.table
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
