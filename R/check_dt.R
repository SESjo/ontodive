#' Check is an object is a `data.table`
#'
#' Allow to rapidly check if an object is a `data.table`
#' @param data
#'
#' @return A boolean:
#' TRUE: the object is a data.table
#' FALSE: the object is not a data.table
#' @export
#'
#' @importFrom data.table data.table
#'
#' @examples
#' # data.table creation
#' dt = data.table()
#'
#' # check if `dt` is a data.table
#' check_dt(dt)
check_dt = function(data){
  # return
  return(any(class(data) == "data.table"))
}
