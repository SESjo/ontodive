#' @title Convert Matlab datenum to POSIX
#'
#' @description Small function to convert Matlab datenum to POSIX time
#'
#' @param x column with Matlab datenum
#' @param timez time zone to be used
#'
#' @return A vector of POSIXct
#' @export
#' @references
#' \href{http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/}{http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/}
#'
#' @examples matlab_to_posix(c(737182.4))
#'
matlab_to_posix <- function(x, timez = "UTC") {
  days <- x - 719529 # 719529 = days from 1-1-0000 to 1-1-1970
  secs <- days * 86400 # 86400 seconds in a day

  return(as.POSIXct(
    strftime(
      as.POSIXct(secs,
        origin = "1970-1-1",
        tz = "UTC"
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC",
      usetz = FALSE
    ),
    tz = timez
  ))
}
