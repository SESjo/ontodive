#' matlab_to_posix
#'
#' @param x column with Matlab datenum
#' @param timez time zone to use
#'
#' @return A vector of POSIXct
#' @export
#' @references
#' \code{\link["http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/"]{http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/}}
#'
#' @examples matlab_to_posix(c(737182.4))
matlab_to_posix = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day

  return(as.POSIXct(
    strftime(
      as.POSIXct(secs, origin = '1970-1-1',
                 tz = 'UTC'),
      format = '%Y-%m-%d %H:%M',
      tz = 'UTC',
      usetz = FALSE
    ),
    tz = timez
  ))
}
