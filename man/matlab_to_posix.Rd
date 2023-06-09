% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matlab_to_posix.R
\name{matlab_to_posix}
\alias{matlab_to_posix}
\title{Convert Matlab datenum to POSIX}
\usage{
matlab_to_posix(x, timez = "UTC")
}
\arguments{
\item{x}{column with Matlab datenum}

\item{timez}{time zone to be used}
}
\value{
A vector of POSIXct
}
\description{
Small function to convert Matlab datenum to POSIX time
}
\examples{
matlab_to_posix(c(737182.4))

}
\references{
\href{http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/}{http://lukemiller.org/index.php/2011/02/converting-matlab-and-r-date-and-time-values/}
}
