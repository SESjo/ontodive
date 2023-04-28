#' @title Diving parameters of weanling Southern Elephant Seals in 2014
#'
#' @description A list of datasets containing diving parameters for:
#' * 9 individuals in 2014
#' These data have been retrieved from the CEBC server
#' (\code{ftpmarin.cebc.cnrs.fr/EarlyLife/PostDoc%20Sam%20Cox/SES_PupData/RawData_2014-2015},
#' please ask Baptiste Picard \email{baptiste.picard@@cebc.cnrs.fr} to get
#' access)
#'
#' @details This dataset have been obtained using the following files:
#' * Pup_140059.rds
#' * Pup_140060.rds
#' * Pup_140062.rds
#' * Pup_140062.rds
#' * Pup_140063.rds
#' * Pup_140068.rds
#' * Pup_140069.rds
#' * Pup_140072.rds
#' * Pup_140073.rds
#' * Pup_140075.rds
#'
#' @md
#'
#' @format A list containing one list per year, that itself contains one dataset per individual.
#'
#' \strong{DATA > YEAR > INDIVIDUAL}
#'
#' \enumerate{
#' \item \strong{2014 Dataset}
#' \describe{
#' \item{.id}{individual ID}
#' \item{divenumber}{# of dive}
#' \item{date}{date and time of the dive}
#' \item{maxdepth}{maximum depth reached (m)}
#' \item{dduration}{dive duration (s)}
#' \item{botttime}{bottom duration (s)}
#' \item{desctime}{descent duration (s)}
#' \item{descrate}{descent rate (m/s)}
#' \item{asctime}{ascent time (s)}
#' \item{ascrate}{ascent rate (m/s)}
#' \item{pdi}{post dive interval (s)}
#' \item{dwigglesdesc}{# of wiggles in descent phase}
#' \item{dwigglesbott}{# of wiggles in bottom phase}
#' \item{dwigglesasc}{# of wiggles in ascent phase}
#' \item{totvertdistbot}{vertical distance at the bottom (m)}
#' \item{driftrate}{drift rate (m/s)}
#' \item{divetype}{Dive type (transit, foraging, drift or benthic)}
#' \item{day_departure}{# of days since departure}
#' \item{lightatsurf}{light level at the surface}
#' \item{lat}{latitude}
#' \item{lon}{longitude}
#' \item{dist_dep}{distance from the first data location point (m)}
#' \item{sp}{species (ses)}
#' \item{ssh}{sea surface height from Copernicus data}
#' \item{psu}{Practical Salinity Unit from Copernicus}
#' \item{vel}{current velocity in m/s derived from Copernicus}
#' \item{temp}{sea surface temperature from Copernicus}
#' \item{bathy}{bathymetry from marmap package}
#' \item{phase}{phase of the day (day or night)}
#' }
#' }
#'
#' @source \email{baptiste.picard@@cebc.cnrs.fr}
#'
#' @usage data(data_ses)
#'
"data_ses"
