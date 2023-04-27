#' @title Diving parameters of weanling Northern Elephant Seals in 2016 and 2018
#'
#' @description A list of datasets containing diving parameters for:
#' * 4 individuals in 2018
#'
#' @details This dataset have been obtained using the following files:
#' * 2018070_nese0000annu_1790212_SubSample_iknos_DiveStat_RSB.csv
#' * 2018070_results-2-GPE3.csv
#' * 2018072_BothTrips_1790214_SubSample_iknos_DiveStat_RSB.csv
#' * 2018072_results-2-GPE3.csv
#' * 2018074_nese0000annu_1790218_SubSample_iknos_DiveStat_RSB.csv
#' * 2018080_nese0000annu_1790226_SubSample_iknos_DiveStat_RSB.csv
#' * 2018080_results-2-GPE3.csv
#'
#' @md
#'
#' @format A list containing one list per year, that itself contains one dataset per individual.
#'
#' \strong{DATA > YEAR > INDIVIDUAL}
#'
#' \enumerate{
#' \item \strong{2018 Dataset}
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
#' \item{sp}{species (nes)}
#' \item{ssh}{sea surface height from Copernicus}
#' \item{psu}{Practical Salinity Unit from Copernicus}
#' \item{vel}{current velocity in m/s derived from Copernicus}
#' \item{temp}{sea surface temperature from Copernicus}
#' \item{bathy}{bathymetry from marmap package}
#' \item{phase}{phase of the day (day or night)}
#' }
#' }
#' @source \email{roxanne@@ucsc.edu}
#'
#' @usage data(data_nes)
#'
"data_nes"
