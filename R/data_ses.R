#' @title Diving parameters of weanling Southern Elephant Seals in 2014
#'
#' @description A list of datasets containing diving parameters for:
#' * 9 individuals in 2014
#' These data have been retrieved from the CEBC server
#' (ftpmarin.cebc.cnrs.fr/EarlyLife/PostDoc%20Sam%20Cox/SES_PupData/RawData_2014-2015,
#' please ask Baptiste Picard \email{baptiste.picard@@cebc.cnrs.fr} to get
#' access)
#'
#' @details These datasets have been processed using the following files:
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
#' @format A list containing one list per year, that itself contains one dataset per individuals.
#'
#' \strong{DATA > YEAR > INDIVIDUAL}
#'
#' \describe{
#' \item{.id}{individual ID}
#' \item{date}{date and time of the dive}
#' \item{lat}{last latitude received}
#' \item{lon}{last longitude received}
#' \item{divenumber}{# of dive}
#' \item{day_departure}{# of days since departure}
#' \item{year}{year of the dive}
#' \item{month}{month of the dive}
#' \item{day}{day of the dive}
#' \item{hour}{hour of the dive}
#' \item{min}{min of the dive}
#' \item{sec}{sec of the dive}
#' \item{juldate}{date as julian date}
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
#' \item{bottrange}{}
#' \item{efficiency}{}
#' \item{idz}{}
#' \item{driftdiveindex}{metric that reflects dive characteristics and how likely it is to be a drift dive}
#' \item{driftrate}{drift rate (m/s)}
#' \item{benthicdiveindex}{metric that reflects dive characteristics and how likely it is to be a benthic dive}
#' \item{benthicdivevertrate}{}
#' \item{cornerindex}{}
#' \item{foragingindex}{}
#' \item{verticalspeed90perc}{}
#' \item{verticalspeed95perc}{}
#' \item{divetype}{Dive type (transit, foraging, drift or transit)}
#' \item{sp}{Acronym for the species}
#' \item{dist_dep}{Distance from the first data location point (m?)}
#' \item{temp}{Sea surface temperature from Copernicus data (data_cop)}
#' \item{ssh}{Sea surface height from Copernicus data (data_cop)}
#' \item{psu}{Practical Salinity Unit from Copernicus data (data_cop)}
#' \item{vel}{Current velocity in m/s derived from Copernicus data (data_cop)}
#' \item{bathy}{Bathymetry from marmap package}
#' \item{phase}{phase of the day (day or night)}
#' }
#'
#' @source \email{baptiste.picard@@cebc.cnrs.fr}
"data_ses"