#' @title Oceanographic information from North East Pacific
#'
#' @description This data where retrieve from
#' \href{https://resources.marine.copernicus.eu/}{Copernicus} using the product
#' \strong{GLOBAL_MULTIYEAR_PHY_001_030} and the dataset
#' \strong{cmems_mod_glo_phy_my_0.083_P1D-m}.
#'
#' @md
#'
#' @format A list of two data tables, one for northern elephant seals and
#' the other for southern elephant seals, where each row correspond to a
#' cell of 0.083° × 0.083° for a specific time and location.
#'
#' * $northen: [lat=c(25, 55); lon=c(-165, -115); time c("2018-05-01", "2019-05-01")]
#' * $southern: [lat=c(-65, -40); lon=c(35, 115); time c("2015-01-01", "2015-11-30")]
#'
#' \describe{
#' \item{time}{date}
#' \item{latitude}{latitude}
#' \item{longitude}{longitude}
#' \item{uo}{eastward ocean current velocity (m/s)}
#' \item{vo}{northward ocean current velocity (m/s)}
#' \item{so}{salinity (psu)}
#' \item{thetao}{potential temperature (°C)}
#' \item{zos}{sea surface height above geoid (m)}
#' \item{vel}{ocean current velocity (m/s)}
#' }
#'
#' @source
#' \href{https://resources.marine.copernicus.eu/product-detail/GLOBAL_MULTIYEAR_PHY_001_030/INFORMATION}{https://resources.marine.copernicus.eu/product-detail/GLOBAL_MULTIYEAR_PHY_001_030/INFORMATION}
#'
#' @usage data(data_cop)
#'

"data_cop"
