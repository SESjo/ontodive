#' Oceanographic information from North East Pacific
#'
#' This data where retrieve from \href{copernicus}{https://resources.marine.copernicus.eu/}
#' using the product \strong{GLOBAL_MULTIYEAR_PHY_001_030}.
#'
#' @md
#'
#' @format A data table with one row = one cell for a specific time and location
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

"data_cop"
