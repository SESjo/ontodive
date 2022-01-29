#' Diving parameters of weanlings in 2016 and 2018
#'
#' A list of datasets containing diving parameters for:
#' * 6 individuals in 2016
#' * 4 individuals in 2018
#'
#' These datasets have been processed using the following files:
#' * 2018070_nese0000annu_1790212_SubSample_iknos_DiveStat_RSB.csv
#' * 2018070_results-2-GPE3.csv
#' * 2018072_BothTrips_1790214_SubSample_iknos_DiveStat_RSB.csv
#' * 2018072_results-2-GPE3.csv
#' * 2018074_nese0000annu_1790218_SubSample_iknos_DiveStat_RSB.csv
#' * 2018080_nese0000annu_1790226_SubSample_iknos_DiveStat_RSB.csv
#' * 2018080_results-2-GPE3.csv
#' * LAT290_3449_170426_211859_00.csv
#' * LAT290_3450_161217_131810_00_Scaled.csv
#' * LAT290_3456_161217_131235_00_Scaled.csv
#' * LAT290_3457_161217_130604_00_Scaled.csv
#' * LAT290_3460_161217_125958_00_Scaled.csv
#' * LAT290_3463_161108_075936_00_Scaled.csv
#'
#' @md
#'
#' @format A list containing one list per year, that itself contains one dataset per individuals.
#'
#' \strong{DATA > YEAR > INDIVIDUAL}
#'
#' \enumerate{
#' \item \strong{2018 Dataset}
#' \describe{
#' \item{.id}{individual ID}
#' \item{date}{date and time of the dive}
#' \item{phase}{phase of the day (day or night)}
#' \item{divenumber}{# of dive}
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
#' \item{lightatbott}{light level at the bottom}
#' \item{lwiggles}{}
#' \item{lightatsurf}{light level at the surface}
#' \item{lattenuation}{}
#' \item{euphoticdepth}{euphotic depth (m)}
#' \item{tempatsurf}{temperature at the surface (C)}
#' \item{tempatbott}{temprature at the bottom (C)}
#' \item{thermoclinedepth}{thermocline depth (m)}
#' \item{driftdiveindex}{metric that reflects dive characteristics and how likely it is to be a drift dive}
#' \item{driftrate}{drift rate (m/s)}
#' \item{benthicdiveindex}{metric that reflects dive characteristics and how likely it is to be a benthic dive}
#' \item{benthicdivevertrate}{}
#' \item{cornerindex}{}
#' \item{foragingindex}{}
#' \item{verticalspeed90perc}{}
#' \item{verticalspeed95perc}{}
#' \item{divetype}{Dive type (transit, foraging, drift or transit)}
#' \item{day_departure}{# of days since departure}
#' \item{lat}{last latitude received}
#' \item{lon}{last longitude received}
#' \item{dist_dep}{distance from departure (m)}
#' }
#' \item \strong{2016 Dataset}
#' \describe{
#' \item{rec#}{# of days recorded}
#' \item{date}{date}
#' \item{time}{time}
#' \item{sunrise}{}
#' \item{sunset}{}
#' \item{tflatn_degs}{}
#' \item{tflats_degs}{}
#' \item{tfnoonn}{}
#' \item{tfnoons}{}
#' \item{sst2_c}{sea surface temperature (C)}
#' \item{sst2depth_dbars}{}
#' \item{sst2time}{}
#' \item{tflaterrn}{}
#' \item{tflaterrs}{}
#' \item{tflonerrn}{}
#' \item{tflonerrs}{}
#' \item{mininttemp_c}{}
#' \item{maxexttemp_c}{}
#' \item{maxpress_dbars}{}
#' \item{latitude_degs}{}
#' \item{longitude_degs}{}
#' \item{tflonn_degs}{}
#' \item{tflons_degs}{}
#' }
#' }
#' @source \email{roxanne@@ucsc.edu}
"data_nes"
