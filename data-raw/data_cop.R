#' This script is used to import copernicus data from *.nc file downloaded
#' using this link: https://resources.marine.copernicus.eu/product-detail/GLOBAL_MULTIYEAR_PHY_001_030/INFORMATION
#'
# import library
library(data.table)
library(magrittr)
library(tidync)

# set the maximum number of threads
setDTthreads(parallel::detectCores())

# set the right locale to deal with date
Sys.setlocale(locale = "C")

# path to dataraw
path_raw <- system.file("extdata",
  package = "ontodive"
)

# import the nc files
nc_file_northern <- tidync(paste0(path_raw, "/copernicus/cmems_mod_glo_phy_my_0.083_P1D-m_1640718340185.nc"))
nc_file_southern <- tidync(paste0(path_raw, "/copernicus/cmems_mod_glo_phy_my_0.083_P1D-m_1647669110588.nc"))

# > print(nc_file_northern)
#
# Data Source (1): cmems_mod_glo_phy_my_0.083_P1D-m_1640718340185.nc ...
#
# Grids (6) <dimension family> : <associated variables>
#
#   [1]   D3,D2,D1,D0 : vo, thetao, uo, so    **ACTIVE GRID** ( 79407726  values per variable)
# [2]   D3,D2,D0    : zos
# [3]   D0          : time
# [4]   D1          : depth
# [5]   D2          : latitude
# [6]   D3          : longitude
#
# Dimensions 4 (all active):
#
#   dim   name      length        min        max start count       dmin       dmax unlim coord_dim
# <chr> <chr>      <dbl>      <dbl>      <dbl> <int> <int>      <dbl>      <dbl> <lgl> <lgl>
#   1 D0    time         366 598980     607740         1   366 598980     607740     FALSE TRUE
#   2 D1    depth          1      0.494      0.494     1     1      0.494      0.494 FALSE TRUE
#   3 D2    latitude     361     25         55         1   361     25         55     FALSE TRUE
#   4 D3    longitude    601   -165       -115         1   601   -165       -115     FALSE TRUE

# extract vo, thetao, uo, so
nc_tbl_north_1 <- nc_file_northern %>%
  activate("D3,D2,D1,D0") %>%
  hyper_tibble(force = TRUE) %>%
  setDT()
nc_tbl_south_1 <- nc_file_southern %>%
  activate("D3,D2,D1,D0") %>%
  hyper_tibble(force = TRUE) %>%
  setDT()

# extract zos
nc_tbl_north_2 <- nc_file_northern %>%
  activate("D3,D2,D0") %>%
  hyper_tibble(force = TRUE) %>%
  setDT()
nc_tbl_south_2 <- nc_file_southern %>%
  activate("D3,D2,D0") %>%
  hyper_tibble(force = TRUE) %>%
  setDT()

# clear memory
gc()

# remove depth column
nc_tbl_north_1[, depth := NULL]
nc_tbl_south_1[, depth := NULL]

# merge both dataset
data_cop_north <- nc_tbl_north_1[nc_tbl_north_2, on = c("longitude", "latitude", "time")]
data_cop_south <- nc_tbl_south_1[nc_tbl_south_2, on = c("longitude", "latitude", "time")]

# clear memory
rm(nc_tbl_south_2, nc_tbl_north_2, nc_tbl_south_1, nc_tbl_north_1)
gc()

# velocity calculation
data_cop_north[, vel := sqrt(uo^2 + vo^2)]
data_cop_south[, vel := sqrt(uo^2 + vo^2)]

# time transformation
# library(dplyr)
# library(ncmeta)
# nc_atts(paste0(path_raw, "/copernicus/cmems_mod_glo_phy_my_0.083_P1D-m_1640718340185.nc"),
#         "time") %>%
#   # we want information on the unit for time column
#   dplyr::filter(name == "units") %>%
#   pull(value)
# > $units
# > [1] "hours since 1950-01-01 00:00:00"
data_cop_north[, time := as.Date(as.POSIXct("1950-01-01") + time * 3600)]
data_cop_south[, time := as.Date(as.POSIXct("1950-01-01") + time * 3600)]

# recreate list
data_cop <- list(
  northern = data_cop_north,
  southern = data_cop_south
)
# export
usethis::use_data(data_cop, overwrite = TRUE)
