## ----setup, include=FALSE---------------------------------------
# global option relative to rmarkdown
knitr::opts_chunk$set(
  echo = TRUE,
  fig.align = "center",
  message = FALSE,
  warning = FALSE
  # tidy = TRUE
)

## ----create-logo-1----------------------------------------------
# loading library
library(hexSticker)
library(showtext)

# automatically use showtext to render text for future devices
showtext_auto()

## ----create-logo-2, fig.cap = "Our logo!!"----------------------
# load the right font
font_add_google("Courier Prime")

# print
print(
  sticker(
    "https://i.imgur.com/QUzPuJK.png",
    package = "weanlingNES",
    p_family = "Courier Prime",
    p_size = 28,
    p_y = .6,
    s_x = 1,
    s_y = 1.3,
    s_width = .8,
    h_color = "#fdc700",
    h_fill = "#003c6c",
    url = "jjoumaa.ddns.net/weanlingNES",
    u_color = "#13a5dc",
    u_size = 6,
    spotlight = T,
    asp = 0.8,
  )
)

## ----create-logo-3, eval=FALSE, include=FALSE-------------------
#  #' just as a back up to recreate the logo with the right proportion, i.e. it
#  #' it renders differently with rmarkdown
#  sticker(
#    "https://i.imgur.com/QUzPuJK.png",
#    package = "weanlingNES",
#    p_family = "Courier Prime",
#    p_size = 18,
#    s_x = 1,
#    s_y = 1.3,
#    p_y = .6,
#    filename = "inst/logo/logo.png",
#    h_color = "#fdc700",
#    h_fill = "#003c6c",
#    spotlight = T,
#    s_width = .8,
#    asp = 0.8,
#    url = "jjoumaa.ddns.net/weanlingNES",
#    u_color = "#13a5dc",
#    u_size = 4
#  )

