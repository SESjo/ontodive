## ----test-1, include = FALSE------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup------------------------------------------------------
library(weanlingNES)

## ----test-2-----------------------------------------------------
library(fpc)
set.seed(665544)
n <- 600
x <- cbind(runif(10, 0, 10)+rnorm(n, sd=0.2), runif(10, 0, 10)+rnorm(n,
  sd=0.2))
par(bg="grey40")
ds <- dbscan(x, eps=0.2,MinPts = 10,
            method = "raw")
# run with showplot=1 to see how dbscan works.
ds
plot(ds, x)

## ----test-1-bis-------------------------------------------------
knitr::include_graphics("https://upload.wikimedia.org/wikipedia/commons/2/2c/Rotating_earth_%28large%29.gif")

## ----test-2-bis-------------------------------------------------
library(ggplot2)
library(gganimate)
library(magick)

anim <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

## ---------------------------------------------------------------
animate(anim, nframe = 20, renderer = magick_renderer())

## ----test-3-----------------------------------------------------
pun = animate(anim, nframe = 20, renderer = magick_renderer())
# pdeux = animate(anim, nframe = 20, renderer = gifski_renderer())

## ---------------------------------------------------------------

pun

