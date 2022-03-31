#' @title A personnalised ggplot theme
#'
#' @param base_size Font size
#'
#' @import ggplot2
#'
#' @return a ggplot theme
#' @export
#'
#' @references
#' \href{https://benjaminlouis-stat.fr/en/blog/2020-05-21-astuces-ggplot-rmarkdown/}{https://benjaminlouis-stat.fr/en/blog/2020-05-21-astuces-ggplot-rmarkdown/}
#'
#' @examples
#' ggplot(cars) +
#'   geom_point(aes(x = speed, y = dist)) +
#'   theme_jjo()
theme_jjo <- function(base_size = 12) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # the whole figure
      # plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # figure area
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # axes
      # axis.title = element_text(size = rel(0.85), face = "bold"),
      # axis.text = element_text(size = rel(0.70), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.2, "lines"), type = "closed")),
      # legend
      # legend.title = element_text(size = rel(0.85), face = "bold"),
      # legend.text = element_text(size = rel(0.70), face = "bold"),
      # legend.key = element_rect(fill = "transparent", colour = NA),
      # legend.key.size = unit(1.5, "lines"),
      # legend.background = element_rect(fill = "transparent", colour = NA),
      # Les <U+00E9>tiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#888888", color = "#888888"),
      strip.text = element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5, 0, 5, 0))
    )
}
