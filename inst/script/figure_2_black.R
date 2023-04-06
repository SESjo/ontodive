#' # Theme
#'
# choose color
col_text <- "white"
col_back <- "black"

# theme
theme_plot_ind <- function(base_size = 12) {
  theme(
    plot.background = element_rect(fill = col_back),
    strip.background = element_rect(fill = col_back),
    strip.text = element_text(colour = col_text),
    legend.background = element_rect(fill = col_back),
    legend.text = element_text(colour = col_text),
    legend.title = element_text(colour = col_text),
    panel.background = element_rect(fill = col_back, colour = NA),
    axis.text = element_text(colour = col_text),
    axis.title = element_text(colour = col_text),
    axis.ticks = element_line(colour = col_text),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = col_back),
    legend.key = element_rect(fill = col_back, colour = "grey30"),
    axis.line = element_line(
      color = col_text,
      arrow = arrow(length = unit(0.2, "lines"), type = "closed")
    )
  )
}

#' # figure 2
# initial plots
fig_maxdepth_ini <- plot_comp(
  data_comp,
  "maxdepth",
  group_to_compare = "sp_rename",
  nb_days = 200,
  cols = "divetype_rename",
  ribbon = T,
  point = F,
  colours = colours,
  linetype_ribbon = 0,
  individual = FALSE,
  # method = "GCV.Cp",
  scales = "free_y"
)
fig_dduration_ini <- plot_comp(
  copy(data_comp)[, dduration_min := dduration / 60],
  "dduration_min",
  group_to_compare = "sp_rename",
  nb_days = 200,
  cols = "divetype_rename",
  ribbon = T,
  point = F,
  colours = colours,
  linetype_ribbon = 0,
  individual = FALSE,
  # method = "GCV.Cp",
  scales = "free_y"
)
# get limits
maxdepth_limits <-
  ggplot_build(fig_maxdepth_ini)$layout$panel_params[[1]]$y.range
dduration_limits <-
  ggplot_build(fig_dduration_ini)$layout$panel_params[[1]]$y.range

# update initial plots
fig_maxdepth <- fig_maxdepth_ini +
  labs(y = "Maximum depth (m)",
       colour = "Elephant seals",
       fill = "Elephant seals") +
  coord_cartesian(ylim = rev(maxdepth_limits)) +
  scale_colour_manual(values = colours,
                      labels = data_comp[, sort(unique(sp_rename))] %>%
                        word(1) %>%
                        str_to_title()) +
  scale_fill_manual(values = colours,
                    labels = data_comp[, sort(unique(sp_rename))] %>%
                      word(1) %>%
                      str_to_title()) +
  # theme_jjo() +
  theme_plot_ind() +
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(colour = "grey20")
  )
fig_dduration <- fig_dduration_ini +
  labs(x = "Number of days since departure",
       y = "Dive duration (min)") +
  coord_cartesian(ylim = dduration_limits) +
  # theme_jjo() +
  theme_plot_ind() +
  theme(legend.position = "none",
        strip.text.x = element_blank())

# density plots
fig_dens_maxdepth <-
  ggplot(data_comp[day_departure <= 200,], aes(y = maxdepth, fill = sp)) +
  geom_density(
    show.legend = F,
    col = NA,
    alpha = 0.4,
    size = 0.3
  ) +
  coord_cartesian(ylim = rev(maxdepth_limits)) +
  scale_fill_manual(values = colours) +
  # theme_void() +
  theme(
    plot.background = element_rect(fill = col_back),
    panel.background = element_rect(fill = col_back),
    panel.grid.major = element_line(colour = col_back),
    panel.grid.minor = element_line(colour = col_back),
    line = element_blank(),
    rect = element_rect(colour = col_back),
    text = element_text(
      face = "plain",
      colour = "black",
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    legend.box = NULL,
    legend.key.size = unit(1.2, "lines"),
    legend.position = "right",
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(hjust = 0),
    strip.text = element_text(size = rel(0.8)),
    panel.ontop = FALSE,
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    plot.title = element_text(
      size = rel(1.2),
      hjust = 0,
      vjust = 1
    ),
    plot.title.position = "panel",
    plot.subtitle = element_text(hjust = 0, vjust = 1),
    plot.caption = element_text(
      size = rel(0.8),
      hjust = 1,
      vjust = 1
    ),
    plot.caption.position = "panel",
    plot.tag = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    complete = TRUE
  )
fig_dens_dduration <-
  ggplot(data_comp[day_departure <= 200,], aes(y = dduration / 60, fill = sp)) +
  geom_density(
    show.legend = F,
    col = NA,
    alpha = 0.4,
    size = 0.3
  ) +
  coord_cartesian(ylim = dduration_limits) +
  scale_fill_manual(values = colours) +
  # theme_void()
  theme(
    plot.background = element_rect(fill = col_back),
    panel.background = element_rect(fill = col_back),
    panel.grid.major = element_line(colour = col_back),
    panel.grid.minor = element_line(colour = col_back),
    line = element_blank(),
    rect = element_rect(colour = col_back),
    text = element_text(
      face = "plain",
      colour = "black",
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    legend.box = NULL,
    legend.key.size = unit(1.2, "lines"),
    legend.position = "right",
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(hjust = 0),
    strip.text = element_text(size = rel(0.8)),
    panel.ontop = FALSE,
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    plot.title = element_text(
      size = rel(1.2),
      hjust = 0,
      vjust = 1
    ),
    plot.title.position = "panel",
    plot.subtitle = element_text(hjust = 0, vjust = 1),
    plot.caption = element_text(
      size = rel(0.8),
      hjust = 1,
      vjust = 1
    ),
    plot.caption.position = "panel",
    plot.tag = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = "topleft",
    complete = TRUE
  )

# patchwork
((fig_maxdepth |
    fig_dens_maxdepth) + plot_layout(ncol = 2, widths = c(5, 1))) /
  ((fig_dduration |
      fig_dens_dduration) + plot_layout(ncol = 2, widths = c(5, 1))) &
  theme(
    plot.background = element_rect(fill = col_back, colour = NA),
    panel.background = element_rect(fill = col_back),
    panel.grid.minor = element_line(colour = col_back)
  )

# save
ggsave(
  "figure_2_black_transparent.png",
  dpi = 300,
  bg = "black",
  width = 917,
  height = 544,
  units = "px",
  scale = 3
)
