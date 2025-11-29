## -------------------------------------------
## Author  : Izi
## Created : seg 20 jul 2020 09:55:55 -03
## -------------------------------------------

StatN <- ggproto("StatN", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, ypos, prefix) {
    y <- data$y
    y <- y[!is.na(y)]
    n <- length(y)
    data.frame(x = data$x[1], y = ypos, label = paste0(prefix, n))
  }
)

stat_n <- function(
  mapping = NULL,
  data = NULL,
  geom = "text",
  ypos = 0,
  position = "identity",
  inherit.aes = TRUE,
  show.legend = NA,
  na.rm = FALSE,
  vjust = 1.5,
  color = "gray30",
  prefix = NULL,
  ...
) {
  ggplot2::layer(
    stat = StatN,
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      prefix = prefix,
      ypos = ypos,
      na.rm = na.rm,
      vjust = vjust,
      color = color,
      ...
    )
  )
}
