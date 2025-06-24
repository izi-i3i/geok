##-------------------------------------------
## Author  : adapted from geom_boxplot
## Project :
## Created : sáb 11 fev 2023 16:13:00 -03
## License :
## Updated :
##-------------------------------------------
geom_boxplot2 <- function(mapping = NULL, data = NULL,
                         stat = "boxplot2", position = "dodge2",
                         ...,
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 21,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          notch = FALSE,
                          notchwidth = 0.5,
                          whiskers.width = 0.3,
                          whiskers.seg = TRUE,
                          meanbox = TRUE,
                          meanbox.colour = NULL,
                          meanbox.color = NULL,
                          meanbox.fill = NULL,
                          meanbox.shape = 10,
                          meanbox.size = 1.5,
                          meanbox.stroke = 0.5,
                          meanbox.alpha = 1,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      whiskers.width = whiskers.width,
      whiskers.seg = whiskers.seg,
      meanbox = meanbox,
      meanbox.colour = meanbox.color %||% meanbox.colour,
      meanbox.fill = meanbox.fill,
      meanbox.shape = meanbox.shape,
      meanbox.size = meanbox.size,
      meanbox.stroke = meanbox.stroke,
      meanbox.alpha = meanbox.alpha,
      ...
    )
  )
}

GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,

  # need to declare `width` here in case this geom is used with a stat that
  # doesn't have a `width` parameter (e.g., `stat_identity`).
  extra_params = c("na.rm", "width", "orientation"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)

    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final  <- pmin(out_min, data$ymin)
      data$ymax_final  <- pmax(out_max, data$ymax)
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
      data$xmin <- data$x - data$width / 2
      data$xmax <- data$x + data$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
      data$xmin <- data$x - data$relvarwidth * data$width / 2
      data$xmax <- data$x + data$relvarwidth * data$width / 2
    }
    data$width <- NULL
    if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL

    flip_data(data, params$flipped_aes)
  },

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier.colour = NULL,
                        outlier.fill = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                        varwidth = FALSE, flipped_aes = FALSE,
                        whiskers.width = 0.3,
                        whiskers.seg = TRUE,
                        meanbox = TRUE,
                        meanbox.colour = NULL,
                        meanbox.fill = NULL,
                        meanbox.shape = 10,
                        meanbox.size = 1.5,
                        meanbox.stroke = 0.5,
                        meanbox.alpha = 1) {
    data <- ggplot2:::check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(
      colour = data$colour,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    whiskers <- ggplot2:::data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    whiskers.width = ((data$x - data$xmin) / (data$xmax - data$x)) / 2.625 * whiskers.width

  if(whiskers.seg)
  {
    segment <- ggplot2:::data_frame0(
       y = c(data$ymin, data$ymax),
       yend = c(data$ymin, data$ymax),
       x = c(data$x - whiskers.width, data$x + whiskers.width),
       xend = c(data$x + whiskers.width,  data$x - whiskers.width),
       alpha = c(NA_real_, NA_real_),
      !!!common
    )
    segment <- flip_data(segment, flipped_aes)
    segment_grob = GeomSegment$draw_panel(segment, panel_params, coord)
  } else{
    segment_grob = NULL
  }

    box <- ggplot2:::data_frame0(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      !!!common
    )
    box <- flip_data(box, flipped_aes)

  if(meanbox)
  {
  boxmean <- ggplot2:::data_frame0(
      y = data$ymean,
      x = data$x,
#     ymean = data$ymean,
#     xmean = mean(data$x),
      colour = meanbox.colour %||% data$colour[1],
      fill = meanbox.fill %||% data$fill[1],
      shape = meanbox.shape %||% data$shape[1],
      size = meanbox.size %||% data$size[1],
      stroke = meanbox.stroke %||% data$stroke[1],
      fill = NA,
      alpha = meanbox.alpha,
     !!!common
   )
      boxmean <- flip_data(boxmean, flipped_aes)
    boxmean_grob = GeomPoint$draw_panel(boxmean, panel_params, coord)
  } else {
    boxmean_grob = NULL
  }

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- ggplot2:::data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    ggplot2:::ggname("geom_boxplot2", grid::grobTree(
      outliers_grob,
      segment_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
      GeomCrossbar$draw_panel(
        box,
        fatten = fatten,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes
      ),
boxmean_grob
    ))
  },

  draw_key = draw_key_boxplot,

  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = NULL,
    alpha = NA, shape = 19, linetype = "solid", linewidth = 0.5),

  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),

  rename_size = TRUE
)


