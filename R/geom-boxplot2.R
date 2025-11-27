#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : ter 30 set 2025 23:19:36
# License :
# Updated :
#-------------------------------------------
geom_boxplot2 <- function(
  mapping = NULL,
  data = NULL,
  stat = "boxplot2",
  position = "dodge2",
  ...,
  outliers = TRUE,
  outlier.colour = NULL,
  outlier.color = NULL,
  outlier.fill = NULL,
  outlier.shape = NULL,
  outlier.size = NULL,
  outlier.stroke = 0.5,
  outlier.alpha = NULL,
  whisker.colour = NULL,
  whisker.color = NULL,
  whisker.linetype = NULL,
  whisker.linewidth = NULL,
  staple.colour = NULL,
  staple.color = NULL,
  staple.linetype = NULL,
  staple.linewidth = NULL,
  median.colour = NULL,
  median.color = NULL,
  median.linetype = NULL,
  median.linewidth = NULL,
  box.colour = NULL,
  box.color = NULL,
  box.linetype = NULL,
  box.linewidth = NULL,
  notch = FALSE,
  notchwidth = 0.5,
  staplewidth = 0.2,
  varwidth = FALSE,
  meanbox = TRUE,
  meanbox.colour = NULL,
  meanbox.color = NULL,
  meanbox.fill = NULL,
  meanbox.shape = 10,
  meanbox.size = 1.5,
  meanbox.stroke = 0.5,
  meanbox.alpha = 1,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") && varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  outlier_gp <- list(
    colour = outlier.color %||% outlier.colour,
    fill   = outlier.fill,
    shape  = outlier.shape,
    size   = outlier.size,
    stroke = outlier.stroke,
    alpha  = outlier.alpha
  )

  whisker_gp <- list(
    colour    = whisker.color %||% whisker.colour,
    linetype  = whisker.linetype,
    linewidth = whisker.linewidth
  )

  staple_gp <- list(
    colour    = staple.color %||% staple.colour,
    linetype  = staple.linetype,
    linewidth = staple.linewidth
  )

  median_gp <- list(
    colour    = median.color %||% median.colour,
    linetype  = median.linetype,
    linewidth = median.linewidth
  )

  box_gp <- list(
    colour    = box.color %||% box.colour,
    linetype  = box.linetype,
    linewidth = box.linewidth
  )

  meanbox_gp <- list(
    colour = meanbox.color %||% meanbox.colour,
    fill   = meanbox.fill,
    shape  = meanbox.shape,
    size   = meanbox.size,
    stroke = meanbox.stroke,
    alpha  = meanbox.alpha
  )

  ggplot2:::check_number_decimal(staplewidth)
  ggplot2:::check_bool(outliers)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      outliers = outliers,
      outlier_gp = outlier_gp,
      whisker_gp = whisker_gp,
      staple_gp = staple_gp,
      median_gp = median_gp,
      box_gp = box_gp,
      meanbox_gp = meanbox_gp,
      meanbox = meanbox,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

# ===========================================
GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,
  extra_params = c("na.rm", "orientation", "outliers"),
  setup_params = function(data, params) {
    if ("fatten" %in% names(params)) {
      ggplot2:::deprecate_soft0(
        "4.0.0", "geom_boxplot(fatten)",
        "geom_boxplot(median.linewidth)"
      )
    } else {
      # For backward compatibility reasons
      params$fatten <- 2
    }
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },
  setup_data = function(self, data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- ggplot2:::compute_data_size(
      data, params$width,
      default = self$default_aes$width,
      zero = FALSE, discrete = TRUE
    )
    if (isFALSE(params$outliers)) {
      data$outliers <- NULL
    }

    if (!is.null(data$outliers)) {
      suppressWarnings({
        out_min <- vapply(data$outliers, min, numeric(1))
        out_max <- vapply(data$outliers, max, numeric(1))
      })

      data$ymin_final <- pmin(out_min, data$ymin)
      data$ymax_final <- pmax(out_max, data$ymax)
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
  draw_group = function(
    self, data, panel_params, coord, lineend = "butt",
    linejoin = "mitre", fatten = 2, outlier_gp = NULL,
    whisker_gp = NULL, staple_gp = NULL, median_gp = NULL,
    box_gp = NULL, notch = FALSE, notchwidth = 0.5,
    staplewidth = 0.2, varwidth = FALSE,
    meanbox = TRUE,
    meanbox_gp = NULL,
    flipped_aes = FALSE
  ) {
    data <- ggplot2:::fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i" = "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(fill = fill_alpha(data$fill, data$alpha), group = data$group)

    whiskers <- ggplot2:::data_frame0(
      x = c(data$x, data$x),
      xend = c(data$x, data$x),
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      colour = rep(whisker_gp$colour %||% data$colour, 2),
      linetype = rep(whisker_gp$linetype %||% data$linetype, 2),
      linewidth = rep(whisker_gp$linewidth %||% data$linewidth, 2),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- transform(
      data,
      y = middle,
      ymax = upper,
      ymin = lower,
      ynotchlower = ifelse(notch, notchlower, NA),
      ynotchupper = ifelse(notch, notchupper, NA),
      notchwidth = notchwidth
    )
    box <- flip_data(box, flipped_aes)

    if (meanbox) {
      boxmean <- ggplot2:::data_frame0(
        y = data$ymean,
        x = data$x[1],
        colour = meanbox_gp$colour %||% data$colour[1],
        fill = meanbox_gp$fill %||% data$fill[1],
        shape = meanbox_gp$shape %||% data$shape[1] %||% 10,
        size = meanbox_gp$size %||% data$size[1] %||% 1.5,
        stroke = meanbox_gp$stroke %||% data$stroke[1] %||% 0.5,
        fill = NA,
        alpha = meanbox_gp$alpha %||% data$alpha[1],
        !!!common
      )
      boxmean <- flip_data(boxmean, flipped_aes)
      boxmean_grob <- GeomPoint$draw_panel(boxmean, panel_params, coord)
    } else {
      boxmean_grob <- NULL
    }

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
      outliers <- ggplot2:::data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier_gp$colour %||% data$colour[1],
        fill = outlier_gp$fill %||% data$fill[1],
        shape = outlier_gp$shape %||% data$shape[1] %||% 19,
        size = outlier_gp$size %||% data$size[1] %||% 1.5,
        stroke = outlier_gp$stroke %||% data$stroke[1] %||% 0.5,
        fill = NA,
        alpha = outlier_gp$alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    if (staplewidth != 0) {
      staples <- ggplot2:::data_frame0(
        x = rep((data$xmin - data$x) * staplewidth + data$x, 2),
        xend = rep((data$xmax - data$x) * staplewidth + data$x, 2),
        y = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        linetype = rep(staple_gp$linetype %||% data$linetype, 2),
        linewidth = rep(staple_gp$linewidth %||% data$linewidth, 2),
        colour = rep(staple_gp$colour %||% data$colour, 2),
        alpha = c(NA_real_, NA_real_),
        !!!common,
        .size = 2
      )
      staples <- flip_data(staples, flipped_aes)
      staple_grob <- GeomSegment$draw_panel(
        staples, panel_params, coord,
        lineend = lineend
      )
    } else {
      staple_grob <- NULL
    }

    ggplot2:::ggname("geom_boxplot2", grid::grobTree(
      outliers_grob,
      staple_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
      GeomCrossbar$draw_panel(
        box,
        fatten = fatten,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes,
        middle_gp = median_gp,
        box_gp = box_gp
      ),
      boxmean_grob
    ))
  },
  draw_key = draw_key_boxplot,
  default_aes = aes(
    weight = 1, colour = from_theme(colour %||% scales::col_mix(ink, paper, 0.2)),
    fill = from_theme(fill %||% paper), size = from_theme(pointsize),
    alpha = NA, shape = from_theme(pointshape), linetype = from_theme(bordertype),
    linewidth = from_theme(borderwidth),
    width = 0.9
  ),
  required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax", "ymean|xmean"),
  rename_size = TRUE
)
