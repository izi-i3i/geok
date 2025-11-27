#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : ter 30 set 2025 23:24:05
# License :
# Updated :
#-------------------------------------------

# ===========================================
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===========================================
StatBoxplot2 <- ggproto("StatBoxplot2", Stat,
  required_aes = c("y|x"),
  non_missing_aes = "weight",
  # either the x or y aesthetic will get dropped during
  # statistical transformation, depending on the orientation
  dropped_aes = c("x", "y", "weight"),
  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    data$x <- data$x %||% 0
    data <- remove_missing(
      data,
      na.rm = params$na.rm,
      vars = "x",
      name = "stat_boxplot2"
    )
    flip_data(data, params$flipped_aes)
  },
  setup_params = function(self, data, params) {
    params$flipped_aes <- has_flipped_aes(data, params,
      main_is_orthogonal = TRUE,
      group_has_equal = TRUE,
      main_is_optional = TRUE
    )
    data <- flip_data(data, params$flipped_aes)

    has_x <- !(is.null(data$x) & is.null(params$x))
    has_y <- !(is.null(data$y) & is.null(params$y))
    if (!has_x && !has_y) {
      cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
    }

    params$width <- params$width %||% (resolution(data$x %||% 0, discrete = TRUE) * 0.75)

    if (
      !ggplot2:::is_mapped_discrete(data$x) &&
        is.double(data$x) &&
        !ggplot2:::has_groups(data) &&
        any(data$x != data$x[1L])
    ) {
      cli::cli_warn(
        c(
          "Continuous {.field {flipped_names(params$flipped_aes)$x}} aesthetic",
          "i" = "did you forget {.code aes(group = ...)}?"
        )
      )
    }

    params
  },
  extra_params = c("na.rm", "orientation"),
  compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1e35, qs = NULL, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    if (is.null(qs)) qs <- c(0, 0.25, 0.5, 0.75, 1)

    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, qs))
    }
    stats <- c(stats, mean(data$y), mean(data$x))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax", "ymean", "xmean")
    iqr <- diff(stats[c(2, 4)])

    outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
    }

    if (vctrs::vec_unique_count(data$x) > 1) {
      width <- diff(range(data$x)) * 0.9
    }

    df <- ggplot2:::data_frame0(!!!as.list(stats))
    df$outliers <- list(data$y[outliers])

    df$ymean <- mean(data$y)
    df$xmean <- mean(data$x)

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
    df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df$width <- width
    df$relvarwidth <- sqrt(n)
    df$flipped_aes <- flipped_aes
    flip_data(df, flipped_aes)
  }
)

# ===========================================
stat_boxplot2 <- ggplot2:::make_constructor(
  StatBoxplot2,
  geom = "boxplot2",
  position = "dodge2",
  orientation = NA,
  omit = "width"
)
