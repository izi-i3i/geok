#------------------------------------------------------------------------
# Author  : izi (izi31416@protonmail.com)
# Project : HBGQ2020
# Created : sábado ago 13, 2022 09:53:32 -03
# License : MIT
# Updated : dom 23 out 2022 01:32:06 -03
#------------------------------------------------------------------------

# rotate ====================================
rotate <- function(
  x,
  angle = c("180", "90"),
  clockwise = TRUE
) {
  rot <- function(x, clockwise) {
    if (clockwise) t(apply(x, 2, rev)) else apply(t(x), 2, rev)
  } # end ratate

  angle <- match.arg(angle)

  out <- switch(angle,
    "90" = rot(x, clockwise = clockwise),
    "180" = t(rot(x, clockwise = clockwise))
  )

  out
}

# even-odd ==================================
is_even <- function(x) x %% 2 == 0 # par
is_odd <- function(x) x %% 2 != 0 # impar

# swap ======================================
swap <- function(x) {
  d1 <- dim(x)[1]
  d2 <- dim(x)[2]
  if (is.vector(x)) {
    h1 <- seq_along(x)
  } else {
    h1 <- seq_len(d1)
  }
  h <- h1[is_odd(h1)]
  a <- NULL
  for (i in h) {
    a <- c(a, c(h1[i + 1], h1[i]))
  }
  if (is.vector(x)) {
    x <- x[a]
  } else {
    x[, 1:d2] <- x[a, ]
  }
  x
}

# position_default ==========================
position_default <- function(p = c("y", "x")) {
  p <- match.arg(p)
  ifelse(p == "y", "left", "bottom")
}

# transf ====================================
transf <- function(g = 0, k = 1) {
  function(x, ...) {
    ng <- length(g)
    nk <- length(k)

    if (nk < ng) {
      k <- rep(k[1], ng)
    }

    for (i in seq_len(ng)) {
      if (g[i] > 0) {
        x <- pmin(x, g[i]) + k[i] * pmax(x - g[i], 0)
      } else {
        x <- pmax(x, g[i]) + k[i] * pmin(x - g[i], 0)
      }
    }
    x
  }
}

# transf_inv ================================
transf_inv <- function(g = 0, k = 1) {
  function(x, ...) {
    ng <- length(g)
    nk <- length(k)

    if (nk < ng) {
      k <- rep(k[1], ng)
    }

    for (i in rev(seq_len(ng))) {
      if (g[i] > 0) {
        x <- pmin(x, g[i]) + pmax(x - g[i], 0) / k[i]
      } else {
        x <- pmax(x, g[i]) + pmin(x - g[i], 0) / k[i]
      }
    }
    x
  }
}

# transf_trans ==============================
transf_trans <- function(g = 0, k = 1) {
  scales::trans_new(
    name = "transf",
    trans = transf(g, k),
    breaks = pretty_transf(g),
    inverse = transf_inv(g, k),
    minor_breaks = scales::regular_minor_breaks(),
    format = scales::label_number(),
    domain = c(-Inf, Inf)
  )
}

# gapend ====================================
gapend <- function(g, p) {
  np <- length(p)
  ng <- length(g)
  if (np < ng) {
    p <- rep(p[1], ng)
  }
  res <- sapply(
    g,
    function(x, p) {
      x + x * p
    },
    p
  )
  out <- diag(as.matrix(res))
  out
}

# ===========================================
pretty_transf <- function(
  cuts = 0,
  k = 1,
  g = 0,
  n = 7
) {
  function(x, ...) {
    ge <- gapend(cuts, g)
    xr <- range(x, na.rm = TRUE)
    bk <- scales::trans_breaks(transf(cuts, k), transf_inv(cuts, k), n)(xr)
    nb <- c(cuts, ge, bk)
    nb <- sort(unique(nb))
    nb
  }
} # end pretty_transf


# ===========================================
setlines <- function(x, default) {
  x <- x[!is.na(x)]
  nx <- length(x)
  ndef <- length(default)
  dname <- names(default)
  xname <- xn <- names(x)
  #' default_type <- default

  if (is.null(xname)) {
    if (nx == 1) {
      default <- rep(x[1], ndef)
    }
    if (nx <= (ndef - 1)) {
      default <- c(x, default[(nx + 1):ndef])
    }
    default <- default[1:ndef]
    names(default) <- dname[1:ndef]
  } else {
    xname <- xname[xname != ""]
    xname <- xname[(xname %in% dname)]
    default[xname] <- x[xname]
  }

  ig_default <- x[!(xn %in% dname)]

  if (length(ig_default)) {
    ignames_default <- names(ig_default)
    ignames_default
    #' ignames_default[ignames_default==""] <- " "
    igs <- paste0("(", ignames_default, " = ", ig_default, ")", collapse = ", ")
    dn <- paste(dname, collapse = " ")
    msg <- sprintf(
      paste(
        "In axisline(), ignoring parameters: '%s'\n",
        "Axis can be specified either text: %s"
      ),
      igs,
      dn
    )
    warning(msg, call. = FALSE)
  }
  default
} # end setlines

# ===========================================
char_num_lty <- function(lty) {
  ltn <- c(
    "blank" = 0,
    "solid" = 1,
    "dashed" = 2,
    "dotted" = 3,
    "dotdash" = 4,
    "longdash" = 5,
    "twodash" = 6
  )
  ltc <- c(
    "blank",
    "solid",
    "dashed",
    "dotted",
    "dotdash",
    "longdash",
    "twodash"
  )
  cha <- NULL
  num <- NULL
  for (i in seq_along(lty)) {
    sly <- lty[[i]]
    asn <- suppressWarnings(as.numeric(sly))
    asn <- if (is.na(asn)) ltn[sly] else ltn[asn + 1]
    cha[i] <- ltc[asn + 1]
    num[i] <- asn
  }
  names(cha) <- names(lty)
  names(num) <- names(lty)
  list(cha, num)
}

# ===========================================
axisline <- function(
  ...,
  size = 0.6,
  color = "gray45",
  linetype = 1L,
  grid_axis = TRUE,
  size_default = NULL,
  color_default = NULL,
  linetype_default = NULL
) {
  if (is.null(size_default)) {
    size_default <- c(t = 0.6, r = 0.6, b = 0.6, l = 0.6)
  }
  if (is.null(color_default)) {
    color_default <- c(t = "gray45", r = "gray45", b = "gray45", l = "gray45")
  }
  if (is.null(linetype_default)) {
    linetype_default <- c(t = 1, r = 1, b = 1, l = 1)
  }

  size <- setlines(size, size_default)

  color <- setlines(color, color_default)

  linetype <- setlines(linetype, linetype_default)
  linetype <- if (is.numeric(linetype)) {
    char_num_lty(linetype)[[2]]
  } else {
    char_num_lty(linetype)[[1]]
  }

  out <- list(
    size = size,
    color = color,
    linetype = linetype,
    grid_axis = grid_axis
  )

  out
}

# ===========================================
gapbox <- function(
  ...,
  fill = "white",
  size = 1,
  color = "gray55",
  linetype = 1
) {
  out <- list(
    fill = fill,
    size = size,
    color = color,
    linetype = linetype
  )
  out
} # end gapbox

# ===========================================
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# ===========================================
draw_panel_fun <- function(
  data,
  panel_scales,
  panel_params,
  self,
  cuts,
  k,
  cut_axis,
  gap,
  gap_box,
  axis_line
) {
  ## line axis
  size_axis <- axis_line[["size"]]
  if (panel_params$clip == "off") {
    size_axis <- size_axis / 2
  }

  col_axis <- axis_line[["color"]]
  lty_axis <- axis_line[["linetype"]]

  # verifcar se é flip
  flip <- class(
    panel_params
  )[1] ==
    "CoordFlip" |
    class(panel_params)[1] == "CoordTransFlip"

  yr <- panel_scales$y.range
  xr <- panel_scales$x.range

  x_major <- panel_scales$x.major
  x_reverse <- if (!is.null(x_major)) {
    x_major[1] > x_major[length(x_major)]
  } else {
    FALSE
  }

  y_major <- panel_scales$y.major
  y_reverse <- if (!is.null(y_major)) {
    y_major[1] > y_major[length(y_major)]
  } else {
    FALSE
  }

  xd <- panel_scales$x$scale_is_discrete
  if (is.null(xd)) {
    xd <- attr(panel_scales$x.major, "class")
    xd <- if (is.null(xd)) FALSE else TRUE
  }

  yd <- panel_scales$y$scale_is_discrete
  if (is.null(yd)) {
    yd <- attr(panel_scales$y.major, "class")
    yd <- if (is.null(yd)) FALSE else TRUE
  }

  if (length(cuts) < length(gap)) {
    gap <- gap[1]
  }

  if (cut_axis == "x") {
    ss <- swap(names(size_axis))
    sc <- swap(names(col_axis))
    sl <- swap(names(lty_axis))
    names(size_axis) <- ss
    names(col_axis) <- sc
    names(lty_axis) <- sl

    if (!flip) {
      pr <- xr
    } else {
      pr <- yr
    }

    if (flip && x_reverse && y_reverse) {
      nc <- -1
      #'' lx <- "xa"
    } else if (!flip && !x_reverse && !y_reverse) {
      nc <- 1
      #'' lx <- "xb"
    } else if (flip && !x_reverse && y_reverse) {
      nc <- -1
      #'' lx <- "xc"
    } else if (!flip && x_reverse && !y_reverse) {
      nc <- -1
      #'' lx <- "xd"
    } else if (!flip && x_reverse && y_reverse) {
      nc <- -1
      #'' lx <- "xe"
    } else if (!flip && !x_reverse && y_reverse) {
      nc <- 1
      #'' lx <- "xf"
    } else if (flip && x_reverse && !y_reverse) {
      # NOTE: confirmar
      nc <- 1
      #'' lx <- "xg"
    } else {
      nc <- 1
      #'' lx <- "xh"
    }

    ppx <- panel_params$trans$x[[1]]

    if (!is.null(ppx)) {
      fun <- gsub("-.*", "", ppx)
      base <- gsub("log-", "", ppx)
      if (fun == "log") {
        base <- as.numeric(base)
      }
      if (fun == "log") {
        cuts <- do.call(fun, list(cuts, base))
      }
      if (fun == "sqrt") cuts <- do.call(fun, list(cuts))
    }
  }

  #'' ly <- ""

  if (cut_axis == "y") {
    if (flip) {
      pr <- xr
    } else {
      pr <- yr
    }

    if (flip && x_reverse && y_reverse) {
      nc <- -1
      #'' ly <- "ya"
    } else if (!flip && !x_reverse && !y_reverse) {
      nc <- 1
      #'' ly <- "yb"
    } else if (flip && !x_reverse && y_reverse) {
      nc <- 1
      #'' ly <- "yc"
    } else if (!flip && x_reverse && !y_reverse) {
      nc <- 1
      #'' ly <- "yd"
    } else if (!flip && x_reverse && y_reverse) {
      nc <- -1
      #'' ly <- "ye"
    } else if (!flip && !x_reverse && y_reverse) {
      nc <- -1
      #'' ly <- "yf"
    } else if (flip && x_reverse && !y_reverse) {
      nc <- -1
      #'' ly <- "yg"
    } else {
      nc <- 1
      #'' ly <- "yh"
    }

    ppy <- panel_params$trans$y[[1]]

    if (!is.null(ppy)) {
      fun <- gsub("-.*", "", ppy)
      base <- gsub("log-", "", ppy)
      if (fun == "log") {
        base <- as.numeric(base)
      }
      if (fun == "log") {
        cuts <- do.call(fun, list(cuts, base))
      }
      if (fun == "sqrt") cuts <- do.call(fun, list(cuts))
    }
  }

  cuts <- nc * cuts

  ge <- gapend(cuts, gap)
  st <- transf(cuts, k)(cuts)
  et <- transf(cuts, k)(ge)

  s <- scales::rescale(st, from = pr)
  e <- scales::rescale(et, from = pr)

  ## center blank box
  mse <- apply(rbind(s, e), 2, mean, na.rm = TRUE)
  p <- sapply(gap, function(x) ifelse(x == 0, 0.003, 0))
  dse <- abs(s - e + 2 * p)

  ## horizontal lines
  yl0 <- c(s + p, e - p)
  yl1 <- c(s + p, e - p)
  #' hl = rbind(x0 = 0, x1 = 1, y0 = c(s - p, e + p), y1 = c(s - p, e + p))
  hl <- rbind(x0 = 0, x1 = 1, y0 = yl0, y1 = yl1)

  ## blank box
  rc <- rbind(x = 0.5, y = mse, width = 1, height = dse)

  ## size cut_axis
  l <- size_axis["l"] / 1000
  r <- size_axis["r"] / 1000
  ## cut_axis
  se <- sort(c(s, e))
  num <- seq_along(se)
  a <- num[is_even(num)]
  b <- num[is_odd(num)]
  ya <- se[a]
  yb <- se[b]

  zz <- c(0, 1)
  z1 <- zz[1] # line grid start
  z2 <- zz[2] # line grid end

  ## left
  v_1 <- rbind(x0 = 0, x1 = 0, y0 = c(z1, ya + l), y1 = c(yb - l, z2))
  ## down
  v_2 <- rbind(x0 = 0, x1 = 1, y0 = 0, y1 = 0)
  ## right
  v_3 <- rbind(x0 = 1, x1 = 1, y0 = c(z1, ya + r), y1 = c(yb - r, z2))
  ## top
  v_4 <- rbind(x0 = 0, x1 = 1, y0 = 1, y1 = 1)

  if (cut_axis == "x") {
    flip <- !flip
  }

  if (flip) {
    hl <- rotate(hl) # horizontal lines
    rc <- swap(rc) # blank box

    v_1 <- rotate(v_1) # cut_axis left
    v_2 <- rotate(v_2) # cut_axis right
    v_3 <- rotate(v_3) # cut_axis down
    v_4 <- rotate(v_4) # cut_axis top

    size_axis <- swap(size_axis) # size cut_axis; l,b,r,t
    col_axis <- swap(col_axis) # colors cut_axis: l,b,r,t
    lty_axis <- swap(lty_axis) # line type cut_axis: l,b,r,t
  } # end flip

  ## cut lines
  hl <- grid::segmentsGrob(
    x0 = hl[1, ],
    x1 = hl[2, ],
    y0 = hl[3, ],
    y1 = hl[4, ],
    gp = grid::gpar(
      lwd = gap_box[["size"]],
      col = gap_box[["color"]],
      lty = gap_box[["linetype"]]
    )
  )

  ## blank box
  rc <- grid::rectGrob(
    x = rc[1, ],
    y = rc[2, ],
    width = rc[3, ],
    height = rc[4, ],
    gp = grid::gpar(fill = gap_box[["fill"]], col = NA),
    just = "center"
  )

  if (axis_line[["grid_axis"]]) {
    # FIXME: corrigir os tamanhos
    # x: bottom - y: left
    v1 <- grid::segmentsGrob(
      x0 = v_1[1, ],
      x1 = v_1[2, ],
      y0 = v_1[3, ],
      y1 = v_1[4, ],
      gp = grid::gpar(
        lwd = size_axis["l"],
        lex = 2,
        col = col_axis["l"],
        lty = lty_axis["l"],
        lineend = "square",
        linejoin = "mitre"
      )
    )
    ## x: left - y: bottom
    v2 <- grid::segmentsGrob(
      x0 = v_2[1, ],
      x1 = v_2[2, ],
      y0 = v_2[3, ],
      y1 = v_2[4, ],
      gp = grid::gpar(
        lwd = size_axis["b"],
        lex = 2,
        col = col_axis["b"],
        lty = lty_axis["b"],
        lineend = "square",
        linejoin = "mitre"
      )
    )
    ## cut_axis down
    v3 <- grid::segmentsGrob(
      x0 = v_3[1, ],
      x1 = v_3[2, ],
      y0 = v_3[3, ],
      y1 = v_3[4, ],
      gp = grid::gpar(
        lwd = size_axis["r"],
        lex = 2,
        col = col_axis["r"],
        lty = lty_axis["r"],
        lineend = "square",
        linejoin = "mitre"
      )
    )
    ## x: top - y: right
    v4 <- grid::segmentsGrob(
      x0 = v_4[1, ],
      x1 = v_4[2, ],
      y0 = v_4[3, ],
      y1 = v_4[4, ],
      gp = grid::gpar(
        lwd = size_axis["t"],
        lex = 2,
        col = col_axis["t"],
        lty = lty_axis["t"],
        lineend = "square",
        linejoin = "mitre"
      )
    )
  } else {
    v1 <- v2 <- v3 <- v4 <- NULL
  } # end if

  ggname("scale_axis_transf", grid::grobTree(v1, v2, v3, v4, rc, hl))
} # end draw_panel_fun

# ===========================================
ScaleAxisTransf <- ggplot2::ggproto(
  "ScaleAxisTransf",
  GeomBlank,
  draw_key = draw_key_blank,
  draw_panel = draw_panel_fun
) # end ggprotp

# ===========================================
scale_transf_continuous <- function(
  cut_axis = c("y", "x"),
  cuts = 0,
  k = 1,
  gap = 0,
  name = waiver(),
  breaks = waiver(), # pretty_transf(cuts, k, gap),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  limits = NULL,
  position = position_default(cut_axis),
  expand = waiver(),
  oob = scales::censor,
  na.value = NA_real_,
  guide = waiver(),
  sec.axis = waiver(),
  axis_line = axisline(),
  gap_box = gapbox(),
  ...
) {
  cut_axis <- match.arg(cut_axis)
  grid_axis <- axis_line[["grid_axis"]]

  scale_fun <- if (cut_axis == "y") {
    "scale_y_continuous"
  } else {
    "scale_x_continuous"
  }

  arg_scale <- list(
    transform = transf_trans(cuts, k),
    breaks = breaks,
    labels = labels,
    position = position,
    minor_breaks = minor_breaks,
    n.breaks = n.breaks,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    guide = guide,
    sec.axis = sec.axis,
    name = name
  )

  # theme: remove axis.line and panel border
  arg_theme <- list(
    axis.line = element_blank(),
    panel.border = element_blank()
  )

  list(
    layer(
      mapping = NULL,
      data = NULL,
      stat = StatIdentity,
      geom = ScaleAxisTransf,
      position = PositionIdentity,
      inherit.aes = TRUE, # NOTE: verificar
      check.aes = FALSE,
      show.legend = FALSE,
      params = list(
        cut_axis = cut_axis,
        cuts = cuts,
        k = k,
        gap = gap,
        axis_line = axis_line,
        gap_box = gap_box,
        ...
      )
    ),
    do.call(scale_fun, arg_scale),
    # theme: remove axis.line and panel border
    if (grid_axis) do.call("theme", arg_theme)
  ) # end list
} # end scale_transf_continuous
