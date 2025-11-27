## -------------------------------------------
## Author  : Izi
## Created :qui 06 jan 2022 21:38:17 -03
## -------------------------------------------

x <- y <- NULL

# ==========================================
is_fw <- function(p) !is.null(p$facet$params$facets)

# ===========================================
glocator <- function(
  panel = 1,
  p = NULL,
  segment = TRUE,
  point = TRUE,
  text = TRUE,
  size = c(text = 10, point = 1, segment = 0.8),
  color = c(text = "grey50", point = "black", segment = "gray30"),
  digits = 2,
  shape = 21,
  just = c("left", "top"),
  hjust = NULL,
  vjust = NULL,
  verbose = TRUE,
  plot = TRUE
) { # start glocator
  if (is.null(dev.list())) {
    stop("No graphics device was selected or is active!")
  }

  just <- match.arg(just)
  switch(just,
    left = {
      if (is.null(hjust)) hjust <- 2
      if (is.null(vjust)) vjust <- 1
    },
    top = {
      if (is.null(hjust)) hjust <- 0
      if (is.null(vjust)) vjust <- 4.3
    }
  )

  if (length(color) < 3) color <- rep(color[1], 3)
  if (length(size) < 3) size <- c(size[1], 1, 1)

  # ggplot
  if (is.null(p)) {
    ggp <- ggplot2::last_plot()
  } else {
    ggp <- p
    print(p)
  }

  # get labels strip-facet
  facet <- strip_labels(ggp)

  # build ggplot for rendering
  gBuild <- ggplot2::ggplot_build(ggp)
  # data
  gData <- gBuild$data[[1]]

  # no layer
  if (nrow(gData) == 0) stop("no plot layer", call. = FALSE)

  # only ggplot2
  if (is.null(ggp)) stop("ggplot graphic not detected in current device", call. = F)

  pts <- NULL
  fac <- data.frame()
  pnl <- panel
  k <- 0

  tryCatch(
    {
      # loop for panels
      for (pan in panel) {
        # find the correct viewport for the npc coordinates
        p <- unlist(grid::current.vpTree())
        p <- unname(p[grep("\\.name$", names(p))])
        p <- grep("panel", p, fixed = TRUE, value = TRUE)
        n_panels <- length(p)
        mnp <- max(panel)
        if (n_panels < mnp) {
          msg <- paste0("c(", paste(panel, collapse = ","), ")")
          stop(sprintf("max(panel = %s) > length panels: %s", msg, n_panels), call. = FALSE)
        } # end if

        if (is_fw(ggp)) {
          p <- sort(p, decreasing = TRUE)
          np <- length(p)
          p1 <- p[1]
          pn <- p[np]
          p[1] <- pn
          p[np] <- p1
        } else {
          p <- sort(p, decreasing = FALSE)
        }

        # panel to locator
        p <- p[pan]

        rp <- tryCatch(
          {
            # ranges x and y
            xrng <- c(
              gBuild$layout$panel_params[[pan]]$x_range, # sf
              gBuild$layout$panel_params[[pan]]$x.range
            )
            yrng <- c(
              gBuild$layout$panel_params[[pan]]$y_range, # sf
              gBuild$layout$panel_params[[pan]]$y.range
            )
          },
          error = function(e) {
            message(sprintf("Panel %s: null", pan))
          }
        )

        # to correct convertX and convertY
        previous_viewport <- grid::current.vpPath()
        grid::seekViewport(p, recording = FALSE)
        # when exiting function, return to previous position in viewport tree
        on.exit(grid::upViewport(0, recording = FALSE))
        if (!is.null(previous_viewport)) {
          on.exit(grid::downViewport(previous_viewport, strict = TRUE, recording = FALSE), add = TRUE)
        } # end if

        # for the correct conversion convertX and convertY
        grid::pushViewport(grid::dataViewport(x, y, xrng, yrng))

        # get point
        m <- c("npc", "native")[1]
        loc <- if (is.null(rp)) NULL else grid::grid.locator(m)

        if (is.null(loc) && k == 0) {
          return()
        }

        k <- k + 1
        i <- 0
        while (!is.null(loc)) {
          # x and y position relative to that viewport
          lc <- as.numeric(loc)
          xd <- (xrng[1] + lc[1] * diff(xrng))
          yd <- (yrng[1] + lc[2] * diff(yrng))

          if (xd < xrng[1] | xd > xrng[2] | yd < yrng[1] | yd > yrng[2]) {
            message(paste0("Warning: Use only panel ", pnl[k], " (", pan, "/", n_panels, ")"))
            loc <- grid::grid.locator(m)
          } else {
            i <- i + 1
            glt <- data.frame(i, x = round(xd, digits), y = round(yd, digits))
            fac <- rbind(fac, facet[pan, , drop = FALSE])
            pts <- rbind(pts, glt)

            if (verbose) {
              message(paste0(
                "Panel ", pan, ": ", i, "(",
                round(pts$x[i], 2), ", ",
                round(pts$y[i], 2), ")"
              ))
            } # end if

            ## segments
            if (segment) {
              grid::grid.segments(
                x0 = grid::unit.c(loc$x, unit(0, m)),
                y0 = grid::unit.c(unit(0, m), loc$y),
                x1 = loc$x,
                y1 = loc$y,
                gp = grid::gpar(lwd = size[3], lty = "dashed", col = color[1])
              )
            }
            ## points
            if (point) {
              grid::grid.points(
                x = loc$x,
                y = loc$y,
                pch = shape,
                size = grid::unit(size[2], "mm"),
                default.units = m,
                gp = grid::gpar(col = color[2])
              )
            }
            ## text
            if (text) {
              grid::grid.text(
                paste0(i, "(", round(xd, digits), ", ", round(yd, digits), ")"),
                x = loc$x + unit(hjust, "mm"),
                y = loc$y + unit(vjust, "mm"),
                just = just,
                gp = grid::gpar(fontsize = size[1], col = color[3])
              )
            }

            loc <- grid::grid.locator(m)
          } # end if
        } # end while
      } # end for

      if (nrow(fac) > 0) {
        pts <- cbind(pts, fac)
      } # end if

      if (!is.null(pts)) rownames(pts) <- seq_len(nrow(pts))

      ## plot with point
      if (is.null(pts)) {
        return()
      } else if (plot) {
        ggp <- ggp +
          geom_point(
            data = pts, aes(x = x, y = y),
            size = size[2], color = color[2], shape = shape, inherit.aes = F
          )
        return(list(points = pts, plot = ggp))
      } else {
        return(list(points = pts, plot = NULL))
      } # end if
    }, # tryCatch
    error = function(e) {
      message("Aborted!")
    }
  )
} # end glocator
