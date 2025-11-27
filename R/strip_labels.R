#------------------------------------------------------------------------
# Author  : izi (izi31416@protonmail.com)
# Created : domingo jan 16, 2022 00:51:10 -03
#------------------------------------------------------------------------

is_fw <- function(p) !is.null(p$facet$params$facets)


data_label <- function(p, strip_name, n_row, n_col) {
  gp <- cowplot::as_grob(p)

  # label ==========================================================
  strip <- lapply(grep(strip_name, gp$layout$name), function(x) {
    gp$grobs[[x]]
  })
  nr <- length(strip)
  nc <- length(strip[[1]])

  lab <- vector("list", nc)
  for (i in 1:nc) {
    for (j in 1:nr) {
      st <- strip[[j]]$grobs[[i]]$children[2][[1]]$children[[1]]$label
      if (is.null(st)) st <- NA
      st <- gsub(".*: ", "", st)
      lab[[i]][j] <- st
    } # end for j
  } # end for i
  lab

  # data ===========================================================
  pos <- p$facet$params$strip.position

  name_c <- names(c(p$facet$params$cols))

  name_r <- if (strip_name == "strip-r") {
    rev(names(c(p$facet$params$rows)))
  } else {
    names(c(p$facet$params$rows))
  }


  if (is.null(pos)) {
    name_w <- names(c(p$facet$params$facets))
  } else {
    name_w <- if (pos == "right") {
      rev(names(c(p$facet$params$facets)))
    } else {
      names(c(p$facet$params$facets))
    }
  }

  tb <- any(c("strip-t", "strip-b") %in% strip_name)
  names_rc <- if (tb) c(name_c, name_w) else c(name_r, name_w)

  if (!all(!is.na(lab))) {
    DT <- data.frame()
  } else {
    DT <- as.data.frame(lab)
    nn <- ncol(DT)
    my_cols <- if (length(names_rc) != 0) names_rc else paste0("V", 1:nn)
    DT
    names(DT) <- my_cols
    DT

    if (!is.null(name_c)) {
      if (n_row == 0) n_row <- 1
      if (n_col == 0) n_col <- 1
      idx <- if (tb) {
        rep(seq_len(nrow(DT)), n_row)
      } else {
        rep(seq_len(nrow(DT)), each = n_col)
      }
      DT <- DT[idx, , drop = FALSE]
    } # end if

    rownames(DT) <- seq_len(nrow(DT))

    if (nn > 1) {
      # DT$VT <- do.call(paste, c(DT[my_cols], sep = ""))
      # colnames(DT)[colnames(DT) == "VT"] <- paste(my_cols, collapse = "+")
    } else {
      DT <- as.data.frame(DT)
      names(DT) <- my_cols
    } # end if
  } # end if

  # panels number ===========================================================
  nrd <- nrow(DT)

  if (!is.null(p$facet$params$facets)) {
    ns <- grep(strip_name, gp$layout$name, value = T)
    nc <- max(as.numeric(gsub(".*?([0-9]+).*", "\\1", ns)))
  }

  if (is.null(pos)) {
    DT$Panel <- seq_len(nrd)
  } else {
    if (pos == "left" || pos == "right") {
      mat <- matrix(seq_len(nr), nrow = nc, byrow = T)
    }
    if (pos == "bottom" || pos == "top") {
      mat <- matrix(seq_len(nr), ncol = nc, byrow = T)
    }

    dm <- dim(mat)
    d <- if (pos == "bottom" || pos == "top") dm[1] else dm[2]
    pan <- NULL

    for (i in rev(seq_len(d))) {
      a <- if (pos == "bottom" || pos == "top") mat[i, ] else mat[, i]
      pan <- c(pan, a)
    }

    DT$Panel <- pan
    DT <- DT[order(pan), , drop = FALSE]
  }
  DT
} # end data_label

strip_labels <- function(p) {
  gp <- cowplot::as_grob(p)

  strip_name_n <- grep("strip", gp$layout$name, value = TRUE)
  strip_name <- unique(gsub("-([0-9])|-([0-9])", "", strip_name_n))

  if (length(strip_name) == 0) {
    return(data.frame())
  }

  # Number of strips
  n_strip <- NULL
  for (stn in strip_name) {
    s <- sum(grepl(pattern = stn, gp$layout$name))
    names(s) <- stn
    n_strip <- c(s, n_strip)
  }
  if (is.na(n_strip["strip-r"])) n_strip["strip-r"] <- 1
  if (is.na(n_strip["strip-t"])) n_strip["strip-t"] <- 1
  if (is.na(n_strip["strip-l"])) n_strip["strip-l"] <- 1
  if (is.na(n_strip["strip-b"])) n_strip["strip-b"] <- 1

  r_row <- sum(grepl(pattern = "strip-r", gp$layout$name))
  l_row <- sum(grepl(pattern = "strip-l", gp$layout$name))
  t_col <- sum(grepl(pattern = "strip-t", gp$layout$name))
  b_col <- sum(grepl(pattern = "strip-b", gp$layout$name))

  n_row <- max(r_row, l_row)
  n_col <- max(t_col, b_col)

  if (!is_fw(p) && n_row != 0 && n_col != 0) {
    DD <- sapply(strip_name,
      data_label,
      p = p, n_row = n_row,
      n_col = n_col,
      simplify = F
    )
    out <- merge(DD[[1]], DD[[2]])
  } else {
    out <- data_label(p, strip_name, n_row, n_col)
  }
  rownames(out) <- seq_len(nrow(out))

  data_gp <- p$data
  cl <- sapply(data_gp, is.factor)
  cl <- names(cl[cl])
  co <- names(out)
  cc <- co[co %in% cl]
  out[cc] <- lapply(out[cc], as.factor)

  return(out)
} # end strip_labels
