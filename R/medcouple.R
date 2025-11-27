#-------------------------------------------
# Author  : adapted from the mrfDepth package - 2023
# Created : sex 20 jun 2025 14:10:22
#-------------------------------------------

medcouple <- function(
  x,
  do.reflect = "auto",
  na.rm = FALSE
) {
  if (na.rm) x <- x[is.finite(x)]

  if (anyNA(x)) stop("Data with NA/NaN/Inf!\n Consider removing using 'na.rm = TRUE'")
  if (is.character(x)) stop("Non-numeric argument!\n Data must be a vector or numeric matrix")

  x <- data.matrix(x)
  nr <- nrow(x)
  nc <- ncol(x)

  # auto reflection
  if (do.reflect == "auto") {
    do.reflect <- if (nr > 100) {
      1L
    } else {
      0L
    }
  }

  mc_result <- NULL
  for (i in seq_len(nc)) {
    temp <- .C("medcoupleC",
      as.double(x[, i]), # 1 Data vector
      as.integer(nr), # 2 Number of observations
      as.double(0.0), # 3 Medcouple
      as.integer(do.reflect), # 4 Logical indicating calculation on -x
      PACKAGE = "geok"
    )
    mc_result[i] <- temp[[3]]
  }

  return(mc_result)
}
