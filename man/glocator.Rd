
% sex 28 nov 2025 23:12:59
\name{glocator}
\alias{glocator}
\title{Point position}
\description{
  Insert point
}

\usage{
  glocator(
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
  )
}
\arguments{
\item{panel}{mapping}
\item{p}{p}
\item{segment}{segment}
\item{point}{point}
\item{text}{text}
\item{size}{size}
\item{color}{color}
\item{digits}{digits}
\item{shape}{shape}
\item{just}{just}
\item{hjust}{hjust}
\item{vjust}{vjust}
\item{verbose}{verbose}
\item{plot}{plot}
}
\value{
A data.frame with points.
}
%\author{ I.F. Sousa-Filho }
\examples{
  library(ggplot2)

  dd <- data.frame(
      y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
      x = 1:16,
      g = factor(c(rep("A", 8), rep("B", 8))),
      m = c(rep("a", 4), rep("b", 4), rep("c", 4), rep("d", 4))
    )
  dd

  p <- ggplot(dd, aes(x, y)) +
    geom_point()

  print(p)
  g <- glocator()
  g
}
