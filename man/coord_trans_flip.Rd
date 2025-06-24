%
\name{coord_trans_flip}
\alias{coord_trans_flip}
\title{Trans coordinates with x and y flipped}
\usage{
coord_trans_flip(x = "identity",
                 y = "identity",
                 xlim = NULL,
                 ylim = NULL,
                 clip = "on",
                 expand = TRUE,
                 flip = FALSE)
}
\arguments{

\item{x, y}{Transformers for x and y axes or their names.}

\item{xlim}{Limits for the x and y axes.}

\item{ylim}{Limits for the x and y axes.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
means no. In most cases, the default of \code{"on"} should not be changed,
as setting \code{clip = "off"} can cause unexpected results. It allows
drawing of data points anywhere on the plot, including in the plot margins. If
limits are set via \code{xlim} and \code{ylim} and some data points fall outside those
limits, then those data points may show up in places such as the axes, the
legend, the plot title, or the plot margins.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If \code{FALSE},
limits are taken exactly from the data or \code{xlim}/\code{ylim}.}

\item{flip}{Flip so that horizontal becomes vertical, and
vertical, horizontal.}

}
\description{
  \code{coord_trans()} is different to scale transformations in that it occurs after
  statistical transformation and will affect the visual appearance of geoms - there is
  no guarantee that straight lines will continue to be straight.
}
\details{
  Transformations only work with continuous values.
}
\examples{
\dontrun{
library(ggplot2)

set.seed(2019-01-19)
d <- data.frame(
  x = 1:30,
  y =  c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 52, rnorm(10, mean=150, sd=30)),
  group = c(rep("A", 10), rep("B", 10), rep("C", 10))
)

ggplot(d, aes(x, y, fill=group)) +
  geom_col() +
  coord_trans_flip(flip = TRUE, x = "reverse", y = "reverse")
}
}
