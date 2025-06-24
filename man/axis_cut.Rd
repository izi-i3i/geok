%
\name{axis_cut}
\alias{axis_cut}
\title{Axis cut}
\description{
  \code{coord_trans_flip()} is different to scale transformations in that it occurs after
  statistical transformation and will affect the visual appearance of geoms - there is
  no guarantee that straight lines will continue to be straight.
}
\usage{
axis_cut(cuts = 0,
         k = 1,
         gap = 0,
         type = "coord",
         cut.axis = "y",
         x = "identity",
         y = "identity",
         flip = FALSE,
         reverse = FALSE,
         name = waiver(),
         breaks = breaks_transf(cuts, k, gap),
         minor_breaks = waiver(),
         n.breaks = NULL,
         labels = waiver(),
         limits = NULL,
         xlim = NULL,
         ylim = NULL,
         position = pos(cut.axis),
         expand = waiver(),
         oob = scales::censor,
         na.value = NA_real_,
         guide = waiver(),
         sec.axis = waiver(),
         clip = "on",
         line.zero = line_zero(),
         line.axis = line_axis(),
         gap.box = gap_box(),
         ...)
}
\arguments{
\item{cuts}{where to place the break in user units}

\item{k}{Axis compression constant}

\item{gap}{Percentage of distance from the value established in cut.axis.}

\item{type}{Coordinate system. The axis transformation takes place in coord_trans_flip or scale_y_continuous.
  \itemize{
    \item There are some limitations on some arguments.
}}

\item{cut.axis}{Which axis to break, x or y.}

\item{x, y}{Transformers for x and y axes or their names.
  \itemize{
    \item Only the axis not chosen in cut.axis is free for use.
}}

\item{flip}{Flip so that horizontal becomes vertical, and vertical, horizontal.}

\item{reverse}{Reverse axis defined in cut.axis.}

\item{limits}{One of:
  \itemize{
    \item \code{NULL} to use the default scale range
    \item A numeric vector of length two providing limits of the scale.
    Use \code{NA} to refer to the existing minimum or maximum
    \item A function that accepts the existing (automatic) limits and returns new limits
    Note that setting limits on positional scales will \strong{remove} data outside of the limits.
}}

\item{xlim}{Limits for the x and y axes.}

\item{ylim}{Limits for the x and y axes.}


\item{name}{The name of the scale. Used as the axis or legend title. If
  \code{waiver()}, the default, the name of the scale is taken from the first
  mapping used for that aesthetic. If \code{NULL}, the legend title will be
  omitted.}

\item{breaks}{One of:
  \itemize{
    \item \code{NULL} for no breaks
    \item \code{waiver()} for the default breaks computed by the \link[scales:trans_new]{transformation object}
    \item A numeric vector of positions}}

\item{minor_breaks}{One of:
  \itemize{
    \item \code{NULL} for no minor breaks
    \item \code{waiver()} for the default breaks (one minor break between each major break)
    \item A numeric vector of positions
    \item A function that given the limits returns a vector of minor breaks.}}

\item{n.breaks}{An integer guiding the number of major breaks. The algorithm
  may choose a slightly different number to ensure nice break labels. Will
  only have an effect if \code{breaks = waiver()}. Use \code{NULL} to use the default
  number of breaks given by the transformation.}

\item{labels}{One of:
  \itemize{
    \item \code{NULL} for no labels
    \item \code{waiver()} for the default labels computed by the transformation object
    \item A character vector giving labels (must be same length as \code{breaks})
    \item A function that takes the breaks as input and returns labels as output}}

\item{oob}{One of:
  \itemize{
    \item Function that handles limits outside of the scale limits
    (out of bounds).
    \item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
    bounds values with \code{NA}.
    \item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
    \item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.}}

\item{na.value}{Missing values will be replaced with this value.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
  the limits to ensure that data and axes don't overlap. If \code{FALSE},
  limits are taken exactly from the data or \code{xlim}/\code{ylim}.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
  setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
  means no. In most cases, the default of \code{"on"} should not be changed,
  as setting \code{clip = "off"} can cause unexpected results. It allows
  drawing of data points anywhere on the plot, including in the plot margins. If
  limits are set via \code{xlim} and \code{ylim} and some data points fall outside those
  limits, then those data points may show up in places such as the axes, the
  legend, the plot title, or the plot margins.}

\item{guide}{A function used to create a guide or its name. See
  \code{\link[=guides]{guides()}} for more information.}

\item{position}{For position scales, The position of the axis.
  \code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}

\item{sec.axis}{\code{\link[=sec_axis]{sec_axis()}} is used to specify a secondary axis.}

\item{line.zero}{axis zero}

\item{line.axis}{line axis}

\item{gap.box}{gap box}

\item{\ldots}{Other arguments passed on to \verb{scale_(x|y)_continuous()}}
}
\details{
Transformations only work with continuous values: see
\code{\link[scales:trans_new]{scales::trans_new()}} for list of transformations, and instructions
on how to create your own.
}
\examples{
\dontrun{
library(ggplot2)

set.seed(2021)
dat <- data.frame(x = 1:100, y = rnorm(100))
dat[50,2] <- 100  ## Outlier!

g0 = ggplot(dat, aes(x = x, y = y)) + geom_line()
g0
g0 + axis_cut(type = "scale", cut_axis = "y", cuts = 4, k = 0.05)
}
}
