%
\name{scale_transf_continuous}
\alias{scale_transf_continuous}
\alias{gapbox}
\alias{position_default}
\title{Scale transf continuous}
\description{
  \code{coord_trans_flip()} is different to scale transformations in that it occurs after
  statistical transformation and will affect the visual appearance of geoms - there is
  no guarantee that straight lines will continue to be straight.
}
\usage{
  scale_transf_continuous(
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
  )

  gapbox(
    ...,
    fill = "white",
    size = 1,
    color = "gray55",
    linetype = 1
  )

position_default(p = c("y", "x"))

}
\arguments{
\item{cut_axis}{Which axis to break, x or y.}
\item{cuts}{where to place the break in user units}

\item{k}{Axis compression constant}

\item{gap}{Percentage of distance from the value established in cuts.}

\item{name}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks computed by the
\link[scales:new_transform]{transformation object}
\item A numeric vector of positions
\item A function that takes the limits as input and returns breaks
as output (e.g., a function returned by \code{\link[scales:breaks_extended]{scales::extended_breaks()}}).
Note that for position scales, limits are provided after scale expansion.
Also accepts rlang \link[rlang:as_function]{lambda} function notation.
}}

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

\item{limits}{One of:
  \itemize{
    \item \code{NULL} to use the default scale range
    \item A numeric vector of length two providing limits of the scale.
    Use \code{NA} to refer to the existing minimum or maximum
    \item A function that accepts the existing (automatic) limits and returns new limits
    Note that setting limits on positional scales will \strong{remove} data outside of the limits.
}}

\item{position}{For position scales, The position of the axis.
  \code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
  the limits to ensure that data and axes don't overlap. If \code{FALSE},
  limits are taken exactly from the data or \code{xlim}/\code{ylim}.}

\item{oob}{One of:
  \itemize{
    \item Function that handles limits outside of the scale limits
    (out of bounds).
    \item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
    bounds values with \code{NA}.
    \item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
    \item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.}}

\item{na.value}{Missing values will be replaced with this value.}

\item{guide}{A function used to create a guide or its name. See
  \code{\link[=guides]{guides()}} for more information.}

\item{sec.axis}{\code{\link[=sec_axis]{sec_axis()}} is used to specify a secondary axis.}

\item{axis_line}{axis}

\item{gap_box}{gap box}

\item{\ldots}{
  Other arguments passed on to \verb{scale_(x|y)_continuous()}.
  For gapbox is
}

\item{fill}{fill}
\item{size}{size}
\item{color}{color}
\item{linetype}{line}

\item{p}{y or x}
}
\details{
Transformations only work with continuous values: see
\code{\link[scales:trans_new]{scales::trans_new()}} for list of transformations, and instructions
on how to create your own.
}
\examples{
\dontrun{
library(ggplot2)

set.seed(2022)
y = rnorm(100)

dat <- data.frame(x = 1:100, y)
dat[50,2] <- 100  ## Outlier!

g0 = ggplot(dat, aes(x = x, y = y)) +
  geom_line()
g0
g1 = g0 + scale_transf_continuous(
                   cut_axis = "y",
                   cuts = 4,
                   gap = 1,
                   k = 0.05)
g1
}
}
