% sex 28 nov 2025 23:12:59
\name{stat_n}
\alias{stat_n}
\title{Count n}
\description{
  Get n.
}

\usage{
  stat_n(
    mapping = NULL,
    data = NULL,
    geom = "text",
    ypos = 0,
    position = "identity",
    inherit.aes = TRUE,
    show.legend = NA,
    na.rm = FALSE,
    vjust = 1.5,
    color = "gray30",
    prefix = NULL,
    ...
  )
}
\arguments{
\item{mapping}{mapping}
\item{data}{data}
\item{geom}{geom}
\item{ypos}{ypos}
\item{position}{position}
\item{inherit.aes}{inherit}
\item{show.legend}{show}
\item{na.rm}{na}
\item{vjust}{vjust}
\item{color}{color}
\item{prefix}{prefix}
\item{\ldots}{args}
}
\value{
A n samples.
}
%\author{ I.F. Sousa-Filho }
\examples{
x = c(-1.1234, 0, 1, 2.123456, 3.12, NA, Inf, -Inf)
format_number(x, nsmall = 2)
}
