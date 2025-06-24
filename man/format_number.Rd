%
\name{format_number}
\alias{format_number}
\title{Format number}
\description{
Format an R object for pretty printing.
}
\usage{
format_number(x,
              nsmall = NULL,
              value = FALSE,
              decimal.mark = ",",
              preffix = NULL,
              suffix = NULL,
              p = FALSE,
              parse_p = FALSE,
              suffix_p = NULL,
              ...)
}
\arguments{
\item{x}{a vector of real numbers.}
\item{nsmall}{the minimum number of digits to the right of the decimal point
          in formatting real/complex numbers in non-scientific formats.
          Allowed values are '0 <= nsmall <= 20'.}
\item{value}{value}
\item{decimal.mark}{decimal.mark}
\item{preffix}{preffix}
\item{suffix}{suffix}
\item{p}{p}
\item{parse_p}{parse_p}
\item{suffix_p}{suffix_p}
\item{...}{further arguments passed to \code{format()}.}
}
\value{
A character vector.
}
\author{ I.F. Sousa-Filho }
\examples{
x = c(-1.1234, 0, 1, 2.123456, 3.12, NA, Inf, -Inf)
format_number(x)
format_number(x, nsmall = 2)
}

