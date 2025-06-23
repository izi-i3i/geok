%
\name{summarize}
\alias{summarize}
\title{ Summary }
\description{
  Summary.
}
\usage{
summarize(x,
          measure.var,
          group.by = NULL,
          interval = NULL,
          params = NULL,
          params.names = NULL,
          language = c("pt", "en"),
          params.digits = NULL,
          variable.name = "Params",
          missing.val = NA,
          missing.rm = FALSE,
          decimal.mark = "auto",
          digits = 3,
          five.numbers = TRUE,
          fun = NULL,
          custom5numbers = NULL,
          sample.name = NULL,
          sample.col = FALSE,
          symb.header = symbs(),
          symb.body = symbs(),
          ...)
}
% maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A data.frame or vector. }
  \item{measure.var}{measure.var}
  \item{group.by}{group.by}
  \item{interval}{interval}
  \item{params}{params}
  \item{params.names}{params.names}
  \item{language}{language}
  \item{params.digits}{params.digits}
  \item{variable.name}{variable.name}
  \item{missing.val}{missing.val}
  \item{missing.rm}{missing.rm}
  \item{decimal.mark}{decimal.mark}
  \item{digits}{digits}
  \item{five.numbers}{five.numbers}
  \item{fun}{fun}
  \item{custom5numbers}{custom5numbers}
  \item{sample.name}{sample.name}
  \item{sample.col}{sample.col}
  \item{symb.header}{symb.header}
  \item{symb.body}{symb.body}
  \item{...}{args to}
}
\value{

A list of value(s) stats.

}
\author{ Izidro F. Sousa-Filho }
\examples{
z = c(15.4,9.21,4.2,7.5,12.2,18.3,17.3,
      14.3,14.02, 20, 12.3, 14.1, 17.3, 15.4,
      12.2,11.4, 9.1,18,14.43,17,15.1,13.4,
      15.3, 12.2,13.3) # c(12,2; 15,2)

summarize(z)
}
