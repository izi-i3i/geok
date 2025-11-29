% sex 28 nov 2025 22:17:52
\name{sumup}
\alias{sumup}
\alias{symbs}
\title{ Summary }
\description{
  Summary.
}
\usage{
sumup(
  x,
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
  ...
)

symbs(
  symb.mean = NULL,
  symb.median = NULL,
  symb.five = NULL,
  symb.custom = NULL,
  minusplus.sign = FALSE,
  ...
)

}
% documented here.
\arguments{
  \item{x}{ A data.table, data.frame or vector. }
  \item{measure.var}{ value to be measured }
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

  \item{symb.mean}{symb}
  \item{symb.median}{symb}
  \item{symb.five}{symb}
  \item{symb.custom}{symb}
  \item{minusplus.sign}{symb}
  \item{...}{args to}
}
\value{
  A list of value(s) stats.
}
%\author{ I.F. Sousa-Filho }
\examples{
  \dontrun{
    set.seed(123)
    z <- c(rnorm(1e3, 6, 2), NA, NaN)

    sumup(z, missing.rm = TRUE, lang = "en")

    sumup(iris, measure.var = "Sepal.Length", group.by = "Species", lang = "en")
  }
}
