%
\name{outliers}
\alias{outliers}
\title{ Find outliers }
\description{
  Finds value with largest difference between it and sample mean, which can be an outlier.
}
\usage{
outliers(data,
         value,
         method = c("tukey", "adjbox", "hampel"),
         remove = c("none", "extreme", "outlier"),
         name.classes = NULL,
         title = NULL,
         language = c("pt", "en"),
         digits = 3,
         coef = 1.5,
         coef.extreme = 2 * coef,
         probs = c(.25, .75),
         add.thres = TRUE,
         breaks.x = 4,
         plot = TRUE,
         na.rm = FALSE,
         a = -4,
         b = 3,
         type = 7)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a data sample, vector in most cases. }
  \item{value}{value}
  \item{method}{ "tukey", "adjbox", "hampel" }
  \item{remove}{ "none", "extreme", "outlier" }
  \item{name.classes}{name.classes}
  \item{title}{title}
  \item{language}{"pt", "en"}
  \item{digits}{digits}
  \item{coef}{coef}
  \item{coef.extreme}{ 2 * coef}
  \item{probs}{.25, .75}
  \item{add.thres}{add.thres}
  \item{breaks.x}{breaks.x}
  \item{plot}{plot}
  \item{na.rm}{na.rm}
  \item{a}{a}
  \item{b}{b}
  \item{type}{type}
}
\value{

A list of value(s) with plot, data.table, fences, stats.

}
\author{ Izidro F. Sousa-Filho }
\examples{
y = c(-2, NA, 0.59, 0.61, 0.59, 1.55, 1.33, 3.50, 1.00, 1.22, 2.50, 3.00, 3.79, 3.98, 4.33, 4.45,
     4.59, 4.72, 4.82, 4.90, 4.96, 7.92, 5.01, 5.01, 4.94, 5.05, 5.04, 5.03, 5.06, 5.10,
     5.04, 5.06, 7.77, 5.07, 5.08, 5.08, NA, 5.12, 5.12, 5.08, 5.17, 5.18, NA, NA, 10)

out = outliers(y, plot = FALSE, method = "tukey", remove = "none", language = "en")
print(out)
}
