%
\name{medcouple}
\alias{medcouple}
\title{A robust measure of skewness for univariate data}
\usage{
medcouple(x, do.reflect = "auto", na.rm = FALSE)
}
\arguments{
\item{x}{An \eqn{n} by \eqn{p} data matrix.}

\item{do.reflect}{
Logical indicating whether the medcouple should also be computed on the reflected sample \code{-x}, with final result \eqn{(mc}(\code{x})\eqn{-mc(-}\code{x}\eqn{))/2}. \cr Defaults \code{TRUE} when \eqn{n<=100} and to \code{FALSE} otherwise.}

\item{na.rm}{remove na.}
}

\value{
mc A \eqn{p}-vector containing the medcouple of each column of the data matrix \code{x}.
}
\description{
Computes the medcouple, a robust measure of skewness for univariate data. 
For multivariate data the medcouple is calculated on each column of the data matrix.
}
\details{
The medcouple is a robust measure of skewness yielding values between \eqn{-1} and \eqn{1}. For left- and right-skewed data the medcouple is negative and positive respectively. \cr
The medcouple is defined as the median of the kernel function
\eqn{h(x_i,x_j) = \frac{(x_j - med(x)) - (med(x)-x_i)}{x_j-x_i}}
evaluated over all couples \eqn{(x_i,x_j)} where
\eqn{x_i} is smaller than the median of \code{x} and \eqn{x_j} larger than the median of \code{x}. When there are multiple observations tied to the median, the kernel is defined separately as the denominator is not defined for these observations. Let \eqn{m_1 < ... < m_k} denote the indices of the observations which are tied to the median. Then \eqn{h(x_{m_i},x_{m_j})} is defined to equal \eqn{-1} if \eqn{i + j - 1 < k}, \eqn{0} when \eqn{i + j - 1 = k} and \eqn{+1} if \eqn{i + j - 1 > k}. To compute the medcouple an algorithm with time complexity \eqn{O(n log(n))} is applied. For details, see \url{https://en.wikipedia.org/wiki/Medcouple}. 
For numerical accuracy it is advised, for small data sets, to compute the medcouple on both \code{x} and \code{-x}. The final value of the medcouple may then be obtained as a linear combination of both calculations. This procedure is warranted by the properties of the medcouple. Indeed the medcouple of the distribution \eqn{X} equals minus the medcouple of the reflected distribution \eqn{-X}. Moreover the medcouple is location and scale invariant.
Note that missing values are not allowed.
}
\note{
This function is extracted from the package mrfDepth - 2023
}
\author{
P. Segaert with original code from M. Maechler and G. Brys.
}
\references{
Brys G., Hubert M., Struyf A. (2004). A robust measure of skewness. \emph{Journal of Computational and Graphical Statistics}, \bold{13}, 996--1017.
}
\examples{
# Calculate the medcouple of univariate data sets.
# For 2000 normally distributed values
# the medcouple value is close to 0 because 
# data are not skewed
x<-rnorm(2000)
medcouple(x) 
# For 2000 values following a lognormal
# distribution (mean 0,sd 1), medcouple is close to 1
# because values are right-skewed
y<-rnorm(2000)
medcouple(y) 
# Use the option do.reflect to increase expected accuracy. 
medcouple(y, do.reflect = TRUE)
}
