% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmfns.R
\name{col.abs}
\alias{col.abs}
\title{Color scale for a specified quantile. This scale is intended for heatmaps containing only positive values, so the range is set to \code{c(0,1-quant)}.}
\usage{
col.abs(x, quant = 0.05)
}
\arguments{
\item{x}{A numeric matrix or vector.}

\item{quant}{The quantile to be used as the upper limit.}
}
\value{
A \code{colorRamp2} scale which can be passed to \code{Heatmap()}.
}
\description{
Color scale for a specified quantile. This scale is intended for heatmaps containing only positive values, so the range is set to \code{c(0,1-quant)}.
}
