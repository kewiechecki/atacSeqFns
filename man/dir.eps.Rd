% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dirfns.R
\name{dir.eps}
\alias{dir.eps}
\title{Concatenates a path, filename, and file extension into an output path, then opens an eps connection at the output path.
Any folders in the path that do not exist are created.}
\usage{
dir.eps(filename, path = ".", ..., append.date = T)
}
\arguments{
\item{filename}{The output file name.}

\item{path}{The path to the output file.}

\item{...}{Additional arguments to \code{postscript}.}

\item{append.date}{Whether to write the output to a directory with today's date.}
}
\description{
Concatenates a path, filename, and file extension into an output path, then opens an eps connection at the output path.
Any folders in the path that do not exist are created.
}
\examples{
dir.eps("example",path="path/to")
plot(1:5,1:5)
dev.off()
}
\seealso{
\code{\link{dir.img}}, \code{\link{grDevices::postscript}}
}
