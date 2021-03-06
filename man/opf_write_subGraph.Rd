% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LibOPF.R
\name{opf_write_subGraph}
\alias{opf_write_subGraph}
\title{Writes into a file a subGraph}
\usage{
opf_write_subGraph(g, file)
}
\arguments{
\item{g}{The subGraph object}

\item{file}{The file name to save the subGraph}
}
\value{
`NULL`
}
\description{
Writes into a file a subGraph
}
\examples{
dataset <- opf_read_subGraph(system.file("extdata/boat.dat",package = "LibOPF"))
opf_write_subGraph(dataset, file.path(tempdir(), "boat.dat"))

}
