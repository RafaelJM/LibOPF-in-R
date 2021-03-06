% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LibOPF.R
\name{opf_read_subGraph}
\alias{opf_read_subGraph}
\title{Reads a file which contains the subGraph}
\usage{
opf_read_subGraph(file)
}
\arguments{
\item{file}{The file name}
}
\value{
Returns the subGraph object
}
\description{
Reads a file which contains the subGraph
}
\examples{
dat <- opf_read_subGraph(system.file("extdata/boat.dat",package = "LibOPF"))
X <- opf_split(dat,0.5,0,0.5,0)
T <- X$training
T2 <- X$testing
Y <- opf_train(T)
class <- opf_classify(T2, Y$classifier)
acc <- opf_accuracy(T2, class)

}
