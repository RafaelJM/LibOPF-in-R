% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LibOPF.R
\name{opf_distance}
\alias{opf_distance}
\title{Generates the precomputed distance file for the OPF classifier}
\usage{
opf_distance(dataSet, distanceOp, normalize = 0)
}
\arguments{
\item{dataSet}{The subGraph object, normaly is the whole data}

\item{distanceOp}{Distance calculation option}

\item{normalize}{Distance normalization? 1- yes 0 - no}
}
\value{
Returns the distance matrix
}
\description{
Generates the precomputed distance file for the OPF classifier
}
\details{
Options for distance calculation:\cr
1 - Euclidean\cr
2 - Chi-Square\cr
3 - Manhattan (L1)\cr
4 - Canberra\cr
5 - Squared Chord\cr
6 - Squared Chi-Squared\cr
7 - BrayCurtis
}
\examples{
dat <- opf_read_subGraph(system.file("extdata/boat.dat",package = "LibOPF"))
dist <- opf_distance(dat,3,0)
X <- opf_split(dat,0.5,0,0.5,0)
T <- X$training
T2 <- X$testing
Y <- opf_train(T,dist)
class <- opf_classify(T2, Y$classifier,dist)
acc <- opf_accuracy(T2, class)

}
