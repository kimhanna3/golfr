% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initmat.R
\name{initmat}
\alias{initmat}
\title{Generates initial matrix}
\usage{
initmat(students)
}
\arguments{
\item{students}{a column of a dataset that has students' unique IDs}
}
\value{
matrix
}
\description{
Generates initial matrix
}
\examples{
# Initial matrix 5x5
data <- GenerateData(5)
initmat(data$Student)
}
