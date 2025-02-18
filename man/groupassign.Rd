% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupassign.R
\name{groupassign}
\alias{groupassign}
\title{Assign Groups and Update Interaction Matrix}
\usage{
groupassign(student_data, students_per_group, iterations)
}
\arguments{
\item{student_data}{A data frame containing student identifiers. The column should be named `Student`.}

\item{students_per_group}{An integer specifying the number of students per group.}

\item{iterations}{An integer defining how many rounds of group assignments should be performed.}
}
\value{
A matrix representing the interaction history of students, where each cell indicates
        the number of times two students have been grouped together.
}
\description{
Assign Groups and Update Interaction Matrix
}
\examples{
n_students <- 4
student_data <- GenerateData(n_students)
students_per_group <- 2
iterations <- 3

matrices_df <- groupassign(student_data, students_per_group, iterations)
matrices_df
}
