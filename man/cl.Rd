% Please edit documentation in R/data_analysis.R
\name{cl}
\alias{cl}
\title{Cluster the students}
\usage{
cl(dat, l = 9)
}
\arguments{
\item{dat}{a data frame with rows being the students and
columns being the scores over the semester.}

\item{l}{max number of clusters to try}
}
\value{
a data frame with the left part being the scaled
\code{dat} and the right part being the clustering labels
for 1:\code{l} clusters.
}
\description{
Cluster the students
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
