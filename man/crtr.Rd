% Please edit documentation in R/data_analysis.R
\name{crtr}
\alias{crtr}
\title{Calculate the criterion values for different number of clusters}
\usage{
crtr(dat, ntopic, l = 9)
}
\arguments{
\item{dat}{output of function \code{cl}}

\item{ntopic}{number of topics}

\item{l}{number of clusters}
}
\value{
a data frame of three columns: cluster, criteria, and values
}
\description{
The criteria include SSR, SSE, and R-square.
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
