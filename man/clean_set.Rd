% Please edit documentation in R/clean_data.R
\name{clean_set}
\alias{clean_set}
\title{Get the Question Set and Objective Set Scores}
\usage{
clean_set(HWsheet, key)
}
\arguments{
\item{HWsheet}{the output of function \code{clean_score} $HWsheet}

\item{key}{the output of function \code{convertkey}}
}
\value{
a list of two data frames of scores for the question
sets and learning objectives respectively.
}
\description{
Get the Question Set and Objective Set Scores
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
