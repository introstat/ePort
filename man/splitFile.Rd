% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanCSV.R
\name{splitFile}
\alias{splitFile}
\title{Split one csv data file into two files}
\usage{
splitFile(filenm, cut_idx, cut_type)
}
\arguments{
\item{filenm}{file name with path.}

\item{cut_idx}{the identified number of the last question in part 1.}

\item{cut_type}{either "RsQ" or "ID". See detail.}
}
\value{
two new csv files
}
\description{
If one homework contains two topics, we need to split the file.
For example, Chapter 2 covers the material for Topics 2 and 4.
}
\details{
There are two types of \code{cut_type}: RsQ and ID.
RsQ refers to the absolute ID for questions, which is consistent
with the Respondus questions.
ID refers to the questions that the students worked on.

For example, students were given 36 questions for Chapter 2,
the first 16 belong to Topic 2, so we set \code{cut_type='ID'}
and \code{cut_idx=16}. An alternative way is to set
\code{cut_type='RsQ'} and \code{cut_idx=53}, because the first
16 questions given to the students contains 53 questions totally.
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
\references{
Refer to package vignette Section 5.1
}

