% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeSemester.R
\name{mergeData}
\alias{mergeData}
\title{Merge the scores from several homeworks}
\usage{
mergeData(files)
}
\arguments{
\item{files}{output from \code{setDir}}
}
\value{
a list of four data frames. First two are the data frames
with rows being the students and columns being the Topics for
scores and correct percentage. The third and fourth data frames
give the count of missings and full scores by section and topic.
}
\description{
With multiple data files from different sections and topics,
this function will merge the total scores and correct percentages
into data frames with rows being students and columns being topics.
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
\references{
Refer to package vignette Section 4.5.1
}

