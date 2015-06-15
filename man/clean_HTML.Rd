% Please edit documentation in R/clean_data.R
\name{clean_HTML}
\alias{clean_HTML}
\title{Remove the HTML strings in the answer}
\usage{
clean_HTML(HWsheet, mark = TRUE, image = FALSE)
}
\arguments{
\item{HWsheet}{a vector or data frame of characters}

\item{mark}{logical. If TRUE then some marks are given to the matching questions.}

\item{image}{logical. If TRUE then some marks are given to the embedded images.}
}
\value{
a vector or data frame with no HTML strings
}
\description{
Remove the HTML strings in the answer
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
