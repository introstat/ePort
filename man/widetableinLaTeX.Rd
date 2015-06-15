% Please edit documentation in R/report_format.R
\name{widetableinLaTeX}
\alias{widetableinLaTeX}
\title{Split wide tables into several tables}
\usage{
widetableinLaTeX(atable, tablecaption, label, cols = 7)
}
\arguments{
\item{atable}{a data frame to be printed by \code{xtable}.}

\item{tablecaption}{caption of the tables. Printed only
after the first table.}

\item{label}{label of the tables, only after the first table.}

\item{cols}{maximum number of columns for each table.}
}
\value{
a list with several tables, each of them has no more
than \code{cols} columns.
}
\description{
When a table contains many columns, the display in pdf file
produced by LaTeX will be too wide to see. This function
cuts the table into several tables, each of which has no
more than \code{cols} columns.
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
