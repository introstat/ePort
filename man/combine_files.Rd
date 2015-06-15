% Please edit documentation in R/clean_csv.R
\name{combine_files}
\alias{combine_files}
\title{Merge two csv files into one file}
\usage{
combine_files(file1, file2, newnm)
}
\arguments{
\item{file1}{file name with path for the first file.}

\item{file2}{file name with path for the second file.}

\item{newnm}{name for the new merged file.}
}
\value{
a new csv file
}
\description{
If one topic is organized in two chapters, then we need to
merge two files together. Note that the Question ID and RsQ ID
are updated for the second file when merging.
}
\examples{
library(ePort)

##### *. Split data files #####
datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files/Ch3"
namelist = list.files(path = datapath, full.names = TRUE)
for (i in namelist) rewrite_data(i)  # May not need this rewriting step!
for (i in c("AB", "CD", "EF", "GH", "JKQ", "LM")) {
    tmp = namelist[grep(i, basename(namelist))]
    split_file(tmp, 29, "ID")
}

##### *. Merge data files #####
datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Data Files/Ch9-Ch11"
namelist = list.files(path = datapath, full.names = TRUE)
for (i in namelist) rewrite_data(i)  # May not need this rewriting step!
for (i in c("AB", "CD", "EF", "GH", "JK", "LM")) {
    tmp = namelist[grep(i, basename(namelist))]
    combine_files(tmp[2], tmp[1], paste("Topic11", i, "csv", sep = "."))
}
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
