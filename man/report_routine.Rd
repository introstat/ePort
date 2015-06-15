% Please edit documentation in R/report_format.R
\name{report_routine}
\alias{report_routine}
\title{Generate a report for one homework}
\usage{
report_routine(keyfile, datafile = NULL, topic = NULL, section = NULL, path = NULL, 
    type = NULL, rewrite = FALSE, skip = NULL, LOfile = NULL, knitfile = NULL, 
    knito = getwd())
}
\arguments{
\item{keyfile}{the file name with path for the answer key}

\item{datafile}{the file name with path for the data}

\item{topic}{the topic number, could be integer or character}

\item{section}{the section name, usually one of 'AB', 'CD', 'GHQ', '201', ...}

\item{path}{the directory to the data files}

\item{type}{the strings before the topic number in the csv file name}

\item{rewrite}{logical. If TRUE then the csv files are rewritten.}

\item{skip}{a vector of integers. Same as the parameter in \code{clean_score}.}

\item{LOfile}{the file name with path for the learning outcomes. Could be
NULL, then will be auto-completed by "path/../Topic Outcomes/*.Outcomes.txt".}

\item{knitfile}{the file name with path for the Rnw file to knit.
Could be NULL, then will be auto-completed by the file in the inst folder.}

\item{knito}{the directory to save the tex file}
}
\value{
a tex file to be compiled
}
\description{
A tex file will be generated from the specified data files,
answer key, and learning outcomes.
}
\examples{
library(ePort)

##### Homework reports #####
## key ##
# save the .doc file as .htm format, to keep the images
key_htm = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Database Questions/Topic05.Questions.htm"
refine_key(key_htm) # generate a clean answer key with paths to the plots in the question
key_txt = gsub("htm$","txt",key_htm) # get the path/name of the new answer key

## data ##
datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files"
topic = gsub('.Questions.txt','',gsub('Topic','',basename(key_txt)))
namelist = list.files(path=datapath,full.names=TRUE)
namelist = namelist[grep(paste('Topic',topic,'\\\\.',sep=''),basename(namelist))]
namelist

## learning objectives ##
LOpath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Topic Outcomes/Topic05.Outcomes.txt"

## 1. Individual report ##
for (i in namelist) rewrite_data(i)
for (i in namelist) report_routine(key_txt,datafile=i,rewrite=FALSE,LOfile=LOpath,knitfile="Rnw/hw-individual-short.Rnw")
for (i in namelist) report_routine(key_txt,datafile=i,rewrite=FALSE,LOfile=LOpath,knitfile="Rnw/hw-individual.Rnw")

## 2. Cross-section report ##
report_routine(key_txt,namelist,LOfile=LOpath,knitfile="Rnw/hw-section-short.Rnw")
report_routine(key_txt,namelist,LOfile=LOpath,knitfile="Rnw/hw-section.Rnw")

## 3. Cross-topic report ##
tabfiles = set_dir(datapath)
tabfiles = tabfiles[tabfiles$section!='201' & tabfiles$topic \%in\% paste0('Topic0',1:5),]
mgdata = merge_data(tabfiles)
for (sctn in unique(tabfiles$section)) {
  merged = subset_data(mgdata,tabfiles,choice=sctn)
  knit("Rnw/hw-topic.Rnw",output=paste0('Stat101hwk_Unit1_Section',sctn,'.tex'))
}

## 4. Cross-section & cross-topic report ##
merged = subset_data(mgdata,tabfiles)
knit("Rnw/hw-topic-section.Rnw",output='Stat101hwk_Unit1_allSections.tex')
}
\author{
Xiaoyue Cheng <\email{xycheng@iastate.edu}>
}
