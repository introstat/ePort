#' Rewrite the data file
#' 
#' Solve two problems that I used to work in gedit:
#' "" --> ", and "\\r\\n" --> \\r\\n.
#' This function will read the data, replace every two quotes ""
#' by one quote ", remove the quotes before and after every line,
#' i.e., replace "\\r\\n" by \\r\\n, then write the result to the
#' same file.
#' 
#' @param filenm file name with path
#' @return NULL
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' @example inst/ex-reportHwk.R
#' 
rewrite_data = function(filenm){
  csv = read.delim(filenm,header=FALSE)[,1]
  csv = as.character(csv)
  
  test = unlist(gregexpr(',\"',csv))
  if (length(test) <= 3*length(csv)) {
    message(paste(basename(filenm), "is not in an expected format."))
    return()
  }
  
  newcsv = sapply(csv,strsplit,split=',\"')
  newcsv = lapply(newcsv,gsub,pattern='\"',replacement='')
  out = data.frame(t(data.frame(newcsv,stringsAsFactors=FALSE)),stringsAsFactors=FALSE)
  names(out) = out[1,]
  out = out[-1,]
  for (i in 7:9) out[,i] = as.numeric(out[,i])
  
  write.csv(out,file=filenm,row.names =FALSE)
  return(NULL)
}

#' Split one csv data file into two files
#' 
#' If one homework contains two topics, we need to split the file.
#' For example, Chapter 2 covers the material for Topics 2 and 4.
#' 
#' There are two types of \code{cut_type}: RsQ and ID.
#' RsQ refers to the absolute ID for questions, which is consistent
#' with the Respondus questions.
#' ID refers to the questions that the students worked on.
#' 
#' For example, students were given 36 questions for Chapter 2,
#' the first 16 belong to Topic 2, so we set \code{cut_type='ID'}
#' and \code{cut_idx=16}. An alternative way is to set
#' \code{cut_type='RsQ'} and \code{cut_idx=53}, because the first
#' 16 questions given to the students contains 53 questions totally.
#' 
#' @param filenm file name with path.
#' @param cut_idx the identified number of the last question in part 1.
#' @param cut_type either "RsQ" or "ID". See detail.
#' @return two new csv files
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' @example inst/ex-mergeSplitChap.R
#' 
split_file = function(filenm,cut_idx,cut_type){
  stopifnot(cut_type %in% c('RsQ','ID'))
  dat = read.csv(filenm, check.names = FALSE, colClasses=c(rep('character',6),rep('numeric',3)))
  if (cut_type=='RsQ') {
    idx = as.integer(substr(dat[,5],9,11))
    dat1 = dat[which(idx<=cut_idx),]
    dat2 = dat[which(idx>cut_idx),]
    idx2 = as.integer(substr(dat2[,5],9,11))
    dat2[,5] = paste('<!--RsQ_',sprintf("%03.0f",idx2-cut_idx),substring(dat2[,5],12),sep='')
  } else if (cut_type=='ID') {
    idx = as.integer(substring(dat[,4],13))
    dat1 = dat[which(idx<=cut_idx),]
    dat2 = dat[which(idx>cut_idx),]
    maxRsQ = max(as.integer(substr(dat1[,5],9,11)))
    idx2 = as.integer(substr(dat2[,5],9,11))
    stopifnot(maxRsQ == min(idx2)-1)
    dat2[,5] = paste('<!--RsQ_',sprintf("%03.0f",idx2-maxRsQ),substring(dat2[,5],12),sep='')
  }
  write.csv(dat1,file=gsub('.csv$','.part1.csv',filenm),row.names=F)
  write.csv(dat2,file=gsub('.csv$','.part2.csv',filenm),row.names=F)
}

#' Merge two csv files into one file
#' 
#' If one topic is organized in two chapters, then we need to 
#' merge two files together. Note that the Question ID and RsQ ID
#' are updated for the second file when merging.
#' 
#' @param file1 file name with path for the first file.
#' @param file2 file name with path for the second file.
#' @param newnm name for the new merged file.
#' @return a new csv file
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' @example inst/ex-mergeSplitChap.R
#' 
combine_files = function(file1,file2,newnm){
  dat1 = read.csv(file1, check.names = FALSE, colClasses=c(rep('character',6),rep('numeric',3)))
  dat2 = read.csv(file2, check.names = FALSE, colClasses=c(rep('character',6),rep('numeric',3)))
  dat1maxID = max(as.integer(substring(dat1[,4],13)))
  dat1maxRsQ = max(as.integer(substr(dat1[,5],10,11)))
  dat2[,4] = paste('Question ID', as.integer(substring(dat2[,4],13))+dat1maxID)
  dat2[,5] = paste('<!--RsQ_0',as.integer(substr(dat2[,5],10,11))+dat1maxRsQ,substring(dat2[,5],12),sep='')
  dat = rbind(dat1,dat2)
  dat[,1] = factor(dat[,1],levels=unique(dat[,1]))
  dat[,4] = factor(dat[,4],levels=unique(dat[,4]))
  dat = dat[order(dat[,1],dat[,4]),]
  write.csv(dat,file=newnm,row.names=F)
}
