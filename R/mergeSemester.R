globalVariables(c("txtProgressBar", "setTxtProgressBar"))

#' Set and parse the data directory
#' 
#' All of the csv files in the directory with specific
#' format will be detected and recorded.
#' 
#' @param data.dir directory of the data files. Could be NULL,
#' then a selection GUI will pop out.
#' @param data.list a vector of data files with path. Suppressed 
#' if data.dir is not NULL.
#' @return a data frame of file names (with path), topics, and sections
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @references Refer to package vignette (Section 4.5.1)
#' @export
#' 
setDir = function(data.dir=NULL, data.list=NULL){
  if (is.null(data.dir) && is.null(data.list)) {
    message("Please select any file in your data directory ...")
    data.dir = dirname(file.choose())
  }
  hwk_list = if(!is.null(data.dir)) {
    list.files(data.dir,full.names=TRUE)[grep("\\.csv$", list.files(data.dir))]
  } else {data.list}
  hwk_topic = gsub("\\..*$","",basename(hwk_list))
  hwk_section = gsub("^[A-Za-z0-9\\-]*\\.","",gsub("\\.csv$","",basename(hwk_list)))
  hwk_files = data.frame(file=hwk_list,topic=hwk_topic,section=hwk_section,stringsAsFactors=FALSE)
  hwk_files = hwk_files[order(hwk_topic,hwk_section),]
  chap10 = grep("^[A-Za-z]{2}1[0-9]",hwk_files[,2])
  chap20 = grep("^[A-Za-z]{2}2[0-9]",hwk_files[,2])
  chap00 = dplyr::setdiff(1:nrow(hwk_files),c(chap10,chap20))
  hwk_files = hwk_files[c(chap00,chap10,chap20),]
  rownames(hwk_files) = 1:nrow(hwk_files)
  return(hwk_files)
}


#' Merge the scores from several homeworks
#' 
#' With multiple data files from different sections and topics,
#' this function will merge the total scores and correct percentages
#' into data frames with rows being students and columns being topics.
#' 
#' @param files output from \code{setDir}
#' @return a list of four data frames. First two are the data frames
#' with rows being the students and columns being the Topics for
#' scores and correct percentage. The third and fourth data frames
#' give the count of missings and full scores by section and topic.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @references Refer to package vignette (Section 4.5.1)
#' @importFrom reshape2 dcast
#' @export
#' 
mergeData = function(files){
  nfile=nrow(files)
  dat=list()
  score=data.frame()
  pb = txtProgressBar(min = 0, max = nfile, style = 3, label="Processing the data ...")
  for (i in 1:nfile) {
    dat[[i]]=data.frame(readScore(files[i,"file"],keepAttempts=FALSE)$score[,c(1,2,4)],Topic=files[i,"topic"],Section=files[i,"section"],stringsAsFactors=FALSE)
    score=rbind(score,dat[[i]])
    setTxtProgressBar(pb, i)
  }
  close(pb)
  names(dat)=paste(files$section,files$topic,sep="-")
  score$CrtPct=score$Score/score$Grade
  score$Topic = factor(score$Topic,levels=unique(score$Topic))
  score$Section = factor(score$Section,levels=unique(score$Section))
  
  FullScore=unique(score[,c('Topic','Section','Grade')])
  FullScore=reshape2::dcast(FullScore,Section~Topic,fun.aggregate=max,na.rm=TRUE,value.var='Grade')
  rownames(FullScore)=FullScore$Section
  FullScore=FullScore[,-1]
    
  mergemtrx=reshape2::dcast(score,Username+Section~Topic,value.var='Score')
  CorrectPct=reshape2::dcast(score,Username+Section~Topic,value.var='CrtPct')
  if (sum(duplicated(mergemtrx$Username))) {
    transtudent = mergemtrx[which(duplicated(mergemtrx$Username)),1]
    for (i in transtudent) {
      w = rev(which(mergemtrx[,1]==i))
      mergemtrx[w[1],-(1:2)] = sapply(mergemtrx[w,-(1:2)],function(x){ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))})
      mergemtrx = mergemtrx[-w[-1],]
      CorrectPct[w[1],-(1:2)] = sapply(CorrectPct[w,-(1:2)],function(x){ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))})
      CorrectPct = CorrectPct[-w[-1],]
    }
  }
  rownames(mergemtrx)=rownames(CorrectPct)=mergemtrx$Username
  # Original line
  MissingTimes=apply(mergemtrx[,-(1:2)],1,function(avec){sum(is.na(avec))})
  #MissingTimes=sum(is.na(mergemtrx[,-(1:2)]))
  CorrectPct=data.frame(CorrectPct,AvgCrtPct=rowMeans(CorrectPct[,-(1:2)],na.rm=TRUE),check.names=FALSE)
  ord=order(7-as.integer(as.factor(CorrectPct$Section)),CorrectPct$AvgCrtPct,ncol(mergemtrx)-2-MissingTimes,decreasing=TRUE)
  mergemtrx=mergemtrx[ord,]
  CorrectPct=CorrectPct[ord,]
  CorrectPct[,-(1:2)]=CorrectPct[,-(1:2)]*100
  
  MissingTimes = data.frame(section=mergemtrx$Section,'Times-Missing'=MissingTimes[ord])
  MissingTimes$section = as.character(MissingTimes$section)
  MissingTimes = MissingTimes[MissingTimes[,2]>0,]
  MissingTimes = MissingTimes[order(MissingTimes$section,-MissingTimes[,2]),]
  
  outcome=list(Score=mergemtrx,CorrectPct=CorrectPct,MissingTimes=MissingTimes,FullScore=FullScore)
  return(outcome)
}


#' Subset one section in use
#' 
#' If we want to generate a report for one section with
#' many topics, this function could help to filter the
#' section from the output of \code{mergeData}.
#' 
#' @param mergedat output from \code{mergeData}
#' @param files output from \code{setDir}
#' @param choice section name. Default to be 'all',
#' which means all the sections are selected.
#' @return a subset of \code{mergedat} with the same format
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @references Refer to package vignette (Section 4.5.1)
#' @export
#' 
subsetData = function(mergedat,files,choice='all'){
  if (choice!='all'){
    stopifnot(length(choice)==1, choice %in% files$section)
    files=files[files$section == choice,,drop=FALSE]
    mergedat$Score = mergedat$Score[mergedat$Score$Section == choice,,drop=FALSE]
    mergedat$CorrectPct = mergedat$CorrectPct[mergedat$CorrectPct$Section == choice,,drop=FALSE]
    mergedat$MissingTimes = mergedat$MissingTimes[mergedat$MissingTimes$section == choice,,drop=FALSE]
    mergedat$FullScore = mergedat$FullScore[choice,,drop=FALSE]
  }
  return(list(dat=mergedat,file=files,format=choice))
}
