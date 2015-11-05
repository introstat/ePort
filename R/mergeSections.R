#' Summarise several files of the same topic
#' 
#' There are usually 6-7 sections each semester. This function
#' is used to extract the scores from different sections and merge
#' them together.
#' 
#' @param filenmlist a vector of file names of the Blackboard data with path
#' @param answerkey the output of function \code{convertKey}
#' @param skip a vector of integers. Same as the parameter in \code{cleanScore}.
#' @return a list of results including the summaries and merged data
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom reshape2 melt
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#' @export
#' @example inst/ex-mergeSections.R
#' 
mergeSection = function(filenmlist, answerkey, skip=NULL){
  dat=tmp=scores=counts=QsetCrtPct=CsetCrtPct=list()
  Qsscores=Csscores=QsFscore=CsFscore=list()
  rows=totalmean=totalstd=totalmin=totalq1=totalmedian=totalq3=totalmax=c()
  totalpctmean=totalpctstd=totalpctmin=totalpctq1=totalpctmedian=totalpctq3=totalpctmax=c()
  QPct=Qscores=Qanswers=NULL
  dotpos=gregexpr("\\.",basename(filenmlist)[which.max(nchar(basename(filenmlist)))])[[1]]
  SectionName=substr(basename(filenmlist),dotpos[1]+1,dotpos[2]-1)
  
  for (i in 1:length(filenmlist)){
    dat[[i]]=readScore(filenmlist[i])
    scores[[i]]=cleanScore(dat[[i]],answerkey,skip)
    tmp[[i]] = cleanSet(scores[[i]]$HWsheet, answerkey)
    
    Qscores = rbind(Qscores, scores[[i]]$HWsheet)
    Qanswers = rbind(Qanswers, scores[[i]]$HWanswer)
    QPct = rbind(QPct,colMeans(t(apply(scores[[i]]$HWsheet[,-1],1,function(x)x/answerkey$simplekey$QFullPt)),na.rm=TRUE))
    counts[[i]] = scores[[i]]$nResponse
    rows[i] = nrow(scores[[i]]$HWsheet)
    
    Qsscores[[i]] = tmp[[i]]$QuestionSet$QuestionSetScore
    QsFscore[[i]] = tmp[[i]]$QuestionSet$QSFullScore
    Csscores[[i]] = tmp[[i]]$ObjectiveSet$ConceptSetScore
    CsFscore[[i]] = tmp[[i]]$ObjectiveSet$CSFullScore
    
    totalmean[i] = mean(scores[[i]]$HWsheet[,1])
    totalstd[i] = sd(scores[[i]]$HWsheet[,1])
    totalmin[i] = min(scores[[i]]$HWsheet[,1])
    totalq1[i] = quantile(scores[[i]]$HWsheet[,1],0.25)
    totalmedian[i] = median(scores[[i]]$HWsheet[,1])
    totalq3[i] = quantile(scores[[i]]$HWsheet[,1],0.75)
    totalmax[i] = max(scores[[i]]$HWsheet[,1])
    
    fullscore = sum(QsFscore[[i]])
    totalpctmean[i] = mean(scores[[i]]$HWsheet[,1]/fullscore)
    totalpctstd[i] = sd(scores[[i]]$HWsheet[,1]/fullscore)
    totalpctmin[i] = min(scores[[i]]$HWsheet[,1]/fullscore)
    totalpctq1[i] = quantile(scores[[i]]$HWsheet[,1]/fullscore,0.25)
    totalpctmedian[i] = median(scores[[i]]$HWsheet[,1]/fullscore)
    totalpctq3[i] = quantile(scores[[i]]$HWsheet[,1]/fullscore,0.75)
    totalpctmax[i] = max(scores[[i]]$HWsheet[,1]/fullscore)
    
    QsetCrtPct[[i]] = data.frame(t(apply(Qsscores[[i]],1,function(x){x/QsFscore[[i]]})))
    CsetCrtPct[[i]] = data.frame(t(apply(Csscores[[i]],1,function(x){x/CsFscore[[i]]})))  
  }
  
  meltPct=reshape2::melt(data.frame(Section=SectionName,QPct,check.names=FALSE),id.vars='Section')
  colnames(meltPct)=c("Section","Question","Percent")
  dotpos=gregexpr("\\.",meltPct$Question)
  meltPct$QuestionSet=answerkey[[2]][meltPct$Question,'Question.Set']
  meltPct$LearningObj=answerkey[[2]][meltPct$Question,'Objective.Set']
  
  sumrytotal=rbind(totalmean,totalstd,totalmin,totalq1,totalmedian,totalq3,totalmax)
  rownames(sumrytotal)=c('Mean','Std.dev','Min','Q1','Median','Q3','Max')
  colnames(sumrytotal)=SectionName
  
  sumrytotalpct=rbind(totalpctmean,totalpctstd,totalpctmin,totalpctq1,totalpctmedian,totalpctq3,totalpctmax)
  rownames(sumrytotalpct)=c('Mean','Std.dev','Min','Q1','Median','Q3','Max')
  colnames(sumrytotalpct)=SectionName
  
  sumryQset=sapply(Qsscores,colMeans,na.rm=TRUE)
  sumryQsetPct=sapply(QsetCrtPct,colMeans,na.rm=TRUE)
  colnames(sumryQset)=colnames(sumrytotal)
  colnames(sumryQsetPct)=colnames(sumrytotal)
  
  sumryCset=sapply(Csscores,colMeans,na.rm=TRUE)
  sumryCsetPct=sapply(CsetCrtPct,colMeans,na.rm=TRUE)
  colnames(sumryCset)=colnames(sumrytotal)
  colnames(sumryCsetPct)=colnames(sumrytotal)
  
  score=as.data.frame(matrix(NA,ncol=3+nrow(sumryCset),nrow=sum(rows)))
  colnames(score)=c('Section','Score','student',rownames(sumryCset))
  score$Section=rep(colnames(sumrytotal),times=rows)
  for (i in 1:length(dat)){
    score[(cumsum(rows)-rows+1)[i]:cumsum(rows)[i],-1]=data.frame(scores[[i]]$HWsheet[,1],rownames(scores[[i]]$HWsheet),Csscores[[i]],stringsAsFactors=FALSE)
  }
  
  outcome=list(sumrytotal=round(sumrytotal,2),
               sumrytotalpct=round(sumrytotalpct*100,2),
               QsetAvgScore=round(sumryQset,2),
               QsetAvgPct=round(sumryQsetPct*100,2),
               CsetAvgScore=round(sumryCset,2),
               CsetAvgPct=round(sumryCsetPct*100,2),
               score=score,
               QsetCrtPct=QsetCrtPct,
               QPct=meltPct,
               Qscore=Qscores[,-1],
               Qanswer=Qanswers[,-1])
  return(outcome)
}

mergeSection2 = function(filenmlist, answerkey, skip=NULL){
  dat=score=set=list()
  Qscore=QSscore=CSscore=NULL
  Qscorelist=QSscorelist=CSscorelist=list()
  QSpctlist=CSpctlist=list()
  
  for (i in 1:length(filenmlist)){
    dat[[i]]=readScore(filenmlist[i])    
    score[[i]]=cleanScore(dat[[i]],answerkey,skip)
    set[[i]]=cleanSet(score[[i]]$HWsheet, answerkey)
    
    Qscore = rbind(Qscores, score[[i]]$HWsheet)
    QSscore = rbind(QSscore, set[[i]]$QuestionSet$QuestionSetScore)
    CSscore = rbind(CSscore, set[[i]]$ObjectiveSet$ConceptSetScore)
    
    Qscorelist[[i]] = score[[i]]$HWsheet
    QSscorelist[[i]] = set[[i]]$QuestionSet$QuestionSetScore
    CSscorelist[[i]] = set[[i]]$ObjectiveSet$ConceptSetScore
    
    QSpctlist[[i]] = data.frame(t(apply(QSscorelist[[i]],1,
                                        function(x){x/set[[i]]$QuestionSet$QSFullScore})))
    CSpctlist[[i]] = data.frame(t(apply(CSscorelist[[i]],1,
                                        function(x){x/set[[i]]$ObjectiveSet$CSFullScore})))
  }
  
  return(list(Qscore=Qscore,
              QSscore=QSscore,
              CSscore=CSscore,
              Qscorelist=Qscorelist,
              QSscorelist=QSscorelist,
              CSscorelist=CSscorelist,
              QSpctlist=QSpctlist,
              CSpctlist=CSpctlist))
}
