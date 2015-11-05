#' Summary on students
#' 
#' Merge the scores at different levels (total, learning
#' objectives, question sets, questions) together.
#' 
#' @param QS a data frame of question scores. First object of
#' the output from function \code{cleanScore}.
#' @param QSS a list. First object of the output from function
#' \code{cleanSet}: QuestionSet.
#' @param CSS a list. Second object of the output from function
#' \code{cleanSet}: ObjectiveSet. Could be NULL.
#' @return a data frame with total scores, scores for question sets,
#' learning objectives (if \code{CSS} is not NULL), and questions.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
summaryScore = function(QS, QSS, CSS=NULL) {
  FullScore=sum(QSS$QSFullScore)
  if (is.null(CSS)){
    tmp = cbind(QS[, 1],rep(FullScore,nrow(QS)), QSS$QuestionSetScore, QS[, 2:ncol(QS)])
  } else{
    tmp = cbind(QS[, 1],rep(FullScore,nrow(QS)), CSS$ConceptSetScore, QSS$QuestionSetScore, QS[, 2:ncol(QS)])
  }
  colnames(tmp)[1:2] = c("TotalScore","FullScore")
  tmp = tmp[order(tmp$TotalScore,decreasing = TRUE),]
  return(tmp)
}


#' Summary on questions
#' 
#' Gives the correct percentage, count, mean, standard deviation,
#' minimum, median, maximum, and the number of NA's
#' by different levels.
#' 
#' @param QS a data frame of question scores. First object of
#' the output from function \code{cleanScore}.
#' @param QSS a list. First object of the output from function
#' \code{cleanSet}: QuestionSet.
#' @param CSS a list. Second object of the output from function
#' \code{cleanSet}: ObjectiveSet. Could be NULL.
#' @return a list of two or three objects (when \code{CSS} is not NULL).
#' The first two objects are the summaries by question,
#' and by question set. The optional third output is the
#' summary by learning objective.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
summaryLevel = function(QS, QSS, CSS=NULL) {
  CorrectPct = round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                           2, mean, na.rm = TRUE)/QS$QFullScore*100, 2)
  sumrymean=round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                        2, mean, na.rm = TRUE),2)
  sumrystd=round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                       2, sd, na.rm = TRUE),2)
  sumrymin=round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                       2, min, na.rm = TRUE),2)
  sumrymedian=round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                          2, median, na.rm = TRUE),2)
  sumrymax=round(apply(QS$HWsheet[, 2:ncol(QS$HWsheet)], 
                       2, max, na.rm = TRUE),2)
  sumryNAs=apply(is.na(QS$HWsheet[, 2:ncol(QS$HWsheet)]),2,sum)
  ByQuestion = cbind(CorrectPct,QS$nResponse,sumrymean,
                     sumrystd,sumrymin,sumrymedian,sumrymax,sumryNAs)
  colnames(ByQuestion) = c("CrtPct","Count","Mean","Std","Min","Median","Max","NA's")
  SCPmean = apply(QSS$QuestionSetScore, 2, mean, na.rm = TRUE)/QSS$QSFullScore*100
  SCPstd = apply(QSS$QuestionSetScore, 2, sd, na.rm = TRUE)/QSS$QSFullScore*100
  SCPmin = apply(QSS$QuestionSetScore, 2, min, na.rm = TRUE)/QSS$QSFullScore*100
  SCPmed = apply(QSS$QuestionSetScore, 2, median, na.rm = TRUE)/QSS$QSFullScore*100
  SCPmax = apply(QSS$QuestionSetScore, 2, max, na.rm = TRUE)/QSS$QSFullScore*100
  SetCorrectPct = round(rbind(SCPmean,SCPstd,SCPmin,SCPmed,SCPmax), 2)
  rownames(SetCorrectPct) = c('Mean','Std.dev','Min','Median','Max')
  output=list(ByQuestion = ByQuestion, SetCorrectPct = SetCorrectPct)
  if (!is.null(CSS)){
    CCPmean = apply(CSS$ConceptSetScore, 2, mean, na.rm = TRUE)/CSS$CSFullScore*100
    CCPstd = apply(CSS$ConceptSetScore, 2, sd, na.rm = TRUE)/CSS$CSFullScore*100
    CCPmin = apply(CSS$ConceptSetScore, 2, min, na.rm = TRUE)/CSS$CSFullScore*100
    CCPmed = apply(CSS$ConceptSetScore, 2, median, na.rm = TRUE)/CSS$CSFullScore*100
    CCPmax = apply(CSS$ConceptSetScore, 2, max, na.rm = TRUE)/CSS$CSFullScore*100
    ConceptCorrectPct = round(rbind(CCPmean,CCPstd,CCPmin,CCPmed,CCPmax), 2)
    rownames(ConceptCorrectPct) = c('Mean','Std.dev','Min','Median','Max')
    output$ConceptCorrectPct=ConceptCorrectPct
  }
  return(output)
}
