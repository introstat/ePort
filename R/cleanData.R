#' Extract useful information from Blackboard data files
#' 
#' Organize the data into a table with rows as students and columns
#' as scores or answers.
#' 
#' @param filename file name of the Blackboard data with path
#' @param savefile logical. If TRUE then the cleaned data is saved.
#' @param keepAttempts logical. If FALSE then only the best attempt
#' would be kept for future use.
#' @return a list of four objects: a data frame of scores, 
#' a data frame of answer, the file information, and the clean questions.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#' @export
#' 
readScore = function(filename, savefile=FALSE, keepAttempts=FALSE){
  scores = read.csv(filename, check.names = FALSE, 
                    colClasses=c(rep('character',6),rep('numeric',3)))
  scores[is.na(scores[,8]),8] = 0
  scores$Question_id = as.integer(substr(scores$Question, 9, 11))
  scores$Question = cleanHTML(scores$Question, mark=FALSE, image=FALSE)
  scores$Answer = cleanHTML(scores$Answer,image=TRUE)
  attempt = paste(scores$Username,scores$`Question ID`,sep='-')
  scores$Attempt = 1
  scores$Attempt[duplicated(attempt)] = 2
  if (savefile) write.csv(scores, file=gsub('csv$','clean.csv',filename))
  
  res0 = plyr::ddply(scores,c('Username','Attempt'),summarise,
               Grade = sum(`Possible Points`),
               Score = sum(`Auto Score`))
  res0 = res0[,c(1,3,2,4)]
  res1 = res2 = matrix(NA,ncol=max(scores$Question_id),nrow=nrow(res0))
  rownames(res1) = rownames(res2) = paste(res0$Attempt, res0$Username, sep='-')
  colnames(res1) = colnames(res2) = 1:ncol(res1)
  attempt = paste(scores$Attempt, scores$Username, sep='-')
  for (i in 1:nrow(scores)){
    res1[attempt[i],scores$Question_id[i]] = scores$`Auto Score`[i]
    res2[attempt[i],scores$Question_id[i]] = scores$Answer[i]
  }
  
  r1 = data.frame(res0,res1)
  r2 = data.frame(res0,res2,stringsAsFactors=FALSE)
  fileInfo = unlist(strsplit(basename(filename),"\\."))[1:2]
  
  if (!keepAttempts){
    r0 = r1[,c(1,3,4)]
    r0idx=plyr::ddply(r0,'Username',summarise,idx=rev(Attempt)[which.max(rev(Score))])
    idx = paste(r0idx$idx, r0idx$Username, sep='-')
    r1 = r1[idx,]
    r2 = r2[idx,]
  }
  
  res = list(score=r1, answer=r2, info=fileInfo, question=unique(scores[order(scores$Question_id,decreasing=FALSE),c('Question','Question_id')]))
  return(res)
}


#' Clean the scores to a simple format
#' 
#' Match the answer key question names with the data files.
#' If there are multiple attempts, the attempt with the highest
#' score is kept. If some questions in the data files are not
#' needed for analysis and not provided in the answer key, then
#' we use argument \code{skip} to skip them.
#' 
#' @param HWsheet the output of function \code{readScore}
#' @param key the output of function \code{convertKey}
#' @param skip a vector of integers to solve the problem like HW1 Question1
#' @return a list of four objects: a data frame of scores,
#' a vector of numbers of response for each question,
#' a vector of full points for each question,
#' a data frame of answers.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom plyr ddply
#' @importFrom plyr summarise
#' @export
#' 
cleanScore = function(HWsheet, key, skip=NULL){
  r1 = HWsheet$score
  r2 = HWsheet$answer
  r0 = r1[,c(1,3,4)]
  r0idx=plyr::ddply(r0,'Username',summarise,idx=rev(Attempt)[which.max(rev(Score))])
  idx = paste(r0idx$idx, r0idx$Username, sep='-')
  res1 = r1[idx,-c(1:3,if(length(skip)){skip+4})]
  res2 = r2[idx,-c(1:3,if(length(skip)){skip+4})]
  colnames(res1) = colnames(res2) = c('TotalScore',key$simplekey$Question)
  rownames(res1) = rownames(res2) = r1[idx,'Username']
  
  for (i in 2:ncol(res2)){
    tmpcol = res2[,i]
    tmpcol = gsub("^ +","",tmpcol)
    tmpcol = gsub(" +$","",tmpcol)
    tmpkey = key$answerkey[key$answerkey$Question==colnames(res2)[i],6:8]
    tmpkey = tmpkey[!duplicated(tmpkey$Answer),]
    rownames(tmpkey) = tmpkey$Answer
    if (tmpkey$Type[1] %in% c("MC","TF")) {
      tmpcol[tmpcol %in% tmpkey$Answer] = tmpkey[tmpcol[tmpcol %in% tmpkey$Answer],3]
    }
    if (tmpkey$Type[1] %in% c("FB","MU","JS")) {
      #tmpcol = gsub("-----stat101-matching-question-ending-mark-----",",",tmpcol)
      tmpa = strsplit(tmpcol, "-----stat101-matching-question-ending-mark-----")
      tmpa = lapply(tmpa, function(a) {gsub("^ +","",gsub(" +$","",a))})
      tmpb = lapply(tmpa, function(a) {
        if (any(a %in% tmpkey$Answer)) a[a %in% tmpkey$Answer]=tmpkey[a[a %in% tmpkey$Answer],3]
        return(a)
      })
      tmpcol = sapply(tmpb, paste, collapse=",")
      tmpcol[tmpcol=='NA']=NA
    }
    if (tmpkey$Type[1] %in% c("MA")) {
      tmpa=gregexpr('-----stat101-matching-question-starting-mark-----',tmpcol)
      tmpb=gregexpr('-----stat101-matching-question-ending-mark-----',tmpcol)
      tmpu=gregexpr('Unanswered',tmpcol)
      tmpmq = max(sapply(tmpa, length))
      tmpc=tmpd=tmpe=matrix(NA,nrow=length(tmpa),ncol=tmpmq)
      for (j in 1:length(tmpa)) {
        if (all(is.na(tmpa[[j]]))) next
        if (tmpcol[j]=="Unanswered") {
          tmpe[j,] = "Unanswered"
          next
        }
        if (any(tmpu[[j]]>0)) {
          tmpua = rank(c(tmpu[[j]],tmpa[[j]]))
          tmpe[j,tmpua[1:length(tmpu[[j]])]] = "Unanswered"
          tmpub = tmpua[1:length(tmpa[[j]])+length(tmpu[[j]])]
          tmpuc = as.vector(tmpa[[j]])+49
          tmpud = if (any(tmpb[[j]]>0)) as.vector(tmpb[[j]])-1 else c()
          if (length(tmpuc)>length(tmpud)){
            tmpud = sort(c(tmpud,tmpu[[j]][(tmpua %in% (tmpub+1))[1:length(tmpu[[j]])]]-2))
          }
          if (length(tmpuc)>length(tmpud)){
            tmpud = c(tmpud,nchar(tmpcol[j]))
          }
          tmpue=c()
          for (k in 1:length(tmpa[[j]])) tmpue[k] = substr(tmpcol[j],tmpuc[k],tmpud[k])
          tmpe[j,tmpub] = tmpue
          next
        }
        tmpc[j,] = as.vector(tmpa[[j]])+49
        tmpd[j,] = c(as.vector(tmpb[[j]])-1,nchar(tmpcol[j]))
        for (k in 1:tmpmq) tmpe[j,k] = substr(tmpcol[j],tmpc[j,k],tmpd[j,k])
        tmpe[j,] = gsub(" +$","",gsub("^ +","",tmpe[j,]))
      }
      tmpf = apply(tmpe, 1, function(a) {
        if (any(a %in% tmpkey$Answer)) a[a %in% tmpkey$Answer]=tmpkey[a[a %in% tmpkey$Answer],3]
        return(a)
      })
      tmpcol = apply(tmpf, 2, function(a) {
        if (all(is.na(a))) return(NA)
        if (all(a=='Unanswered')) return('Unanswered')
        paste(a, collapse=',')})
    }
    res2[,i] = tmpcol
  }
  
  nResponse = apply(res1[,-1], 2, function(x) sum(!is.na(x)))
  return(list(HWsheet = res1, nResponse = nResponse, QFullScore = key$simplekey$QFullPt, HWanswer = res2))
}


#' Get the Question Set and Objective Set Scores
#' 
#' @param HWsheet The output of function \code{cleanScore} $HWsheet
#' @param key The output of function \code{convertKey}
#' @return List of two data frames of scores for the question
#' sets and learning outcomes respectively.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
cleanSet = function(HWsheet, key){
  Qset = unique(key$simplekey$Question.Set)
  nQset = length(Qset)
  Oset = unique(key$simplekey$Objective.Set)
  nOset = length(Oset)
  res0 = t(apply(HWsheet,1,function(x){
    q = tapply(unlist(x)[-1],key$simplekey$Question.Set,sum,na.rm=TRUE)
    o = tapply(unlist(x)[-1],key$simplekey$Objective.Set,sum,na.rm=TRUE)
    return(c(q,o))
  }))
  Qscore = data.frame(res0[,1:nQset,drop=FALSE])[,Qset,drop=FALSE]
  Oscore = data.frame(res0[,1:nOset+nQset,drop=FALSE])[,Oset,drop=FALSE]
  QuestionSet = list(QuestionSetScore = Qscore, QSFullScore = key$simplekey$QSFullPt[!duplicated(key$simplekey$Question.Set)], nQuestionSet = nQset)
  ObjectiveSet = list(ConceptSetScore = Oscore, CSFullScore = key$simplekey$CSFullPt[!duplicated(key$simplekey$Objective.Set)], nConceptSet = nOset)
  return(list(QuestionSet = QuestionSet, ObjectiveSet = ObjectiveSet))
}


#' Remove the HTML strings in the answer
#' 
#' @param HWsheet Vector or data frame of characters
#' @param mark Logical. If TRUE then some marks are given to the matching questions
#' @param image Logical. If TRUE then some marks are given to the embedded images
#' @return Vector or data frame with no HTML strings
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom XML getNodeSet
#' @importFrom XML htmlParse
#' @importFrom XML xmlChildren
#' @importFrom XML xmlGetAttr
#' @importFrom XML xmlValue
#' @importFrom XML xpathSApply
#' @export
#' 
cleanHTML = function(HWsheet, mark=TRUE, image=FALSE){
  if (is.vector(HWsheet)){
    HWsheet = gsub(' +$','',gsub('^ +','',HWsheet))
    if (mark) {
      HWsheet = gsub('</p>-<p>','-----stat101-matching-question-starting-mark-----',HWsheet)
      HWsheet = gsub('</p>,<p>','-----stat101-matching-question-ending-mark-----',HWsheet)
    }
    if (image) {
      img.exist = grep("<IMG (.*?) align=[a-z]* border=[0-9]*>", HWsheet)
      if (length(img.exist)) {
        for (i in img.exist){
          HWsheet[i] = gsub("<span [^>]*>","",gsub("</span>","",HWsheet[i]))
          doc.html = XML::getNodeSet(XML::htmlParse(HWsheet[i],useInternalNodes=TRUE,encoding='UTF-8'),'//body')[[1]]
          doc.body = XML::xmlChildren(doc.html)
          doc.body = basename(XML::xmlGetAttr(doc.body$img,"src"))
          doc.img = XML::xpathSApply(doc.html, "//p", XML::xmlChildren)
          if (length(doc.img)) doc.body = c(doc.body,basename(XML::xmlGetAttr(doc.img$img,"src")))
          HWsheet[i] = paste(doc.body, collapse='-----stat101-matching-question-ending-mark-----')
        }
        #HWsheet = gsub('<IMG (.*?) align=[a-z]* border=[0-9]*>',"-----img-start-----\\1-----img-end-----",HWsheet)
      }
    }
    HWsheet = gsub('<Unanswered>','Unanswered',HWsheet)    
    HWsheet = gsub('<[^>]*>','',HWsheet)
    HWsheet = gsub('&gt;','>',HWsheet)
    HWsheet = gsub('&[a-z]+;','',HWsheet)
    #HWsheet = gsub(' *- *','-',HWsheet)
    return(HWsheet)
  }
  n = ncol(HWsheet)
  for (i in 1:n) HWsheet[,i] = cleanHTML(HWsheet[,i])
  return(HWsheet)
}
