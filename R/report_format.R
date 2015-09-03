#' Generate a report for one homework
#' 
#' A tex file will be generated from the specified data files,
#' answer key, and learning outcomes.
#' 
#' @param keyfile the file name with path for the answer key
#' @param datafile the file name with path for the data
#' @param topic the topic number, could be integer or character
#' @param section the section name, usually one of 'AB', 'CD', 'GHQ', '201', ...
#' @param path the directory to the data files
#' @param type the strings before the topic number in the csv file name
#' @param rewrite logical. If TRUE then the csv files are rewritten.
#' @param skip a vector of integers. Same as the parameter in \code{clean_score}.
#' @param LOfile the file name with path for the learning outcomes. Could be
#' NULL, then will be auto-completed by "path/../Topic Outcomes/*.Outcomes.txt".
#' @param knitfile the file name with path for the Rnw file to knit.
#' Could be NULL, then will be auto-completed by the file in the inst folder.
#' @param knito the directory to save the tex file
#' @return a tex file to be compiled
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom knitr knit
#' @importFrom stringr str_trim
#' @export
#' @example inst/ex-reportHwk.R
#' 
report_routine = function(keyfile,datafile=NULL,topic=NULL,section=NULL,path=NULL,type=NULL,rewrite=FALSE,skip=NULL,LOfile=NULL,knitfile=NULL,knito=getwd()){
  
  stopifnot(!is.null(datafile) || all(!is.null(c(topic,section,path,type))))
  
  # Find the file name
  if (is.null(datafile)) {
    Score_filename = paste(path,type,topic,'.',section,'.csv',sep='')
  } else {
    Score_filename = datafile
    path = dirname(datafile)[1]
    type = basename(datafile)[1]
    number1 = as.vector(gregexpr("[0-9]",type)[[1]])
    topic = substr(type, number1[1], number1[min(2,length(number1))])
    dot1 = as.vector(gregexpr("\\.",type)[[1]])
    section = substr(type, dot1[1]+1, dot1[2]-1)
    type = substr(type, 1, number1[1]-1)
  }
  
  # Read the answer key
  answerkey = convertkey(keyfile)
  if (type=="Unit") {
    answerkey[[1]]$Objective.Set = paste0(answerkey[[1]]$Topic,answerkey[[1]]$Objective.Set)
    answerkey[[2]]$Objective.Set = paste0(answerkey[[2]]$Topic,answerkey[[2]]$Objective.Set)
  }
  
  # Learning objective file
  if (is.null(LOfile)) {
    chpt_outcome_file=gsub('Data Files.*$','Topic Outcomes/',Score_filename)
    chpt_outcome_file=paste(chpt_outcome_file,type,topic,'.Outcomes.txt',sep='')
  } else {
    chpt_outcome_file = LOfile
  }
  chapter=as.character(read.delim(chpt_outcome_file[1],header=FALSE)[,1])
  chapter_outcomes=chapter[grep('^[A-Z]\\. ',chapter)]
  
  for (i in 1:length(chapter_outcomes)){
    chapter_outcomes[i] = paste(str_trim(unlist(strsplit(chapter_outcomes[i], "[.]"))[2]),".",sep="")
  }
  
  # Cross-section report
  if (length(Score_filename)>1){
    instructor_scores = merge_section(Score_filename, answerkey,skip=NULL)
    if (is.null(knitfile)) knitfile=system.file("inst","hw-section.Rnw",package="ePort")
    knit(knitfile,paste0(knito,"/Stat101hwk_",type,topic,"_",gsub("Rnw$","tex",gsub("hw-","",basename(knitfile)))))
    return()
  }
  
  ##### WARNING !!! ############################################################
  ##### the original data files would be rewritten! ############################
  if (rewrite) rewrite_data(Score_filename) ####################################
  ##############################################################################
  
  # Read the data 
  Student_Score = read_score(Score_filename)
  tmpoutput = clean_score(Student_Score, answerkey, skip=skip) # skip=1 for Topic 1 Fall 2013
  ScorebyQuestion = tmpoutput$HWsheet
  CountbyQuestion = tmpoutput$nResponse
  Student_SetScore = clean_set(ScorebyQuestion, answerkey)
  
  # Summary
  sumry1 = summary_score(ScorebyQuestion, Student_SetScore$QuestionSet, Student_SetScore$ObjectiveSet)
  sumry2 = summary_level(tmpoutput, Student_SetScore$QuestionSet, Student_SetScore$ObjectiveSet)
  stopifnot(all(sumry2$ByQuestion[,1]<=100),all(sumry2$SetCorrectPct[5,]<=100),all(sumry2$ConceptCorrectPct[5,]<=100))
  # if stopping here, check the points in question titles with the max points given by sumry2$ByQuestion
  
  # Produce a tex report by kniting an rnw file
  if (is.null(knitfile)) knitfile=system.file("inst","hw-individual.Rnw",package="ePort")
  knit(knitfile,paste0(knito,"/Stat101hwk_",type,topic,"_",section,gsub("Rnw$","tex",gsub("hw-individual","",basename(knitfile)))))
}


#' Split wide tables into several tables
#' 
#' When a table contains many columns, the display in pdf file
#' produced by LaTeX will be too wide to see. This function
#' cuts the table into several tables, each of which has no
#' more than \code{cols} columns.
#' 
#' @param atable a data frame to be printed by \code{xtable}.
#' @param tablecaption caption of the tables. Printed only
#' after the first table.
#' @param label label of the tables, only after the first table.
#' @param cols maximum number of columns for each table.
#' @return a list with several tables, each of them has no more
#' than \code{cols} columns.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom xtable xtable
#' @export
#' 
widetableinLaTeX=function(atable, tablecaption, label, cols=7){
  n = ncol(atable)
  lasttablecolumn = n%%cols
  if (lasttablecolumn==0){
    ntable = n %/% cols
  }else{
    ntable = 1 + n %/% cols
  }
  if (lasttablecolumn != 1){
    startcol = 1 + (0:(ntable-1))*cols
    endcol = startcol + cols - 1
    endcol[ntable] = n
  } else {
    ntable = ntable - 1
    startcol = 1 + (0:(ntable-1))*cols
    endcol = startcol + cols - 1
    endcol[ntable] = n
  }
  outcome = list()
  outcome[[1]]=atable[,startcol[1]:endcol[1]]
  print(xtable::xtable(outcome[[1]],caption=tablecaption,label=label), floating=FALSE, tabular.environment = "longtable")
  if (ntable > 1) {
    for (i in 2:ntable){
      outcome[[i]]=atable[,startcol[i]:endcol[i]]
      print(xtable::xtable(outcome[[i]]), floating=FALSE, tabular.environment = "longtable")
    }
  }
  return(outcome)
}


#' Abbreviate the long answers
#' 
#' The long answers are replaced by the first letters of words,
#' except that "not answered" is kept the same, and
#' "" is replaced by "not answered".
#' 
#' @param avec a vector of character or factor
#' @param l maximum length of the 
#' @return a list of two objects: a new vector with abbreviated
#' names, and a glossary key.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
abbr = function(avec, l=12) {
  avecformat = class(avec)
  if (avecformat %in% c('factor','character')){
    avec=as.character(avec)
    avec[avec==""]="not answered"
    veclen= nchar(avec)
    abbrwords=avec
    if (any(veclen>l)) {abbrwords=substr(abbreviate(avec),1,35)}
    abbrwords[is.na(avec)]=NA
    abbrwords[avec=='not answered']='not answered'
    if (avecformat=='factor') abbrwords=as.factor(abbrwords)
    glossary=levels(factor(paste(abbrwords,avec,sep=':---:')))
    glossary=gsub("%","\\\\%",glossary)
    glossary=gsub("\\$","\\\\$",glossary)
    glossary=matrix(unlist(strsplit(glossary,':---:')),byrow=TRUE,ncol=2)
    colnames(glossary)=c('abbr.','original answer')
  }else{glossary=NULL}
  return(list(abbrv=abbrwords,glossary=glossary))
}
