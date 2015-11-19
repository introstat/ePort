#' Generate a report for one homework
#'
#' A tex file will be generated from the specified data files,
#' answer key, and learning outcomes.
#'
#' @param keyFile file name with path for the answer key
#' @param dataFile the file name with path for the data
#' @param loFile the file name with path for the learning outcomes.
#' @param reportType the type of report
#' @param keepFiles keep files (.aux, .log, etc) used to generate .tex file, default is FALSE
#' @param keepTex keep .tex file, default is FALSE
#' @param keepImage keep individual image files used to generate .tex file, default is FALSE
#' @param topic the topic number, could be integer or character
#' @param section the section name, usually one of 'AB', 'CD', 'GHQ', '201', ...
#' @param path the directory to the data files
#' @param type the strings before the topic number in the csv file name
#' @param rewrite logical. If TRUE then the csv files are rewritten.
#' @param skip a vector of integers. Same as the parameter in \code{cleanScore}.
#' @param outFile the directory to save the tex file
#' @return a tex file to be compiled
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom knitr knit
#' @importFrom stringr str_trim
#' @export
#' @example inst/ex-reportHwk.R
#'
makeReport = function(keyFile=NULL, dataFile=NULL, loFile=NULL, reportType = NULL, keepFiles=FALSE, keepTex=FALSE, keepImage=FALSE, topic=NULL, section=NULL, path=NULL, type=NULL, rewrite=FALSE, skip=NULL, outFile=NULL){  
    
    if (is.null(reportType)){
      reportMenu <- menu(choices=c("One topic for one section - short version (\"secTopicShort\")", "One topic for one section - long version (\"secTopicLong\")", "One topic comparing multiple sections - short version (\"crossSecTopicShort\")", "One topic comparing multiple sections - long version (\"crossSecTopicLong\")", "One unit (group of topics) for one section (\"secUnit\")", "One unit (group of topics) comparing multiple sections (\"crossSecUnit\")"), title=paste("\nPlease enter integer (1-6) corresponding to desired report type below.", "\n\n", trimws("Note: If running many reports, it is more efficient to exit now and hard-code the reportType parameter. See help(makeReport).", which="both"), sep=""))
      trans <- setNames(c("secTopicShort", "secTopicLong", "crossSecTopicShort", "crossSecTopicLong", "secUnit", "crossSecUnit"),c(1,2,3,4,5,6))
      reportType <- trans[as.character(reportMenu)]
    }
    
    try(if(is.null(outFile)) stop("Need to define outFile, see help(makeReport)"))
    if (reportType=="secTopicShort"){
      reportType = system.file("inst/Rnw/hw-individual-short.Rnw", package="ePort")
    }
    if (reportType=="secTopicLong"){
      reportType = system.file("inst/Rnw/hw-individual.Rnw", package="ePort")
    }
    
    # Find the file name
    if (is.null(dataFile)) {
      Score_filename = paste(path,type,topic,'.',section,'.csv',sep='')
    } else {
      Score_filename = dataFile
      path = dirname(dataFile)[1]
      type = basename(dataFile)[1]
      number1 = as.vector(gregexpr("[0-9]",type)[[1]])
      topic = substr(type, number1[1], number1[min(2,length(number1))])
      dot1 = as.vector(gregexpr("\\.",type)[[1]])
      section = substr(type, dot1[1]+1, dot1[2]-1)
      type = substr(type, 1, number1[1]-1)
    }
    
    # Read the answer key
    answerkey = convertKey(keyFile)
    if (type=="Unit") {
      answerkey[[1]]$Objective.Set =
        paste0(answerkey[[1]]$Topic,answerkey[[1]]$Objective.Set)
      answerkey[[2]]$Objective.Set =
        paste0(answerkey[[2]]$Topic,answerkey[[2]]$Objective.Set)
    }
    
    # Learning objective file
    chpt_outcome_file = loFile
    chapter=as.character(read.delim(chpt_outcome_file[1],header=FALSE)[,1])
    chapter_outcomes=chapter[grep('^[A-Z]\\. ',chapter)]
    
    #for (i in 1:length(chapter_outcomes)){
      #chapter_outcomes[i] =
        #paste(str_trim(unlist(strsplit(chapter_outcomes[i],
        #                              "[.]"))[2]),".",sep="")
        #paste(str_trim(unlist(strsplit(chapter_outcomes[i], "[.]"))),".",sep="")
    #}
    
    # Cross-section report
    if (length(Score_filename)>1){
      instructor_scores = mergeSection(Score_filename, answerkey,skip=NULL)
      knit(reportType,paste0(outFile,"/Stat101hwk_",type,topic,"_",gsub("Rnw$","tex",gsub("hw-","",basename(reportType)))))
      return()
    }

    if (rewrite) rewriteData(Score_filename)
    
    # Read the data
    Student_Score = readScore(Score_filename)
    tmpoutput = cleanScore(Student_Score, answerkey, skip=skip)
    #skip=1 for Topic 1 Fall 2013
    ScorebyQuestion = tmpoutput$HWsheet
    CountbyQuestion = tmpoutput$nResponse
    Student_SetScore = cleanSet(ScorebyQuestion, answerkey)
    
    # Summary
    sumry1 = summaryScore(ScorebyQuestion,
                           Student_SetScore$QuestionSet, Student_SetScore$ObjectiveSet)
    sumry2 = summaryLevel(tmpoutput, Student_SetScore$QuestionSet,
                           Student_SetScore$ObjectiveSet)
    stopifnot(all(sumry2$ByQuestion[,1]<=100),all(sumry2$SetCorrectPct[5,]<=100),all(sumry2$ConceptCorrectPct[5,]<=100))
    
    # Produce a tex report by kniting an .Rnw file
    if (keepFiles){
      knit2pdf(reportType,paste0(outFile,"/Stat101hwk_",type,topic,"_",section,gsub("Rnw$","tex",gsub("hw-individual","",basename(reportType)))))
    }else{
      knit2pdf(reportType,paste0(outFile,"/Stat101hwk_",type,topic,"_",section,gsub("Rnw$","tex",gsub("hw-individual","",basename(reportType)))),clean=T)
    }
    if (!keepTex){
      on.exit(unlink(paste0(outFile,"/Stat101hwk_",type,topic,"_",section,gsub("Rnw$","tex",gsub("hw-individual","",basename(reportType)))))) 
    }
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
wideTableLatex=function(atable, tablecaption, label, cols=7){
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
  print(xtable::xtable(outcome[[1]],caption=tablecaption,label=label),
        floating=FALSE, tabular.environment = "longtable")
  if (ntable > 1) {
    for (i in 2:ntable){
      outcome[[i]]=atable[,startcol[i]:endcol[i]]
      print(xtable::xtable(outcome[[i]]), floating=FALSE,
            tabular.environment = "longtable")
    }
  }
  return(outcome)
}


#' Abbreviate verbose answers
#'
#' Some answer vectors may be too verbose to represent in visual plots. Verbose answers are abbreviated to a string containing the first letter of each word, "not answered" answers remain the same, and "" answers are returned as "not answered"
#'
#' @param avec Original answer vector (in character or factor format)
#' @param l Maximum length (in characters) of an original answer vector that does not need abbreviation
#' @return List with two objects. The first object is the returned abbreviated answer vector. The second object is a glossary key
#' @examples
#' avec = "The answer is more than l characters"
#' shortenAnswers(avec, l=13)
#' @export
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#'
shortenAnswers = function(avec, l=13) {
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
