#' Convert a txt answer key to a data frame
#'
#' Answer keys downloaded from Respondus are plain txt files.
#' This function will convert it into data frames.
#'
#' @param filenm file name of the answer key with path.
#' @return a list of two data frames. The rows of the first one
#' are the answers for each question, like a,b,c,d. The rows of
#' the second data frame are the questions.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#'
convertkey = function(filenm){
  key = readLines(filenm)
  key = gsub("\xa0","",key)
  key = gsub("\x93","",key)
  key = gsub("\x94","",key)
  key = gsub("^ +","",key)
  key = gsub(" +$","",key)
  key = key[key!=""]
  question = key[grep("^([0-9]+\\. )*((T)|(Ch))[0-9]",key)->name]
  question = gsub("^[0-9]+\\. ","",question)
  question = gsub("( )*\\([1-9]+\\.[0-9] points*\\)","",question)

  keyframe = data.frame(t(data.frame(lapply(sapply(question,strsplit,split="\\."),function(x)x[1:7]))),stringsAsFactors=FALSE)
  colnames(keyframe) = c('Topic','Objective.Set','Question.Set','X..in.Set','QFullPt','Type','ID.in.Set')
  keyframe$Question = rownames(keyframe)
  tmp = data.frame(t(data.frame(sapply(keyframe[,4],strsplit,split="-"))),stringsAsFactors=FALSE)
  keyframe[,4] = as.integer(tmp$X2)
  keyframe$FromQues = as.integer(tmp$X1)
  keyframe$QFullPt = as.integer(keyframe$QFullPt)
  keyframe$QSFullPt = keyframe$QFullPt * keyframe$X..in.Set
  tmp = paste0(keyframe$Topic,keyframe$Objective.Set)
  for (i in 1:nrow(keyframe)){
    idx3 = which(tmp == tmp[i])
    idx31 = which(!duplicated(keyframe$Question.Set[idx3]))
    keyframe$CSFullPt[i] = sum(keyframe$QSFullPt[idx3][idx31])
  }
  keyframe=keyframe[,c(8,1:3,7:4,9:11)]

  keylist = list()
  name = c(name,length(key)+1)
  answer = data.frame(keyframe[1,],Answer='NULL', Points=1)
  answer = answer[0,]
  for (i in 1:length(question)){
    keylist[[i]]=list()
    keylist[[i]][[1]] = key[(name[i]+1):(name[i+1]-1)]
    keylist[[i]][[1]] = gsub("^\\\t","",keylist[[i]][[1]])
    keylist[[i]][[1]] = gsub("\\$","\\\\$",keylist[[i]][[1]])
    keylist[[i]][[1]] = gsub("\\_","\\\\_",keylist[[i]][[1]])
    keylist[[i]][[1]] = gsub("\\&","\\\\&",keylist[[i]][[1]])
    keylist[[i]][[1]] = gsub("%","\\%",keylist[[i]][[1]])
    test1 = grep("^\\*[a-z]\\.",keylist[[i]][[1]]) # MC correct answer
    test2 = grep("^[a-z]\\.",keylist[[i]][[1]]) # MC incorrect answer # MA choice
    test3 = grep("^\\[[a-z]\\]",keylist[[i]][[1]]) # MA answer
    test6 = which(keylist[[i]][[1]]=="Correct Answer:") #Calculated
    test7 = which(keylist[[i]][[1]] %in% c("Correct Answer(s):","Answers:")) #Short Answer #Fill in the Blank #Calculated
    test4 = setdiff(1:length(keylist[[i]][[1]]),c(test1,test2,test3,test6,test7))
    test5 = if(length(test4)){-test4}else{1:length(keylist[[i]][[1]])}
    test9 = grep("^ *<img> ",keylist[[i]][[1]])

    if (length(test9)){
      img = gsub("^ *<img> ","",keylist[[i]][[1]][test9])
      img = gsub("\\\\_","_",img)
      fname=paste0(dirname(filenm),"/",img)
      keylist[[i]][[1]][test9] = sprintf('\\begin{marginfigure}\\includegraphics[width=0.98\\linewidth]{%s}\\end{marginfigure}',sub('\\..{3}$','',fname))
      attr(keylist[[i]][[1]],"img")=test9
    }

    if (keyframe$Type[i] %in% c("MC","TF","MU","JS")) {
      if (length(test1)) {
        keylist[[i]][[2]]=gsub("^\\*[a-z]\\. *","",keylist[[i]][[1]][test1])
        keylist[[i]][[3]]=gsub("^(\\*)*[a-z]\\. *","",keylist[[i]][[1]][test5])
        keylist[[i]][[4]]=rep(0,length(keylist[[i]][[3]]))
        keylist[[i]][[4]][keylist[[i]][[3]] %in% keylist[[i]][[2]]]=100/length(keylist[[i]][[2]])
        keylist[[i]][[5]]=substr(keylist[[i]][[1]][test5],1,2)
        keylist[[i]][[5]]=gsub("\\*","",gsub('\\.','',keylist[[i]][[5]]))
      } else if (length(keylist[[i]][[1]])-length(test4)==1) {
        keylist[[i]][[2]]=gsub("^[a-z]\\. *","",keylist[[i]][[1]][test5])
        keylist[[i]][[3]]=keylist[[i]][[2]]
        keylist[[i]][[4]]=100
        keylist[[i]][[5]]=substr(keylist[[i]][[1]][test5],1,1)
      } else {
        message(paste("Can not specify the",keyframe$Type[i],"question",i))
      }
    }
    if (keyframe$Type[i]=="MA") {
      if (length(test2) & length(test3)) {
        keylist[[i]][[2]]=keylist[[i]][[3]]=gsub("^[a-z]\\. *","",keylist[[i]][[1]][test2])
        tmp3 = length(keylist[[i]][[3]])
        keylist[[i]][[4]]=rep(100/tmp3,tmp3)
        keylist[[i]][[5]]=substr(keylist[[i]][[1]][test2],1,1)
      } else {
        message(paste("Can not specify the matching question",i))
      }
    }
    if (keyframe$Type[i] %in% c("FB","Short Answer")) {
      tmp9 = keylist[[i]][[1]][(test7+1):length(keylist[[i]][[1]])]
      keylist[[i]][[5]]=substr(tmp9,1,1)
      tmp10 = gsub("^[a-z]\\. *","",tmp9)
      keylist[[i]][[5]][tmp10==tmp9]=NA
      keylist[[i]][[2]]=keylist[[i]][[3]]=tmp10
      keylist[[i]][[4]]=rep(100,length(tmp10))
    }
    if (keyframe$Type[i]=="CA") {
      if (length(test6)==0 & length(test7)>0) test6=test7
      tmp9 = keylist[[i]][[1]][(test6+1):length(keylist[[i]][[1]])]
      keylist[[i]][[2]]=keylist[[i]][[3]]=tmp9
      keylist[[i]][[4]]=rep(100,length(tmp9))
      keylist[[i]][[5]]=rep(NA,length(tmp9))
    }
    tmplength = length(keylist[[i]][[3]])
    tmpanswer = data.frame(t(matrix(unlist(rep(keyframe[i,],tmplength)),ncol=tmplength)),
                           Answer=keylist[[i]][[3]],
                           ShortAnswer=keylist[[i]][[5]],
                           Points=keylist[[i]][[4]]*keyframe$QFullPt[i]/100,
                           stringsAsFactors=FALSE)
    answer = rbind(answer,tmpanswer)
  }
  colnames(answer)[1:11]=colnames(keyframe)
  for (i in 7:11) answer[,i] = as.integer(answer[,i])
  answer = answer[,c(1:6,12:14,7:11)]

  questionlist = lapply(keylist,`[[`,1)
  names(questionlist) = question

  return(list(answerkey=answer,simplekey=keyframe,question=questionlist))
}


#' Parse the html file to get the answer key.
#'
#' @param key path of the .htm file
#' @return the cleaned answer key in plain text format.
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom XML htmlParse
#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
#' @importFrom XML xmlChildren
#' @importFrom XML xmlGetAttr
#' @export
#'
refine_key = function(key){
  doc.html = htmlParse(key,useInternalNodes=TRUE,encoding='UTF-8')
  doc.text = xpathSApply(doc.html, "//p//span", xmlValue)
  doc.img = xpathSApply(doc.html, "//p//span", xmlChildren)
  img.exist = sapply(doc.img,function(a){"img" %in% names(a)})
  for (i in which(img.exist)){
    doc.text[[i]] = paste(doc.text[[i]],"\r\n <img>",xmlGetAttr(doc.img[[i]]$img,"src"))
  }
  doc.text = gsub("\xc2\xa0","",doc.text)
  doc.text = gsub("\xa0","",doc.text)
  correctAnswers = grep("Correct Answer\\(s\\):",doc.text)
  if (length(correctAnswers)) doc.text[correctAnswers] = gsub("\r\n"," \r\n",doc.text[correctAnswers])
  followtext = "Use the (following scenario to answer|scenario below for questions)"
  followingScenario = grep(followtext,doc.text)
  if (length(followingScenario)) doc.text[followingScenario] = gsub("Use the"," \r\nUse the",doc.text[followingScenario])
  doc.text = gsub("([^{)| })])\r\n","\\1 ",doc.text)
  doc.text = paste(doc.text,collapse="\r\n")
  write.table(doc.text,file=gsub("htm$","txt",key),quote=FALSE,row.names=FALSE,col.names=FALSE)
}
