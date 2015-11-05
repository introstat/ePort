#' Get a full list of the students' names
#' 
#' Read the students' names from the data files, and write
#' a dictionary for the names with de-identified code.
#' 
#' @param dpath path to the data files
#' @param section the digits in the file names that indicates
#' the section. For example, "T01.201.csv" gives the section
#' digits from 5 to 7. Default to be NULL.
#' @param semester appendix to the code, e.g. "2014FALL". Default to be NULL.
#' @param secblind if the section information should be de-identified or not.
#' @param save if the dictionary from the students' names
#' to the code should be saved or not
#' @return a data frame of two columns: students' names
#' and the corresponding code
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' @example inst/ex-deidentify.R
#' 
getNameList = function(dpath, section=NULL, semester=NULL, secblind=TRUE, save=TRUE){
  filenames = list.files(dpath,full.names=TRUE)
  filenames = filenames[grep('.csv$',filenames)]
  if (is.null(section)) {
    tmpsec = t(data.frame(gregexpr("\\.",basename(filenames))))
    tmpsec[,1] = tmpsec[,1]+1
    tmpsec[,2] = tmpsec[,2]-1
  }
  nameList = data.frame()
  for (i in 1:length(filenames)) {
    tmp = read.csv(filenames[i],colClasses=c("character",rep("NULL",8)))[,1,drop=FALSE]
    tmpsection = if (is.null(section)) tmpsec[i,] else section
    tmp$section = substr(basename(filenames[i]),tmpsection[1],max(tmpsection))
    nameList = rbind(nameList,tmp)
  }
  nameList = nameList[!duplicated(nameList[,1],fromLast=TRUE),]
  section = if (secblind) {'ID'} else {nameList$section}
  nameList$Code = sprintf("%s%s%04d",section,semester,sample(1:nrow(nameList)))
  nameList = nameList[,-2]
  if (save) write.csv(nameList,file=paste0(dpath,'/nameCode.csv'),row.names=FALSE)
  return(nameList)
}


#' Encode all student names in the data files
#' 
#' Read the name dictionary, and rewrite the data files
#' into the de-identified names
#' 
#' @param dpath Path to the data files
#' @param dict File name with path for the dictionary. Default is "nameCode.csv"
#' @return NULL
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
encodeName = function(dpath, dict="nameCode.csv"){
  dictnry = read.csv(dict,colClasses=rep('character',2))
  rownames(dictnry) = dictnry$Username
  
  filenames = list.files(dpath,full.names=TRUE)
  filenames = filenames[grep('.csv$',filenames)]
  if (length(k <- which(basename(filenames)==basename(dict)))) filenames = filenames[-k]
  for (i in 1:length(filenames)) {
    tmp = read.csv(filenames[i],colClasses=c("character",rep(NA,8)),check.names=FALSE)
    tmp$Username = dictnry[tmp$Username,2]
    tmp[,2] = NA
    tmp[,3] = NA
    write.csv(tmp,filenames[i],row.names=FALSE)
  }
}
