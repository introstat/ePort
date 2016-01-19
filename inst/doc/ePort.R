## ----include=FALSE,echo=FALSE---------
library(knitr)
opts_chunk$set(
concordance=TRUE
)
options(width=40)

## ----echo=FALSE-------------------------------------------
options(width = 60)
options(SweaveHooks = list(fig = function() par(mar=c(3,3,1,0.5),mgp = c(2,1,0))))

## ----echo=FALSE-------------------------------------------
rm(list=ls())
library(knitr)

## ----eval=FALSE-------------------------------------------
#  install.packages("ePort")

## ----eval=FALSE-------------------------------------------
#  library(ePort)

## ----eval=FALSE-------------------------------------------
#  help(package="ePort")

## ----eval=FALSE-------------------------------------------
#  help(mergeSection)

## ----eval=FALSE-------------------------------------------
#  system.file("inst/extdata/", package = "ePort")

## ----eval=FALSE-------------------------------------------
#  keyHTM = system.file("inst/extdata/KeyFiles/Topic06.Questions.htm", package =
#  "ePort")
#  
#  refineKey(keyHTM)
#  
#  keyPath = gsub("htm$", "txt", keyHTM)
#  
#  dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package =
#  "ePort")
#  
#  rewriteData(dataPath)
#  
#  loPath = system.file("inst/extdata/LOFiles/Topic06.Outcomes.txt", package = "ePort")
#  
#  outPath = system.file("inst/extdata/OutputFiles", package = "ePort")
#  
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath)

## ----eval=FALSE-------------------------------------------
#  keyHTM = system.file("inst/extdata/KeyFiles/Topic06.Questions.htm", package =
#  "ePort")

## ----eval=FALSE-------------------------------------------
#  refineKey(keyHTM)

## ----eval=FALSE-------------------------------------------
#  keyPath = gsub("htm$", "txt", keyHTM)

## ----eval=FALSE-------------------------------------------
#  dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package =
#  "ePort")

## ----eval=FALSE-------------------------------------------
#  rewriteData(dataPath)

## ----eval=FALSE-------------------------------------------
#  loPath = system.file("inst/extdata/LOFiles/Topic06.Outcomes.txt", package = "ePort")

## ----eval=FALSE-------------------------------------------
#  outPath = system.file("inst/extdata/OutputFiles", package = "ePort")

## ----eval=FALSE-------------------------------------------
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath)

## ----eval=FALSE-------------------------------------------
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath, reportType = "secTopicShort")

## ----eval=FALSE-------------------------------------------
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath, reportType = "secTopicLong")

## ----eval=FALSE-------------------------------------------
#  dataFolder = system.file("inst/extdata/DataFiles/Topic06", package = "ePort")
#  dataList = list.files(path = dataFolder, full.names = TRUE)[1:2]
#  makeReport(keyFile = keyPath, dataFile = dataList, loFile = loPath, outFile =
#  outPath, reportType = "crossSecTopicShort")

## ----eval=FALSE-------------------------------------------
#  makeReport(keyFile = keyPath, dataFile = dataList, loFile = loPath, outFile =
#  outPath, reportType = "crossSecTopicLong")

## ----eval=FALSE-------------------------------------------
#  dataFolder = system.file("inst/extdata/DataFiles/Topic03_06", package="ePort")
#  dataList = list.files(path = dataFolder, full.names = TRUE)
#  unit = 1
#  
#  for (file in dataList){
#    rewriteData(file)
#  }
#  
#  dataTable = setDir(dataFolder)
#  mergedData = mergeData(dataTable)
#  
#  for (section in unique(dataTable$section)) {
#    merged = subsetData(mergedData, dataTable, choice = section)
#    makeReport(outFile = outPath, unit = 1, section = section, reportType = "secUnit")
#  }

## ----eval=FALSE-------------------------------------------
#  merged = subsetData(mergedData, dataTable)
#  makeReport(outFile = outPath, unit = 1, reportType = "crossSecUnit")

## ----eval=FALSE-------------------------------------------
#  dataFolder = system.file("inst/extdata/DataFiles/Topic03_Split", package="ePort")
#  dataList = list.files(path = dataFolder,full.names = TRUE)[1:2]
#  for(file in dataList){
#    rewriteData(file)
#    splitFile(file, 9, "ID")
#  }

## ----eval=FALSE-------------------------------------------
#  dataFolder = system.file("inst/extdata/DataFiles/Topic03_Merge", package="ePort")
#  dataList = list.files(path = dataFolder, full.names=TRUE)[1:4]
#  topicNum = "Topic11"
#  
#  for (i in dataList) rewriteData(i)
#  for(i in c('AB', 'CD')){
#    tmp = dataList[grep(i, basename(dataList))]
#    combineFiles(tmp[2], tmp[1], paste(dirname(tmp[1]), "/", paste(topicNum, i, "csv",
#    sep = '.'), sep = ""))
#  }

## ----eval=FALSE-------------------------------------------
#  dataFolder = system.file("inst/extdata/DataFiles/Topic03_Deidentified", package =
#  "ePort")
#  getNameList(dataFolder, section = NULL, semester = NULL, secblind = TRUE, save =
#  TRUE)
#  encodeName(dataFolder, dict = paste(dataFolder, "nameCode.csv", sep = '/'))

## ----eval=FALSE-------------------------------------------
#  dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package =
#  "ePort")
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath, reportType = "secTopicShort")
#  
#  dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package =
#  "ePort")
#  makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile =
#  outPath, reportType = "secTopicShort")

## ----eval=FALSE-------------------------------------------
#  dataListPath = c(system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package
#  = "ePort"), system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package =
#  "ePort"))

## ----eval=FALSE-------------------------------------------
#  for (i in dataListPath){
#    rewriteData(i)
#    makeReport(keyFile = keyPath, dataFile = i, loFile = loPath, outFile = outPath,
#    reportType = "secTopicShort")
#  }

