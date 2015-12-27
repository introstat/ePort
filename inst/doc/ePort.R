### R code from vignette source 'ePort.Rnw'

###################################################
### code chunk number 1: ePort.Rnw:36-41
###################################################
library(knitr)
opts_chunk$set(
concordance=TRUE
)
options(width=40)


###################################################
### code chunk number 2: R options
###################################################
options(width = 60)
options(SweaveHooks = list(fig = function() par(mar=c(3,3,1,0.5),mgp = c(2,1,0))))


###################################################
### code chunk number 3: ePort.Rnw:66-68
###################################################
rm(list=ls())
library(knitr)


###################################################
### code chunk number 4: ePort.Rnw:91-92 (eval = FALSE)
###################################################
## install.packages("ePort")


###################################################
### code chunk number 5: ePort.Rnw:98-99
###################################################
library(ePort)


###################################################
### code chunk number 6: ePort.Rnw:106-107 (eval = FALSE)
###################################################
## help(package="ePort")


###################################################
### code chunk number 7: ePort.Rnw:112-113 (eval = FALSE)
###################################################
## help(mergeSection)


###################################################
### code chunk number 8: ePort.Rnw:264-265 (eval = FALSE)
###################################################
## system.file("inst/extdata/", package="ePort")


###################################################
### code chunk number 9: ePort.Rnw:299-314 (eval = FALSE)
###################################################
## key_htm = system.file("inst/extdata/KeyFiles/Topic06.Questions.htm", package="ePort")
## 
## refineKey(key_htm)
## 
## keyPath = gsub("htm$","txt",key_htm)
## 
## dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort")
## 
## rewriteData(dataPath)
## 
## loPath = system.file("inst/extdata/LOFiles/Topic06.Outcomes.txt", package="ePort")
## 
## outPath = system.file("inst/extdata/OutputFiles", package="ePort")
## 
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath)


###################################################
### code chunk number 10: ePort.Rnw:319-320 (eval = FALSE)
###################################################
## key_htm = system.file("inst/extdata/KeyFiles/Topic06.Questions.htm", package="ePort")


###################################################
### code chunk number 11: ePort.Rnw:325-326 (eval = FALSE)
###################################################
## refineKey(key_htm)


###################################################
### code chunk number 12: ePort.Rnw:333-334 (eval = FALSE)
###################################################
## keyPath = gsub("htm$","txt",key_htm)


###################################################
### code chunk number 13: ePort.Rnw:339-340 (eval = FALSE)
###################################################
## dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort")


###################################################
### code chunk number 14: ePort.Rnw:345-346 (eval = FALSE)
###################################################
## rewriteData(dataPath)


###################################################
### code chunk number 15: ePort.Rnw:351-352 (eval = FALSE)
###################################################
## loPath = system.file("inst/extdata/LOFiles/Topic06.Outcomes.txt", package="ePort")


###################################################
### code chunk number 16: ePort.Rnw:357-358 (eval = FALSE)
###################################################
## outPath = system.file("inst/extdata/OutputFiles", package="ePort")


###################################################
### code chunk number 17: ePort.Rnw:363-364 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath)


###################################################
### code chunk number 18: ePort.Rnw:385-387 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath,
## reportType = "secTopicShort")


###################################################
### code chunk number 19: ePort.Rnw:401-402 (eval = FALSE)
###################################################
## makeReport(keyFile = keyPath, dataFile = dataPath, loFile = loPath, outFile = outPath, reportType = "secTopicLong")


###################################################
### code chunk number 20: ePort.Rnw:411-414 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic06", package = "ePort")
## dataList = list.files(path = dataFolder, full.names = TRUE)[1:2]
## makeReport(keyFile = keyPath, dataFile = dataList, loFile = loPath, outFile = outPath, reportType = "crossSecTopicShort")


###################################################
### code chunk number 21: ePort.Rnw:421-422 (eval = FALSE)
###################################################
## makeReport(keyFile = keyPath, dataFile = dataList, loFile = loPath, outFile = outPath, reportType = "crossSecTopicLong")


###################################################
### code chunk number 22: ePort.Rnw:427-439 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic03_06", package="ePort")
## dataList = list.files(path = dataFolder, full.names = TRUE)
## for (file in dataList){
##   rewriteData(file)
## }
## dataTable = setDir(dataFolder)
## mergedData = mergeData(dataTable)
## # Add this to makeReport if we are using this file!!!!!!!!!!!!!!!!!!!
## for (sctn in unique(dataTable$section)) {
##   merged = subsetData(mergedData, dataTable, choice = sctn)
##   knit("/Users/lindz/ePort/inst/Rnw/hw-topic.Rnw", output = paste0('Stat101hwk_Unit1_Section',sctn, '.tex'))
## }


###################################################
### code chunk number 23: ePort.Rnw:444-446 (eval = FALSE)
###################################################
## merged = subsetData(mergedData, dataTable)
## knit("/Users/lindz/ePort/inst/Rnw/hw-topic-section.Rnw", output = 'Stat101hwk_Unit1_allSections.tex')


###################################################
### code chunk number 24: ePort.Rnw:460-466 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic03_Split", package="ePort")
## dataList = list.files(path=dataFolder,full.names=TRUE)[1:2]
## for(file in dataList){
##   rewriteData(file)
##   splitFile(file, 9, "ID")
## }


###################################################
### code chunk number 25: ePort.Rnw:473-480 (eval = FALSE)
###################################################
## dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Data Files/Ch9-Ch11"
## namelist = list.files(path=dataPath,full.names=TRUE)
## for (i in namelist) rewriteData(i) # May not need this rewriting step!
## for(i in c('AB','CD','EF','GH','JK','LM')){
##   tmp = namelist[grep(i,basename(namelist))]
##   combineFiles(tmp[2],tmp[1],paste("Topic11",i,"csv",sep='.'))
## }


###################################################
### code chunk number 26: ePort.Rnw:494-497 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic03_Deidentified", package="ePort")
## getNameList(dataFolder, section=NULL, semester=NULL, secblind=TRUE, save=TRUE)
## encodeName(dataFolder, dict=paste(dataFolder, "nameCode.csv", sep='/'))


###################################################
### code chunk number 27: ePort.Rnw:507-509 (eval = FALSE)
###################################################
## dataListPath = c(system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort"),
## system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package="ePort"))


###################################################
### code chunk number 28: ePort.Rnw:514-518 (eval = FALSE)
###################################################
## for (i in dataListPath){
##   rewriteData(i)
##   makeReport(keyFile=keyPath, dataFile=i, loFile=loPath, outFile=outPath, reportType="secTopicShort")
## }


###################################################
### code chunk number 29: ePort.Rnw:523-533 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic06", package="ePort")
## #namelist = list.files(path=dataFolder, pattern = "^[^.]*\.[^.]*\.[^.]*$", full.names=FALSE)
## #http://stackoverflow.com/questions/9949176/match-string-with-exactly-2-of-a-given-character-e-g-2-literal-periods
## 
## #dataListPath = c(system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort"), system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package="ePort"))
## #topic = gsub('.Questions.txt','',gsub('Topic','',basename(key)))
## #namelist = list.files(path=dataPath,full.names=TRUE)
## #namelist = namelist[grep(paste('Topic',topic,'\\.',sep=''),basename(namelist))]
## #namelist
## 

