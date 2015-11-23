### R code from vignette source 'ePort.Rnw'

###################################################
### code chunk number 1: ePort.Rnw:32-37
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
### code chunk number 3: ePort.Rnw:62-64
###################################################
rm(list=ls())
library(knitr)


###################################################
### code chunk number 4: ePort.Rnw:89-90 (eval = FALSE)
###################################################
## install.packages("ePort")


###################################################
### code chunk number 5: ePort.Rnw:96-97
###################################################
library(ePort)


###################################################
### code chunk number 6: ePort.Rnw:104-105 (eval = FALSE)
###################################################
## help(package="ePort")


###################################################
### code chunk number 7: ePort.Rnw:110-111 (eval = FALSE)
###################################################
## help(mergeSection)


###################################################
### code chunk number 8: ePort.Rnw:122-123 (eval = FALSE)
###################################################
## system.file("inst/extdata/", package="ePort")


###################################################
### code chunk number 9: ePort.Rnw:157-172 (eval = FALSE)
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
### code chunk number 10: ePort.Rnw:177-178 (eval = FALSE)
###################################################
## key_htm = system.file("inst/extdata/KeyFiles/Topic06.Questions.htm", package="ePort")


###################################################
### code chunk number 11: ePort.Rnw:183-184 (eval = FALSE)
###################################################
## refineKey(key_htm)


###################################################
### code chunk number 12: ePort.Rnw:191-192 (eval = FALSE)
###################################################
## keyPath = gsub("htm$","txt",key_htm)


###################################################
### code chunk number 13: ePort.Rnw:197-198 (eval = FALSE)
###################################################
## dataPath = system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort")


###################################################
### code chunk number 14: ePort.Rnw:203-204 (eval = FALSE)
###################################################
## rewriteData(dataPath)


###################################################
### code chunk number 15: ePort.Rnw:209-210 (eval = FALSE)
###################################################
## loPath = system.file("inst/extdata/LOFiles/Topic06.Outcomes.txt", package="ePort")


###################################################
### code chunk number 16: ePort.Rnw:215-216 (eval = FALSE)
###################################################
## outPath = system.file("inst/extdata/OutputFiles", package="ePort")


###################################################
### code chunk number 17: ePort.Rnw:221-222 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath)


###################################################
### code chunk number 18: ePort.Rnw:243-245 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath,
## reportType = "secTopicShort")


###################################################
### code chunk number 19: ePort.Rnw:259-261 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath, dataFile=dataPath, loFile=loPath, outFile=outPath,
## reportType="secTopicLong")


###################################################
### code chunk number 20: ePort.Rnw:270-273 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic06", package="ePort")
## dataList = list.files(path=dataFolder,full.names=TRUE)[1:2]
## makeReport(keyFile=keyPath,dataFile=dataList,loFile=loPath,outFile=outPath,reportType="crossSecTopicShort")


###################################################
### code chunk number 21: ePort.Rnw:280-281 (eval = FALSE)
###################################################
## makeReport(keyFile=keyPath,dataFile=dataList,loFile=loPath,outFile=outPath,reportType="crossSecTopicLong")


###################################################
### code chunk number 22: ePort.Rnw:291-293 (eval = FALSE)
###################################################
## dataListPath = c(system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort"),
## system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package="ePort"))


###################################################
### code chunk number 23: ePort.Rnw:298-302 (eval = FALSE)
###################################################
## for (i in dataListPath){
##   rewriteData(i)
##   makeReport(keyFile=keyPath, dataFile=i, loFile=loPath, outFile=outPath, reportType="secTopicShort")
## }


###################################################
### code chunk number 24: ePort.Rnw:307-317 (eval = FALSE)
###################################################
## dataFolder = system.file("inst/extdata/DataFiles/Topic06/", package="ePort")
## #namelist = list.files(path=dataFolder, pattern = "^[^.]*\.[^.]*\.[^.]*$", full.names=FALSE)
## #http://stackoverflow.com/questions/9949176/match-string-with-exactly-2-of-a-given-character-e-g-2-literal-periods
## 
## #dataListPath = c(system.file("inst/extdata/DataFiles/Topic06/Topic06.AB.csv", package="ePort"), system.file("inst/extdata/DataFiles/Topic06/Topic06.CD.csv", package="ePort"))
## #topic = gsub('.Questions.txt','',gsub('Topic','',basename(key)))
## #namelist = list.files(path=dataPath,full.names=TRUE)
## #namelist = namelist[grep(paste('Topic',topic,'\\.',sep=''),basename(namelist))]
## #namelist
## 


