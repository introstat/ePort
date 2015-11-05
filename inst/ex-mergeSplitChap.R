library(ePort)

##### *. Split data files #####
dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files/Ch3"
namelist = list.files(path=dataPath,full.names=TRUE)
for (i in namelist) rewriteData(i) # May not need this rewriting step!
for(i in c('AB','CD','EF','GH','JKQ')){
  tmp = namelist[grep(i,basename(namelist))]
  splitFile(tmp, 29, "ID")
}

##### *. Merge data files #####
dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Data Files/Ch9-Ch11"
namelist = list.files(path=dataPath,full.names=TRUE)
for (i in namelist) rewriteData(i) # May not need this rewriting step!
for(i in c('AB','CD','EF','GH','JK','LM')){
  tmp = namelist[grep(i,basename(namelist))]
  combineFiles(tmp[2],tmp[1],paste("Topic11",i,"csv",sep='.'))
}
