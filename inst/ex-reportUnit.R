library(ePort)

##### Unit reports #####
# save the .doc file as .htm format
key_htm = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Unit Assessment Questions/Unit4Assessment.htm"
refineKey(key_htm)
keyPath = gsub("htm$","txt",key_htm)

dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Unit Assessment Data Files"
topic = gsub('Assessment.txt','',gsub('Unit','',basename(keyFile)))
namelist = list.files(path=dataPath,full.names=TRUE)
namelist = namelist[grep(paste('Unit',topic,'\\.',sep=''),basename(namelist))]
namelist

# choose the corresponding topics
loPath = list.files(path="~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Topic Outcomes",pattern='\\.txt$',full.names=TRUE)[12:15]

## 1. Individual unit report ##
for (i in namelist) makeReport(keyPath=keyFile,dataFile=i,rewrite=FALSE,loFile=loPath,reportType="Rnw/unit-individual.Rnw")

## 2. Cross-section unit report ##
makeReport(keyPath=keyFile,namelist,loFile=loPath,reportType="Rnw/unit-section.Rnw")
