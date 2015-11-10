library(ePort)
library(knitr)

##### Homework reports #####
## key ##
# save the .doc file as .htm format, to keep the images
key_htm = "C:/Users/amyf/Dropbox/Stat 101 Coordinating Materials/Online Homework Assessment Data/Questions/Topic03.Questions.htm"
refineKey(key_htm) # generate a clean answer key with paths to the plots in the question
keyPath = gsub("htm$","txt",key_htm) # get the path/name of the new answer key

## data ##
dataPath = "C:/Users/amyf/Dropbox/Stat 101 Coordinating Materials/Online Homework Assessment Data/Topic03Data"
topic = gsub('.Questions.txt','',gsub('Topic','',basename(keyFile)))
namelist = list.files(path=dataPath,full.names=TRUE)
namelist = namelist[grep(paste('Topic',topic,'\\.',sep=''),basename(namelist))]
namelist

## learning objectives ##
loPath = "C:/Users/amyf/Dropbox/Stat 101 Coordinating Materials/Online Homework Assessment Data/Outcomes/Topic03.Outcomes.txt" 

## 1. Individual report ##
for (i in namelist) rewriteData(i)
for (i in namelist) makeReport(keyPath=keyFile,dataFile=i,rewrite=FALSE,loFile=loPath,reportType="inst/Rnw/hw-individual-short.Rnw")
for (i in namelist) makeReport(keyPath=keyFile,dataFile=i,rewrite=FALSE,loFile=loPath,reportType="inst/Rnw/hw-individual.Rnw")

## 2. Cross-section report ##
makeReport(keyPath=keyFile,namelist,loFile=loPath,reportType="inst/Rnw/hw-section-short.Rnw")
makeReport(keyPath=keyFile,namelist,loFile=loPath,reportType="inst/Rnw/hw-section.Rnw")

## 3. Cross-topic report ##
tabfiles = setDir(dataPath)
tabfiles = tabfiles[tabfiles$section!='201' & tabfiles$topic %in% paste0('Topic0',1:5),]
mgdata = mergeData(tabfiles)
for (sctn in unique(tabfiles$section)) {
  merged = subsetData(mgdata,tabfiles,choice=sctn)
  knit("/Users/lindz/ePort/inst/Rnw/hw-topic.Rnw",output=paste0('Stat101hwk_Unit1_Section',sctn,'.tex'))
}

## 4. Cross-section & cross-topic report ##
merged = subsetData(mgdata,tabfiles)
knit("/Users/lindz/ePort/inst/Rnw/hw-topic-section.Rnw",output='Stat101hwk_Unit1_allSections.tex')
