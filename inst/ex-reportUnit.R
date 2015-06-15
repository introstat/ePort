library(ePort)

##### Unit reports #####
# save the .doc file as .htm format
key_htm = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Unit Assessment Questions/Unit4Assessment.htm"
refine_key(key_htm)
key_txt = gsub("htm$","txt",key_htm)

datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Unit Assessment Data Files"
topic = gsub('Assessment.txt','',gsub('Unit','',basename(key_txt)))
namelist = list.files(path=datapath,full.names=TRUE)
namelist = namelist[grep(paste('Unit',topic,'\\.',sep=''),basename(namelist))]
namelist

# choose the corresponding topics
LOpath = list.files(path="~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Topic Outcomes",pattern='\\.txt$',full.names=TRUE)[12:15]

## 1. Individual unit report ##
for (i in namelist) report_routine(key_txt,datafile=i,rewrite=FALSE,LOfile=LOpath,knitfile="Rnw/unit-individual.Rnw")

## 2. Cross-section unit report ##
report_routine(key_txt,namelist,LOfile=LOpath,knitfile="Rnw/unit-section.Rnw")
