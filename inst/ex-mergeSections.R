library(ePort)
library(plyr)
library(dplyr)

##### save data files into one csv file #####
keyFile = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Database Questions/Topic05.Questions.txt"
dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files"
topic = "05"
namelist = list.files(path=dataPath,full.names=TRUE)
namelist = namelist[grep(paste('Topic',topic,'\\.',sep=''),basename(namelist))]
namelist

answerkey = convertKey(keyFile)
instructor_scores = mergeSection(namelist, answerkey,skip=NULL)
rownames(instructor_scores$score) = instructor_scores$score$student
tmp1 = instructor_scores$Qscore
l = ncol(tmp1)
tmp1$TotalScore = rowSums(tmp1, na.rm=TRUE)
tmp1$Section = instructor_scores$score[rownames(tmp1),'Section']
tmp1 = tmp1[,c(l+2,l+1,1:l)]
tmp2 = instructor_scores$Qanswer
tmp2$Section = instructor_scores$score[rownames(tmp2),'Section']
tmp2 = tmp2[,c(l+1,1:l)]
write.csv(tmp1,file='Topic3.score.csv')
write.csv(tmp2,file='Topic3.answer.csv')
