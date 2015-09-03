library(ePort)

##### *. Split data files #####
datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files/Ch3"
namelist = list.files(path=datapath,full.names=TRUE)
for (i in namelist) rewrite_data(i) # May not need this rewriting step!
for(i in c('AB','CD','EF','GH','JKQ')){
  tmp = namelist[grep(i,basename(namelist))]
  split_file(tmp, 29, "ID")
}

##### *. Merge data files #####
datapath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Fall 2014/Data Files/Ch9-Ch11"
namelist = list.files(path=datapath,full.names=TRUE)
for (i in namelist) rewrite_data(i) # May not need this rewriting step!
for(i in c('AB','CD','EF','GH','JK','LM')){
  tmp = namelist[grep(i,basename(namelist))]
  combine_files(tmp[2],tmp[1],paste("Topic11",i,"csv",sep='.'))
}
