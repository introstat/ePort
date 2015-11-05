library(ePort)

##### *. De-identify students #####

# Copy the data files into a new folder, say, "Data Files (de-identified)".
# Make sure that any csv files other than the student data are already removed, e.g. "nameCode.csv"

dataPath = "~/Dropbox/NSF Grant 2013-2015/Semesters/Spring 2015/Data Files (de-identified)"
a = getNameList(dataPath, section=NULL, semester="s15", secblind=TRUE, save=TRUE)
encodeName(dataPath, dict=paste(dataPath,"nameCode.csv",sep='/'))
