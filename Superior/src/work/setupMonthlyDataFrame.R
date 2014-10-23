# Read monthly data files and merge data by dates to common dataframe
#
# Read 
# Clear memory
rm(list=ls())
# generate components dataframe
pName     <- getwd()
fName     <- "/Superior/src/work/checkCompNBS.R"
fullName  <- paste(pName,fName,sep="")
source(fullName)
#
# Add data for residuals method
fName     <- "/Superior/src/work/checkResidNBS.R"
pName     <- getwd()
fullName  <- paste(pName,fName,sep="")
source(fullName)
#
# Merge the two data files by DateSeq
NBSrcDf <- merge(NBScDf,NBSrDf)
#
rm(list=setdiff(ls(),"NBSrcDf"))

