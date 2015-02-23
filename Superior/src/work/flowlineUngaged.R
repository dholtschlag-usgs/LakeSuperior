# Read dbf files of flowlines and find ungaged flowlines
# load foreign package to read dbf files
library(foreign)
getwd()
setwd("../../Data/GIS/")
dbf04119000  <- "Flowlines04119000.dbf"
tmp  <- read.dbf(dbf04119000)
fLine04119000 <- subset(tmp, select = "COMID")
#
dbf04116000  <- "Flowlines04116000.dbf"
tmp  <- read.dbf(dbf04116000)
fLine04116000 <- subset(tmp, select = "COMID")
#
dbf04116500  <- "Flowlines04116500.dbf"
tmp  <- read.dbf(dbf04116500)
fLine04116500 <- subset(tmp, select = "COMID")
#
dbf04118000  <- "Flowlines04118000.dbf"
tmp  <- read.dbf(dbf04118000)
fLine04118000 <- subset(tmp, select = "COMID")
#
dbf04118500  <- "Flowlines04118500.dbf"
tmp  <- read.dbf(dbf04118500)
fLine04118500 <- subset(tmp, select = "COMID")
#
# Apply set differences to discover 
sDif04119000  <- setdiff(fLine04119000$COMID,fLine04116000$COMID)
sDif04119000  <- setdiff(sDif04119000,fLine04116500$COMID)
sDif04119000  <- setdiff(sDif04119000,fLine04118000$COMID)
sDif04119000  <- setdiff(sDif04119000,fLine04118500$COMID)

comId04119000 <-  9008387
comID04116000 <-  9008227
comID04116500 <-  9005311
comID04118000 <-  12145646
comID04118500 <-  9004023

