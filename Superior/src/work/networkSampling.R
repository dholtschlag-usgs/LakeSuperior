# Experiment with sampling in a nested grid
#
# Clear workspace
rm(list=ls())
# read Net Basin Supply with residual and components Data frame (NBSrcLevelDf)
pName      <- getwd()
fName      <- "/Superior/data/GrandRiverGageNetwork.txt"
fullName   <- paste(pName,fName,sep="")
gageNetwork    <- read.table(file=fullName,header=TRUE,sep="\t",
                              stringsAsFactors = FALSE, comment.char="#")
#
# Read in the data
x  <- scan(fullName, what="", sep="\n")
# Separate elements by one or more whitepace
y  <- strsplit(x, "\t")
#
# Universal set 
U          <- unlist(lapply(y, function(x) x[1]))
y1         <- U
networkMtrx       <- matrix(0,length(y1),length(y1))
diag(networkMtrx) <- 1
#
rownames(networkMtrx) <- y1
colnames(networkMtrx) <- y1
#
for (i in 1:length(y1)){
  j <- 2
  while (j <= length(y[[i]])) {
    networkMtrx[y[[i]][[1]],y[[i]][[j]] ] <- 1
    j <- j + 1
  }
}
#
fName      <- "/Superior/data/Monthly/Streamgage/grandRiverGageDA.txt"
fullName   <- paste(pName,fName,sep="")
gageDA     <- read.table(file=fullName, sep=",", skip=1, 
                         colClasses=c("character","numeric"))
colnames(gageDA) <- c("streamgage","drnAreaMi2")
# generate n x 1 matrix of drainage areas
drnAreaMtrx  <- as.matrix(gageDA$drnAreaMi2,nrow(gageDA),1)
#
incDrnArea <- solve(networkMtrx) %*% drnAreaMtrx[1:(nrow(drnAreaMtrx)-1)]
#
# Select year-month for estimation
#
# generic url for water service retrieval 
urlH2O <- "http://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=stnNumber&statReportType=monthly&statTypeCd=all&missingData=off"
for (i in 1:length(y1)){
  urlGage <- sub("stnNumber",y1[i],urlH2O)
  download.file(urlGage,destfile=paste(pName,"/Superior/data/Monthly/Streamgage/",y1[i],'.txt',sep=""))
  con     <- file(paste(pName,"/Superior/data/Monthly/Streamgage/",y1[i],'.txt',sep=""))
  lineIn  <- readLines(con) 
  lineOut <- sub("5s","# 5s",lineIn)
  writeLines(lineOut, con=paste(pName,"/Superior/data/Monthly/Streamgage/",y1[i],'.txt',sep=""))
}
#
# Get monthly flows for 04119000 Grand River at Grand Rapids, MI
urlGage      <- sub("stnNumber","04119000",urlH2O)
download.file(urlGage,destfile=paste(pName,"/Superior/data/Monthly/Streamgage/04119000.txt",sep=""))
con     <- file(paste(pName,"/Superior/data/Monthly/Streamgage/04119000.txt",sep=""))
lineIn  <- readLines(con) 
lineOut <- sub("5s","# 5s",lineIn)
writeLines(lineOut, con=paste(pName,"/Superior/data/Monthly/Streamgage/04119000.txt",sep=""))

# Determine which streamgages were active during the selected month
for (i in 1:length(y1)){
  fName      <- paste("/Superior/data/Monthly/Streamgage/",y1[i],'.txt',sep="")
  fullName   <- paste(pName,fName,sep="")
  tblName    <- read.table(file=fullName, sep="\t", header=TRUE, 
                           # colClasses=c(rep("character",3),rep("numeric",4)),
                           comment.char="#")
  tblName$dates <- with(tblName, as.Date(paste(month_nu,"-15-",year_nu,sep=""),"%m-%d-%Y")) 
  # drops         <- c("agency_cd","site_no","parameter_cd","dd_nu","year_nu","month_nu")
  keepVar       <- c("dates","mean_va")
  assign(paste("Q",y1[i],sep=""), tblName[,keepVar])
}

# Modify network to eliminate streamgages that do not have flow data for period

# Repeated sample network with replacement 

# For each sample estimate the flow at Grand River at Grand Rapids, MI 04119000














urlGage <- "http://waterdata.usgs.gov/nwis/monthly?site_no=04109500&agency_cd=USGS&por_04109500_1=891569,00060,1,1944-04,1956-09&referred_module=sw&format=rdb"


download.file(_cd=USGS&por_04109500_1=891569,00060,1,1944-04,1956-09&referred_module=sw&format=rdb",
              )

install.packages("devtools",repos="http://cran.mtu.edu/bin/windows/contrib/3.1/")

install.packages("dataRetrieval")
library(dataRetrieval)

04109000,174
04109500,55
04110000,49
04111000,661
04111379,163
04111500,16.3
04112000,9.34
04112500,355
04112850,80.6
04112904,4.28
04113000,1230
04113097,12.1
04114000,1385
04114498,280
04114500,280
04115000,434
04115265,39.7
04116000,2840
04116500,528
04117000,7.60
04117500,385
04118000,773
04118500,234
04119000,4900



nSamp   <- 10
selGage <- rep(NA,nSamp) 
i <- 0
while (length(U) > 0){
  # Select a streamgage at random
  i <- i + 1
  selGage[i] <- sample(U, 1)
  print(paste(i,selGage[i]))
  # Eliminate gages in the remaining U sites that are nested in the selected site  
  ndxDrop    <- grep(selGage[i],y1)
  U          <- setdiff(U,y[[ndxDrop]])
  # print(length(U))
}
