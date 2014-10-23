# Read spreadsheet of monthly data from Jacob Bauxer 
#   Source: Email attachment from Bauxer on Oct. 21, 2014
# NBS data sheet was copied from spreadsheet and pasted to a tab delimited text file

fName <- "/Superior/data/Monthly/NBS.txt"
pName <- getwd()
fullName <- paste(pName,fName,sep="")
nbs_df   <- read.table(fullName, header = TRUE, sep = "\t")
nbs_mat  <- as.matrix(nbs_df[,2:13])
# Convert transposed matrix to vector
nbs_vec  <- as.vector(t(nbs_mat))
# Generate a monthly date sequence
dateNBS  <- seq(from = as.Date("1900/01/01"), to = as.Date("2013/12/01"),by = "month")
# Plot net basin supply
plot(dateNBS,nbs_vec,type="l",main="Net Basin Supply for Lake Superior",
     ylab="cubic meters per second", xlab="year")
# Clean up 
rm(list=c("nbs_mat","nbs_df"))
#
# Read and plot monthly flow in St. Marys River
fName    <- "/Superior/data/Monthly/StMarysRiver.txt"
fullName <- paste(pName,fName,sep="")
smr_df   <- read.table(fullName, header = TRUE, sep = "\t")
smr_mat  <- as.matrix(smr_df[,2:13])
# Convert transposed matrix to vector
smr_vec  <- as.vector(t(smr_mat))
# Plot net basin supply
plot(dateNBS,smr_vec,type="l",main="Outflow from Lake Superior through St. Marys River",
     ylab="cubic meters per second", xlab="year",col="blue")
# Clean up 
rm(list=c("smr_mat","smr_df"))
#
# Ogoki and Long Lake Diversions (URL documenting Great Lakes diversions follows.)
# http://www.watershedcouncil.org/water%20resources/great%20lakes/threats-to-the-great-lakes/great-lakes-water-use-and-diversion/
#
<<<<<<< HEAD
# Read and plot raw monthly flow in Ogoki and Long Lake Diversions to Lake Superior (6% of flow)
=======
# Read and plot monthly flow in St. Marys River
>>>>>>> c479ee152e128e5053b581943d3d527545cb3eff
fName    <- "/Superior/data/Monthly/OgokiLongLakeDiversions.txt"
fullName <- paste(pName,fName,sep="")
gll_df   <- read.table(fullName, header = TRUE, sep = "\t")
gll_mat  <- as.matrix(gll_df[,2:13])
# Convert transposed matrix to vector
gll_vec  <- as.vector(t(gll_mat))
# Plot net basin supply
plot(dateNBS,gll_vec,type="l",main="Inflow to Lake Superior from Ogoki and Long Lake Diversions",
     ylab="cubic meters per second", xlab="year",col="blue")
# Clean up 
rm(list=c("gll_mat","gll_df"))
<<<<<<< HEAD
#
# Read and plot monthly changes in Lake Superior storage associated with lake level fluctuations 
fName    <- "/Superior/data/Monthly/ChangeInStorage.txt"
fullName <- paste(pName,fName,sep="")
df1      <- read.table(fullName, header = TRUE, sep = "\t")
mat1     <- as.matrix(df1[,2:13])
# Convert transposed matrix to vector
vec1     <- as.vector(t(mat1))
# Plot net basin supply
plot(dateNBS,vec1,type="l",
     main="Change in Storage of Lake Superior from Lake Level \nChanges and Coordinated Lake Surface Area",
     ylab="cubic meters per second", xlab="year",col="blue")
# Clean up 
rm(list=c("mat1","df1"))
#
=======

>>>>>>> c479ee152e128e5053b581943d3d527545cb3eff
