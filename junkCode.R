# Assess uncertainty of monthly values from daily values
pName       <- 'Superior/data/Daily/Qd04116000newMillennium.txt'
dvTable     <- read.table(pName,sep="\t", header=TRUE,comment.char="#",
                          stringsAsFactors = FALSE)
dvTable$datetime    <- as.Date(dvTable$datetime)
# Drop first two columns for agency and gage
dvTable <- dvTable[-c(1:2)]
# Rename columns 
colnames(dvTable) <- c("datetime","flow","flow_cd")
# Create log flow
dvTable$lFlow     <- log(dvTable$flow)

dvTable$ExpError  <- rep(0,nrow(dvTable))

ndxA  <- which(dvTable$flow_cd == "A")
nexAe <- which(dvTable$flow_cd == "A:e")
dvTable$ExpError[ndxA]  <- 0.1
dvTable$ExpError[ndxAe] <- 0.2
dvTable$ExpStd          <- dvTable$flow * dvTable$ExpError
dvTable$year_mo         <- as.character(dvTable$datetime,"%Y_%m")
#
# Aggregate data by month/year
mo_meanQ                <- aggregate(dvTable$flow, by = list(dvTable$year_mo), FUN="mean")
mo_varQ                 <- aggregate(dvTable$ExpVar,  by = list(dvTable$year_mo), FUN="mean")
mo_stdQ                 <- sqrt(mo_varQ$x)
