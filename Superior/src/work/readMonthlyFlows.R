# Function to read monthly flows for analysis of runoff uncertainty
readMonthlyFlows <- function(staNumber){
  # Read in monthly flows for staNumber
  fileName   <- paste0("Superior/data/Monthly/Streamgage/",staNumber,".txt")
  flowTable  <- read.table(file=fileName,header=TRUE,sep="\t",
                           stringsAsFactors = FALSE, comment.char="#")
  
  flowTable$cDate   <- as.Date(paste(flowTable$month_nu,"-01-",
                                     flowTable$year_nu,sep=""),"%m-%d-%Y")
  keepNames  <- c("cDate","mean_va")
  flowTable  <- flowTable[,keepNames]
  names(flowTable)[names(flowTable)=="mean_va"] <- paste0("o",substr(staNumber,2,8))
  return(flowTable)
}
  
