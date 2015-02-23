plotModelCompare <- function(GrandRiver,meaNumber,estNumber,
                             meaSite,estSite,titleMod=NULL){
#   meaNumber <- paste0("o",substr(meaSite,2,8))
#   estNumber <- paste0("o",substr(estSite,2,8))
  par(cex.lab=0.8,cex.main=0.9,mar=c(5,5,4,2)+0.1)
  #
  plot(eval(parse(text=paste0("GrandRiver$",meaNumber))),
       eval(parse(text=paste0("GrandRiver$",estNumber))),
       xlab=paste0(" Measured Monthly Flow at ",meaSite,", in ft3/s"),
       ylab=paste0("Estimated Monthly Flows at ",meaNumber,"\ngiven Yields at ",estNumber,", in ft3/s"),
       main=paste0(titleMod),
       pch=20, cex=0.6, col="blue",log="xy")
  abline(a=0,b=1,col="red",lty="solid")
  legend("bottomright",legend=c("Data pairs","Line of agreement"),
         col=c("blue","red"),pch=c(20,NA),lty=c(NA,"solid"),cex=0.8)
  #
#   boxplot(eval(parse(text=paste0("GrandRiver$",meaNumber))) -
#             eval(parse(text=paste0("GrandRiver$",meaNumber,"_",estNumber))) ~ 
#             GrandRiver$month_nu, xlab="Month",ylab="Flow Residuals, cfs",
#           axes=FALSE,notch=TRUE,
#           main=paste0("Monthly Distribution of Flow Residuals for ",meaSite,
#                       "based on ",titleMod," at ",estSite))
#   axis(side=1,at=1:12,labels=month.abb)
#   axis(side=2)
#   abline(h=0,col="red",lty="solid")
  #  
}
