corr <- function(directory,threshold=0){
#  yn <- vector(mode ="numeric",length = 0)
#  xs <- vector(mode ="numeric",length = 0)
  ret<- vector(mode ="numeric",length = 0)
  for ( i in c(1:332)) {
    if (i < 10 ){
      fullcharid <- paste("/00",as.character(i),sep = "")
    } else if (i < 100) {
      fullcharid <- paste("/0",as.character(i),sep = "")
    } else  {
      fullcharid <- paste("/",as.character(i),sep = "")
    }       
    df <- read.csv(paste(directory,paste(fullcharid,".csv",sep=""),sep=""))
    good <- complete.cases(df)
    df1 <- df[good,]
    if(nrow(df1) > threshold){
      xs<- df1[  , c("sulfate")]
      yn<- df1[  , c("nitrate")]
      ret <- c(ret,cor(xs,yn))
    }
  }
   ret
}   


