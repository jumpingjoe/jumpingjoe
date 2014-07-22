complete <- function(directory, id=1:332){
  retnobs <- vector(mode ="integer",length = 0)
  for ( i in c(id)) {
    if (i < 10 ){
            fullcharid <- paste("00",as.character(i),sep = "")
    } else if (i < 100) {
            fullcharid <- paste("0",as.character(i),sep = "")
    } else  {
            fullcharid <- as.character(i)
    }
     df <- read.csv(paste(directory,paste(fullcharid,".csv",sep=""),sep="/"))
     good <- complete.cases(df)  
     df2 <- df[good,]
     retnobs  <- c(retnobs,nrow(df2))
  }
  ret <- data.frame(id=id,nobs=retnobs)
}
