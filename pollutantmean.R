pollutantmean <- function(directory, polutant, id=1:332)  {
        tot <- 0
        num <- 0
        for (i in c(id))   {
           if (i < 10) {  
              fullcharid <- paste("00",as.character(i),sep="")
           }         
           else if(i < 100){
              fullcharid <- paste("0",as.character(i),sep="")
           }
           else {
              fullcharid <- as.character(i)
           }
           df <- read.csv(paste(directory,paste(fullcharid,'.csv',sep=""),sep="/"))
           df1 <- df[ ,c(polutant)]
           bad <- is.na(df1)
           df2 <- df1[!bad]
           tot <- tot + sum(df2)
           num <- num + length(df2)
        }
        ret <- tot/num
}
