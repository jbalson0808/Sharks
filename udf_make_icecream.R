udf_make_icecream <- function(fileName){
  tmp <- read.csv(fileName, header = T)
  tmp <- tmp[1:588,]
  tmp$Date <- as.POSIXct(tmp$Date, format='%Y-%m-%d')
  tmp$year <- as.integer(format(tmp$Date, format='%Y'))
  tmp$month <- substr(months(tmp$Date),1,3)
  names(tmp)[which(names(tmp)=='Ice.Cream.Production.Index')] <- 'icpi'
  tmp2 <- data.frame(tmp$year, tmp$month, tmp$icpi)
  names(tmp2) <- c('year','month','icpi')
  return(tmp2)
}