dow <- read.csv("DOWdaily.csv")
gold <- read.csv("Gold.csv")
bonds10 <- read.csv("DGS10.csv")
SP500 <- read.csv("^GSPC.csv")
nyse <- read.csv("^NYA.csv")

dow <- dow[,c("Date", "Close")]
SP500 <- SP500[,c("Date", "Close")]
nyse <- nyse[,c("Date", "Close")]

colnames(dow) <- c("DATE", "DOW")
colnames(SP500) <- c("DATE", "SP500")
colnames(nyse) <- c("DATE", "NYSE")
colnames(gold) <- c("DATE", "gold")


fullData <- merge(dow, bonds10, all.x = TRUE)
fullData <- merge(fullData, SP500, all.x = TRUE)
fullData <- merge(fullData, gold, all.x = TRUE)
fullData <- merge(fullData, nyse, all.x = TRUE)

write.table(fullData, "economicDataDaily.txt", col.names = TRUE)