data <- read.table("Ankylosing_Spondilitus_data.txt", skip = 4)
control <- data[,1:4]
colnames(control) <- c("flexionBefore", "flexionAfter", "rotationBefore", "rotationAfter")
control <- control[control$flexionBefore != "*",]
control$treatment <- rep("Control", nrow(control))
treat <- data[,5:8]
colnames(treat) <- c("flexionBefore", "flexionAfter", "rotationBefore", "rotationAfter")
treat$treatment <- rep("Stretch", nrow(treat))
data <- rbind(control, treat)
data$patientNo <- 1:78 
data$hip <- rep(c("Right", "Left"), nrow(data)/2)
data$patientNo <- ceiling(data$patientNo/2)
data$flexionBefore <- as.numeric(data$flexionBefore)
data$flexionAfter <- as.numeric(data$flexionAfter)
data$rotationBefore <- as.numeric(data$rotationBefore)
data$rotationAfter <- as.numeric(data$rotationAfter)

write.table(data, "tidyAS.txt", col.names = TRUE)