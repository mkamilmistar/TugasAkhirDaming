data <- read.csv("~/R/TA/bank.csv", header = TRUE, sep = ";")
str(data)
summary(data)

data$balance[data$balance <= 0] <- NA
data$balance[is.na(data$balance)] <- mean (data$balance, na.rm= TRUE)
summary(data$balance)

#boxplot(data$age)
#plot(density(data$age), main="Density Plot For Age")

#boxplot(data$duration)
#plot(density(data$duration), main="Density Plot For Duration")

job <- data$job
job <- as.data.frame(job)
deposit <- data$y
deposit <- as.data.frame(deposit)

bank_data <- cbind(job, deposit)

summary(bank_data)

#show who deposit vs job category
job_data <- bank_data[which(bank_data$deposit=="yes"), ]
summary(job_data)

#reduksi data job
data_temp <- NULL
data_temp[data$job=="retired" | data$job=="student" | data$job == "unemployed" | data$job == "unknown"] <- "other"
data_temp[data$job=="services" | data$job=="housemaid"] <- "pink-collar"
data_temp[data$job=="management" | data$job=="admin."] <- "white-collar"
data_temp[data$job=="blue-collar"] <- "blue-collar"
data_temp[data$job=="technician"] <- "technician"
data_temp[data$job=="self-employed"] <- "self-employed"
data_temp[data$job=="entrepreneur"] <- "entrepreneur"
data$job <- data_temp

summary(data)

library(mice)
md.pattern(data)

#reduksi data poutcome
data_temp[data$poutcome=="unknown" | data$poutcome=="other"] <- "unknown"
data_temp[data$poutcome=="failure"] <- "failure"
data_temp[data$poutcome=="success"] <- "success"
data$poutcome <- data_temp
summary(data$poutcome)

#delete contact, karna gabutuh
data$contact<-NULL

#ubah nilai default, housing and loan
data_temp <- NULL
data_temp[data$default=="yes"] <- 1
data_temp[data$default=="no"] <- 0
data_temp[data$housing=="yes"] <- 1
data_temp[data$housing=="no"] <- 0
data_temp[data$loan=="yes"] <- 1
data_temp[data$loan=="no"] <- 0
data$default<-data_temp
data$housing<-data_temp
data$loan<-data_temp
summary(data)

#hapus bulan dan hari
data$month<-NULL
data$day<-NULL

#ubah nilai deposit
data_temp <- NULL
data_temp[data$y=="yes"] <- 1
data_temp[data$y=="no"] <- 0
data$y<-data_temp

#ubah nilai pdays -1 ke 1000
data$pdays[data$pdays==-1] <- 1000

#buat colom baru recent_days dan drop pdays
data$recent_pdays <- 1/data$pdays
data$pdays <- NULL

summary(data)

