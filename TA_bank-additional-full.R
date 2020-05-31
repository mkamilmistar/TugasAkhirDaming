#data <- read.csv("~/R/TA/bank.csv", header = TRUE, sep = ";")
data <- read.csv("~/Github/TugasAkhirDaming/bank-additional-full.csv", header = TRUE, sep=";")
names(data) <- c("age", "job", "marital", "education", "default", "housing", "loan", "contact", "month", "day_of_week", "duration", "campaign", "pdays", "previous", "poutcome", "emp.var.rate", "consPriceIdx", "cons.conf.idx", "euribor3m", "nr.employed", "y")
str(data)
summary(data)

#Cek ada data yg NA sama valuenya negatif engga
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) sum(x<0, na.rm=TRUE))

#Ganti tipe data yg integer jadi numeric
data$age <- as.numeric(data$age)
data$duration <- as.numeric(data$duration)
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)


######################### KALO LANGSUNG PROSES DT (Rpart) ######################################
library(dplyr)
library(plyr)
library(rpart)
set.seed(11)
myFormula <- y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + consPriceIdx + cons.conf.idx + euribor3m + nr.employed
folds <- split(data, cut(sample(1:nrow(data)),10))
errs <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  testData <- ldply(folds[i], data.frame)
  trainData <- ldply(folds[-i], data.frame)
  hasil_rpart <- rpart(myFormula , data=trainData, method = "class")
  rpart_pred <- predict(hasil_rpart, newdata = testData, type = "class")
  confMatrix <- table(testData$y, rpart_pred)
  errs[i] <- 1-sum(diag(confMatrix))/sum(confMatrix)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))


##################### KALO LANGSUNG PROSES DT (CTREE) ###############################
library(party)
library(caret)

for (i in 1:length(folds)) {
  testData <- ldply(folds[i], data.frame)
  trainData <- ldply(folds[-i], data.frame)
  hasil_ctree <- ctree(myFormula , data=trainData)
  ctree_pred <- predict(hasil_ctree, newdata = testData)
  confMatrix <- table(testData$y, ctree_pred)
  errs[i] <- 1-sum(diag(confMatrix))/sum(confMatrix)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))


################################################################################################
########################## INI KALO NYOBA NGUBAH2 ATRIBUTNYA DULU ###########################

#Reduksi atribut education (gabungin yg valuenya basic.[x]y ke basic aja)
education_temp <- NULL
education_temp[data$education=="basic.4y" | data$education=="basic.6y" | data$education=="basic.9y"] <- "basic"
education_temp[data$education=="high.school"] <- "high.school"
education_temp[data$education=="illiterate"] <- "illiterate"
education_temp[data$education=="professional.course"] <- "professional.course"
education_temp[data$education=="university.degree"] <- "university.degree"
education_temp[data$education=="unknown"] <- "unknown"
data$education <- education_temp
data$education <- as.factor(data$education)


#Missing value (80 unknown) atribut marital langsung gua drop
#soalnya gua gabisa kategoriin status org cuma dari umurnya/pendidikannya
data <- data %>% filter(marital != "unknown") 

#Missing value atribut loan sama housing juga gua drop sementara karena bingung mau diapain
#dan kebetulan missing value mereka tuh barengan semua datanya pas dicek
data <- data %>% filter(housing != "unknown")
data <- data %>% filter(loan != "unknown")

#Atribut pdays gua drop karena pas diliat nilai 999 (gapernah ada kontak) itu banyak bgt
#ga sebanding sama yg nilainya selain 999 jadi kayaknya atribut ini ga ngaruh bgt ke kelas y
data$pdays <- NULL

############################## COBA PROSES DT LAGI (Rpart) ###################################
set.seed(11)
myFormula <- y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + previous + poutcome + emp.var.rate + consPriceIdx + cons.conf.idx + euribor3m + nr.employed
folds <- split(data, cut(sample(1:nrow(data)),10))
errs <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  testData <- ldply(folds[i], data.frame)
  trainData <- ldply(folds[-i], data.frame)
  hasil_rpart <- rpart(myFormula , trainData, method = "class")
  rpart_pred <- predict(hasil_rpart, newdata = testData, type = "class")
  confMatrix <- table(testData$y, rpart_pred)
  errs[i] <- 1-sum(diag(confMatrix))/sum(confMatrix)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))

############################## COBA PROSES DT LAGI (CTREE) ###################################
for (i in 1:length(folds)) {
  testData <- ldply(folds[i], data.frame)
  trainData <- ldply(folds[-i], data.frame)
  hasil_ctree <- ctree(myFormula , data=trainData)
  ctree_pred <- predict(hasil_ctree, newdata = testData)
  confMatrix <- table(testData$y, ctree_pred)
  errs[i] <- 1-sum(diag(confMatrix))/sum(confMatrix)
}
print(sprintf("average error using k-fold cross-validation: %.3f percent", 100*mean(errs)))

##Abis diubah rata-rata errornya nambah, berarti kayaknya yg gua proses (drop missing value, atribut, dll) salah