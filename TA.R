#data <- read.csv("~/R/TA/bank.csv", header = TRUE, sep = ";")
data <- read.csv("~/Github/TugasAKhirDaming/bank.csv", header = TRUE, sep = ";")
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



################# Apriori ######################
library(arules)
library(arulesViz)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$campaign <- as.factor(data$campaign)
data$previous <- as.factor(data$previous)
data$y <- as.factor(data$y)
data$recent_pdays <- as.factor(data$recent_pdays)
data$age <- as.factor(data$age)
data$job <- as.factor(data$job)
data$balance <- as.factor(data$balance)
data$education <- as.factor(data$education)
data$marital <- as.factor(data$marital)
data$duration <- as.factor(data$duration)
data$poutcome <- as.factor(data$poutcome)

#Apriori langsung (Setting Default bawaan package)
rules_default <- apriori(data)
rules_default <- sort(rules_default, by="confidence")
inspect(rules_default[1:50])

#Apriori (Panjang rules min_panjangKolom=2, max_panjangKolom=15, minsup=0.05, minconfidence=0.8)
params = list(minlen=2, maxlen=15, support=0.05, confidence=0.8)
rules_2 <- apriori(data, parameter = params, appearance = list
                        (rhs = c("y=0", "y=1")))
rules_2 <- sort(rules_2, by="confidence")
inspect(rules_2[1:50])

#Hapus data redundant
subset_matrix <- is.subset(rules_2, rules_2)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rulesPruned <- rules_2[!redundant]
inspect(rulesPruned[1:50]) 
#note: kayaknya masih perlu mainan nilai support atau mungkin reduksi atribut, soalnya output yg keluar kbnyakan yg y=0/No semua

#Plot
plot(rulesPruned, method="grouped")
plot(rulesPruned, method="graph", control=list(type="items"))


##################### Decision Tree ###############################
library(party)
library(caret)
set.seed(11)

indeks <- sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainData <- data[indeks==1,]
testData <- data[indeks==2,]

myFormula <- y ~ age + job + marital + education + default +
  housing + loan  + campaign + previous + poutcome


### Disini kok error ya "Error in trafo(data = data, numeric_trafo = numeric_trafo, factor_trafo = factor_trafo,  : data class "character" is not supported""
#padahal pas dicek pake sapply, semua atribut sama class y nya kebaca sbg "factor", bukan "character"
#udah bisa deng

#Pruning Maxdepth = 3
hasil_tree <- ctree(myFormula, data=trainData, controls=ctree_control(maxdepth = 3))

sapply(data, class)

print(hasil_tree)
plot(hasil_tree)
plot(hasil_tree, type="simple")

ctree_pred <- predict(hasil_tree, newdata=testData)

#Akurasinya tinggi tapi kok nilai kappanya kecil bgt ya 
confusionMatrix(ctree_pred, testData$y)
