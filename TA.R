data <- read.csv("~/R/TA/bank.csv", header = TRUE, sep = ";")
str(data)
summary(data$balance)


data$balance[data$balance <= 0] <- NA
data$balance[is.na(data$balance)] <- mean (data$balance, na.rm= TRUE)

library(mice)
md.pattern(data)

boxplot(data$age)
plot(density(data$age), main="Density Plot For Age")

boxplot(data$duration)
plot(density(data$duration), main="Density Plot For Duration")


set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
testData <- data[ind==2,]

myFormula <- y ~ age + job + marital + education + default + balance + housing + loan + contact + day + month + duration  + campaign + pdays + previous + poutcome
iris_ctree <- ctree(myFormula, data=trainData)

print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")
