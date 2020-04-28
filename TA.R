data <- read.csv("~/R/TA/bank.csv", header = TRUE, sep = ";")
library(mice)
md.pattern(data)

boxplot(data$age)
plot(density(data$age), main="Density Plot For Age")

boxplot(data$duration)
plot(density(data$duration), main="Density Plot For Duration")

dataBank = data

