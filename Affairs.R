
library(AER)

data(Affairs,package = "AER")

View(Affairs)

Affairs$affairs[which(Affairs$affairs > 1)] = 1

Affairs$affairs <- as.factor(Affairs$affairs)

str(Affairs)

sam <- sample(x = 2, size = nrow(Affairs), replace = TRUE, prob = c(0.8,0.2))

train <- Affairs[sam==1,]

test <- Affairs[sam==2,]

model <- glm(affairs~gender+age+yearsmarried+religiousness+rating+, data = train,family = "binomial")

summary(model)

p <- predict(model,test,type = "response")

tab <- table(test$affairs,p>0.5)

tab

library(caret)

dimnames(tab)[[2]] = c("0","1")

confusionMatrix(tab)



