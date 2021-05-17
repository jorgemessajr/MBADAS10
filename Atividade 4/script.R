library(modeldata)
library(C50)
library(gmodels)
library(caret)

heart <- read.csv("./Atividade 4/heart.csv")
heart$OUTPUT <- as.factor(heart$OUTPUT)

set.seed(12345)
heart_rand <- heart[order(runif(270)),]

heart_train <- heart_rand[1:230,]
heart_test <- heart_rand[231:270,]


heart_model <- C5.0(heart_train[-14],
                    heart_train$OUTPUT)
heart_pred <- predict(heart_model, heart_test)



confusionMatrix(heart_pred, 
                heart_test$OUTPUT, positive="2")

CrossTable(heart_test$OUTPUT, heart_pred, 
           prop.chisq = FALSE, prop.c = TRUE,
           prop.r = TRUE, dnn = c("actual", "predict"))

error_cost <- matrix(c(0,1,4,0),nrow=2)
rownames(error_cost) <- colnames(error_cost) <- c("1", "2")
error_cost

heart_cost_model <- C5.0(heart_train[-14],
                    heart_train$OUTPUT, 
                    cost=error_cost)
heart_cost_pred <- predict(heart_cost_model, heart_test)


CrossTable(heart_test$OUTPUT, heart_cost_pred, 
           prop.chisq = FALSE, prop.c = TRUE,
           prop.r = TRUE, dnn = c("actual", "predict"))


confusionMatrix(heart_cost_pred, 
                heart_test$OUTPUT, positive="2")


