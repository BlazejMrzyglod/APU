data("infert")
library(magrittr)
library(dplyr)
infert3 <- infert %>%
  select(education, age, parity, induced, case, spontaneous)

data1 <- c("training" , "test" ) %>%
  sample(nrow(infert3), replace = T) %>%
  split(infert3, .)
library(rpart)
rtree_fit <-rpart(case~.,data1$training)
plot(rtree_fit)
text(rtree_fit, use.n = TRUE)









library(mlr)
load("Macbooki.csv")
Macbooki
Macbooki$ekran <- as.factor(Macbooki$ekran)
task <- makeClassifTask(id = "macbooki", data = Macbooki, target = "ocena")
learner <- makeLearner("classif.randomForest", predict.type = "response")
rdesc <- makeResampleDesc("CV",stratify = F,iters=5)
lrns <- makeLearners(c("rpart","rFerns",
                       "randomForest"), type = "classif")
r <- resample(learner, task, rdesc)
porownanie <-benchmark(learners = lrns, tasks=task, resampling=cv5)

r
model <- train(learner, task)
predictions <- predict(model, newdata = Macbooki)
print(calculateConfusionMatrix(predictions))
performance(predictions)
plotBMRBoxplots(porownanie)
