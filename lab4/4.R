data("infert")
library(magrittr)
infert3 <- infert %>%
  select(education, age, parity, induced, case, spontaneous)

data1 <- c("training" , "test" ) %>%
  sample(nrow(infert3), replace = T) %>%
  split(infert3, .)
library(rpart)
rtree_fit <-rpart(case~.,data1$training)
plot(rtree_fit)
text(rtree_fit, use.n = TRUE)
help(rpart)








# Load necessary libraries
library(mlr)

load("Macbooki.csv")
Macbooki
# Convert the response variable to a factor
Macbooki$nazwa <- as.factor(Macbooki$nazwa)

# Define the classification task
task <- makeClassifTask(id = "macbooki", data = Macbooki, target = "ocena")
learner <- makeLearner("classif.randomForest", predict.type = "response")
# Perform 5-fold cross-validation
rdesc <- makeResampleDesc("CV",stratify = F,iters=5)
# Choose a learner (e.g., Random Forest)
lrns <- makeLearners(c("rpart", "C50","rFerns",
                       "randomForest"), type = "classif")


# Train the model and evaluate performance using cross-validation
r <- resample(learner, task, rdesc)
porownanie <-benchmark(learners = lrns, tasks=task, resampling=cv5)

r
model <- train(learner, task)
predictions <- predict(model, newdata = Macbooki)
print(calculateConfusionMatrix(predictions))
performance(predictions)
