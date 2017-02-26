#!/usr/bin/env Rscript

# Example classifier on Numerai data using logistic regression classifier.

# Libraries
library(rpart)
library(class)

# Logloss function
MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

# Set seed for reproducibility
set.seed(0)

print("Loading data...")
# Load the data from the CSV files
trainDF <- read.csv("numerai_training_data.csv", head=T)
testDF <- read.csv("numerai_tournament_data.csv", head=T)

# Split the training set
smpl <- sample(nrow(trainDF), size=0.6*nrow(trainDF))
train <- trainDF[smpl,]
test <- trainDF[-smpl,]

# Train a GLM model
modelGLM <- glm(as.factor(target) ~ ., data=train, 
                family=binomial(link='logit'))

# Get predictions from the GLM model on the test set
predictionsGLM <- predict(modelGLM, test[,-51], type="response")

# Compare logloss to actual
MultiLogLoss(test$target, predictionsGLM)

# Train a RF model
modelRF <- rpart(as.factor(target) ~ ., data = train,
                 method = "class")

# Get predictions from RF model on the test set
predictionsRF <- predict(modelRF, test[,-51])

# Compare logloss to actual
MultiLogLoss(test$target, predictionsRF)

# Train a KNN model
modelKNN <- knn(train=train[,-51], test=test[,-51],
                cl=train$target, k=25, prob=TRUE)

# How accurate are the predictions
MultiLogLoss(test$target, modelKNN)

print("Writing predictions to predictions.csv")
# Save the predictions out to a CSV file
write.csv(pred, file="predictions.csv", quote=F, row.names=F)
# Now you can upload your predictions on numer.ai
