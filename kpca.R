library(plyr)
library(dummies)
library(glm2)
library(car)
library(ROCR)
library(party)
library(caret)
library(e1071)

setwd("E:/Summer17/research/canvas/Analysis_Files/PCA")

data <- read.csv("raw_data.csv", header = TRUE)
str(data)

for (Var in names(data)) 
{
  missing <- sum(is.na(data[,Var]))
  if (missing > 0)
  {
    print(c(Var,missing))
  }
}

data_0 <- cbind(data, dummy(data$discipline), dummy(data$primary_reason), dummy(data$learner_type), dummy(data$expected_hours_week), 
                dummy(data$LoE_ID), dummy(data$age_ID))
head(data_0)
str(data_0)
summary(data_0)

names(data_0)
data_1 <- data_0[-c(2,8,13:23,52,61,67)]
names(data_1)


set.seed(7601)
x = sort(sample(nrow(data_1),nrow(data_1) * 0.7))
train_log <- data_1[x,]
test_log <- data_1[-x,]


# Applying Kernel PCA
install.packages('kernlab')
library(kernlab)
kpca = kpca(~., data = train_log[-7], kernel = 'rbfdot', features = 2)
training_set_pca = as.data.frame(predict(kpca, train_log))
training_set_pca$Target_Variable = train_log$Target_Variable
test_set_pca = as.data.frame(predict(kpca, test_log))
test_set_pca$Target_Variable = test_log$Target_Variable

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Target_Variable ~ .,
                 family = binomial,
                 data = training_set_pca)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set_pca[, 3], y_pred)

