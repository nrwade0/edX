library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(rpart.plot)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


############################## QUESTION 1 ##########################
# Split titanic_clean into test and training sets - after running the
#  setup code, it should have 891 rows and 9 variables.
# Set the seed to 42, then use the caret package to create a 20% data
#  partition based on the Survived column. Assign the 20% partition
#  to test_set and the remaining 80% partition to train_set.
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times=1, p=0.2, list=FALSE)
test <- titanic_clean[test_index,]
train <- titanic_clean[-test_index,]

# How many observations are in the training set?
dim(train)

# How many observations are in the test set?
dim(test)  

# What proportion of individuals in the training set survived?
mean(train$Survived == 1)


############################## QUESTION 2 ##########################
# The simplest prediction method is randomly guessing the outcome
#  without using additional predictors. These methods will help us
#  determine whether our machine learning algorithm performs better
#  than chance. How accurate are two methods of guessing Titanic
#  passenger survival?
# Set the seed to 3. For each individual in the test set, randomly
#  guess whether that person survived or not by sampling from the
#  vector c(0,1). Assume that each person has an equal chance of
#  surviving or not surviving.
# What is the accuracy of this guessing method?
set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test), replace = TRUE)
mean(guess == test$Survived)


############################## QUESTION 3a ##########################
# Use the training set to determine whether members of a given sex were
#  more likely to survive or die. Apply this insight to generate
#  survival predictions on the test set.

# What proportion of training set females survived?
x <- train %>% filter(Sex == 'female')
mean(x$Survived==1)

# What proportion of training set males survived?
x <- train %>% filter(Sex == 'male')
mean(x$Survived==1)

  
############################## QUESTION 3b ##########################
# Predict survival using sex on the test set: if the survival rate for
#  a sex is over 0.5, predict survival for all individuals of that
#  sex, and predict death if the survival rate for a sex is under 0.5.
  
# What is the accuracy of this sex-based prediction method on the
#  test set?
sex_model <- ifelse(test$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test$Survived)    # calculate accuracy


############################## QUESTION 4a ##########################
# In which class(es) (Pclass) were passengers more likely to survive
#  than die?
test %>% group_by(Pclass) %>%
  summarize(p1 = mean(Survived==1))


############################## QUESTION 4b ##########################
# Predict survival using passenger class on the test set: predict
#  survival if the survival rate for a class is over 0.5, otherwise
#  predict death.
# What is the accuracy of this class-based prediction method on the
#  test set?
class_model <- ifelse(test$Pclass == 1, 1, 0)
mean(class_model == test$Survived)


############################## QUESTION 4c ##########################
# Group passengers by both sex and passenger class.
# Which sex and class combinations were more likely to survive
#  than die?
train %>% group_by(Pclass, Sex) %>%
  summarize(prob = mean(Survived==1))


############################## QUESTION 4d ##########################
# Predict survival using both sex and passenger class on the test set.
#  Predict survival if the survival rate for a sex/class combination
#  is over 0.5, otherwise predict death.
# What is the accuracy of this sex- and class-based prediction method
#  on the test set?
class_sex_model <- ifelse( (test$Pclass==1 & test$Sex=='female') | (test$Pclass==2 & test$Sex =='female'), 1, 0)
mean(class_sex_model == test$Survived)


############################## QUESTION 5a ##########################
# Use the confusionMatrix function to create confusion matrices for the
#  sex model, class model, and combined sex and class model. You will
#  need to convert predictions and survival status to factors to use
#  this function.
# What is the "positive" class used to calculate confusion matrix
#  metrics?
confusionMatrix(data=factor(sex_model), reference=factor(test$Survived))
# Sensitivity : 0.873 --- Specificity : 0.739 ---- Accuracy: 0.806
confusionMatrix(data=factor(class_model), reference=factor(test$Survived))
# Sensitivity : 0.855 --- Specificity : 0.464 ---- Accuracy: 0.659
confusionMatrix(data=factor(class_sex_model), reference=factor(test$Survived))
# Sensitivity : 0.991 --- Specificity : 0.551 ---- Accuracy: 0.771


############################## QUESTION 6 ##########################
# Use the F_meas function to calculate 𝐹1 scores for the sex model
#  class model, and combined sex and class model. You will need to
#  convert predictions to factors to use this function.
# Which model has the highest 𝐹1 score?
F_meas(data=factor(sex_model), reference=factor(test$Survived))
F_meas(data=factor(class_model), reference=factor(test$Survived))
F_meas(data=factor(class_sex_model), reference=factor(test$Survived))


############################## QUESTION 7 ##########################
# Set the seed to 1. Train a model using linear discriminant analysis
#  (LDA) with the caret lda method using fare as the only predictor.
# What is the accuracy on the test set for the LDA model?
set.seed(1, sample.kind = "Rounding")
lda_model <- train(Survived ~ Fare, method = "lda", data = train)
confusionMatrix(predict(lda_model, test), test$Survived)

# Set the seed to 1. Train a model using quadratic discriminant analysis
#  (QDA) with the caret qda method using fare as the only predictor.
# What is the accuracy on the test set for the QDA model?
set.seed(1, sample.kind = "Rounding")
qda_model <- train(Survived ~ Fare, method = "qda", data = train)
confusionMatrix(predict(qda_model, test), test$Survived)


############################## QUESTION 8 ##########################
# Set the seed to 1. Train a logistic regression model with the caret
#  glm method using age as the only predictor.
# What is the accuracy on the test set using age as the only predictor?
set.seed(1, sample.kind = "Rounding")
glm_model <- train(Survived ~ Age, method = "glm", data = train)
confusionMatrix(predict(glm_model, test), test$Survived)

# What is the accuracy of a logistic regression model with the caret
#  glm method using four predictors: sex, class, fare, and age?
set.seed(1, sample.kind = "Rounding")
glm_model <- train(Survived ~ Sex+Pclass+Age+Fare, method = "glm", data = train)
confusionMatrix(predict(glm_model, test), test$Survived)

# What is the accuracy of a logistic regression model with the caret
#  glm method using all predictors?
set.seed(1, sample.kind = "Rounding")
glm_model <- train(Survived ~ ., method = "glm", data = train)
confusionMatrix(predict(glm_model, test), test$Survived)


############################## QUESTION 9a ##########################
# Set the seed to 6. Train a kNN model on the training set using caret.
#  Try tuning with k = seq(3, 51, 2).
# What is the optimal value of the number of neighbors k?
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

############################## QUESTION 9b ##########################
# Plot the kNN model to investigate the relationship between the number
#  of neighbors and accuracy on the training set. Of these values of k,
#  which yields the highest accuracy?
ggplot(train_knn)

############################## QUESTION 9c ##########################
# What is the accuracy of the kNN model on the test set?
set.seed(6, sample.kind="Rounding")
confusionMatrix(predict(train_knn, test), test$Survived)


############################## QUESTION 10 ##########################
# Set the seed to 8 and train a new kNN model. Instead of the default
#  training control, use 10-fold cross-validation where each partition
#  consists of 10% of the total.
# What is the optimal value of k using cross-validation?
control <- trainControl(method="cv", number=10, p=0.9)
set.seed(8, sample.kind="Rounding")
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train,
                   trControl = control,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

# What is the accuracy on the test set using the cross-validated kNN model
cv_knn_preds <- predict(train_knn, test)
mean(cv_knn_preds == test$Survived)

############################## QUESTION 11a ##########################
# Set the seed to 10. Use caret to train a decision tree with the rpart
#  method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
# What is the optimal value of the complexity parameter (cp)?
set.seed(10, sample.kind="Rounding")
train_rpart <- train(Survived ~ .,
                   method = "rpart",
                   data = train,
                   tuneGrid = data.frame(cp=seq(from=0, to=0.05, by=0.002)))
train_rpart$bestTune

# What is the accuracy of the decision tree model on the test set?
rpart_preds <- predict(train_rpart, test)
mean(rpart_preds == test$Survived)

############################## QUESTION 11b ##########################
# Inspect the final model and plot the decision tree:
plot(train_rpart$finalModel, margin=0.2)
text(train_rpart$finalModel)

############################## QUESTION 11c ##########################
# Look at specific cases of survival
rpart.plot(train_rpart)


############################## QUESTION 12 ##########################
# Set the seed to 14. Use the caret train function with the rf method
#  to train a random forest. Test values of mtry ranging from 1 to 7.
#  Set ntree to 100.
# What mtry value maximizes accuracy?
set.seed(14, sample.kind="Rounding")
fit <- train(Survived ~ .,
             method = "rf",
             ntree=100,
             data=train,
             tuneGrid = data.frame(mtry=seq(1, 7, 1)))
plot(fit)

# What is the accuracy of the random forest model on the test set?
rf_preds <- predict(fit, test)
mean(rf_preds == test$Survived)

# Use varImp on the random forest model object to determine the
#  importance of various predictors to the random forest model.
imp <- varImp(fit)

# What is the most important variable?



