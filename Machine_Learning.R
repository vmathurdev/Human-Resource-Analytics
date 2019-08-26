# Current work directory
getwd()
#Set the desired work directory
setwd("C:/Users/samsung/Desktop/homework")
#validate the set directory as above
getwd()
# Loading the data from CSV file
HR <- read.csv("./HR_Data.csv")
#Structure of the data (Pre-analysis)
str(HR)
# Question - 1 

# Create training (60%) and test (40%) subsets of the HR dataframe

# Setting the dependent variable as a factor 

HR$Left_company <- as.factor(HR$Left_company)
str(HR)
#As mentioned in the question set the random number seed to 1234 
set.seed(1234)

# Assigning the row numbers

n.train <- round(nrow(HR)* 0.6,0) 

# This produces the list of rows for the training set

sub <- sample(nrow(HR),n.train) 

head(sub)

# Creating the training subset

HR.train <- HR[sub,] 

# All the remaining data gets assigned to test set
HR.test <- HR[-sub,] 

# Check the dimensions of train dataset
dim(HR.train)

# Check the dimensions of test dataset
dim(HR.test)

# check the proportion of class variable
table(table(HR.train$Left_company))
table(table(HR.test$Left_company))

# Question - 2
# A Part
set.seed(333)
library(rpart)
#modelling the data using decision tree model and rpart package 
fit.tree <- rpart(Left_company ~  ., data=HR.train, method = "class",control=rpart.control(cp=-1))
printcp(fit.tree)

#Observation - As can be seen when we have the Cp value = -1, there is a strong evidence of overfitting in the model

#B Part

set.seed(333)
#modelling the data using decision tree model and rpart package 
fit.tree <- rpart(Left_company~  ., data=HR.train, method = "class",control=rpart.control(cp=0.0013941))
printcp(fit.tree)
#Observation - As can be seen when we have the Cp value = min, there is no evidence of overfitting in the model. Where min = the CP value corresponding to the smallest xerror.

install.packages("rpart.plot")
library(rpart.plot)
#Getting the decision trees for the cp value designated as above  
rpart.plot(fit.tree, type=1, under= TRUE)
plot(fit.tree, uniform= TRUE, main="Classification of employees left company", margin =.2)
text(fit.tree, use.n = TRUE, all = TRUE, cex= .8)

#Question - 3
## Training a model on the data
install.packages("C50")
library(C50)
HR.model <- C5.0(Left_company ~  .,control=C5.0Control(minCases=25), data=HR.train)

## display detailed information about the tree
summary(HR.model)

## Evaluating model performance for test dataset
HR.pred <- predict(HR.model, HR.test, type="class")

install.packages("caret")
library(caret)

# creating the confusion matrix to get the sensitivity and accuracy of the data using the package "caret"
confusionMatrix(HR.pred,HR.test$Left_company, positive="Did not leave")

## Evaluating model performance for training dataset
HR.pred1 <- predict(HR.model, HR.train, type="class")

confusionMatrix(HR.pred1,HR.train$Left_company, positive="Did not leave")

#Alternate method using rpart instead of C50 package 
## Training a model on the data
fit.tree <- rpart(Left_company~  ., data=HR.train, method = "class",control=rpart.control(cp=0.0013941))

## display detailed information about the tree
summary(fit.tree)

## Evaluating model performance for test dataset
HR.pred <- predict(fit.tree, HR.test, type="class")

install.packages("caret")
library(caret)

# creating the confusion matrix to get the sensitivity and accuracy of the data using the package "caret"
confusionMatrix(HR.pred,HR.test$Left_company, positive="Did not leave")

## Evaluating model performance for training dataset
HR.pred1 <- predict(fit.tree, HR.train, type="class")

confusionMatrix(HR.pred1,HR.train$Left_company, positive="Did not leave")


## ===========================================================
# Create ROC curve for the decision tree. To understand whether the model is overfitting
## ===========================================================
install.packages("plotROC")
library(plotROC)
pr.test.tree <- predict(HR.model,newdata = HR.test,type = "prob")[,2]

roc.data <- data.frame(D = as.integer(HR.test$Left_company), M=pr.test.tree)
roc.data$D <- as.integer(roc.data$D) - 1
plt <- ggplot(roc.data, aes(m = M, d = D)) + 
  geom_roc(n.cuts = 5, labelsize = 3,labelround = 2) +
  style_roc(xlab = "1 - Specificity",theme = theme_grey ) +
  ggtitle("ROC Curve for HR to predict the employees who left company") + 
  geom_segment(aes( x=0, y=0, xend=1, yend=1),col="red")

plt +  annotate("text", x = .75, y = .25, label = paste("Area under the curve =", round(calc_auc(plt)$AUC, 3.5)))


#Question - 3C

#Creating a new data frame
a <- data.frame(satisfaction_level =  0.5 ,              
                last_evaluation = 0.9,
                number_project = 3,
                average_montly_hours = 144,
                time_spend_company = 6 ,
                Work_accident = 0,
                promotion_last_5years = 1 ,
                department = "sales",
                salary= "medium")

#checking the structure of the newly created dataframe
str(a)
#Setting the predictions on the model developed using fittree
HR.pred2 <- predict(fit.tree,newdata=a, type="class")

#Setting the type to prob to get the probability of employee leaving the company 
probability <- predict(fit.tree,newdata=a, type="prob")
probability
