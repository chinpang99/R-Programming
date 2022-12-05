# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using Backward Elimination

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) #Not compulsory, can use training set. Just taking the whole dataset to have a clearer image of which variable is significant
summary(regressor) # Step 2: Find and remove the variable's P-Value > SL (0.05)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset) #Step 3: Remove the variable
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset) #This can be identified as best team in predicting profit as Marketing Spend P-value is just 0.06
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)


# BONUS CONTENT
#----------------
# Automatically Backward Elimination
backwardElimination <- function(x, sl) { # Create a function to automatically 
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}
SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)