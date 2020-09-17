dataset <- read.csv('50_Startups.csv')
dataset <- read.csv(file.choose())

# 
##### Data Preprocessing ####

dataset <- dataset[, 3:5]
# Missing value management
dataset$Age <- ifelse(is.na(dataset$Age),
                      ave(dataset$Age, FUN = function(x) mean(x, na.rm = T)),
                      dataset$Age)
dataset$Salary <- ifelse(is.na(dataset$Salary),
                      ave(dataset$Salary, FUN = function(x) mean(x, na.rm = T)),
                      dataset$Salary)
# Categorical Encoding
dataset$State <- factor(dataset$State, c('California', 'Florida', 'New York'), 
                          labels = c(2,3,1))
dataset$Purchased <- factor(dataset$Purchased, c('No', 'Yes'), 
                          labels = c(0,1))

# Splitting the Dataset
# install.packages("caTools")
library(caTools)
set.seed(seed = 123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)

training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

# Feature Scaling
training_set[, 1:2] <- scale(training_set[, 1:2])
test_set[, 1:2] <- scale(test_set[, 1:2])




##### Linear Regression  ####
names(dataset)
# Simple Linear Regression ####
regressor <- lm(formula = Salary ~ YearsExperience, 
                data = training_set)
summary(regressor)

test_set$pred <- predict(regressor, newdata = test_set)

library(ggplot2)
ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), 
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, training_set)), 
            colour = 'blue') +
  ggtitle('Salary v/s Exp (Training Set)') + 
  xlab('Years of Exp') +
  ylab('Sal')

ggplot() + 
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), 
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, training_set)), 
            colour = 'blue') +
  ggtitle('Salary v/s Exp (Test Set)') + 
  xlab('Years of Exp') +
  ylab('Sal')


# Multiple Linear Regression ####
regressor <- lm(formula = Profit ~ .,
                data = training_set)
summary(regressor)

y_pred <- predict(regressor, test_set)

# Backward elimination 
regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                data = dataset)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = dataset)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend,
                data = dataset)
summary(regressor)


# Polynomial Linear Regression ####
dataset <- dataset[ , 2:3]
lin_reg <- lm(Salary ~ ., data = dataset)
summary(lin_reg)

poly_reg <- lm(Salary ~ poly(Level, degree = 4, raw = T), data = dataset)
summary(poly_reg)

# Smoother curve
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg, data.frame(Level = x_grid))), 
            colour = 'blue') +
  ggtitle('Salary v/s Level - Poly Reg') + 
  xlab('Level') +
  ylab('Sal')

y_pred_lin <- predict(lin_reg, data.frame(Level = 6.5))
y_pred_poly <- predict(poly_reg_same, data.frame(Level = 6.5))

# Support Vector Regression ####
library(e1071)
dataset <- dataset[, -c(1)]
regressor <-  svm(formula = Salary ~ .,
                  data = dataset,
                  type = 'eps-regression')
y_pred <- predict(regressor, data.frame(Level =6.5))

# Smoother curve
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)

ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, data.frame(Level = x_grid))), 
            colour = 'blue') +
  ggtitle('Salary v/s Level - Poly Reg') + 
  xlab('Level') +
  ylab('Sal')


# Decision Trees ####
library(randomForest)
set.seed(1234)
regressor <- randomForest(x = dataset[1], 
                          y = dataset$Salary,
                          ntree = 500)
y_pred <- predict(regressor, data.frame(Level = 6.5))

x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.001)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, data.frame(Level = x_grid))), 
            colour = 'blue') +
  ggtitle('Salary v/s Level - Random Forest') + 
  xlab('Level') +
  ylab('Sal')

summary(regressor)


##### Classification ####
# classification template #####
# create classifer here
prob_pred <- predict(classifer, type = 'response', newdata = test_set[, -3])
y_pred <- ifelse(prob_pred>0.5, 1, 0)

scatter.smooth(prob_pred)
scatter.smooth(y_pred)

cm <- table(test_set$Purchased, y_pred)
confusionMatrix(data = as.factor(y_pred), reference = as.factor(test_set$Purchased))

library(ElemStatLearn)

set <-  training_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.1)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
prob_set <- predict(classifer, type = 'response', grid_set)
y_grid <- ifelse(prob_set>0.5,1 ,0)

plot(set[,-3],
     main = 'Log Reg - Training Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

# Linear Regression ####
classifer <- glm(formula = Purchased ~ ., 
                 family = binomial,
                 data = training_set)

prob_pred <- predict(classifer, type = 'response', newdata = test_set[, -3])
y_pred <- ifelse(prob_pred>0.5, 1, 0)

scatter.smooth(prob_pred)
scatter.smooth(y_pred)

cm <- table(test_set$Purchased, y_pred)
confusionMatrix(data = as.factor(y_pred), reference = as.factor(test_set$Purchased))

library(ElemStatLearn)

set <-  training_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.1)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.1)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
prob_set <- predict(classifer, type = 'response', grid_set)
y_grid <- ifelse(prob_set>0.5,1 ,0)

plot(set[,-3],
     main = 'Log Reg - Training Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))



# K-Nearest Neighbour (K-NN) ####
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# Fitting to Training Set and predicting on Test Set 
library(class)
y_pred <- knn(train = training_set[,-3], 
              test = test_set[,-3],
              cl = training_set$Purchased,
              k = 5)

# prob_pred <- predict(classifer, type = 'response', newdata = test_set[, -3])
# y_pred <- ifelse(prob_pred>0.5, 1, 0)

scatter.smooth(prob_pred)
scatter.smooth(y_pred)

cm <- table(test_set$Purchased, y_pred)
confusionMatrix(data = as.factor(y_pred), reference = as.factor(test_set$Purchased))

library(ElemStatLearn)

set <-  training_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- knn(train = training_set[,-3], 
              test = grid_set,
              cl = training_set$Purchased,
              k = 5)
plot(set[,-3],
     main = 'K-NN - Training Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

# Visualising the Test set results
set <-  test_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- knn(train = training_set[,-3], 
              test = grid_set,
              cl = training_set$Purchased,
              k = 5)
plot(set[,-3],
     main = 'K-NN - Test Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))
# SVM ####
# create classifer here
library(e1071)
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = 'C-classification',
                  kernel = 'linear')

y_pred <- predict(classifier, newdata = test_set[, -3])

cm <- table(test_set$Purchased, y_pred)
confusionMatrix(data = as.factor(y_pred), reference = as.factor(test_set$Purchased))

library(ElemStatLearn)

# Visualisation on Training Set
set <-  training_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, type = 'response', grid_set)

plot(set[,-3],
     main = 'SVM - Training Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

# On Test Set
set <-  test_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, type = 'response', grid_set)

plot(set[,-3],
     main = 'SVM - Test Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))
# Kernel SVM ####
# create classifer here
library(e1071)
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = 'C-classification',
                  kernel = 'linear')

y_pred <- predict(classifier, newdata = test_set[, -3])

cm <- table(test_set$Purchased, y_pred)
confusionMatrix(data = as.factor(y_pred), reference = as.factor(test_set$Purchased))

library(ElemStatLearn)

# Visualisation on Training Set
set <-  training_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, type = 'response', grid_set)

plot(set[,-3],
     main = 'SVM - Training Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))

# On Test Set
set <-  test_set
x1 <- seq(min(set[, 1]) - 1 , max(set[,1]) + 1, by = 0.01)
x2 <- seq(min(set[, 2]) - 1 , max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(x1, x2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, type = 'response', grid_set)

plot(set[,-3],
     main = 'SVM - Test Set',
     xlab = 'Age', ylab = 'Est Sal',
     xlim = range(x1), ylim = range(x2))
contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))
# Naive Bayes ####
# Feature Scaling
training_set[-3] <- scale(training_set[-3])
test_set[-3] <- scale(test_set[-3])

# Fitting Naive Bayes classifier to the Training set
# Create your classifier here
library(e1071)
training_set$Purchased <- as.factor(training_set$Purchased)
test_set$Purchased <- as.factor(test_set$Purchased)

classifier <- naiveBayes(x = training_set[-3],
                         y = training_set$Purchased)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Naive Byes (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Naive Bayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
# Decision Tree Classification ####
# Feature Scaling

# Fitting Naive Bayes classifier to the Training set
# Create your classifier here
library(rpart)
classifier <- rpart(formula = Purchased ~ .,
                    data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

plot(classifier)
text(classifier)

# Random Forest ####
training_set$Purchased <- as.factor(training_set$Purchased)
test_set$Purchased <- as.factor(test_set$Purchased)
# Fitting Random Forest classifier to the Training set
# Create your classifier here
library(randomForest)
classifier <- randomForest(x = training_set[, 1:2],
                           y = training_set[, 3],
                           ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Random Forest (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Random Forest (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



##### Clustering ####
# K-means ####
# K-means clustering

# import dataset
dataset <- read.csv("Mall_Customers.csv")
x <- dataset[, 4:5]

# Using elbow method to find optimal number of clusters
set.seed(6)
wcss <- vector()

for (i  in 1:10) wcss[i] <- sum(kmeans(x, i)$withinss)
plot(1:10, wcss, type = 'b',
     main = 'Clust of clients',
     xlab = ' N Clusters',
     ylab = 'WCSS')

# Applying k-means to the data
set.seed(29)
kmeans <- kmeans(x, centers = 5, iter.max = 300, nstart = 10)

# Visualising the clusters (do not try for more than 2 dimensions)
library(cluster)
clusplot(x, kmeans$cluster,
         shade = T, color = T, labels = 2, plotchar = F, span = T,
         main = 'Clusters of clients', xlab = 'Annual Inc', ylab = 'Spending score')

# Hierarchical Clustering ####
# import dataset
dataset <- read.csv('Mall_Customers.csv')

x <- dataset[, 4:5]

# Using the dendrogram to find the optimal number of clusters
dendrogram <- hclust(dist(x, method = 'euclidean'), method = 'ward.D') # ward.D = minimises the within cluster variance
plot(dendrogram, 
     main = 'Dendrogram',
     xlab = 'Custoemrs',
     ylab = 'Euclidean Dist')


# Fitting hierarchical clustering to the dataset
hc <- hclust(dist(x, method = 'euclidean'), method = 'ward.D')
y_hc <- cutree(hc, k = 5 )

# Visualising the clusters (do not try for more than 2 dimensions)
library(cluster)
clusplot(x, y_hc,
         shade = T, color = T, labels = 2, plotchar = F, span = T,
         main = 'Clusters of clients', xlab = 'Annual Inc', ylab = 'Spending score')

##### Association Rule Learning ####
# Apriori Algo ####
# install.packages("arules")
library(arules)

# Data Preprocessing
dataset <- read.csv(file.choose(), header = F)
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules <- apriori(dataset, parameter = list(support = 0.004, confidence = 0.2))

# Visualising the results
inspect(sort(rules, by= 'lift')[1:10])

# Eclat Association Model ####
library(arules)

# Data Preprocessing
dataset <- read.csv(file.choose(), header = F)
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Eclat on the dataset
rules <- eclat(dataset, parameter = list(support = 0.004, minlen = 2))

# Visualising the results
inspect(sort(rules, by= 'support')[1:10])


##### Reinforcement Learning ####
# Upper Confidence Bound - Reinforcement Learning ####

# import dataset
dataset <- read.csv('Ads_CTR_Optimisation.csv')

# Implementing UCB
N <- nrow(dataset)
d <- length(dataset)
ads_selected <- integer(0)
numbers_of_selections <- integer(d)
sums_of_rewards <- integer(d)
total_reward <- 0

for (n in 1:N){
  ad <- 0
  max_upper_bound <- 0
  for (i in 1:d){
    if(numbers_of_selections[i]>0){
      average_reward <- sums_of_rewards[i] / numbers_of_selections[i]
      delta_i <- sqrt(3/2* log(n) / numbers_of_selections[i] )
      upper_bound <- average_reward + delta_i
    } else{ 
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad <- i
    }
  }
  ads_selected <- append(ads_selected, ad)
  numbers_of_selections[ad] <- numbers_of_selections[ad] + 1
  reward <- dataset[n,ad]
  sums_of_rewards[ad] <- sums_of_rewards[ad] + reward
  total_reward <- total_reward + reward
}

# Visualising the results
hist(ads_selected, col = 'blue',
     main = 'Hist of ad selection',
     xlab = 'Ads',
     ylab = 'Number of times ad was selected')
# Thompon Sampling - Reinforcement Learning ####

# import dataset
dataset <- read.csv('Ads_CTR_Optimisation.csv')
# Implementing Thompson Sampling
N <- nrow(dataset)
d <- length(dataset)
ads_selected <- integer(0)
numbers_of_reward_1 <- integer(d)
numbers_of_reward_0 <- integer(d)
total_reward <- 0

for (n in 1:N){
  ad <- 0
  max_random <- 0
  for (i in 1:d){
    random_beta <- rbeta(n = 1,
                         shape1 = numbers_of_reward_1[i] + 1,
                         shape2 = numbers_of_reward_0[i] + 1)
    if(random_beta > max_random){
      max_random = random_beta
      ad <- i
    }
  }
  ads_selected <- append(ads_selected, ad)
  reward <- dataset[n,ad]
  if(reward == 1){
    numbers_of_reward_1[ad] <- numbers_of_reward_1[ad] + 1
  } else {
    numbers_of_reward_0[ad] <- numbers_of_reward_0[ad] + 1
  }
  total_reward <- total_reward + reward
}

# Visualising the results
hist(ads_selected, col = 'blue',
     main = 'Hist of ad selection',
     xlab = 'Ads',
     ylab = 'Number of times ad was selected')

#### Natural Language Processing ####
# Bag of Words ####
# Import dataset
dataset <- read.csv("Restaurant_Reviews.tsv", sep = "\t", quote = "", stringsAsFactors = F)

# Cleaning the texts
# install.packages('tm')
library(tm)
library(SnowballC)
corpus <- VCorpus(VectorSource(dataset$Review))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# Creating Bag of Words model
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)

df <- as.data.frame(as.matrix(dtm))
df$Liked <- dataset$Liked

# Encoding the dependent var as factor
df$Liked <- as.factor(df$Liked)

# Train/Test Split
library(caTools)
set.seed(seed = 123)
split <- sample.split(df$Liked, SplitRatio = 0.8)

training_set <- subset(df, split == T)
test_set <- subset(df, split == F)

library(randomForest)
classifier <- randomForest(x = training_set[-692],
                           y = training_set$Liked,
                           ntree = 500)

# Predicting the Test set results
y_pred <- predict(classifier, newdata = test_set[-692])

# Confusion matrix
cm = table(test_set[, 692], y_pred)

#### Artificial Neural Network ####
# Artificial Neural Network

# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)  
# ip - to connect to the remote computer instance
# hidden - parameter to provide both No. of hidden layers and No. of nodes in each layer => hidden(x1, x2) - x1  no of nodes in layer 1; x2 no of nodes in layer 2.
# No. of hidden nodes - guide only - avg of input vars and output vars => (10+1) / 2 = 5.5 ~ 6
# No. of hidden layers - depending on complexity of model
classifier <- h2o.deeplearning(y = 'Exited',
                               training_frame = as.h2o(training_set),
                               activation = 'Rectifier',
                               hidden = c(6, 6),
                               epochs = 100,
                               train_samples_per_iteration = -2)

# Predicting the Test set results
prob_pred <-  h2o.predict(classifier, newdata = as.h2o(test_set[-11]))
y_pred <- prob_pred > 0.5
y_pred <- as.vector(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)

h2o.shutdown()
#### Dimensionality Reduction ####
# PCA (with SVM for Classification) ####
# Importing the dataset
dataset = read.csv('Wine.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying PCA
library(caret)
library(e1071)
pca <- preProcess(x = training_set[-14],
                  method = 'pca',
                  pcaComp = 2)
training_set_orgnl <- training_set
training_set <- predict(pca, training_set)
training_set <- training_set[, c(2,3,1)]
test_set_orgnl <- test_set
test_set <- predict(pca, test_set)
test_set <- test_set[, c(2,3,1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3' , 
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3' , 
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))
# LDA (with SVM for Classification) ####

# Importing the dataset
dataset = read.csv('Wine.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying LDA
library(MASS)
lda <- lda(formula = Customer_Segment ~ . ,
           data = training_set)
training_set <- as.data.frame(predict(object = lda, training_set))
training_set <- training_set[, c(5,6,1)]
test_set <- as.data.frame(predict(lda, test_set))
test_set <- test_set[, c(5,6,1)]


# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD1',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3' , 
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', 
                                         ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3' , 
                                  ifelse(set[, 3] == 1, 'green4', 'red3')))
# Kernel PCA ####

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Applying Kernel PCA
# install.packages("kernlab")
library(kernlab)
kpca <- kpca(~. , data = training_set[-3], kernel = 'rbfdot', features = 2)

training_set_pca <- as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased <- training_set$Purchased
test_set_pca <- as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased <- test_set$Purchased

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set_pca)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set_pca[, 3], y_pred > 0.5)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.1)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.1)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
#### Model Performance ####
# k-fold cross validation  & Grid Search - on SVM Classification ####
# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)


# Applying k-fold cross validation 
library(caret)
folds <- createFolds(training_set$Purchased, k = 10)
cv <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy <- (cm[1,1] + cm[2,2]) / (sum(cm))
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))

# Applying Grid Search to find the best parameters
library(caret)
classifier <- train(form = Purchased ~ ., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune

#### XGBoost applied alongside ANN ####
# Importing the dataset
dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting XGBoost to the training set
# install.packages('xgboost')
library(xgboost)
classifier <- xgboost(data = as.matrix(training_set[-11]),
                      label = training_set$Exited,
                      nrounds = 10)

# Applying k-fold cross validation 
library(caret)
folds <- createFolds(training_set$Exited, k = 10)
cv <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- xgboost(data = as.matrix(training_set[-11]),
                        label = training_set$Exited,
                        nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred <- y_pred >= 0.5
  cm = table(test_fold[, 11], y_pred)
  accuracy <- (cm[1,1] + cm[2,2]) / (sum(cm))
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))
