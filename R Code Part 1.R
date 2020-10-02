# ST443 Final Project Part 1

# Clear workspace
rm(list=ls())

###########################################################
######################## Load Data ########################
###########################################################

# Read in player statistics from each season
players_1993 <- read.csv("NBA_per_36m_statistics_1993_94.txt", header = TRUE, sep = ",")
players_1994 <- read.csv("NBA_per_36m_statistics_1994_95.txt", header = TRUE, sep = ",")
players_1995 <- read.csv("NBA_per_36m_statistics_1995_96.txt", header = TRUE, sep = ",")
players_1996 <- read.csv("NBA_per_36m_statistics_1996_97.txt", header = TRUE, sep = ",")
players_1997 <- read.csv("NBA_per_36m_statistics_1997_98.txt", header = TRUE, sep = ",")
players_2013 <- read.csv("NBA_per_36m_statistics_2013_14.txt", header = TRUE, sep = ",")
players_2014 <- read.csv("NBA_per_36m_statistics_2014_15.txt", header = TRUE, sep = ",")
players_2015 <- read.csv("NBA_per_36m_statistics_2015_16.txt", header = TRUE, sep = ",")
players_2016 <- read.csv("NBA_per_36m_statistics_2016_17.txt", header = TRUE, sep = ",")
players_2017 <- read.csv("NBA_per_36m_statistics_2017_18.txt", header = TRUE, sep = ",")
players_2018 <- read.csv("NBA_per_36m_statistics_2018_19.txt", header = TRUE, sep = ",")

# Load the 'dplyr' and 'magrittr' libraries
library(magrittr)
library(dplyr)

# Add "season" variable
players_1993 %<>% mutate(Season = "1993")
players_1994 %<>% mutate(Season = "1994")
players_1995 %<>% mutate(Season = "1995")
players_1996 %<>% mutate(Season = "1996")
players_1997 %<>% mutate(Season = "1997")
players_2013 %<>% mutate(Season = "2013")
players_2014 %<>% mutate(Season = "2014")
players_2015 %<>% mutate(Season = "2015")
players_2016 %<>% mutate(Season = "2016")
players_2017 %<>% mutate(Season = "2017")
players_2018 %<>% mutate(Season = "2018")

# Combine seasons into a single dataset
players_all <- rbind(players_1993, players_1994, players_1995, 
                     players_1996, players_1997, players_2013,
                     players_2014, players_2015, players_2016, 
                     players_2017, players_2018)

# combine season and player name/ID
players_all %<>% mutate(Player = paste(Player, Season, sep = "_"))

###########################################################
######### Create Era 1, Era 2, and 2018 Datasets ##########
###########################################################

# Split datasets into era1 (1993-1997), era2 (2013-2017), and 2018
players_era1 <- players_all %>% filter(Season <= 1997)
players_era2 <- players_all %>% filter(Season >= 2013 & Season < 2018)
players_2018 <- players_all %>% filter(Season == 2018)

# Remove season variable
players_era1 %<>% dplyr::select(-Season)
players_era2 %<>% dplyr::select(-Season)
players_2018 %<>% dplyr::select(-Season)

# Calculate number of predictors (same for all datasets)
(n_predictors <- ncol(players_era1) - 1)

# Calculate number of obs in each dataset
(n_obs_era1 <- nrow(players_era1))
(n_obs_era2 <- nrow(players_era2))
(n_obs_2018 <- nrow(players_2018))

###########################################################
###################### Preprocessing ######################
###########################################################

### Summarize Data 

# Look at the dimensions of the different datasets
dim(players_era1); dim(players_era2); dim(players_2018)

# Look at the different variables and their types
sapply(players_era1, class)
sapply(players_era2, class)
sapply(players_2018, class)

###########################################################

### Look at Response Variable (Pos) 

# Look at the response (Position)
levels(players_era1$Pos); levels(players_era2$Pos); levels(players_2018$Pos)
(n_levels <- nlevels(players_era1$Pos))
(n_comb_levels <- n_levels - 5)

# Exclude players with multiple positions 
# (i.e. Michael Curry, Pos = "SG-SF")
players_era1 %<>% filter(!grepl("-", Pos))
players_era2 %<>% filter(!grepl("-", Pos))
players_2018 %<>% filter(!grepl("-", Pos))

# Drop unused levels from the Pos variable
players_era1$Pos <- droplevels(players_era1$Pos)
players_era2$Pos <- droplevels(players_era2$Pos)
players_2018$Pos <- droplevels(players_2018$Pos)

# Check that we now only have 5 levels
levels(players_era1$Pos); levels(players_era2$Pos); levels(players_2018$Pos)

###########################################################

### Look at Minutes Played 

# Load 'ggplot2' and gridExtra libraries
library(ggplot2)
library(gridExtra)

# Look at the distribution of MP across each dataset
hist_era1 <- ggplot(data = players_era1, aes(MP, fill = cut(MP, 100))) + 
  geom_histogram(bins = 50, show.legend = FALSE) +
  geom_vline(xintercept = 400, linetype = "dashed")

hist_era2 <- ggplot(data = players_era2, aes(MP, fill = cut(MP, 100))) + 
  geom_histogram(bins = 50, show.legend = FALSE) +
  geom_vline(xintercept = 400, linetype = "dashed")

dev.new()
grid.arrange(hist_era1, hist_era2)

# Exclude players with less than 400 minutes per season
players_era1 %<>% filter(MP > 400)
players_era2 %<>% filter(MP > 400)
players_2018 %<>% filter(MP > 400)

# Look at the dimensions of each dataset
dim(players_era1); dim(players_era2); dim(players_2018)

# Load 'tidyverse' library
library(tidyverse)

# Make the row names = the player names
players_era1 %<>% remove_rownames %>% column_to_rownames(var="Player")
players_era2 %<>% remove_rownames %>% column_to_rownames(var="Player")
players_2018 %<>% remove_rownames %>% column_to_rownames(var="Player")

# Look at number of observations now
(n_obs_new_era1 <- nrow(players_era1))
(n_obs_new_era2 <- nrow(players_era2))
(n_obs_new_2018 <- nrow(players_2018))

###########################################################

### Variable Selection (Intuitive) 

# Peak at the data
head(players_era1)
head(players_era2)
head(players_2018)

# Look at summary statistics for each variable
summary(players_era1)
summary(players_era2)
summary(players_2018)

# Remove Age, Team and Rank (they are clearly not relevant)
players_era1 %<>% dplyr::select(-c(Age, Tm, Rk))
players_era2 %<>% dplyr::select(-c(Age, Tm, Rk))
players_2018 %<>% dplyr::select(-c(Age, Tm, Rk))

# Remove efficiency statistics (percentages, made shots)
# We are interested in playing style, not playing efficiency
players_era1 %<>% dplyr::select(-c(FG, FG., X3P, X3P., X2P, X2P., FT, FT.))
players_era2 %<>% dplyr::select(-c(FG, FG., X3P, X3P., X2P, X2P., FT, FT.))
players_2018 %<>% dplyr::select(-c(FG, FG., X3P, X3P., X2P, X2P., FT, FT.))

###########################################################

### Variable Selection (Multicollinearity) 

# Load 'ggcorrplot' library
library(ggcorrplot)

# Create correlation matrix
cor.mat.era1 <- cor(players_era1[,-1])
cor.mat.era2 <- cor(players_era2[,-1])

# Visualize correlation matrix
cor_era1 <- ggcorrplot(round(cor.mat.era1, 1), hc.order = TRUE, type = "lower", lab = TRUE)
cor_era2 <- ggcorrplot(round(cor.mat.era2, 1), hc.order = TRUE, type = "lower", lab = TRUE)

dev.new()
grid.arrange(cor_era1, cor_era2, ncol = 2)

# Display predictors with correlations >= 0.7
(highCor_era1 <- round(cor.mat.era1, 3) %>%
  as.data.frame() %>% mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% arrange(desc(value)) %>%
  group_by(value) %>% filter(value >= 0.7 & value != 1) %>% 
  filter(row_number()==1) %>% as.data.frame())

(highCor_era2 <- round(cor.mat.era2, 3) %>%
  as.data.frame() %>% mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>% arrange(desc(value)) %>%
  group_by(value) %>% filter(value >= 0.7 & value != 1) %>% 
  filter(row_number()==1) %>% as.data.frame())

# Remove highly correlated predictors
players_era1 %<>% dplyr::select(-c(ORB, DRB, G, GS, PTS, FGA))
players_era2 %<>% dplyr::select(-c(ORB, DRB, G, GS, PTS, FGA))
players_2018 %<>% dplyr::select(-c(ORB, DRB, G, GS, PTS, FGA))

###########################################################

### Additional Preprocessing Steps 

# Check if there are any "NA" values
sum(is.na(players_era1)) 
sum(is.na(players_era2)) 
sum(is.na(players_2018)) 

# Make sure the data is balanced
cbind(freq = table(players_era1$Pos), percentage = prop.table(table(players_era1$Pos)) * 100)
cbind(freq = table(players_era2$Pos), percentage = prop.table(table(players_era2$Pos)) * 100)
cbind(freq = table(players_2018$Pos), percentage = prop.table(table(players_2018$Pos)) * 100)

###########################################################

### Train/Test Split 

# Randomly assign observations to training/testing for Era 1
set.seed(1)
trainingRows <- sample(1:nrow(players_era1), 0.7*nrow(players_era1))
era1_train_initial <- players_era1[trainingRows, ]
era1_test_initial <- players_era1[-trainingRows, ]

# Randomly assign observations to training/testing for Era 2
set.seed(1)
trainingRows <- sample(1:nrow(players_era2), 0.7*nrow(players_era2))
era2_train_initial <- players_era2[trainingRows, ]
era2_test_initial <- players_era2[-trainingRows, ]

# Make sure training datasets are balanced
plot(era1_train_initial$Pos)
plot(era2_train_initial$Pos)

# Check dimensions of train and test
dim(era1_train_initial); dim(era1_test_initial)
dim(era2_train_initial); dim(era2_test_initial)

###########################################################

#### Standardizing

# Read in the 'caret' library
library(caret)

# Estimate preprocessing parameters
preproc.param.era1 <- era1_train_initial %>% preProcess(method = c("center", "scale"))
preproc.param.era2 <- era2_train_initial %>% preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
era1_train <- preproc.param.era1 %>% predict(era1_train_initial)
era1_test  <- preproc.param.era1 %>% predict(era1_test_initial)
era1_2018_test <- preproc.param.era1 %>% predict(players_2018)
era2_train <- preproc.param.era2 %>% predict(era2_train_initial)
era2_test  <- preproc.param.era2 %>% predict(era2_test_initial)
era2_2018_test <- preproc.param.era2 %>% predict(players_2018)

###########################################################
######################## Analysis #########################
###########################################################

### Naive Bayes

# Load the 'e1071' library
library(e1071)

# Fit naive bayes on training data
(nb.fit.era1 <- naiveBayes(Pos ~ ., data = era1_train))
(nb.fit.era2 <- naiveBayes(Pos ~ ., data = era2_train))

# Predict the values on test dataset
nb.pred.era1 <- predict(nb.fit.era1, era1_test)
nb.pred.era2 <- predict(nb.fit.era2, era2_test)

# Calculate misclassification error rate
(nb.mer.era1 <- mean(era1_test$Pos != nb.pred.era1))
(nb.mer.era2 <- mean(era2_test$Pos != nb.pred.era2))
# Result: 0.337 for Era 1 and 0.364 for Era 2

###########################################################

### Multinomial Logistic Regression

# Load the 'nnet' library
library(nnet)

# Train the multinomial model
mlr.fit.era1 <- multinom(Pos ~ ., data = era1_train)
mlr.fit.era2 <- multinom(Pos ~ ., data = era2_train)

# Train the multinomial model (without intercept)
mlr.nointercept.era1 <- multinom(Pos ~ . -1, data = era1_train)
mlr.nointercept.era2 <- multinom(Pos ~ . -1, data = era2_train)

# Load the 'lmtest' library
library(lmtest)

# Perform likelihood ratio test between two models
lrtest(mlr.fit.era1, mlr.nointercept.era1)
lrtest(mlr.fit.era2, mlr.nointercept.era2)
# Result: we should include intercept in both models

# Check the model
summary(mlr.fit.era1)
summary(mlr.fit.era2)

# Load the 'stargazer' library
library(stargazer)

# Calculate significance of coefficients
stargazer(mlr.fit.era1, type = "html", out = "mlr.fit.era1.htm")
# Era 1: X2PA, FTA, and TOV not significant relative to C
stargazer(mlr.fit.era2, type = "html", out = "mlr.fit.era2.htm")
# Era 2: FTA and TOV not significant relative to C

# Predict the values for test dataset
mlr.pred.era1 <- predict(mlr.fit.era1, era1_test)
mlr.pred.era2 <- predict(mlr.fit.era2, era2_test)

# Calculate misclassification error rate
(mlr.mer.era1 <- mean(era1_test$Pos != mlr.pred.era1)) 
(mlr.mer.era2 <- mean(era2_test$Pos != mlr.pred.era2))
# Result: 0.284 for Era 1 and 0.321 for Era 2

###########################################################

### Multinomial Logistic Regression (w/ Lasso Penalty)

# Load the 'glmnet' library
library(glmnet)

# Split x and y variables for test and train datasets
x_era1_train <- era1_train %>% dplyr::select(-Pos) %>% as.matrix()
x_era2_train <- era2_train %>% dplyr::select(-Pos) %>% as.matrix()
x_era1_test  <- era1_test  %>% dplyr::select(-Pos) %>% as.matrix()
x_era2_test  <- era2_test  %>% dplyr::select(-Pos) %>% as.matrix()
y_era1_train <- as.vector(era1_train$Pos)
y_era2_train <- as.vector(era2_train$Pos)

# Perform cross-validation to obtain optimal lambda
set.seed(1)
lasso.fit.era1 <- cv.glmnet(x_era1_train, y_era1_train, family = "multinomial", 
                            type.multinomial = "grouped", alpha = 1)
set.seed(1)
lasso.fit.era2 <- cv.glmnet(x_era2_train, y_era2_train, family = "multinomial", 
                            type.multinomial = "grouped", alpha = 1)

# Plot results
plot(lasso.fit.era1)
plot(lasso.fit.era2)

# Display optimal lambda and one standard error lambda
lasso.fit.era1$lambda.min; lasso.fit.era1$lambda.1se
# Era 1: min = 0.002 and 1se = 0.013
lasso.fit.era2$lambda.min; lasso.fit.era2$lambda.1se
# Era 2: min = 0.002 and 1se = 0.016

# Look at coefficeints using lambda.min
coef(lasso.fit.era1, lasso.fit.era1$lambda.min)
coef(lasso.fit.era2, lasso.fit.era2$lambda.min)
# Result: all variables included for Era 1 and Era 2

# Look at coefficients using lambda.1se
coef(lasso.fit.era1, lasso.fit.era1$lambda.1se)
# Era 1: MP removed
coef(lasso.fit.era2, lasso.fit.era2$lambda.1se)
# Era 2: all variables included

# Make predictions using lambda.min
lasso.min.pred.era1 <- as.factor(predict(lasso.fit.era1, newx = x_era1_test, 
                                     s = "lambda.min", type = "class"))
lasso.min.pred.era2 <- as.factor(predict(lasso.fit.era2, newx = x_era2_test,
                                     s = "lambda.min", type = "class"))

# Calculate misclassification error rate
(lasso.min.mer.era1 <- mean(era1_test$Pos != lasso.min.pred.era1))
(lasso.min.mer.era2 <- mean(era2_test$Pos != lasso.min.pred.era2))
# Result: 0.280 for Era 1 and 0.319 for Era 2

# Make predictions using lambda.1se
lasso.1se.pred.era1 <- as.factor(predict(lasso.fit.era1, newx = x_era1_test, 
                                    s = "lambda.1se", type = "class"))
lasso.1se.pred.era2 <- as.factor(predict(lasso.fit.era2, newx = x_era2_test,
                                    s = "lambda.1se", type = "class"))

# Calculate misclassification error rate
(lasso.1se.mer.era1 <- mean(era1_test$Pos != lasso.1se.pred.era1))
(lasso.1se.mer.era2 <- mean(era2_test$Pos != lasso.1se.pred.era2))
# Result: 0.299 for Era 1 and 0.324 for Era 2

# Calculate difference between lambda.min and lambda.1se MER
lasso.1se.mer.era1 - lasso.min.mer.era1 # 0.019
lasso.1se.mer.era2 - lasso.min.mer.era2 # 0.005

###########################################################

### Linear Discriminant Analysis (LDA)

# Load the 'MASS' library
library(MASS)

# Fit LDA 
(lda.fit.era1 <- lda(Pos ~., data = era1_train))
(lda.fit.era2 <- lda(Pos ~., data = era2_train))

# Predict the values for test dataset
lda.pred.era1 <- predict(lda.fit.era1, era1_test)$class
lda.pred.era2 <- predict(lda.fit.era2, era2_test)$class

# Calculate misclassification error rate
(lda.mer.era1 <- mean(era1_test$Pos != lda.pred.era1))
(lda.mer.era2 <- mean(era2_test$Pos != lda.pred.era2))
# Result: 0.309 for Era 1 and 0.339 for Era 2

###########################################################

### Quadratic Discriminant Analysis (QDA)

# Fit QDA
(qda.fit.era1 <- qda(Pos ~., data = era1_train))
(qda.fit.era2 <- qda(Pos ~., data = era2_train))

# Predict the values for test dataset
qda.pred.era1 <- predict(qda.fit.era1, era1_test)$class
qda.pred.era2 <- predict(qda.fit.era2, era2_test)$class

# Calculate misclassification error rate
(qda.mer.era1 <- mean(era1_test$Pos != qda.pred.era1))
(qda.mer.era2 <- mean(era2_test$Pos != qda.pred.era2))
# Result: 0.320 for Era 1 and 0.337 for Era 2

###########################################################

### Mixture Discriminant Analysis (MDA)

# Load the 'mda' library
library(mda)

# Fit MDA
set.seed(1)
(mda.fit.era1 <- mda(Pos ~., data = era1_train))
set.seed(1)
(mda.fit.era2 <- mda(Pos ~., data = era2_train))

# Predict the values for the test dataset
mda.pred.era1 <- predict(mda.fit.era1, era1_test, type = "class")
mda.pred.era2 <- predict(mda.fit.era2, era2_test, type = "class")

# Calculate misclassification error rate
(mda.mer.era1 <- mean(era1_test$Pos != mda.pred.era1))
(mda.mer.era2 <- mean(era2_test$Pos != mda.pred.era2))
# Result: 0.326 for Era 1 and 0.324 for Era 2

###########################################################

### Regularised Discriminant Analysis (RDA)

# Load the 'klaR' library
library(klaR)

# Fit RDA
set.seed(1)
(rda.fit.era1 <- rda(Pos ~., data = era1_train))
set.seed(1)
(rda.fit.era2 <- rda(Pos ~., data = era2_train))

# Predict the values for test dataset
rda.pred.era1 <- predict(rda.fit.era1, era1_test)$class
rda.pred.era2 <- predict(rda.fit.era2, era2_test)$class

# Calculate misclassification error rate
(rda.mer.era1 <- mean(era1_test$Pos != rda.pred.era1))
(rda.mer.era2 <- mean(era2_test$Pos != rda.pred.era2))
# Result: 0.291 for Era 1 and 0.326 for Era 2

###########################################################

### K Nearest Neighbors

# Run cross-validation to choose optimal k
tr.out <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(1)
knn.min.fit.era1 <- train(Pos ~ ., method = "knn", tuneGrid = expand.grid(k = 1:50), 
                          trControl = tr.out, metric = "Accuracy", data = era1_train)
set.seed(1)
knn.min.fit.era2 <- train(Pos ~ ., method = "knn", tuneGrid = expand.grid(k = 1:50), 
                          trControl = tr.out, metric = "Accuracy", data = era2_train)

# Plot results
plot(knn.min.fit.era1)
plot(knn.min.fit.era2)

# Display optimal k
knn.min.fit.era1$bestTune
knn.min.fit.era2$bestTune
# Result: K = 15 for Era 1 and K = 6 for Era 2

# Load the 'class' library
library(class)

# Split x and y variables for test and train datasets
x_era1_train <- era1_train %>% dplyr::select(-Pos)
x_era2_train <- era2_train %>% dplyr::select(-Pos)
x_era1_test  <- era1_test  %>% dplyr::select(-Pos)
x_era2_test  <- era2_test  %>% dplyr::select(-Pos)

# Make predictions using optimal k
knn.min.pred.era1 <- knn(x_era1_train, x_era1_test, era1_train$Pos, 
                         k = knn.min.fit.era1$bestTune, prob = TRUE)
knn.min.pred.era2 <- knn(x_era2_train, x_era2_test, era2_train$Pos, 
                         k = knn.min.fit.era2$bestTune, prob = TRUE)

# Calculate misclassification error rate
(knn.min.mer.era1 <- mean(era1_test$Pos != knn.min.pred.era1))
(knn.min.mer.era2 <- mean(era2_test$Pos != knn.min.pred.era2))
# Result: 0.286 for Era 1 and 0.333 for Era 2

# Run cross-validation to choose optimal k (1-SE rule)
tr.out <- trainControl(method = "repeatedcv", number = 10, 
                       repeats = 10, selectionFunction = "oneSE")
set.seed(1)
knn.1se.fit.era1 <- train(Pos ~ ., method = "knn", tuneGrid = expand.grid(k = 1:50), 
                          trControl = tr.out, metric = "Accuracy", data = era1_train)
set.seed(1)
knn.1se.fit.era2 <- train(Pos ~ ., method = "knn", tuneGrid = expand.grid(k = 1:50), 
                          trControl = tr.out, metric = "Accuracy", data = era2_train)

# Display 1-SE k
knn.1se.fit.era1$bestTune
knn.1se.fit.era2$bestTune
# Result: K = 27 for Era 1 and K = 6 for Era 2

# Make predictions using 1-SE k
knn.1se.pred.era1 <- knn(x_era1_train, x_era1_test, era1_train$Pos, 
                         k = knn.1se.fit.era1$bestTune, prob = TRUE)
knn.1se.pred.era2 <- knn(x_era2_train, x_era2_test, era2_train$Pos, 
                         k = knn.1se.fit.era2$bestTune, prob = TRUE)

# Calculate misclassification error rate
(knn.1se.mer.era1 <- mean(era1_test$Pos != knn.1se.pred.era1))
(knn.1se.mer.era2 <- mean(era2_test$Pos != knn.1se.pred.era2))
# Result: 0.299 for Era 1 and 0.332 for Era 2

###########################################################

### Classification Tree

# Load the 'tree' library
library(tree)

# Build a classification tree
tree.fit.era1 <- tree(Pos ~., data = era1_train)
tree.fit.era2 <- tree(Pos ~., data = era2_train)

# Check results
summary(tree.fit.era1)
summary(tree.fit.era2)
# Result: 8 and 11 terminal nodes for Era 1 and Era 2, respectively
# Result: TRB, AST, BLK, and X3PA only variabeles considered for both

# Look at the trees
dev.new()
plot(tree.fit.era1); text(tree.fit.era1, pretty = 0)
plot(tree.fit.era2); text(tree.fit.era2, pretty = 0)

# Predict the values for test dataset
tree.pred.era1 <- predict(tree.fit.era1, era1_test, type = "class")
tree.pred.era2 <- predict(tree.fit.era2, era2_test, type = "class")

# Calculate misclassification error rate
(tree.mer.era1 <- mean(era1_test$Pos != tree.pred.era1))
(tree.mer.era2 <- mean(era2_test$Pos != tree.pred.era2))
# 0.356 for Era 1 and 0.370 for Era 2

# Perform cross-validation to determine optimal tree complexity
set.seed(1)
cv.tree.era1 <- cv.tree(tree.fit.era1, FUN = prune.misclass)
set.seed(1)
cv.tree.era2 <- cv.tree(tree.fit.era2, FUN = prune.misclass)

# Plot cv results
plot(cv.tree.era1)
plot(cv.tree.era2)

# Extract optimal tree complexity
(best.size.era1 <- cv.tree.era1$size[which.min(cv.tree.era1$dev)])
(best.size.era2 <- cv.tree.era2$size[which.min(cv.tree.era2$dev)])
# Result: 5 for Era 1 and 11 for Era 2

# Prune the tree
tree.prune.fit.era1 <- prune.misclass(tree.fit.era1, best = best.size.era1)
tree.prune.fit.era2 <- prune.misclass(tree.fit.era2, best = best.size.era2)

# Look at the pruned trees
dev.new()
plot(tree.prune.fit.era1); text(tree.prune.fit.era1, pretty = 0)
plot(tree.prune.fit.era2); text(tree.prune.fit.era2, pretty = 0)

# Predict the values for test dataset
tree.prune.pred.era1 <- predict(tree.prune.fit.era1, era1_test, type = "class")
tree.prune.pred.era2 <- predict(tree.prune.fit.era2, era2_test, type = "class")

# Calculate misclassification error rate
(tree.prune.mer.era1 <- mean(era1_test$Pos != tree.prune.pred.era1))
(tree.prune.mer.era2 <- mean(era2_test$Pos != tree.prune.pred.era2))
# Result: 0.392 for Era 1 and 0.372 for Era 2

###########################################################

### Bagging

# Load the 'randomForest' library
library(randomForest)

# Perform bagging
set.seed(1)
(tree.bag.fit.era1 <- randomForest(Pos ~., data = era1_train, 
               mtry = ncol(era1_train)-1, importance = TRUE))
set.seed(1)
(tree.bag.fit.era2 <- randomForest(Pos ~., data = era2_train, 
               mtry = ncol(era2_train)-1, importance = TRUE))

# Predict the values for test dataset
tree.bag.pred.era1 <- predict(tree.bag.fit.era1, era1_test, type = "class")
tree.bag.pred.era2 <- predict(tree.bag.fit.era2, era2_test, type = "class")

# Calculate misclassification error rate
(tree.bag.mer.era1 <- mean(era1_test$Pos != tree.bag.pred.era1))
(tree.bag.mer.era2 <- mean(era2_test$Pos != tree.bag.pred.era2))
# Result: 0.288 for Era 1 and 0.302 for Era 2

###########################################################

###  Double Bagging

# Load the 'ipred' and 'rpart' libraries
library(ipred)
library(rpart)

set.seed(1)
scomb <- list(list(model=slda, predict=function(object, newdata)
  + predict(object, newdata)$x))

# Perform double bagging
tree.dblbag.fit.era1 <- bagging(Pos ~., data = era1_train, comb = scomb)
tree.dblbag.fit.era2 <- bagging(Pos ~., data = era2_train, comb = scomb)

# Predict the values for test dataset
tree.dblbag.pred.era1 <- predict(tree.dblbag.fit.era1, newdata = era1_test, type = "class")
tree.dblbag.pred.era2 <- predict(tree.dblbag.fit.era2, newdata = era2_test, type = "class")

# Calculate misclassification error rate
(tree.dblbag.mer.era1 <- mean(era1_test$Pos != tree.dblbag.pred.era1))
(tree.dblbag.mer.era2 <- mean(era2_test$Pos != tree.dblbag.pred.era2))
# Result: 0.290 for Era 1 and 0.335 for Era 2

###########################################################

###  Boosting

# Load the "adabag" library
library(adabag)

# Perform boosting
set.seed(1)
(tree.boost.fit.era1 <- boosting( Pos ~., data = era1_train, mfinal = 100))
set.seed(1)
(tree.boost.fit.era2 <- boosting( Pos ~., data = era2_train, mfinal = 100))

# Predict the values for test dataset
tree.boost.pred.era1 <- as.factor(predict(tree.boost.fit.era1, era1_test, type = "class")$class)
tree.boost.pred.era2 <- as.factor(predict(tree.boost.fit.era2, era2_test, type = "class")$class)

# Calculate misclassification error rate
(tree.boost.mer.era1 <- mean(era1_test$Pos != tree.boost.pred.era1))
(tree.boost.mer.era2 <- mean(era2_test$Pos != tree.boost.pred.era2))
# Result: 0.307 for Era 1 and 0.322 for Era 2

###########################################################

### Random Forest

# Grow random forest
set.seed(1)
(tree.rf.fit.era1 <- randomForest(Pos ~., data = era1_train,
                                  mtry = sqrt(ncol(era1_train) - 1), importance = TRUE))
set.seed(1)
(tree.rf.fit.era2 <- randomForest(Pos ~., data = era2_train,
                                  mtry = sqrt(ncol(era2_train) - 1), importance = TRUE))

# Predict the values for test dataset
tree.rf.pred.era1 <- predict(tree.rf.fit.era1, era1_test, type = "class")
tree.rf.pred.era2 <- predict(tree.rf.fit.era2, era2_test, type = "class")

# Calculate misclassification error rate
(tree.rf.mer.era1 <- mean(era1_test$Pos != tree.rf.pred.era1))
(tree.rf.mer.era2 <- mean(era2_test$Pos != tree.rf.pred.era2))
# Result: 0.276 for Era 1 and 0.279 for Era 2

###########################################################

### Support Vector Classifier (One vs. One)

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed (1)
svc.fit.era1 <- tune(svm, Pos ~., data = era1_train, kernel = "linear", 
                            ranges = list(cost = c(0.1 ,1 ,10 ,100, 1000)))
set.seed (1)
svc.fit.era2 <- tune(svm, Pos ~., data = era2_train, kernel = "linear", 
                            ranges = list(cost = c(0.1 ,1 ,10 ,100, 1000)))

# Check results
summary(svc.fit.era1)
summary(svc.fit.era2)
# Result: cost = 1,000 for Era 1 and cost = 10 for Era 2

# Predict the values for test dataset
svc.pred.era1 <- predict(svc.fit.era1$best.model, era1_test)
svc.pred.era2 <- predict(svc.fit.era2$best.model, era2_test)

# Calculate misclassification error rate
(svc.mer.era1 <- mean(era1_test$Pos != svc.pred.era1))
(svc.mer.era2 <- mean(era2_test$Pos != svc.pred.era2))
# Result: 0.272 for Era 1 and 0.317 for Era 2

###########################################################

### Support Vector Machine (Radial Kernel, One vs. One)

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed (1)
svm.one.fit.era1 <- tune(svm, Pos ~., data = era1_train, kernel = "radial",
                         ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                       gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed (1)
svm.one.fit.era2 <- tune(svm, Pos ~., data = era2_train, kernel = "radial",
                         ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                       gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.one.fit.era1)
summary(svm.one.fit.era2)
# Result: cost = 1 and gamma = 0.1 for both Era 1 and Era 2

# Predict the values for test dataset
svm.one.pred.era1 <- predict(svm.one.fit.era1$best.model, era1_test)
svm.one.pred.era2 <- predict(svm.one.fit.era2$best.model, era2_test)

# Calculate misclassification test error 
(svm.one.mer.era1 <- mean(era1_test$Pos != svm.one.pred.era1)) 
(svm.one.mer.era2 <- mean(era2_test$Pos != svm.one.pred.era2))
# Result: 0.265 for Era 1 and 0.313 for Era 2

###########################################################

### Support Vector Machine (Radial Kernel, One vs. All)

### (1) C vs. All

# Create training datasets
era1_train_C  <- era1_train %>% mutate(Pos = ifelse(Pos %in% 'C' , 1, 0))
era2_train_C  <- era2_train %>% mutate(Pos = ifelse(Pos %in% 'C' , 1, 0))

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed(1)
svm.C.fit.era1 <- tune(svm, Pos ~., data = era1_train_C, kernel = "radial",
                       ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                     gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed(1)
svm.C.fit.era2 <- tune(svm, Pos ~., data = era2_train_C, kernel = "radial",
                       ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                     gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.C.fit.era1)
summary(svm.C.fit.era2)
# Result: cost = 1 and gamma = 0.1

# Predict the values for test dataset
predicted_C_era1 <- predict(svm.C.fit.era1$best.model, era1_test)
predicted_C_era2 <- predict(svm.C.fit.era2$best.model, era2_test)

### (2) PF vs. All

# Create training datasets
era1_train_PF <- era1_train %>% mutate(Pos = ifelse(Pos %in% 'PF', 1, 0))
era2_train_PF <- era2_train %>% mutate(Pos = ifelse(Pos %in% 'PF', 1, 0))

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed(1)
svm.PF.fit.era1 <- tune(svm, Pos ~., data = era1_train_PF, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed(1)
svm.PF.fit.era2 <- tune(svm, Pos ~., data = era2_train_PF, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.PF.fit.era1)
# Era 1: cost = 1 and gamma = 0.1
summary(svm.PF.fit.era2)
# Era 2: cost = 10 and gamma = 1

# Predict the values for test dataset
predicted_PF_era1 <- predict(svm.PF.fit.era1$best.model, era1_test)
predicted_PF_era2 <- predict(svm.PF.fit.era2$best.model, era2_test)

### (3) PG vs. All

# Create training datasets
era1_train_PG <- era1_train %>% mutate(Pos = ifelse(Pos %in% 'PG', 1, 0))
era2_train_PG <- era2_train %>% mutate(Pos = ifelse(Pos %in% 'PG', 1, 0))

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed(1)
svm.PG.fit.era1 <- tune(svm, Pos ~., data = era1_train_PG, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed(1)
svm.PG.fit.era2 <- tune(svm, Pos ~., data = era2_train_PG, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.PG.fit.era1)
summary(svm.PG.fit.era2)
# cost = 1 and gamma = 0.1 for both

# Predict the values for test dataset
predicted_PG_era1 <- predict(svm.PG.fit.era1$best.model, era1_test)
predicted_PG_era2 <- predict(svm.PG.fit.era2$best.model, era2_test)

### (4) SF vs. All

# Create training datasets
era1_train_SF <- era1_train %>% mutate(Pos = ifelse(Pos %in% 'SF', 1, 0))
era2_train_SF <- era2_train %>% mutate(Pos = ifelse(Pos %in% 'SF', 1, 0))

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed(1)
svm.SF.fit.era1 <- tune(svm, Pos ~., data = era1_train_SF, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed(1)
svm.SF.fit.era2 <- tune(svm, Pos ~., data = era2_train_SF, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.SF.fit.era1)
summary(svm.SF.fit.era2)
# Result: cost = 10 and gamma = 1 for both

# Predict the values for test dataset
predicted_SF_era1 <- predict(svm.SF.fit.era1$best.model, era1_test)
predicted_SF_era2 <- predict(svm.SF.fit.era2$best.model, era2_test)

### (5) SG vs. All

# Create training datasets
era1_train_SG <- era1_train %>% mutate(Pos = ifelse(Pos %in% 'SG', 1, 0))
era2_train_SG <- era2_train %>% mutate(Pos = ifelse(Pos %in% 'SG', 1, 0))

# Perform cross-validation to obtain optimal tuning parameter(s)
set.seed(1)
svm.SG.fit.era1 <- tune(svm, Pos ~., data = era1_train_SG, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))
set.seed(1)
svm.SG.fit.era2 <- tune(svm, Pos ~., data = era2_train_SG, kernel = "radial",
                        ranges = list(cost = c(0.1 ,1 ,10 ,100 ,1000),
                                      gamma = c(0.001,0.01,0.1,1,2,3)))

# Check results
summary(svm.SG.fit.era1)
# Era 1: cost = 1 and gamma = 0.1
summary(svm.SG.fit.era2)
# Era 2: cost = 10 and gamma = 0.1

# Predict the values for test dataset
predicted_SG_era1 <- predict(svm.SG.fit.era1$best.model, era1_test)
predicted_SG_era2 <- predict(svm.SG.fit.era2$best.model, era2_test)

### (6) Combine results

# Combine predictions from all five models
predictions_era1 <- cbind(predicted_C_era1, predicted_PF_era1, predicted_PG_era1, 
                          predicted_SF_era1, predicted_SG_era1)
predictions_era2 <- cbind(predicted_C_era2, predicted_PF_era2, predicted_PG_era2, 
                          predicted_SF_era2, predicted_SG_era2)
colnames(predictions_era1) <- c('C', 'PF', 'PG', 'SF', 'SG')
colnames(predictions_era2) <- c('C', 'PF', 'PG', 'SF', 'SG')

# Predict the values for test dataset
svm.all.pred.era1 <- as.factor(colnames(predictions_era1)[apply(predictions_era1, 1, which.max)])
svm.all.pred.era2 <- as.factor(colnames(predictions_era2)[apply(predictions_era2, 1, which.max)])

# Calculate misclassification error rate
(svm.all.mer.era1 <- mean(era1_test$Pos != svm.all.pred.era1))
(svm.all.mer.era2 <- mean(era2_test$Pos != svm.all.pred.era2))
# Result: 0.297 for Era 1 and 0.322 for Era 2

###########################################################
##################  Model Selection  ######################
###########################################################

eval_metrics <- function(predictions, test) {
  mer <- mean(test != predictions)
  accuracy <- 1 - mer
  results <- confusionMatrix(predictions, test, positive = "Pos")
  precision <- mean(results$byClass[,"Pos Pred Value"])
  recall <- mean(results$byClass[,"Recall"])
  F1 <- mean(results$byClass[,"F1"])
  kappa <- results$overall[["Kappa"]]
  return(c(mer, accuracy, precision, recall, F1, kappa))
}

# Naive Bayes
nb.era1 <- eval_metrics(nb.pred.era1, era1_test$Pos)
nb.era2 <- eval_metrics(nb.pred.era2, era2_test$Pos)

# Multinomial Logistic Regression (MLR)
mlr.era1 <- eval_metrics(mlr.pred.era1, era1_test$Pos)
mlr.era2 <- eval_metrics(mlr.pred.era2, era2_test$Pos)

# Linear Discriminant Analysis (LDA)
lda.era1 <- eval_metrics(lda.pred.era1, era1_test$Pos)
lda.era2 <- eval_metrics(lda.pred.era2, era2_test$Pos)

# Quadratic Discriminant Analysis (QDA)
qda.era1 <- eval_metrics(qda.pred.era1, era1_test$Pos)
qda.era2 <- eval_metrics(qda.pred.era2, era2_test$Pos)

# Mixture Discriminant Analysis (MDA)
mda.era1 <- eval_metrics(mda.pred.era1, era1_test$Pos)
mda.era2 <- eval_metrics(mda.pred.era2, era2_test$Pos)

# Regularized Discriminant Analysis (RDA)
rda.era1 <- eval_metrics(rda.pred.era1, era1_test$Pos)
rda.era2 <- eval_metrics(rda.pred.era2, era2_test$Pos)

# K Nearest Neighbors (KNN)
knn.min.era1 <- eval_metrics(knn.min.pred.era1, era1_test$Pos)
knn.min.era2 <- eval_metrics(knn.min.pred.era2, era2_test$Pos)

# Classification Tree
tree.era1 <- eval_metrics(tree.pred.era1, era1_test$Pos)
tree.era2 <- eval_metrics(tree.pred.era2, era2_test$Pos)

# Bagging
tree.bag.era1 <- eval_metrics(tree.bag.pred.era1, era1_test$Pos)
tree.bag.era2 <- eval_metrics(tree.bag.pred.era2, era2_test$Pos)

# Double Bagging
tree.dblbag.era1 <- eval_metrics(tree.dblbag.pred.era1, era1_test$Pos)
tree.dblbag.era2 <- eval_metrics(tree.dblbag.pred.era2, era2_test$Pos)

# Boosting
tree.boost.era1 <- eval_metrics(tree.boost.pred.era1, era1_test$Pos)
tree.boost.era2 <- eval_metrics(tree.boost.pred.era2, era2_test$Pos)

# Random Forest
tree.rf.era1 <- eval_metrics(tree.rf.pred.era1, era1_test$Pos)
tree.rf.era2 <- eval_metrics(tree.rf.pred.era2, era2_test$Pos)

# Support Vector Classifier (SVC) - One vs. One
svc.era1 <- eval_metrics(svc.pred.era1, era1_test$Pos)
svc.era2 <- eval_metrics(svc.pred.era2, era2_test$Pos)

# Support Vector Machine - Radial Kernel, One vs. One
svm.one.era1 <- eval_metrics(svm.one.pred.era1, era1_test$Pos)
svm.one.era2 <- eval_metrics(svm.one.pred.era2, era2_test$Pos)

# Support Vector Machine - Radial Kernel, One vs. All
svm.all.era1 <- eval_metrics(svm.all.pred.era1, era1_test$Pos)
svm.all.era2 <- eval_metrics(svm.all.pred.era2, era2_test$Pos)

# Combine accuracy metrics
tests <- c("NB", "MLR", "LDA", "QDA", "MDA", "RDA", 
           "KNN", "Tree", "Bagging", "Double Bagging", "Boosting", "RF", 
           "SVC", "1v1 SVM (Radial Kernel)", "1vAll SVM (Radial Kernel)")
cols  <- c("Method", "Misclassification Error Rate", "Accuracy", "Precision", "Recall", "F1", "Kappa")

metrics_era1 <- rbind(nb.era1, mlr.era1, lda.era1, qda.era1,
                  mda.era1, rda.era1, knn.min.era1, tree.era1, tree.bag.era1, tree.dblbag.era1,
                  tree.boost.era1, tree.rf.era1, svc.era1, svm.one.era1, svm.all.era1)
metrics_era2 <- rbind(nb.era2, mlr.era2, lda.era2, qda.era2,
                  mda.era2, rda.era2, knn.min.era2, tree.era2, tree.bag.era2, tree.dblbag.era2,
                  tree.boost.era2, tree.rf.era2, svc.era2, svm.one.era2, svm.all.era2)

# Round metrics to 3 decimals
metrics_era1 <- round(metrics_era1, 3)
metrics_era2 <- round(metrics_era2, 3)

# Combine into a dataframe
era1_results <- data.frame(tests, metrics_era1)
era2_results <- data.frame(tests, metrics_era2)
colnames(era1_results) <- cols
colnames(era2_results) <- cols

###########################################################
##############  Interpreting MLR Results  #################
###########################################################

# Check most important variables
imp.var.era1 <- data.frame(varImp(mlr.fit.era1))
imp.var.era1 %<>% rownames_to_column("Variables")
imp.var.era1 %<>% arrange(desc(Overall))
head(imp.var.era1)
# Era 1: TRB, BLK, and STL are three most important vars
imp.var.era2 <- data.frame(varImp(mlr.fit.era2))
imp.var.era2 %<>% rownames_to_column("Variables")
imp.var.era2 %<>% arrange(desc(Overall))
head(imp.var.era2)
# Era 2: TRB, BLK, and STL are three most important vars

# Calculate significance of coefficients
stargazer(mlr.fit.era1, type = "html", out = "mlr.fit.era1.htm")
# Era 1: X2PA, FTA, and TOV not significant relative to C
stargazer(mlr.fit.era2, type = "html", out = "mlr.fit.era2.htm")
# Era 2: FTA and TOV not significant relative to C

# Calculate relative risk ratios
exp(coef(mlr.fit.era1))
exp(coef(mlr.fit.era2))

# Calculate confusion matrix
table(mlr.pred.era1, era1_test$Pos)
table(mlr.pred.era2, era2_test$Pos)

###########################################################
################  Testing on 2018 Data  ###################
###########################################################

# Predict the values for test dataset
### Era 1: Test 1v1 SVM (Radial Kernel) on 2018 data
predicted_2018_era1 <- predict(svm.one.fit.era1$best.model, era1_2018_test)
### Era 2: Test RF on 2018 data
predicted_2018_era2 <- predict(tree.rf.fit.era2, era2_2018_test, type = "class")

# Create confusion matrix
table(predicted_2018_era1, era1_2018_test$Pos)
table(predicted_2018_era2, era2_2018_test$Pos)

# Calculate misclassification error rate
(mer_2018_era1 <- mean(era1_2018_test$Pos != predicted_2018_era1))
(mer_2018_era2 <- mean(era2_2018_test$Pos != predicted_2018_era2))
# Result: 0.476 for Era 1 and 0.289 for Era 2
