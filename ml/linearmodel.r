# Algorithms
if(FALSE){
  "
  Linear Algorithms:
  
      Algorithm 1: Linear Regression
      Algorithm 2: Logistic Regression
      Algorithm 3: Linear Discriminant Analysis
  
  Nonlinear Algorithms:
  
      Algorithm 4: Classification and Regression Trees
      Algorithm 5: Naive Bayes
      Algorithm 6: K-Nearest Neighbors
      Algorithm 7: Learning Vector Quantization
      Algorithm 8: Support Vector Machines
  
  Ensemble Algorithms:
  
      Algorithm 9: Bagged Decision Trees and Random Forest
      Algorithm 10: Boosting and AdaBoost
  
  Bonus #1: Gradient Descent
  
  
      Logistic regression
      Linear discriminant analysis
      Mixture disriminant analysis
      Quadratic Discriminant Analysis
      Neural Network
      Flexible Discriminant Analysis
      Support Vector Machine
      k-Nearest Neighbors
      Naive Bayes
      Classification and Regression Trees (CART)
      C4.5
      PART
      Bagging CART
      Random Forest
      Gradient Boosted Machine
  "
}

# -Linear Regression Assignment- #
# https://www.kaggle.com/c/job-salary-prediction

train <- read.csv("train.csv", as.is = TRUE)
train$ContractType <- as.factor(train$ContractType)
train$ContractTime <- as.factor(train$ContractTime)
train$Category <- as.factor(train$Category)
train$SourceName <- as.factor(train$SourceName)
train$Company <- as.factor(train$Company)
train$LocationNormalized <- as.factor(train$LocationNormalized)

# ---Data Exploration--- #
#  利用分页迅速可视化各各对应关系
library(ggplot2)
ggplot(train, aes(SalaryNormalized)) + geom_bar() + facet_grid(ContractType ~.)
ggplot(train, aes(SalaryNormalized)) + geom_bar() + facet_grid(ContractTime ~.)

tapply(train$SalaryNormalized, train$Category, mean)
table(train$Category)
tapply(train$SalaryNormalized, train$SourceName, mean)
table(train$SourceName)

# output suppressed
table(train$Company)
table(train$LocationNormalized)


# ---Creating New Features--- #
# 衍生出来新的特征是需要滴 
con <- file("location_tree.txt", "r")
tree <- readLines(con)
close(con)

for (i in 1:nrow(train)) {
    # get city name
    loc <- train$LocationNormalized[i]

    # find the first line in the tree in which that city name appears
    line.id <- which(grepl(loc, tree))[1]

    # use regular expressions to pull out the broad location
    r <- regexpr("~.+?~", tree[line.id])
    match <- regmatches(tree[line.id], r)

    # store the broad location
    train$Location[i] <- gsub("~", "", match)
}

train$Location <- as.factor(train$Location)
table(train$Location)
train$London <- as.factor(ifelse(train$Location == "London", "Yes", "No"))
train$TitleSenior <- as.factor(ifelse(grepl("[Ss]enior", train$Title), "Yes", "No"))
train$TitleManage <- as.factor(ifelse(grepl("[Mm]anage", train$Title), "Yes", "No"))
train$DescripSenior <- as.factor(ifelse(grepl("[Ss]enior", train$FullDescription), "Yes", "No"))
train$DescripManage <- as.factor(ifelse(grepl("[Mm]anage", train$FullDescription), "Yes", "No"))


# Modeling: Intercept-only
# 分一�?
set.seed(100)
train.index <- sample(10000, 7000, replace = FALSE)
tr <- train[train.index, ]
val <- train[-train.index, ]

sqrt(mean((mean(tr$SalaryNormalized) - val$SalaryNormalized)^2))

# Modeling: Linear Models
# lm模型要整好多�?
lm.fit1 <- lm(SalaryNormalized ~ ContractType + ContractTime + Category, data = tr)
summary(lm.fit1)

lm.pred1 <- predict(lm.fit1, newdata = val)

# 关键指标误差
sqrt(mean((lm.pred1 - val$SalaryNormalized)^2))

lm.fit2 <- lm(SalaryNormalized ~ ContractType + ContractTime + Category + Location, 
    data = tr)
summary(lm.fit2)

lm.pred2 <- predict(lm.fit2, newdata = val)
sqrt(mean((lm.pred2 - val$SalaryNormalized)^2))

lm.fit3 <- lm(SalaryNormalized ~ ContractType + ContractTime + Category + London, 
    data = tr)
summary(lm.fit3)

lm.pred3 <- predict(lm.fit3, newdata = val)
sqrt(mean((lm.pred3 - val$SalaryNormalized)^2))

lm.fit4 <- lm(SalaryNormalized ~ ContractType + ContractTime + Category + London + 
    TitleSenior + TitleManage + DescripSenior + DescripManage, data = tr)

summary(lm.fit4)
lm.pred4 <- predict(lm.fit4, newdata = val)
sqrt(mean((lm.pred4 - val$SalaryNormalized)^2))


# Modeling: Ridge Regression
# build model matrices for train and validation
library(glmnet)
x.tr <- model.matrix(SalaryNormalized ~ ContractType + ContractTime + Category + 
    London + TitleSenior + TitleManage + DescripSenior + DescripManage, data = tr)[, 
    -1]
y.tr <- tr$SalaryNormalized
x.val <- model.matrix(SalaryNormalized ~ ContractType + ContractTime + Category + 
    London + TitleSenior + TitleManage + DescripSenior + DescripManage, data = val)[, 
    -1]
y.val <- val$SalaryNormalized

# CV to obtain best lambda
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha = 0)
plot(rr.cv)

rr.bestlam <- rr.cv$lambda.min
rr.goodlam <- rr.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar = "lambda", label = TRUE)

rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.val)
sqrt(mean((rr.pred - y.val)^2))

# Modeling: Lasso
# CV to obtain best lambda
set.seed(10)
las.cv <- cv.glmnet(x.tr, y.tr, alpha = 1)
plot(las.cv)

las.bestlam <- las.cv$lambda.min
las.goodlam <- las.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
las.fit <- glmnet(x.tr, y.tr, alpha = 1)
plot(las.fit, xvar = "lambda", label = TRUE)

las.pred <- predict(las.fit, s = las.bestlam, newx = x.val)
sqrt(mean((las.pred - y.val)^2))


# Modeling: Forward Stepwise
library(leaps)
fwd.fit <- regsubsets(SalaryNormalized ~ ContractType + ContractTime + Category + 
    London + TitleSenior + TitleManage + DescripSenior + DescripManage, data = tr, 
    nvmax = 36, method = "forward")
plot(fwd.fit, scale = "adjr2")

# loop through models of each size and compute test RMSE for each model
# (clever approach from ISLR page 248)
fwd.errors <- rep(NA, 36)
val.mat <- model.matrix(SalaryNormalized ~ ContractType + ContractTime + Category + 
    London + TitleSenior + TitleManage + DescripSenior + DescripManage, data = val)
for (i in 1:36) {
    # extract the coefficients from the best model of that size
    coefi <- coef(fwd.fit, id = i)
    # multiply them into the appropriate columns of the test model matrix to
    # predict
    pred <- val.mat[, names(coefi)] %*% coefi
    # compute the test MSE
    fwd.errors[i] <- sqrt(mean((y.val - pred)^2))
}

# find the best model
fwd.errors

min(fwd.errors)
which.min(fwd.errors)


# ---Test Model Against Solution--- #
solution <- read.csv("solution.csv", as.is = TRUE)
solution$ContractType <- as.factor(solution$ContractType)
solution$ContractTime <- as.factor(solution$ContractTime)
solution$Category <- as.factor(solution$Category)
solution$SourceName <- as.factor(solution$SourceName)
solution$Company <- as.factor(solution$Company)
solution$LocationNormalized <- as.factor(solution$LocationNormalized)

for (i in 1:nrow(solution)) {
    loc <- solution$LocationNormalized[i]
    line.id <- which(grepl(loc, tree))[1]
    r <- regexpr("~.+?~", tree[line.id])
    match <- regmatches(tree[line.id], r)
    solution$Location[i] <- gsub("~", "", match)
}

solution$Location <- as.factor(solution$Location)
solution$London <- as.factor(ifelse(solution$Location == "London", "Yes", "No"))
solution$TitleSenior <- as.factor(ifelse(grepl("[Ss]enior", solution$Title), "Yes", "No"))
solution$TitleManage <- as.factor(ifelse(grepl("[Mm]anage", solution$Title), "Yes", "No"))
solution$DescripSenior <- as.factor(ifelse(grepl("[Ss]enior", solution$FullDescription), "Yes", "No"))
solution$DescripManage <- as.factor(ifelse(grepl("[Mm]anage", solution$FullDescription), "Yes", "No"))

pt.index <- which(solution$Category == "Part time Jobs")
solution$Category[pt.index] <- "Admin Jobs"
solution$Category <- factor(solution$Category)
sqrt(mean((mean(train$SalaryNormalized) - solution$SalaryNormalized)^2))

lm.fit5 <- lm(SalaryNormalized ~ ContractType + ContractTime + Category + London + 
    TitleSenior + TitleManage + DescripSenior + DescripManage, data = train)
summary(lm.fit5)

lm.pred5 <- predict(lm.fit5, newdata = solution)
sqrt(mean((lm.pred5 - solution$SalaryNormalized)^2))


