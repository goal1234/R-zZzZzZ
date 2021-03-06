devtools::install_github("thomasp85/lime")

load("data_15_16.RData")
# configure multicore
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(caret)
set.seed(42)
index <- createDataPartition(data_15_16$Happiness.Score.l, p = 0.7, list = FALSE)
train_data <- data_15_16[index, ]
test_data  <- data_15_16[-index, ]
set.seed(42)
model_mlp <- caret::train(Happiness.Score.l ~ .,
                         data = train_data,
                         method = "mlp",
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 5,
                                                  verboseIter = FALSE))

library(lime)
explain <- lime(train_data, model_mlp, bin_continuous = TRUE, n_bins = 5, n_permutations = 1000)

pred <- data.frame(sample_id = 1:nrow(test_data),
                   predict(model_mlp, test_data, type = "prob"),
                   actual = test_data$Happiness.Score.l)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")

  library(tidyverse)
  pred_cor <- filter(pred, correct == "correct")
  pred_wrong <- filter(pred, correct == "wrong")

  test_data_cor %
    mutate(sample_id = 1:nrow(test_data)) %>%
    filter(sample_id %in% pred_cor$sample_id) %>%
    sample_n(size = 3) %>%
    remove_rownames() %>%
    tibble::column_to_rownames(var = "sample_id") %>%
    select(-Happiness.Score.l)

  test_data_wrong %
    mutate(sample_id = 1:nrow(test_data)) %>%
    filter(sample_id %in% pred_wrong$sample_id) %>%
    sample_n(size = 3) %>%
    remove_rownames() %>%
    tibble::column_to_rownames(var = "sample_id") %>%
    select(-Happiness.Score.l)

explanation_cor <- explain(test_data_cor, n_labels = 3, n_features = 5)
explanation_wrong <- explain(test_data_wrong, n_labels = 3, n_features = 5)

plot_features(explanation_cor, ncol = 3)

tibble::glimpse(explanation_cor)
pred %>%
    filter(sample_id == 22)

train_data %>%
  gather(x, y, Economy..GDP.per.Capita.:Dystopia.Residual) %>%
    ggplot(aes(x = Happiness.Score.l, y = y)) +
      geom_boxplot(alpha = 0.8, color = "grey") +
      geom_point(data = gather(test_data[22, ], x, y, Economy..GDP.per.Capita.:Dystopia.Residual), color = "red", size = 3) +
      facet_wrap(~ x, scales = "free", ncol = 4)

as.data.frame(explanation_cor[1:9]) %>%
  filter(case == "22")

sessionInfo()


############################################################################################################################
#---get a logistic Regression ---#
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))
sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

library(Amelia)
missmap(training.data.raw, main = 'Missing values vs observed')
data <- subset(training.data.raw, raw, select = c(2,3,5,6,7,8,12))
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = T)
is.factor(data$Sex)
is.factor(data$Embarked)

contrasts(data$Sex)
contrasts(data$Embarked)

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

#Model fitting
train <- data[1:800, ]
test <- data[801:889, ]
model <- glm(Survived ~., family = binomial(link = 'logit'), data = train)
summary(model)

anova(model, test = 'Chisq')
library(pscl)
pR2(model)

fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc$y.values[[1]]
auc

#---------------------------------Dealing with unbalanced data in machine learning------------------------#
library(caret)
summary(bc_data$classes)

#Modeling the original unbalanced data

set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]

set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  verboseIter = FALSE))

final <- data.frame(actual = test_data$classes,
                    predict(model_rf, newdata = test_data, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")

cm_original <- confusionMatrix(final$predict, test_data$classes)

#---Under-sampling---#
#向上采样trainControl()中进�?
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "down")

set.seed(42)
model_rf_under <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)

1
final_under <- data.frame(actual = test_data$classes,
                    predict(model_rf_under, newdata = test_data, type = "prob"))
final_under$predict <- ifelse(final_under$benign > 0.5, "benign", "malignant")

cm_under <- confusionMatrix(final_under$predict, test_data$classes)

#---oversampling---#

ctrl <- trainControl(method = 'repeatedcv',
                     numer = 10,
                     repeates = 10,
                     verboseIter = FALSE,
                     sampling ='up')
set.seed(42)
model_rf_over <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = ctrl)

1
final_over <- data.frame(actual = test_data$classes,
                          predict(model_rf_over, newdata = test_data, type = "prob"))
final_over$predict <- ifelse(final_over$benign > 0.5, "benign", "malignant")
cm_over <- confusionMatrix(final_over$predict, test_data$classes)

#---combine under-sampling with the generation of additional data. Two of the most popular are ROSE and SMOTE.---#

#-rose-#
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "rose")

set.seed(42)
model_rf_rose <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)
final_rose <- data.frame(actual = test_data$classes, predict(model_rf_rose, newdata = test_data,type ='prob'))
final_rose$predict <- ifelse(final_rose$begin >0.5, 'benign', 'malignant')

#smote
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(classes ~ .,
                              data = train_data,
                              method = "rf",
                              preProcess = c("scale", "center"),
                              trControl = ctrl)


final_smote <- data.frame(actual = test_data$classes,
                         predict(model_rf_smote, newdata = test_data, type = "prob"))
final_smote$predict <- ifelse(final_smote$benign > 0.5, "benign", "malignant")

cm_smote <- confusionMatrix(final_smote$predict, test_data$classes)

#-----------predictions---------------#
models <- list(original = model_rf,
                  under = model_rf_under,
                  over = model_rf_over,
                  smote = model_rf_smote,
                  rose = model_rf_rose)

resampling <- resamples(models)
bwplot(resampling)

library(dplyr)
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models))
                         )

for (name in names(models)){
  model <- get(paste0("cm_", name))

  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(Sensitivity = model$byClass["Sensitivity"],
           Specificity = model$byClass["Specificity"],
           Precision = model$byClass["Precision"],
           Recall = model$byClass["Recall"],
           F1 = model$byClass["F1"])
}

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity) %>%
  ggplot(aes(x = x, y = y, color = model)) +
    geom_jitter(width = 0.2, alpha = 0.5, size =3)

sessionInfo()

#-----Building meaningful machine learning models for disease prediction---#
bc_data <- read.table("datasets/breast-cancer-wisconsin.data.txt",
                      header = FALSE,
                      sep = ",")
colnames(bc_data) <- c("sample_code_number",
                       "clump_thickness",
                       "uniformity_of_cell_size",
                       "uniformity_of_cell_shape",
                       "marginal_adhesion",
                       "single_epithelial_cell_size",
                       "bare_nuclei",
                       "bland_chromatin",
                       "normal_nucleoli",
                       "mitosis",
                       "classes")

bc_data$classes <- ifelse(bc_data$classes == "2", "benign",
                          ifelse(bc_data$classes == "4", "malignant", NA))

#---Missing data---#
bc_data[bc_data == "?"] <- NA

# how many NAs are in the data
length(which(is.na(bc_data)))

nrow(bc_data)
nrow(bc_data[is.na(bc_data), ])

# impute missing data
library(mice)

bc_data[,2:10] <- apply(bc_data[, 2:10], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(bc_data[, 2:10],  print = FALSE)
bc_data <- cbind(bc_data[, 11, drop = FALSE], mice::complete(dataset_impute, 1))

bc_data$classes <- as.factor(bc_data$classes)

# how many benign and malignant cases are there?
summary(bc_data$classes)


#---Data exploration---#
library(ggplot2)
ggplot(bc_data, aes(x = classes, fill = classes)) + geom_bar()

ggplot(bc_data, aes(x = clump_thickness)) + geom_histogram(bins = 10)

#Principal Component Analysis
ibrary(pcaGoPromoter)
library(ellipse)

# perform pca and extract scores
pcaOutput <- pca(t(bc_data[, -1]), printDropped = FALSE, scale = TRUE, center = TRUE)
pcaOutput2 <- as.data.frame(pcaOutput$scores)

# define groups for plotting
pcaOutput2$groups <- bc_data$classes

centroids <- aggregate(cbind(PC1, PC2) ~ groups, pcaOutput2, mean)

conf.rgn  <- do.call(rbind, lapply(unique(pcaOutput2$groups), function(t)
  data.frame(groups = as.character(t),
             ellipse(cov(pcaOutput2[pcaOutput2$groups == t, 1:2]),
                   centre = as.matrix(centroids[centroids$groups == t, 2:3]),
                   level = 0.95),
             stringsAsFactors = FALSE)))

ggplot(data = pcaOutput2, aes(x = PC1, y = PC2, group = groups, color = groups)) +
    geom_polygon(data = conf.rgn, aes(fill = groups), alpha = 0.2) +
    geom_point(size = 2, alpha = 0.6) +
    scale_color_brewer(palette = "Set1") +
    labs(color = "",
         fill = "",
         x = paste0("PC1: ", round(pcaOutput$pov[1], digits = 2) * 100, "% variance"),
         y = paste0("PC2: ", round(pcaOutput$pov[2], digits = 2) * 100, "% variance"))

#---Features---#
library(tidyr)

gather(bc_data, x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = classes, fill = classes)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ x, scales = "free", ncol = 3)

#--Machine Learning packages for R--#
# configure multicore
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(caret)

#--Training, validation and test data---#
set.seed(42)
index <- createDataPartition(bc_data$classes, p = 0.7, list = FALSE)
train_data <- bc_data[index, ]
test_data  <- bc_data[-index, ]

library(dplyr)

rbind(data.frame(group = "train", train_data),
      data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
    geom_density(alpha = 0.3) +
    facet_wrap( ~ x, scales = "free", ncol = 3)

#---Regression---#
set.seed(42)
model_glm <- caret::train(clump_thickness ~ .,
                          data = train_data,
                          method = "glm",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  savePredictions = TRUE,
                                                  verboseIter = FALSE))

model_glm

predictions <- predict(model_glm, test_data)

# model_glm$finalModel$linear.predictors == model_glm$finalModel$fitted.values
data.frame(residuals = resid(model_glm),
           predictors = model_glm$finalModel$linear.predictors) %>%
  ggplot(aes(x = predictors, y = residuals)) +
    geom_jitter() +
    geom_smooth(method = "lm"

)

# y == train_data$clump_thickness
data.frame(residuals = resid(model_glm),
           y = model_glm$finalModel$y) %>%
  ggplot(aes(x = y, y = residuals)) +
    geom_jitter() +
    geom_smooth(method = "lm")

    data.frame(actual = test_data$clump_thickness,
               predicted = predictions) %>%
      ggplot(aes(x = actual, y = predicted)) +
        geom_jitter() +
        geom_smooth(method = "lm")

#---Classification---#
library(rpart)
library(rpart.plot)

set.seed(42)
fit <- rpart(classes ~ .,
            data = train_data,
            method = "class",
            control = rpart.control(xval = 10,
                                    minbucket = 2,
                                    cp = 0),
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)

#---Random Forests---#
set.seed(42)
model_rf <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  savePredictions = TRUE,
                                                  verboseIter = FALSE))

model_rf$finalModel$confusion

imp <- model_rf$finalModel$importance
imp[order(imp, decreasing = TRUE), ]

# estimate variable importance
importance <- varImp(model_rf, scale = TRUE)
plot(importance)

confusionMatrix(predict(model_rf, test_data), test_data$classes)
results <- data.frame(actual = test_data$classes,
                      predict(model_rf, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

  ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
    geom_jitter(size = 3, alpha = 0.6)

#---Extreme gradient boosting trees---#
set.seed(42)
model_xgb <- caret::train(classes ~ .,
                          data = train_data,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  savePredictions = TRUE,
                                                  verboseIter = FALSE))

importance <- varTmp(model_xgb, scale = TRUE)
plot(importance)

confusionMatrix(predict(model_xgb, test_data), test_data$classes)
results <- data.frame(actual = test_data$classes,
                      predict(model_xgb, test_data, type = "prob"))

results$prediction <- ifelse(results$benign > 0.5, "benign",
                             ifelse(results$malignant > 0.5, "malignant", NA))

results$correct <- ifelse(results$actual == results$prediction, TRUE, FALSE)

ggplot(results, aes(x = prediction, fill = correct)) +
  geom_bar(position = "dodge")

  ggplot(results, aes(x = prediction, y = benign, color = correct, shape = correct)) +
    geom_jitter(size = 3, alpha = 0.6)

#---Feature Selection---#
library(corrplot)

# calculate correlation matrix
corMatMy <- cor(train_data[, -1])
corrplot(corMatMy, order = "hclust")

#Apply correlation filter at 0.70,
highlyCor <- colnames(train_data[, -1])[findCorrelation(corMatMy, cutoff = 0.7, verbose = TRUE)]

# which variables are flagged for removal?
highlyCor
#then we remove these variables
train_data_cor <- train_data[, which(!colnames(train_data) %in% highlyCor)]

set.seed(7)
results_rfe <- rfe(x = train_data[, -1],
                   y = train_data$classes,
                   sizes = c(1:9),
                   rfeControl = rfeControl(functions = rfFuncs, method = "cv", number = 10))

1
# chosen features
predictors(results_rfe)

set.seed(27)
model_ga <- gafs(x = train_data[, -1],
                 y = train_data$classes,
                 iters = 10, # generations of algorithm
                 popSize = 10, # population size for each generation
                 levels = c("malignant", "benign"),
                 gafsControl = gafsControl(functions = rfGA, # Assess fitness with RF
                                           method = "cv",    # 10 fold cross validation
                                           genParallel = TRUE, # Use parallel programming
                                           allowParallel = TRUE))

#-
plot(model_ga) # Plot mean fitness (AUC) by generation
train_data_ga <- train_data[, c(1, which(colnames(train_data) %in% model_ga$ga$final))]
set.seed(42)
model_rf_tune_auto <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  savePredictions = TRUE,
                                                  verboseIter = FALSE,
                                                  search = "random"),
                         tuneLength = 15)

+1
model_rf_tune_auto
plot(model_rf_tune_auto)

set.seed(42)
grid <- expand.grid(mtry = c(1:10))

model_rf_tune_man <- caret::train(classes ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 10,
                                                  repeats = 10,
                                                  savePredictions = TRUE,
                                                  verboseIter = FALSE,
                                                  search = "random"),
                         tuneGrid = grid)
1
model_rf_tune_man
plot((model_rf_tune_man))
#Grid search with h2o
library(h2o)
h2o.init(nthreads = -1)
bc_data_hf <- as.h2o(bc_data)
h2o.describe(bc_data_hf) %>%
  gather(x, y, Zeros:Sigma) %>%
  mutate(group = ifelse(x %in% c("Min", "Max", "Mean"), "min, mean, max",
                        ifelse(x %in% c("NegInf", "PosInf"), "Inf", "sigma, zeros"))) %>%
  ggplot(aes(x = Label, y = as.numeric(y), color = x)) +
    geom_point(size = 4, alpha = 0.6) +
    scale_color_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_grid(group ~ ., scales = "free") +
    labs(x = "Feature",
         y = "Value",
         color = "")

#---#
library(reshape2) # for melting

bc_data_hf[, 1] <- h2o.asfactor(bc_data_hf[, 1])

cor <- h2o.cor(bc_data_hf)
rownames(cor) <- colnames(cor)

melt(cor) %>%
  mutate(Var2 = rep(rownames(cor), nrow(cor))) %>%
  mutate(Var2 = factor(Var2, levels = colnames(cor))) %>%
  mutate(variable = factor(variable, levels = colnames(cor))) %>%
  ggplot(aes(x = variable, y = Var2, fill = value)) +
    geom_tile(width = 0.9, height = 0.9) +
    scale_fill_gradient2(low = "white", high = "red", name = "Cor.") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "",
         y = "")

#raining, validation and test data
splits <- h2o.splitFrame(bc_data_hf,
                         ratios = c(0.7, 0.15),
                         seed = 1)

train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

response <- "classes"
features <- setdiff(colnames(train), response)
summary(train$classes, exact_quantiles = TRUE)
summary(valid$classes, exact_quantiles = TRUE)
summary(test$classes, exact_quantiles = TRUE)

pca <- h2o.prcomp(training_frame = train,
           x = features,
           validation_frame = valid,
           transform = "NORMALIZE",
           impute_missing = TRUE,
           k = 3,
           seed = 42)

eigenvec <- as.data.frame(pca@model$eigenvectors)
eigenvec$label <- features

library(ggrepel)
ggplot(eigenvec, aes(x = pc1, y = pc2, label = label)) +
geom_point(color = "navy", alpha = 0.7) +
geom_text_repel()


#---Classification---#
#Random Forest

hyper_params <- list(
                     ntrees = c(25, 50, 75, 100),
                     max_depth = c(10, 20, 30),
                     min_rows = c(1, 3, 5)
                     )

search_criteria <- list(
                        strategy = "RandomDiscrete",
                        max_models = 50,
                        max_runtime_secs = 360,
                        stopping_rounds = 5,
                        stopping_metric = "AUC",
                        stopping_tolerance = 0.0005,
                        seed = 42
                        )

#!
rf_grid <- h2o.grid(algorithm = "randomForest", # h2o.randomForest,
                                                # alternatively h2o.gbm
                                                # for Gradient boosting trees
                    x = features,
                    y = response,
                    grid_id = "rf_grid",
                    training_frame = train,
                    validation_frame = valid,
                    nfolds = 25,
                    fold_assignment = "Stratified",
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    seed = 42
                    )

#!
# performance metrics where smaller is better -> order with decreasing = FALSE
sort_options_1 <- c("mean_per_class_error", "mse", "err", "logloss")

for (sort_by_1 in sort_options_1) {

  grid <- h2o.getGrid("rf_grid", sort_by = sort_by_1, decreasing = FALSE)

  model_ids <- grid@model_ids
  best_model <- h2o.getModel(model_ids[[1]])

  h2o.saveModel(best_model, path="models", force = TRUE)

}


# performance metrics where bigger is better -> order with decreasing = TRUE
sort_options_2 <- c("auc", "precision", "accuracy", "recall", "specificity")

for (sort_by_2 in sort_options_2) {

  grid <- h2o.getGrid("rf_grid", sort_by = sort_by_2, decreasing = TRUE)

  model_ids <- grid@model_ids
  best_model <- h2o.getModel(model_ids[[1]])

  h2o.saveModel(best_model, path = "models", force = TRUE)

}

#2
files <- list.files(path = "models")
rf_models <- files[grep("rf_grid_model", files)]

for (model_id in rf_models) {

  path <- paste0("U:\\Github_blog\\Webinar\\Webinar_ML_for_disease\\models\\", model_id)
  best_model <- h2o.loadModel(path)
  mse_auc_test <- data.frame(model_id = model_id,
                             mse = h2o.mse(h2o.performance(best_model, test)),
                             auc = h2o.auc(h2o.performance(best_model, test)))

  if (model_id == rf_models[[1]]) {

    mse_auc_test_comb <- mse_auc_test

  } else {

    mse_auc_test_comb <- rbind(mse_auc_test_comb, mse_auc_test)

  }
}

mse_auc_test_comb %>%
  gather(x, y, mse:auc) %>%
  ggplot(aes(x = model_id, y = y, fill = model_id)) +
    facet_grid(x ~ ., scales = "free") +
    geom_bar(stat = "identity", alpha = 0.8, position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.margin = unit(c(0.5, 0, 0, 1.5), "cm")) +
    labs(x = "", y = "value", fill = "")

#3
for (model_id in rf_models) {

  best_model <- h2o.getModel(model_id)

  finalRf_predictions <- data.frame(model_id = rep(best_model@model_id,
                                                   nrow(test)),
                                    actual = as.vector(test$classes),
                                    as.data.frame(h2o.predict(object = best_model,
                                                              newdata = test)))

  finalRf_predictions$accurate <- ifelse(finalRf_predictions$actual ==
                                           finalRf_predictions$predict,
                                         "yes", "no")

  finalRf_predictions$predict_stringent <- ifelse(finalRf_predictions$benign > 0.8,
                                                  "benign",
                                                  ifelse(finalRf_predictions$malignant
                                                         > 0.8, "malignant", "uncertain"))

  finalRf_predictions$accurate_stringent <- ifelse(finalRf_predictions$actual ==
                                                     finalRf_predictions$predict_stringent, "yes",
                                         ifelse(finalRf_predictions$predict_stringent ==
                                                  "uncertain", "na", "no"))

  if (model_id == rf_models[[1]]) {

    finalRf_predictions_comb <- finalRf_predictions

  } else {

    finalRf_predictions_comb <- rbind(finalRf_predictions_comb, finalRf_predictions)

  }
}

#4
finalRf_predictions_comb %>%
  ggplot(aes(x = actual, fill = accurate)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(~ model_id, ncol = 3) +
    labs(fill = "Were\npredictions\naccurate?",
         title = "Default predictions")

#
finalRf_predictions_comb %>%
  subset(accurate_stringent != "na") %>%
  ggplot(aes(x = actual, fill = accurate_stringent)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(~ model_id, ncol = 3) +
    labs(fill = "Were\npredictions\naccurate?",
         title = "Stringent predictions")
rf_model <- h2o.loadModel("models/rf_grid_model_6")
h2o.varimp_plot(rf_model)

#h2o.varimp(rf_model)
h2o.mean_per_class_error(rf_model, train = TRUE, valid = TRUE, xval = TRUE)

h2o.confusionMatrix(rf_model, valid = TRUE)

plot(rf_model,
     timestep = "number_of_trees",
     metric = "classification_error")

plot(rf_model,
     timestep = "number_of_trees",
     metric = "logloss")
#
plot(rf_model,
     timestep = "number_of_trees",
     metric = "AUC")

#
plot(rf_model,
     timestep = "number_of_trees",
     metric = "rmse")

h2o.auc(rf_model, train = TRUE)
h2o.auc(rf_model, valid = TRUE)
h2o.auc(rf_model, xval = TRUE)
perf <- h2o.performance(rf_model, test)
perf

plot(perf)
h2o.logloss(perf)
h2o.mse(perf)
h2o.auc(perf)
head(h2o.metric(perf))

finalRf_predictions <- data.frame(actual = as.vector(test$classes),
                                  as.data.frame(h2o.predict(object = rf_model,
                                                            newdata = test)))

#!
finalRf_predictions$accurate <- ifelse(finalRf_predictions$actual ==
                                         finalRf_predictions$predict, "yes", "no")

finalRf_predictions$predict_stringent <- ifelse(finalRf_predictions$benign > 0.8, "benign",
                                                ifelse(finalRf_predictions$malignant
                                                       > 0.8, "malignant", "uncertain"))
finalRf_predictions$accurate_stringent <- ifelse(finalRf_predictions$actual ==
                                                   finalRf_predictions$predict_stringent, "yes",
                                       ifelse(finalRf_predictions$predict_stringent ==
                                                "uncertain", "na", "no"))

finalRf_predictions %>%
  group_by(actual, predict) %>%
  dplyr::summarise(n = n())

#1
finalRf_predictions %>%
  group_by(actual, predict_stringent) %>%
  dplyr::summarise(n = n())

#�?
finalRf_predictions %>%
  ggplot(aes(x = actual, fill = accurate)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    labs(fill = "Were\npredictions\naccurate?",
         title = "Default predictions")

#2
finalRf_predictions %>%
  subset(accurate_stringent != "na") %>%
  ggplot(aes(x = actual, fill = accurate_stringent)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    labs(fill = "Were\npredictions\naccurate?",
         title = "Stringent predictions")

##3
df <- finalRf_predictions[, c(1, 3, 4)]

thresholds <- seq(from = 0, to = 1, by = 0.1)

prop_table <- data.frame(threshold = thresholds, prop_true_b = NA, prop_true_m = NA)

for (threshold in thresholds) {
  pred <- ifelse(df$benign > threshold, "benign", "malignant")
  pred_t <- ifelse(pred == df$actual, TRUE, FALSE)

  group <- data.frame(df, "pred" = pred_t) %>%
  group_by(actual, pred) %>%
  dplyr::summarise(n = n())

  group_b <- filter(group, actual == "benign")

  prop_b <- sum(filter(group_b, pred == TRUE)$n) / sum(group_b$n)
  prop_table[prop_table$threshold == threshold, "prop_true_b"] <- prop_b

  group_m <- filter(group, actual == "malignant")

  prop_m <- sum(filter(group_m, pred == TRUE)$n) / sum(group_m$n)
  prop_table[prop_table$threshold == threshold, "prop_true_m"] <- prop_m
}

prop_table %>%
  gather(x, y, prop_true_b:prop_true_m) %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
    geom_point() +
    geom_line() +
    scale_color_brewer(palette = "Set1") +
    labs(y = "proportion of true predictions",
         color = "b: benign cases\nm: malignant cases")

h2o.shutdown()

sessionInfo()

                                                            
