
library(mlr)
data(iris)


# task -> learner -> train -> test -> prediction -> Evaluate -> Resampling -> Plot ->>>>>>
# 分类    对应算法   一遍
# 回归    ...        又一遍
# 聚类



## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
task = makeClassifTask(data = iris, target = "Species")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn = makeLearner("classif.lda")
      
  # classif
    # classif.rpart
    # classif.C50
    # classif.cvglmnet 
    # classif.clusterSVM
    # classif.dbnDNN 
    # classif.glmboos
    # classif.h2o.deeplearning 
    # ...
  # regr
    # regr.bcart
    # regr.gamboost 
    # ...
  # surv
    # surv.coxph
    # surv.cvglmnet
    # surv.gbm
    # ...
  # cluster
    # cluster.EM
    # cluster.cmeans
    # cluster.kmeans
    # ...
  # Cost-sensitive classification
  # multilabel.cforest 

  # predict.type = 'prob'('response')

#---这个相当的简便---#

n = nrow(iris)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = train(lrn, task, subset = train.set)

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred = predict(model, task = task, subset = test.set)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred, measures = list(mmce, acc))


#---------------不平衡数据分类---------------#
# ---向上向下采样，混合方法
data.imbal.train = rbind(
  data.frame(x = rnorm(100, mean = 1), class = "A"),
  data.frame(x = rnorm(5000, mean = 2), class = "B")
)
task = makeClassifTask(data = data.imbal.train, target = "class")
#小的多8倍
task.over = oversample(task, rate = 8)

#大滴少了8倍
task.under = undersample(task, rate = 1/8)
table(getTaskTargets(task))
table(getTaskTargets(task.over))
table(getTaskTargets(task.under))

lrn = makeLearner("classif.rpart", predict.type = "prob")
mod = train(lrn, task)

mod.over = train(lrn, task.over)
mod.under = train(lrn, task.under)
data.imbal.test = rbind(
  data.frame(x = rnorm(10, mean = 1), class = "A"),
  data.frame(x = rnorm(500, mean = 2), class = "B")
)

performance(predict(mod, newdata = data.imbal.test), measures = list(mmce, ber, auc))
performance(predict(mod.over, newdata = data.imbal.test), measures = list(mmce, ber, auc))
performance(predict(mod.under, newdata = data.imbal.test), measures = list(mmce, ber, auc))

# --更新一下- #
lrn.over = makeOversampleWrapper(lrn, osw.rate = 8)
lrn.under = makeUndersampleWrapper(lrn, usw.rate = 1/8)
mod = train(lrn, task)
mod.over = train(lrn.over, task)
mod.under = train(lrn.under, task)

performance(predict(mod, newdata = data.imbal.test), measures = list(mmce, ber, auc))
performance(predict(mod.over, newdata = data.imbal.test), measures = list(mmce, ber, auc))
performance(predict(mod.under, newdata = data.imbal.test), measures = list(mmce, ber, auc))

#- SMOTE - 算法-#
task.smote  = smote(task, rate = 8, nn = 5)
table(getTaskTargets(task))
table(getTaskTargets(task.smote))

lrn.smote = makeSMOTEWrapper(lrn, sw.rate = 8, sw.nn = 4)
mod.smote = train(lrn.smote, task)
performance(predict(mod.smote. newdata = data.imbal.test), measures = list(mmce, ber, auc))

#--bagging向上采样-#


lrn = makeLearner("classif.rpart", predict.type = "response")
obw.lrn = makeOverBaggingWrapper(lrn, obw.rate = 10, obw.iters = 5)


#- 对比出伤害 -# 
# bag采太多但是最后不用也尴尬-#
lrn = setPredictType(lrn, "prob")
rdesc = makeResampleDesc("CV", iters = 5)
r1 = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE,
              measures = list(mmce, ber, auc))
r1$aggr

obw.lrn = setPredictType(obw.lrn, "prob")
r2 = resample(learner = obw.lrn, task = task, resampling = rdesc, show.info = FALSE,
              measures = list(mmce, ber, auc))
r2$aggr

#- 用RF来伤害一下
lrn = makeLearner("classif.randomForest")
obw.lrn = makeOverBaggingWrapper(lrn, obw.rate = 8, obw.iters = 3)

lrn = setPredictType(lrn, "prob")
r1 = resample(learner = lrn, task = task, resampling = rdesc, show.info = FALSE,
              measures = list(mmce, ber, auc))
r1$aggr

obw.lrn = setPredictType(obw.lrn, "prob")
r2 = resample(learner = obw.lrn, task = task, resampling = rdesc, show.info = FALSE,
              measures = list(mmce, ber, auc))
r2$aggr



lrn = makeLearner("classif.logreg")
wcw.lrn = makeWeightedClassesWrapper(lrn, wcw.weight = 0.01)
lrn = makeLearner("classif.ksvm")
wcw.lrn = makeWeightedClassesWrapper(lrn, wcw.weight = 0.01)










