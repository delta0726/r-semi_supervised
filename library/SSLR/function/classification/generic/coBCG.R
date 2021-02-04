# *********************************************************************************************
# Title     : CoBC generic method
# Function  : coBCG
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/coBCG.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
# coBCG(y, gen.learner, gen.pred, N = 3, perc.full = 0.7, u = 100, max.iter = 50)


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 モデリング
# 3 モデル精度


# 0 準備 -------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)


# データ準備
data(wine)


# 1 データ作成 --------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- wine$Wine %>% createDataPartition(p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

# 行列数
train %>% dim()
test %>% dim()

# データ確認
train %>% glimpse()

# 列番号の取得
# --- Wineは14列目
cls <- which(colnames(wine) == "Wine")




# Use the other 50% of instances for inductive testing
tst.idx <- setdiff(1:length(y), tra.idx)
xitest <- x[tst.idx,] # testing instances
yitest <- y[tst.idx] # classes of testing instances

## Example: Training from a set of instances with 1-NN (knn3) as base classifier.
gen.learner1 <- function(indexes, cls)
  caret::knn3(x = xtrain[indexes,], y = cls, k = 1)

gen.pred1 <- function(model, indexes)
  predict(model, xtrain[indexes,])

set.seed(1)

trControl_coBCG <- list(gen.learner = gen.learner1, gen.pred = gen.pred1)
md1 <- train_generic(ytrain, method = "coBCG", trControl = trControl_coBCG)


# Predict probabilities per instances using each model
h.prob <- lapply(
  X = md1$model,
  FUN = function(m) predict(m, xitest)
)
# Combine the predictions
cls1 <- coBCCombine(h.prob, md1$classes)
table(cls1, yitest)
#>     yitest
#> cls1  1  2  3
#>    1 29  5  0
#>    2  0 25  2
#>    3  0  4 24

confusionMatrix(cls1, yitest)$overall[1]
#>  Accuracy
#> 0.8764045


## Example: Training from a distance matrix with 1-NN (oneNN) as base classifier.
dtrain <- as.matrix(proxy::dist(x = xtrain, method = "euclidean", by_rows = TRUE))
gen.learner2 <- function(indexes, cls) {
  m <- SSLR::oneNN(y = cls)
  attr(m, "tra.idxs") <- indexes
  m
}

gen.pred2 <- function(model, indexes) {
  tra.idxs <- attr(model, "tra.idxs")
  d <- dtrain[indexes, tra.idxs]
  prob <- predict(model, d, distance.weighting = "none")
  prob
}

set.seed(1)

trControl_coBCG2 <- list(gen.learner = gen.learner2, gen.pred = gen.pred2)
md2 <- train_generic(ytrain, method = "coBCG", trControl = trControl_coBCG2)



# Predict probabilities per instances using each model
ditest <- proxy::dist(x = xitest, y = xtrain[md2$instances.index,],
                      method = "euclidean", by_rows = TRUE)

h.prob <- list()
ninstances <- nrow(dtrain)
for (i in 1:length(md2$model)) {
  m <- md2$model[[i]]
  D <- ditest[, md2$model.index.map[[i]]]
  h.prob[[i]] <- predict(m, D)
}
# Combine the predictions
cls2 <- coBCCombine(h.prob, md2$classes)
table(cls2, yitest)
#>     yitest
#> cls2  1  2  3
#>    1 29  5  0
#>    2  0 25  2
#>    3  0  4 24

confusionMatrix(cls2, yitest)$overall[1]
#>  Accuracy
#> 0.8764045