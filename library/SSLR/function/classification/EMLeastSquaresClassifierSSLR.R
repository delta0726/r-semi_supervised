# *********************************************************************************************
# Title     : EMLeastSquaresClassifier model
# Function  : EMLeastSquaresClassifierSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/EMLeastSquaresClassifierSSLR.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
#  EMLeastSquaresClassifierSSLR(
#   x_center = FALSE,
#   scale = FALSE,
#   verbose = FALSE,
#   intercept = TRUE,
#   lambda = 0,
#   eps = 1e-09,
#   y_scale = FALSE,
#   alpha = 1,
#   beta = 1,
#   init = "supervised",
#   method = "block",
#   objective = "label",
#   save_all = FALSE,
#   max_iter = 1000
# )


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 モデリング


# 0 準備 -------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)


# データ準備
data(breast)


# 1 データ作成 --------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- createDataPartition(breast$Class, p = .7, list = FALSE)
train <- breast[ train.index,]
test  <- breast[-train.index,]

# 列番号の取得
cls <- which(colnames(breast) == "Class")

# ラベルをNAに置換
labeled.index <- createDataPartition(breast$Class, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA


# 2 モデリング ----------------------------------------------------------------------------

# モデル構築
m <-
  EMLeastSquaresClassifierSSLR() %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)

