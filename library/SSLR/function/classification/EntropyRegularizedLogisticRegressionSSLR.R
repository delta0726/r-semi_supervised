# *********************************************************************************************
# Title     : エントロピーベースの正則化ロジスティック回帰
# Function  : EntropyRegularizedLogisticRegressionSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/EntropyRegularizedLogisticRegressionSSLR.html
# *********************************************************************************************


# ＜概要＞
# - RSSLパッケージからのエントロピーベースの正則化ロジスティック回帰の実装


# ＜構文＞
#  EntropyRegularizedLogisticRegressionSSLR(
#   lambda = 0,
#   lambda_entropy = 1,
#   intercept = TRUE,
#   init = NA,
#   scale = FALSE,
#   x_center = FALSE
# )


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 モデリング


# 0 準備 -------------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(tidymodels)
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
  EntropyRegularizedLogisticRegressionSSLR() %>%
    fit(Class ~ ., data = train)


# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)
