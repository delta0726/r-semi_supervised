# *********************************************************************************************
# Title     : Decision Tree model
# Function  : SSLRDecisionTree
# Created by: Owner
# Created on: 2021/01/26
# URL       :
# *********************************************************************************************


# ＜概要＞
# - 半教師ありディシジョンツリー
#   --- "Semi-supervised classification trees"論文より
#   --- https://dl.acm.org/doi/abs/10.1007/s10844-017-0457-4


# ＜構文＞
# SSLRDecisionTree(
#   max_depth = 30,
#   w = 0.5,
#   min_samples_split = 20,
#   min_samples_leaf = ceiling(min_samples_split/3)
# )


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 モデリング


# 0 準備 -------------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(SSLR)
library(tidymodels)

# データ準備
data(wine)


# 1 データ作成 --------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

# 列番号の取得
cls <- which(colnames(wine) == "Wine")

# ラベルをNAに置換
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA


# 2 モデリング ----------------------------------------------------------------------------

# モデル構築
m <-
  SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                   w = 0.3) %>%
    fit(Wine ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)

# 予測
# --- クラス確率
# --- クラス分類
m %>% predict(test, type = "prob")
m %>% predict(test, type = "class")
