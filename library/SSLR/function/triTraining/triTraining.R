# *********************************************************************************************
# Title     : Tri-training model
# Function  : triTraining
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/triTraining.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
# triTraining(learner)


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
train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

# 列番号の取得
cls <- which(colnames(wine) == "Wine")

# ラベルをNAに置換
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA


# 2 モデリング ----------------------------------------------------------------------------

# ベースモデル
rf <-
  rand_forest(trees = 100, mode = "classification") %>%
    set_engine("randomForest")

# 共訓練モデル
m <-
  triTraining(learner = rf) %>%
    fit(Wine ~ ., data = train)

# 確認
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  dplyr::select(.pred_class, Wine) %>%
  print(n = nrow(.))


# 3 モデル精度 ----------------------------------------------------------------------------

# メトリック出力
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)