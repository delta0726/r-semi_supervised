# *********************************************************************************************
# Title     : 共訓練モデルのインターフェース
# Function  : coBCG
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/coBC.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
# coBC(learner, N = 3, perc.full = 0.7, u = 100, max.iter = 50)


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 ベースモデル
# 3 共学習
# 4 スキーマありの共学習


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

# 共学習モデル
m <-
  coBC(learner = rf, N = 3, erc.full = 0.7, u = 100, max.iter = 3) %>%
    fit(Wine ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)
