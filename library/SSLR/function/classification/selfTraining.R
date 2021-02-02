# *********************************************************************************************
# Title     : General Interface for Self-training model
# Function  : selfTraining
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/selfTraining.html
# *********************************************************************************************


# ＜概要＞
# - 自己トレーニング分類器は、最初にラベル付けされた例の削減されたセットでトレーニングする
# - 次に、ラベルのない例に対する独自の最も信頼できる予測を使用して、繰り返し再トレーニングする
# - ベースの教師あり分類器を使用するラッパー手法に従って、ラベルのないインスタンスの可能なクラスを確立する


# ＜構文＞
# selfTraining(learner, max.iter = 50, perc.full = 0.7, thr.conf = 0.5)


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
labeled.index <- createDataPartition(train$Wine, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA


# 2 モデリング ----------------------------------------------------------------------------

# ベースモデル
rf <-
  rand_forest(trees = 100, mode = "classification") %>%
    set_engine("randomForest")

# 自己訓練モデル
m <-
  selfTraining(learner = rf,
               perc.full = 0.7,
               thr.conf = 0.5, max.iter = 10) %>%
    fit(Wine ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)

