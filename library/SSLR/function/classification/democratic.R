# *********************************************************************************************
# Title     : デモクラティック・モデル
# Function  : democratic
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/democratic.html
# *********************************************************************************************


# ＜概要＞
# - 複数アルゴリズムによる共学習モデル


# ＜構文＞
# democratic(learners, schemes = NULL)


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


# 2 ベースモデル ----------------------------------------------------------------------------

# モデル構築
# --- ランダムフォレスト
rf <-
  rand_forest(trees = 100, mode = "classification") %>%
    set_engine("randomForest")

# モデル構築
# --- XGBoost
bt <-
  boost_tree(trees = 100, mode = "classification") %>%
    set_engine("xgboost")


# 3 共学習 ----------------------------------------------------------------------------

# デモクラティックモデル
# --- 共学習
m <-
  democratic(learners = list(rf, bt)) %>%
    fit(Wine ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)


# 4 スキーマありの共学習 -----------------------------------------------------------------

# デモクラティックモデル
# --- 共学習
set.seed(1)
m <- democratic(learners = list(rf, bt),
                schemes = list(c("Malic.Acid","Ash"), c("Magnesium","Proline")) ) %>%
  fit(Wine ~ ., data = train)


# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Wine", estimate = .pred_class)
