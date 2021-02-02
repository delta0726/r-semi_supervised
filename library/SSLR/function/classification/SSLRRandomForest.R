# *********************************************************************************************
# Title     :
# Function  :
# Created by: Owner
# Created on: 2021/01/26
# URL       :
# *********************************************************************************************


# ＜概要＞
# - 従来のランダムフォレストアルゴリズムと同じですが、半教師あり決定木を使用する点が異なる


# ＜構文＞
# SSLRRandomForest(
#   mtry = NULL,
#   trees = 500,
#   min_n = NULL,
#   w = 0.5,
#   replace = TRUE,
#   tree_max_depth = Inf,
#   sampsize = NULL,
#   min_samples_leaf = NULL,
#   allowParallel = TRUE
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
  SSLRRandomForest(trees = 5,  w = 0.3) %>%
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
