# *********************************************************************************************
# Title     : Decision Tree model
# Function  : SSLRDecisionTree
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/SSLRDecisionTree.html
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
# 2 ラベル欠損の作成
# 3 モデリング
# 4 モデル評価


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


# 2 ラベル欠損の作成 ---------------------------------------------------------------------

# ラベルにNAを挿入
labeled.index <- wine$Wine %>% createDataPartition(p = 0.2, list = FALSE)
train[-labeled.index, cls] <- NA

# Wine列の確認
train$Wine %>% print()
train$Wine %>% summary()

# 確認
train %>% print()


# 3 モデリング ----------------------------------------------------------------------------

# モデル構築
m <-
  SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                   w = 0.3) %>%
    fit(Wine ~ ., data = train)


# 4 モデル評価 ---------------------------------------------------------------------------

# 予測
# --- クラス確率
# --- クラス分類
m %>% predict(test, type = "prob")
m %>% predict(test, type = "class")

# 予測
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  select(Wine, .pred_class) %>%
  metrics(truth = "Wine", estimate = .pred_class)

