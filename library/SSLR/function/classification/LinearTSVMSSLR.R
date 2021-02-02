# *********************************************************************************************
# Title     : LinearTSVM model
# Function  : LinearTSVMSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       :
# *********************************************************************************************


# ＜概要＞
# - RSSLパッケージからのモデル線形サポートベクター分類器の実装


# ＜構文＞
#  LinearTSVMSSLR(
#   C = 1,
#   Cstar = 0.1,
#   s = 0,
#   x_center = FALSE,
#   scale = FALSE,
#   eps = 1e-06,
#    verbose = FALSE,
#   init = NULL
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
  LinearTSVMSSLR() %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)
