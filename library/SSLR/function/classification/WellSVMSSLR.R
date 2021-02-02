# *********************************************************************************************
# Title     : WellSVM model
# Function  : WellSVMSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/WellSVMSSLR.html
# *********************************************************************************************


# ＜概要＞
# - RSSLパッケージWellSVMのモデル
# - SVM目的関数でラベルなしデータの最適なラベルを見つけるという混合整数計画問題を緩和したもの


# ＜構文＞
# WellSVMSSLR(
#   C1 = 1,
#   C2 = 0.1,
#   gamma = 1,
#   x_center = TRUE,
#   scale = FALSE,
#   use_Xu_for_scaling = FALSE,
#   max_iter = 20
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

# データ確認
# --- 特徴量にカテゴリカルデータを含む
breast %>% glimpse()


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

# データ確認
train %>% head()


# 2 モデリング ----------------------------------------------------------------------------

# モデル構築
m <-
  WellSVMSSLR() %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)