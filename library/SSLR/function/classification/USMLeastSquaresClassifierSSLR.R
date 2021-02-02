# *********************************************************************************************
# Title     : 2次モーメント最小二乗分類器
# Function  : USMLeastSquaresClassifierSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/USMLeastSquaresClassifierSSLR.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
# USMLeastSquaresClassifierSSLR(
#   lambda = 0,
#   intercept = TRUE,
#   x_center = FALSE,
#   scale = FALSE,
#   y_scale = FALSE,
#   ...,
#   use_Xu_for_scaling = TRUE
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
  USMLeastSquaresClassifierSSLR() %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)
