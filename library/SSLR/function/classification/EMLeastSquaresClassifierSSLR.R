# *********************************************************************************************
# Title     : EMLeastSquaresClassifier model
# Function  : EMLeastSquaresClassifierSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/EMLeastSquaresClassifierSSLR.html
# *********************************************************************************************


# ＜概要＞
# - 半教師あり最小二乗分類へのアプローチのような期待値最大化
#   ---ラベル付きオブジェクトとラベルなしオブジェクトの総損失を最小化
#   --- アルゴリズムはEMと同様に進行し、その後、重みの更新とラベルのないオブジェクトのソフトラベルを適用
#   --- ハードラベルの自己学習に相当


# ＜構文＞
#  EMLeastSquaresClassifierSSLR(
#   x_center = FALSE,
#   scale = FALSE,
#   verbose = FALSE,
#   intercept = TRUE,
#   lambda = 0,
#   eps = 1e-09,
#   y_scale = FALSE,
#   alpha = 1,
#   beta = 1,
#   init = "supervised",
#   method = "block",
#   objective = "label",
#   save_all = FALSE,
#   max_iter = 1000
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
data(breast)


# 1 データ作成 --------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- breast$Class %>% createDataPartition(p = .7, list = FALSE)
train <- breast[ train.index,]
test  <- breast[-train.index,]

# 行列数
train %>% dim()
test %>% dim()

# データ確認
train %>% glimpse()

# 列番号の取得
# --- Classは1番目
cls <- which(colnames(breast) == "Class")


# 2 ラベル欠損の作成 ---------------------------------------------------------------------

# ラベルにNAを挿入
labeled.index <- breast$Class %>% createDataPartition(p = .2, list = FALSE)
train[-labeled.index,cls] <- NA

# Class列の確認
train$Class %>% print()
train$Class %>% summary()

# 確認
train %>% print()


# 3 モデリング ----------------------------------------------------------------------------

# モデル構築
m <-
  EMLeastSquaresClassifierSSLR() %>%
    fit(Class ~ ., data = train)


# 4 モデル評価 ----------------------------------------------------------------------------

# 予測
pred <-
  m %>%
    predict(test) %>%
    bind_cols(test) %>%
    select(Class, .pred_class)

# モデル精度の検証
pred %>% metrics(truth = "Class", estimate = .pred_class)
