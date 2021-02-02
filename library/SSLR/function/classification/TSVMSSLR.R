# *********************************************************************************************
# Title     : Transductive SVM classifier using the convex concave procedure
# Function  : TSVMSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/TSVMSSLR.html
# *********************************************************************************************


# ＜概要＞
# - CCCPアルゴリズムを使用したRSSLパッケージトランスダクティブSVMからのモデル
#   --- quadprogパッケージを使用してRで実装
#   --- 実装は大きなデータセットをうまく処理できない


# ＜構文＞
# TSVMSSLR(
#  C = 1,
#  Cstar = 0.1,
#  kernel = kernlab::vanilladot(),
#  balancing_constraint = TRUE,
#  s = 0,
#  x_center = TRUE,
#  scale = FALSE,
#  eps = 1e-09,
#  max_iter = 20,
#  verbose = FALSE
#)


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 モデリング


# 0 準備 -------------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(tidymodels)
library(SSLR)
library(kernlab)

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
  TSVMSSLR(kernel = kernlab::vanilladot()) %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)
