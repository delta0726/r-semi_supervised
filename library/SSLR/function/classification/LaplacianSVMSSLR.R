# *********************************************************************************************
# Title     : LaplacianSVM model
# Function  : LaplacianSVMSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/LaplacianSVMSSLR.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
#  LaplacianSVMSSLR(
#   lambda = 1,
#   gamma = 1,
#   scale = TRUE,
#   kernel = kernlab::vanilladot(),
#   adjacency_distance = "euclidean",
#   adjacency_k = 6,
#   normalized_laplacian = FALSE,
#   eps = 1e-09
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
  LaplacianSVMSSLR(kernel=kernlab::vanilladot()) %>%
    fit(Class ~ ., data = train)

# モデル精度の検証
m %>%
  predict(test) %>%
  bind_cols(test) %>%
  metrics(truth = "Class", estimate = .pred_class)
