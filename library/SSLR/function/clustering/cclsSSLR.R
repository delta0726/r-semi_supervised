# *********************************************************************************************
# Title     : Pairwise Constrained Clustering
# Function  : cclsSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/cclsSSLR.html
# *********************************************************************************************


# ＜概要＞
# -


# ＜構文＞
# clsSSLR(
#   n_clusters = NULL,
#   mustLink = NULL,
#   cantLink = NULL,
#   max_iter = 1,
#   tabuIter = 100,
#   tabuLength = 20
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
data <- iris


# 1 データ作成 --------------------------------------------------------------------------

# 列番号の取得
cls <- which(colnames(iris) == "Species")

# ラベルをNAに置換
set.seed(1)
labeled.index <- data$Species %>% createDataPartition(p = .2, list = FALSE)
data[-labeled.index,cls] <- NA

# 確認
data %>% print()
data$Species %>% summary()


# 2 モデリング ----------------------------------------------------------------------------

# モデル構築
m <- cclsSSLR(max_iter = 1) %>% fit(Species ~ ., data)

# ラベル分類の取得
# --- k-meansなので番号を出力
labels <- m %>% cluster_labels()

# 確認
data %>%
  mutate(label = unlist(cluster_labels(m)))