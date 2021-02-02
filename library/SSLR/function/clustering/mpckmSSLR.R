# *********************************************************************************************
# Title     : MPC K-Means Algorithm
# Function  : mpckmSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/mpckmSSLR.html
# *********************************************************************************************


# ＜概要＞
# - {conclust}からの関数でPairwise Constraints Clusteringを実行する
# - ラベルのないデータセットと、リンクが必要な制約とリンクできない制約の2つのリストを入力として受け取り、クラスタリングを出力として生成


# ＜構文＞
# mpckmSSLR(n_clusters = NULL, mustLink = NULL, cantLink = NULL, max_iter = 10)


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
data <- iris


# 1 データ作成 --------------------------------------------------------------------------

# 列番号の取得
cls <- which(colnames(iris) == "Species")

# ラベルをNAに置換
set.seed(1)
labeled.index <- createDataPartition(data$Species, p = .2, list = FALSE)
data[-labeled.index,cls] <- NA


# 2 モデリング ----------------------------------------------------------------------------

# モデル構築
m <- mpckmSSLR() %>% fit(Species ~ ., data)

# ラベル分類の取得
# --- k-meansなので番号を出力
labels <- m %>% cluster_labels()

# 確認
data %>%
  mutate(label = unlist(cluster_labels(m)))

