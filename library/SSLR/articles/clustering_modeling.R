# *********************************************************************************************
# Title     : Clustering Modeling
# Objective : TODO
# Created by: Owner
# Created on: 2021/01/25
# URL       : https://dicits.ugr.es/software/SSLR/articles/clustering.html
# *********************************************************************************************


# ＜概要＞
# - クラスタリングで半教師あり学習を行う
#   --- ラベルを直接予測するのではなくクラスターに分類するだけ


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 k-means


# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)
library(DataExplorer)
library(factoextra)


# データ準備
data <- iris

# データ確認
data %>% glimpse()


# 1 データ作成 ----------------------------------------------------------------------------------

# 列番号取得
cls <- which(colnames(iris) == "Species")

# ラベル欠損の作成
# --- 訓練データの5%のみ保存（その他はNA）
labeled.index <- createDataPartition(data$Species, p = .2, list = FALSE)
data[-labeled.index,cls] <- NA

# データ確認
# --- Wine列は大半が欠損している
data %>% print()
data %>% plot_missing()


# 2 k-means --------------------------------------------------------------------------------

# モデル構築
m <-
  constrained_kmeans() %>%
    fit(Species ~ ., data)

# 検証データ作成
m %>% cluster_labels()
iris %>% mutate(estimate = m %>% cluster_labels() %>% .$.pred_class)

# 中心の確認
m %>% get_centers()

# プロット作成
m$model %>% fviz_cluster(as.matrix(data[,-cls]))