# *********************************************************************************************
# Title     : Basic Usage
# Objective : TODO
# Created by: Owner
# Created on: 2021/01/25
# URL       : https://dicits.ugr.es/software/SSLR/
# *********************************************************************************************


# ＜ポイント＞
# - このパッケージは、半教師あり学習の問題を解決するために設計および実装されている
# - 半教師あり問題では、ラベル付きデータとラベルなしデータの両方を使用してモデルをトレーニングすることができる


# ＜アルゴリズム＞
# - 自己訓練（self-training）
# - 共訓練（co-training）
# - democratic
# - decision tree
# - random forest
# - S3VM


# ＜目次＞
# 0 準備
# 1 データ操作
# 2 欠損データの作成
# 3 モデル構築


# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)
library(DataExplorer)


# データ準備
data(wine)

# データ確認
# --- 3種類のワイン
wine %>% glimpse()
wine$Wine %>% table()


# 1 データ作成 ----------------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

# レコード数
train %>% dim()
test %>% dim()

# 列番号取得
# --- Wineは14列目
cls <- which(colnames(wine) == "Wine")


# 2 欠損データの作成 ----------------------------------------------------------------------------

# ラベル欠損の作成
# --- 訓練データの5%のみ保存（その他はNA）
labeled.index <- wine$Wine %>% createDataPartition(p = .05, list = FALSE)
train[-labeled.index, cls] <- NA

# データ確認
# --- Wine列は大半が欠損している
train %>% print()
train %>% plot_missing()


# 3 モデル構築 ----------------------------------------------------------------------------------

# モデル構築
# --- 決定木（rpart）
# --- Parsnipベース
dt <-
  decision_tree(mode = "classification", tree_depth = 10) %>%
  set_engine("rpart")


#
m1 <- SSLRDecisionTree(w = 0.3) %>% fit(Wine ~ ., data = train)
m1 %>% summary()

# 自己訓練
m2 <- selfTraining(learner = dt) %>% fit(Wine ~ ., data = train)
m2 %>% summary()

# 共訓練（Co-Training by Committee）
m3 <- coBC(learner = dt,N = 10,perc.full = 0.6) %>% fit(Wine ~ ., data = train)

# ３つの共訓練
m4 <- triTraining(learner = dt) %>% fit(Wine ~ ., data = train)

#
m5 <- setred(learner = dt) %>% fit(Wine ~ ., data = train)
