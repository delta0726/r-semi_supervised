# *********************************************************************************************
# Title     : EMNearestMeanClassifier model
# Function  : EMNearestMeanClassifierSSLR
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/EMNearestMeanClassifierSSLR.html
# *********************************************************************************************


# ＜概要＞
# - RSSLパッケージのモデル期待値最大化を使用した半教師あり最も近い平均分類器
# - 球面共分散行列を持つガウスクラスを想定した最も近い平均分類器に適用される期待値最大化


# ＜構文＞
# EMNearestMeanClassifierSSLR(method = "EM", scale = FALSE, eps = 1e-04)



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
  EMNearestMeanClassifierSSLR() %>%
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
