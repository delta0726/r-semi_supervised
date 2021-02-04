# *********************************************************************************************
# Title     : General Interface for Self-training model
# Function  : selfTraining
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/selfTraining.html
# *********************************************************************************************


# ＜概要＞
# - 自己トレーニング分類器は、最初にラベル付けされた例の削減されたセットでトレーニングする
# - 次に、ラベルのない例に対する独自の最も信頼できる予測を使用して、繰り返し再トレーニングする
# - ベースの教師あり分類器を使用するラッパー手法に従って、ラベルのないインスタンスの可能なクラスを確立する


# ＜構文＞
# selfTraining(learner, max.iter = 50, perc.full = 0.7, thr.conf = 0.5)




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
data(wine)


# 1 データ作成 --------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index <- wine$Wine %>% createDataPartition(p = .7, list = FALSE)
train <- wine[ train.index,]
test  <- wine[-train.index,]

# 行列数
train %>% dim()
test %>% dim()

# データ確認
train %>% glimpse()

# 列番号の取得
# --- Wineは14列目
cls <- which(colnames(wine) == "Wine")


# 2 ラベル欠損の作成 ---------------------------------------------------------------------

# ラベルにNAを挿入
labeled.index <- wine$Wine %>% createDataPartition(p = 0.2, list = FALSE)
train[-labeled.index, cls] <- NA

# Wine列の確認
train$Wine %>% print()
train$Wine %>% summary()

# 確認
train %>% print()


# 3 モデリング ----------------------------------------------------------------------------

# ベースモデル
rf <-
  rand_forest(trees = 100, mode = "classification") %>%
    set_engine("ranger")

# 自己訓練モデル
m <-
  selfTraining(learner = rf,
               perc.full = 0.7,
               thr.conf = 0.5, max.iter = 10) %>%
    fit(Wine ~ ., data = train)


# 4 モデル評価 ---------------------------------------------------------------------------

# 予測
pred <-
  m %>%
    predict(test) %>%
    bind_cols(test) %>%
    select(Wine, .pred_class)

# モデル精度の検証
pred %>% metrics(truth = "Wine", estimate = .pred_class)
