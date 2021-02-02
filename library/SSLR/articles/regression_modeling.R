# *********************************************************************************************
# Title     : Regression Modeling
# Objective : TODO
# Created by: Owner
# Created on: 2021/01/25
# URL       : https://dicits.ugr.es/software/SSLR/articles/regression.html
# *********************************************************************************************


# ＜概要＞
# - 様々なアルゴリズムを用いて回帰問題を半教師あり学習で解く


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 決定木
# 3 ランダムフォレスト
# 4 k近傍法


# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(caret)
library(SSLR)
library(DataExplorer)


# データ準備
data <- airquality %>% select(-Solar.R)

# データ確認
data %>% glimpse()


# 1 データ作成 ----------------------------------------------------------------------------------

# データ分割
set.seed(1)
train.index  <- sample(nrow(data), round(0.7 * nrow(data)))
train <- data[ train.index,] %>% as_tibble()
test  <- data[-train.index,] %>% as_tibble()

# レコード数
train %>% dim()
test %>% dim()

# 列番号取得
cls <- which(colnames(airquality) == "Ozone")

# ラベル欠損の作成
# --- 訓練データの5%のみ保存（その他はNA）
labeled.index <- sample(nrow(train), round(0.1 * nrow(train)))
train[-labeled.index,cls] <- NA

# データ確認
# --- Wine列は大半が欠損している
train %>% print()
train %>% plot_missing()


# 2 決定木 --------------------------------------------------------------------------------

# モデル構築
# --- 決定木（rpart）
# --- ラベル数(10)の1/4で｢2｣に設定（NAでないラベルの数に合わせて設定）
m_dt <-
  SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                   w = 0.3) %>%
    fit(Ozone ~ ., data = train)

# 検証データ作成
m_dt %>%
  predict(test)%>%
  bind_cols(test) %>%
  metrics(truth = "Ozone", estimate = .pred)


# 3 ランダムフォレスト -----------------------------------------------------------------

# ランダムフォレストで共訓練
m_rf1 <-
  SSLRRandomForest(trees = 5,  w = 0.3) %>%
    fit(Ozone ~ ., data = train)

# ステップ1：parsnipモデルの設定
m_rf <-
  rand_forest( mode = "regression") %>%
  set_engine("ranger")

# ステップ2：共訓練
m_co_rf <-
  coBC(learner = m_rf, max.iter = 1) %>%
    fit(Ozone ~ ., data = train)


# 4 k近傍法 -------------------------------------------------------------------------

library(kknn)
m_coreg <- COREG(max.iter = 1)  %>% fit(Ozone ~ ., data = train)
