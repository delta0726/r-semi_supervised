# *********************************************************************************************
# Title     : Model Fitting
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

# ラベル欠損の作成
# --- 訓練データの5%のみ保存（その他はNA）
labeled.index <- createDataPartition(wine$Wine, p = .2, list = FALSE)
train[-labeled.index, cls] <- NA

# データ確認
# --- Wine列は大半が欠損している
train$Wine %>% summary()
train %>% print()
train %>% dim()
train %>% plot_missing()


# 2 学習パターン ----------------------------------------------------------------------------------

# フォーミュラで学習
m <-
  SSLRDecisionTree() %>%
    fit(Wine ~ ., data = train)

# 引数で学習
# --- Xは行列、yはベクトル
m <-
  SSLRDecisionTree() %>%
    fit_xy(x = train[,-cls], y = train$Wine)



# 3 半教師ありを明示して学習 ------------------------------------------------------------------------

# 引数となるデータ
# --- x  : ラベルありの観測値のX
# --- y  : ラベルありの観測値のy
# --- x_U: ラベルなしの観測値のX
train[labeled.index,-cls] %>% dim()
train[labeled.index,cls] %>% length()
train[-labeled.index,-cls] %>% dim()

# 学習
m <-
  SSLRDecisionTree() %>%
    fit_x_u(x = train[labeled.index,-cls],
            y = train[labeled.index,cls],
            x_U = train[-labeled.index,-cls])
