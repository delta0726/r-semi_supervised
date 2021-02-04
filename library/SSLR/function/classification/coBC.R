# *********************************************************************************************
# Title     : 共訓練モデルのインターフェース
# Function  : coBCG
# Created by: Owner
# Created on: 2021/01/26
# URL       : https://dicits.ugr.es/software/SSLR/reference/coBC.html
# *********************************************************************************************


# ＜概要＞
# - CoBCは共同トレーニングスタイルの半教師あり学習アルゴリズム
#   --- ラベル付けされたサンプルの削減されたセットを使用して、学習器の引数で定義された学習スキームでN個の分類器をトレーニング
#   --- 最終的な予測は、N個のリグレッサーの推定値の平均


# ＜構文＞
# coBC(learner, N = 3, perc.full = 0.7, u = 100, max.iter = 50)
#
# - learner   ： Parsnipで作成した分類モードの学習器
# - N         ： コミッティーのメンバーとして
# - perc.full ： 新しいラベル付きの例の割合がこの値に達すると自己ラベル付けプロセスが停止(0-1)
# - u         ： プール内のラベルのないインスタンスの数
# - max.iter  ： 自己ラベル付けプロセスで実行する反復の最大数


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 ラベル欠損の作成
# 3 モデリング


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
# --- Parsnipで作成
# --- 未学習（フォーミュラ定義もしていない）
rf <-
  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest")

# 共学習モデル
# --- 訓練データで学習
# --- 大半のラベルがNA
m <-
  coBC(learner = rf, N = 3, perc.full = 0.7, u = 100, max.iter = 3) %>%
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
