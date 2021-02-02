# *********************************************************************************************
# Title     : Classification Modeling
# Objective : TODO
# Created by: Owner
# Created on: 2021/01/25
# URL       : https://dicits.ugr.es/software/SSLR/articles/classification.html
# *********************************************************************************************


# ＜概要＞
# - 決定木アルゴリズムを用いて分類問題を半教師あり学習


# ＜目次＞
# 0 準備
# 1 データ操作
# 2 モデル構築
# 3 予測精度の検証
# 4 予測データの作成


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
train <- wine[ train.index,] %>% as_tibble()
test  <- wine[-train.index,] %>% as_tibble()

# レコード数
train %>% dim()
test %>% dim()

# 列番号取得
# --- Wineは14列目
cls <- which(colnames(wine) == "Wine")

# ラベル欠損の作成
# --- 訓練データの5%のみ保存（その他はNA）
labeled.index <- wine$Wine %>% createDataPartition(p = .05, list = FALSE)
train[-labeled.index, cls] <- NA

# データ確認
# --- Wine列は大半が欠損している
train %>% print()
train %>% plot_missing()


# 2 モデル構築 ----------------------------------------------------------------------------------

# モデル構築
# --- 決定木（rpart）
# --- ラベル数(10)の1/4で｢2｣に設定（NAでないラベルの数に合わせて設定）
m <-
  SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                   w = 0.3) %>%
    fit(Wine ~ ., data = train)



# 3 予測精度の検証 -----------------------------------------------------------------------------

# 検証データ作成
# --- 正解データと予測データを並べて表示
test_results <-
  test %>%
    select(Wine) %>%
    as_tibble() %>%
    mutate(dt_class = m %>% predict(test) %>% pull(.pred_class))

# 確認
test_results %>% print()

# 予測精度の検証
test_results %>% accuracy(truth = Wine, estimate = dt_class)

# 混合行列の作成
test_results %>% conf_mat(truth = Wine, estimate = dt_class)

# 複数のメトリックを一括表示
multi_metric <- metric_set(accuracy, kap, sens, spec, f_meas )
test_results %>% multi_metric(truth = Wine, estimate = dt_class)


# 4 予測データの作成 ----------------------------------------------------------------------------

# 分類結果
m %>% predict(test, "raw")

# クラス確率
m %>% predict(test, "prob")
