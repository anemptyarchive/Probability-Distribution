
# カテゴリ分布 ------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
phi_v <- c(0.2, 0.4, 0.1, 0.3)

# 確率変数の値を指定
x_v <- c(0, 1, 0, 0)


# 定義式により確率を計算
prob <- prod(phi_v^x_v)
prob

# 対数をとった定義式により確率を計算
log_prob <- sum(x_v * log(phi_v))
prob <- exp(log_prob)
prob; log_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = 1, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
log_prob <- dmultinom(x = x_v, size = 1, prob = phi_v, log = TRUE)
prob <- exp(log_prob)
prob; log_prob

# インデックスにより確率を抽出
v <- which(x_v == 1)
prob <- phi_v[v]
prob


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
phi_v <- c(0.2, 0.4, 0.1, 0.3)

# クラス番号を指定
v <- 1

# クラスvの平均を計算
E_x <- phi_v[v]
E_x

# クラスvの分散を計算
V_x <- phi_v[v] * (1 - phi_v[v])
V_x


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
phi_v <- c(0.2, 0.4, 0.1, 0.3)

# クラス数を取得
V <- length(phi_v)

# カテゴリ分布の情報を格納
prob_df <- tidyr::tibble(
  v = 1:V, # クラス
  probability = phi_v # 確率
)

# カテゴリ分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = v, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  labs(title = "Categorical Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), ")")) # ラベル


# パラメータと分布の形状の関係：アニメーション --------------------------------------------------

# 作図用のphi_1の値を作成
phi_vals <- seq(from = 0, to = 1, by = 0.01)

# クラス数を指定
V <- 3

# phiの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(phi in phi_vals) {
  # phi_1以外の割り当てを指定
  phi_v <- c(phi, (1 - phi) * 0.6, (1 - phi) * 0.4)
  
  # カテゴリ分布の情報を格納
  tmp_prob_df <- tidyr::tibble(
    v = 1:V, # クラス
    probability = phi_v, # 確率
    parameter = paste0("phi=(", paste0(round(phi_v, 2), collapse = ", "), ")") %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}

# アニメーション用のカテゴリ分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = v, y = probability)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 分布
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Categorical Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### ・乱数の可視化 -----

# パラメータを指定
phi_v <- c(0.2, 0.4, 0.1, 0.3)

# クラス数を取得
V <- length(phi_v)

# データ数を指定
N <- 1000

# カテゴリ分布に従う乱数を生成
x_nv <- rmultinom(n = N, size = 1, prob = phi_v) %>% 
  t()

# クラス番号を抽出
x_n <- which(x = t(x_nv) == 1, arr.ind = TRUE)[, "row"]

# 乱数を集計して格納
freq_df <- tidyr::tibble(
  v = 1:V, # クラス
  frequency = colSums(x_nv), # 頻度
  proportion = frequency / N # 構成比
)

# サンプルのヒストグラムを作成
ggplot(data = freq_df, mapping = aes(x = v, y = frequency)) + # データ
  geom_bar(stat = "identity", position = "dodge", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  labs(title = "Categorical Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), ")", 
                         ", N=", N, "=(", paste0(colSums(x_nv), collapse = ", "), ")")) # ラベル

# カテゴリ分布の情報を格納
prob_df <- tidyr::tibble(
  v = 1:V, # クラス
  probability = phi_v # 確率
)

# サンプルの構成比を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = v, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = prob_df, mapping = aes(x = v, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  labs(title = "Categorical Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), ")", 
                         ", N=", N, "=(", paste0(colSums(x_nv), collapse = ", "), ")")) # ラベル


### ・アニメーションによる可視化 -----

# データ数を指定
N <- 100

# 乱数を1つずつ生成
x_nv <- matrix(NA, nrow = N, ncol = V)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_prob_df <- tidyr::tibble()
for(n in 1:N) {
  # カテゴリ分布に従う乱数を生成
  x_nv[n, ] <- rmultinom(n = 1, size = 1, prob = phi_v) %>% 
    as.vector()
  
  # ラベル用のテキストを作成
  label_text <- paste0(
    "phi=(", paste0(phi_v, collapse = ", "), ")", 
    ", N=", n, "=(", paste0(colSums(x_nv, na.rm = TRUE), collapse = ", "), ")"
  )
  
  # n個の乱数を集計して格納
  tmp_freq_df <- tidyr::tibble(
    v = 1:V, # クラス
    frequency = colSums(x_nv, na.rm = TRUE), # 頻度
    proportion = frequency / n, # 割合
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    v = which(x_nv[n, ] == 1), # サンプルのクラス番号
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_prob_df <- prob_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}


# アニメーション用のサンプルのヒストグラムを作成
anime_freq_graph <- ggplot() + # データ
  geom_bar(data = anime_freq_df, mapping = aes(x = v, y = frequency), 
           stat = "identity", position = "dodge", fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = v, y = 0), 
             color = "orange", size = 5) + # サンプル
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Categorical Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 100)


# アニメーション用のサンプルの構成比を作図
anime_prop_graph <- ggplot() + # データ
  geom_bar(data = anime_freq_df, mapping = aes(x = v, y = proportion), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 構成比
  geom_bar(data = anime_prob_df, mapping = aes(x = v, y = probability), 
           stat = "identity", position = "dodge", alpha = 0, color = "darkgreen", linetype = "dashed") + # 真の分布
  geom_point(data = anime_data_df, mapping = aes(x = v, y = 0), 
             color = "orange", size = 5) + # サンプル
  scale_x_continuous(breaks = 0:V, labels = 0:V) + # x軸目盛
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Categorical Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100)


