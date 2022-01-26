
# ポアソン分布 ------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
lambda <- 4

# 確率変数の値を指定
x <- 2


# 定義式により確率を計算
prob <- lambda^x / gamma(x + 1) * exp(-lambda)
prob

# 対数をとった定義式により確率を計算
log_prob <- x * log(lambda) - lgamma(x + 1) - lambda
prob <- exp(log_prob)
prob; log_prob

# ポアソン分布の関数により確率を計算
prob <- dpois(x = x, lambda = lambda)
prob

# ポアソン分布の対数をとった関数により確率を計算
log_prob <- dpois(x = x, lambda = lambda, log = TRUE)
prob <- exp(log_prob)
prob; log_prob


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
lambda <- 4


# 定義式により平均を計算
E_x <- lambda
E_x

# 定義式により分散を計算
V_x <- lambda
V_x


# 分布の可視化 ------------------------------------------------------------------

# パラメータを指定
lambda <- 4

# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(lambda) * 4)

# ポアソン分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = lambda) # 確率
)

# ポアソン分布を作図
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda)) # ラベル

# 統計量の計算
E_x <- lambda
s_x <- sqrt(lambda)

# 統計量を重ねたポアソン分布を作図
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  geom_vline(xintercept = E_x, color = "orange", size = 1, linetype = "dashed") + # 平均
  geom_vline(xintercept = E_x - s_x, color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = E_x + s_x, color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda)) # ラベル


# パラメータと分布の形状の関係 ----------------------------------------------------------

# lambdaとして利用する値を指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)
length(lambda_vals) # フレーム数

# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(max(lambda_vals)) * 2)

# lambdaの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(lambda in lambda_vals) {
  # ポアソン分布を計算
  tmp_prob_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    probability = dpois(x = x_vals, lambda = lambda), # 確率
    parameter = paste0("lambda=", lambda) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 計算結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}

# アニメーション用のポアソン分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  gganimate::transition_manual(parameter) + # フレーム
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(lambda_vals), fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### 乱数を生成 -----

# パラメータを指定
lambda <- 4

# データ数(サンプルサイズ)を指定
N <- 1000

# ポアソン分布に従う乱数を生成
x_n <- rpois(n = N, lambda = lambda)

# 乱数を集計して格納
freq_df <- tidyr::tibble(x = x_n) %>% # 乱数を格納
  dplyr::count(x, name = "frequency") %>% # 頻度を測定
  dplyr::mutate(proportion = frequency / N) # 構成比を計算


# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(lambda) * 4)

# ポアソン分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = lambda) # 確率
)


### 乱数の可視化 -----

# サンプルのヒストグラムを作成
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # ヒストグラム
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda)) # ラベル

# サンプルの構成比を作図
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", fill = "#00A968") + # 構成比
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda)) # ラベル


### アニメーションによる可視化 -----

# フレーム数を指定
N_frame <- 100

# 乱数を1つずつ生成
x_n <- rep(NA, times = N_frame)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_prob_df <- tidyr::tibble()
for(n in 1:N_frame) {
  # ポアソン分布に従う乱数を生成
  x_n[n] <- rpois(n = 1, lambda = lambda)
  
  # 乱数を集計して格納
  tmp_freq_df <- tidyr::tibble(x = x_n[1:n]) %>% # 乱数を格納
    dplyr::count(x, name = "frequency") %>% # 頻度を集計
    dplyr::full_join(tidyr::tibble(x = x_vals), by = "x") %>% # サンプルにない値を補完
    dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) %>% # サンプルにない場合のNAを0に置換
    dplyr::mutate(proportion = frequency / n) # 構成比を計算

  # ラベル用のテキストを作成
  label_text <- paste0(
    "lambda=", lambda, ", N=", n, "=(", paste0(tmp_freq_df[["frequency"]], collapse = ", "), ")"
  )
  
  # フレーム切替用のラベルを付与
  tmp_freq_df <- tmp_freq_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    x = x_n[n], # サンプル
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
anime_freq_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N_frame, fps = 100)


# アニメーション用のサンプルの構成比を
anime_prop_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = proportion), 
           stat = "identity", fill = "#00A968") + # 構成比
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(-0.01, 0.5)) + # y軸の表示範囲
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N_frame, fps = 100)

