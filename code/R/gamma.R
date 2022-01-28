
# ガンマ分布 -------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
a <- 2
b <- 2

# 確率変数の値を指定
lambda <- 2


# 定義式により確率密度を計算
C <- b^a / gamma(a)
dens <- C * lambda^(a - 1) * exp(-b * lambda)
dens

# 対数をとった定義式により確率密度を計算
log_C <- a * log(b) - lgamma(a)
log_dens <- log_C + (a - 1) * log(lambda) - b * lambda
dens <- exp(log_dens)
dens; log_dens

# ガンマ分布の関数により確率密度を計算
dens <- dgamma(x = lambda, shape = a, rate = b)
dens

# ガンマ分布の関数により確率密度を計算
dens <- dgamma(x = lambda, shape = a, scale = 1 / b)
dens

# ガンマ分布の対数をとった関数により確率密度を計算
log_dens <- dgamma(x = lambda, shape = a, rate = b, log = TRUE)
dens <- exp(log_dens)
dens; log_dens

# ガンマ分布の対数をとった関数により確率密度を計算
log_dens <- dgamma(x = lambda, shape = a, scale = 1 / b, log = TRUE)
dens <- exp(log_dens)
dens; log_dens


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
a <- 2
b <- 2


# 平均を計算
E_lambda <- a / b
E_lambda

# 分散を計算
V_lambda <- a / b^2
V_lambda

# 最頻値を計算
mode_lambda <- (a - 1) / b
mode_lambda


# 分布の可視化 ------------------------------------------------------------------

# パラメータを指定
a <- 2
b <- 2

# 作図用のlambdaの点を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 250)

# ガンマ分布を計算
dens_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)


# ガウス分布を作図
ggplot(data = dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968") + # 折れ線グラフ
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


# 統計量の計算
E_lambda <- a / b
s_lambda <- sqrt(a / b^2)
mode_lambda <- (a - 1) / b

# 統計量を重ねたガウス分布を作図
ggplot(data = dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968") + # 分布
  geom_vline(xintercept = E_lambda, color = "orange", size = 1, linetype = "dashed") + # 平均
  geom_vline(xintercept = E_lambda - s_lambda, color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = E_lambda + s_lambda, color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  geom_vline(xintercept = mode_lambda, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


# パラメータと分布の形状の関係 ----------------------------------------------------------

# パラメータとして利用する値を指定
a_vals <- seq(from = 0.1, to = 10, by = 0.1)
b_vals <- seq(from = 0.1, to = 10, by = 0.1)

# 固定するパラメータを指定
a <- 2
b <- 2

# 作図用のlambdaの点を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 250)

# a, bの値ごとに分布を計算
anime_dens_df <- tidyr::tibble()
#for(a in a_vals) {
for(b in b_vals) {
  # ガンマ分布を計算
  tmp_dens_df <- tidyr::tibble(
    lambda = lambda_vals, # 確率変数
    density = dgamma(x = lambda_vals, shape = a, rate = b), # 確率密度
    parameter = paste0("a=", a, ", b=", b) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
}

# アニメーション用のガウス分布を作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968") + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(a_vals), fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### 乱数を生成 -----

# パラメータを指定
a <- 2
b <- 2

# データ数(サンプルサイズ)を指定
N <- 1000

# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# サンプルを格納
data_df <- tidyr::tibble(lambda = lambda_n)

# 作図用のlambdaの点を作成
lambda_vals <- seq(from = 0, to = max(lambda_n) + 1, length.out = 250)

# ガンマ分布を計算
dens_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)


### 乱数の可視化 -----

# サンプルの頻度を作図
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = lambda), 
                 breaks = seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968", color = "#00A968") + # ヒストグラム
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル

# サンプルの密度を作図
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = lambda, y = ..density..), 
                 breaks = seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968", color = "#00A968") + # ヒストグラム
  geom_line(data = dens_df, mapping = aes(x = lambda, y = density), 
            color = "darkgreen", linetype = "dashed") + # 元の分布
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


### アニメーションによる可視化 -----

# フレーム数を指定
N_frame <- 100

# 乱数を1つずつ生成
lambda_n <- rep(NA, times = N_frame)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_dens_df <- tidyr::tibble()
for(n in 1:N_frame) {
  # ガンマ分布に従う乱数を生成
  lambda_n[n] <- rgamma(n = 1, shape = a, rate = b)
  
  # ラベル用のテキストを作成
  label_text <- paste0("a=", a, ", b=", b, ", N=", n)
  
  # n個の乱数を格納
  tmp_freq_df <- tidyr::tibble(
    lambda = lambda_n[1:n], # サンプル
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    lambda = lambda_n[n], # サンプル
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_dens_df <- dens_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
}


# アニメーション用のサンプルの頻度を作図
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = lambda), 
                 breaks = seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968", color = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = lambda, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N_frame, fps = 100)


# アニメーション用のサンプルの密度を作図
anime_prop_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = lambda, y = ..density..), 
                 breaks = seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968", color = "#00A968") + # ヒストグラム
  geom_line(data = anime_dens_df, mapping = aes(x = lambda, y = density), 
            color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = lambda, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  ylim(c(-0.01, max(dens_df[["density"]]) * 2)) + # y軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N_frame, fps = 100)


# 分布の生成 -------------------------------------------------------------------

### パラメータを生成 -----

# パラメータを指定
a <- 5
b <- 2

# サンプルサイズを指定
N <- 10

# ガウス分布・ポアソン分布のパラメータを生成
lambda_n <- rgamma(n = N, shape = a, rate = b)


### 分布の作図:(1次元ガウス分布) -----

# 平均パラメータを指定
mu = 0.0

# 精度パラメータの期待値を計算
E_lambda <- a / b

# 標準偏差の期待値を計算
E_sigma <- sqrt(1.0 / E_lambda)

# 作図用のxの点を作成
x_vals <- seq(from = mu - E_sigma*4, to = mu + E_sigma*4, length.out = 250)

# 精度パラメータの期待値による1次元ガウス分布を計算
res_dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = E_sigma), # 確率密度
  parameter = paste0("E[lambda]=", round(E_lambda, 2)) %>% 
    as.factor() # 色分け用のラベル
)

# lambdaの値ごとに分布を計算
for(lambda in lambda_n) {
  # 1次元ガウス分布を計算
  tmp_dens_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = sqrt(1 / lambda)), # 確率密度
    parameter = paste0("lambda=", round(lambda, 2)) %>% 
      as.factor() # 色分け用のラベル
  )
  
  # 結果を結合
  res_dens_df <- rbind(res_dens_df, tmp_dens_df)
}

# サンプルによる分布を作図
ggplot() + 
  geom_line(data = res_dens_df, 
            mapping = aes(x = x, y = density, color = parameter, alpha = parameter, linetype = parameter), size = 0.8) + # 分布
  scale_linetype_manual(values = c("dashed", rep("solid", times = N))) + # 線の種類
  scale_alpha_manual(values = c(1, rep(0.5, times = N))) + # 透過度
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


### 分布の作図:(ポアソン分布) -----

# パラメータの期待値を計算
E_lambda <- a / b

# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(E_lambda) * 4)

# パラメータの期待値によるガンマ分布を計算
res_prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = E_lambda), # 確率
  parameter = paste0("E[lambda]=", round(E_lambda, 2)) %>% 
    as.factor() # 色分け用のラベル
)

# lambdaの値ごとに分布を計算
for(lambda in lambda_n) {
  # ガンマ分布を計算
  tmp_prob_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    probability = dpois(x = x_vals, lambda = lambda), # 確率
    parameter = paste0("lambda=", round(lambda, 2)) %>% 
      as.factor() # 色分け用のラベル
  )
  
  # 結果を結合
  res_prob_df <- rbind(res_prob_df, tmp_prob_df)
}

# サンプルによる分布を作図
ggplot() + 
  geom_step(data = res_prob_df, 
            mapping = aes(x = x, y = probability, color = parameter, alpha = parameter, linetype = parameter), 
            direction = "mid", size = 0.8) + # 分布
  #geom_bar(data = res_prob_df, mapping = aes(x = x, y = probability, color = parameter, linetype = parameter), 
  #         stat = "identity", alpha = 0) + # 分布
  scale_linetype_manual(values = c("dashed", rep("solid", times = N))) + # 線の種類
  scale_alpha_manual(values = c(1, rep(0.5, times = N))) + # 透過度
  scale_x_continuous(breaks = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル

