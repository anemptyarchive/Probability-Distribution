
# 1次元ガウス分布 ----------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# 確率密度の計算 -----------------------------------------------------------------

# 平均を指定
mu <- 1

# 標準偏差を指定
sigma <- 2.5

# 確率変数の値を指定
x <- 1


# 定義式により確率密度を計算
C <- 1 / sqrt(2 * pi * sigma^2)
dens <- C * exp(-0.5 * (x - mu)^2 / sigma^2)
dens

# 対数をとった定義式により確率密度を計算
log_C <- -0.5 * log(2 * pi) - log(sigma)
log_dens <- log_C - 0.5 * (x - mu)^2 / sigma^2
dens <- exp(log_dens)
dens; log_dens

# ガウス分布の関数により確率密度を計算
dens <- dnorm(x = x, mean = mu, sd = sigma)
dens

# ガウス分布の対数をとった関数により確率密度を計算
log_dens <- dnorm(x = x, mean = mu, sd = sigma, log = TRUE)
dens <- exp(log_dens)
dens; log_dens


# 統計量の計算 ------------------------------------------------------------------

# 平均を指定
mu <- 1

# 標準偏差を指定
sigma <- 2.5


# 平均を計算
E_x <- mu
E_x

# 分散を計算
V_x <- sigma^2
V_x


# 分布の可視化 ------------------------------------------------------------------

# 平均を指定
mu <- 0

# 標準偏差を指定
sigma <- 1

# 作図用のxの点を作成
x_vals <- seq(from = mu - sigma*4, to = mu + sigma*4, length.out = 250)

# 1次元ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)

# 1次元ガウス分布を作図
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968") + # 折れ線グラフ
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", sigma=", sigma)) # ラベル

# 統計量を重ねた1次元ガウス分布を作図
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968") + # 分布
  geom_vline(xintercept = mu, color = "orange", size = 1, linetype = "dashed") + # 平均
  geom_vline(xintercept = mu - sigma, color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = mu + sigma, color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", sigma=", sigma)) # ラベル


# パラメータと分布の形状の関係 ----------------------------------------------------------

### 平均の影響 -----

# muとして利用する値を指定
mu_vals <- seq(from = -5, to = 5, by = 0.1)
length(mu_vals) # フレーム数

# 標準偏差を指定
sigma <- 1

# 作図用のxの点を作成
x_vals <- seq(from = median(mu_vals) - sigma*4, to = median(mu_vals) + sigma*4, length.out = 250)

# muの値ごとに分布を計算
anime_dens_df <- tidyr::tibble()
for(mu in mu_vals) {
  # 1次元ガウス分布を計算
  tmp_dens_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = sigma), # 確率密度
    parameter = paste0("mu=", round(mu, 1), ", sigma=", sigma) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
}

# アニメーション用の1次元ガウス分布を作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968") + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 100)


### 標準偏差の影響 -----

# sigmaとして利用する値を指定
sigma_vals <- seq(from = 1, to = 10, by = 0.1)
length(sigma_vals) # フレーム数

# 平均を指定
mu <- 0

# 作図用のxの点を作成
x_vals <- seq(from = mu - max(sigma_vals)*2, to = mu + max(sigma_vals)*2, length.out = 250)

# sigmaの値ごとに分布を計算
anime_dens_df <- tidyr::tibble()
for(sigma in sigma_vals) {
  # 1次元ガウス分布を計算
  tmp_dens_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = sigma), # 確率密度
    parameter = paste0("mu=", mu, ", sigma=", round(sigma, 1)) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
}

# アニメーション用の1次元ガウス分布を作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968") + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### 乱数を生成 -----

# 平均を指定
mu <- 1

# 標準偏差を指定
sigma <- 2.5

# データ数を指定
N <- 1000

# 1次元ガウス分布に従う乱数を生成
x_n <- rnorm(n = N, mean = mu, sd = sigma)

# サンプルを格納
data_df <- tidyr::tibble(x = x_n)

# 作図用のxの点を作成
x_vals <- seq(from = mu - sigma*4, to = mu + sigma*4, length.out = 250)

# 1次元ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)


### 乱数の可視化 -----

# サンプルの頻度を作図
ggplot(data = data_df, mapping = aes(x = x)) + # 
  geom_histogram(breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 50), 
                 fill = "#00A968") + # 
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", sigma=", sigma, ", N=", N)) # ラベル

# サンプルの密度を格納
ggplot(data = data_df, mapping = aes(x = x, y = ..density..)) + # 
  geom_histogram(breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 50), 
                 fill = "#00A968", linetype = "dashed") + # 
  geom_line(data = dens_df, mapping = aes(x = x, y = density), color = "darkgreen") + # 
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", sigma=", sigma, ", N=", N)) # ラベル


### アニメーションによる可視化 -----

# フレーム数を指定
N_frame <- 100

# 乱数を1つずつ生成
lambda_n <- rep(NA, times = N_frame)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_dens_df <- tidyr::tibble()
for(n in 1:N_frame) {
  # 1次元ガウス分布に従う乱数を生成
  x_n[n] <- rnorm(n = 1, mean = mu, sd = sigma)
  
  # ラベル用のテキストを作成
  label_text <- paste0("mu=", mu, ", sigma=", sigma, ", N=", n)
  
  # n個の乱数を格納
  tmp_freq_df <- tidyr::tibble(
    x = x_n[1:n], # サンプル
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    x = x_n[n], # サンプル
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
anime_freq_graph <- ggplot() + # 
  geom_histogram(data = anime_freq_df, mapping = aes(x = x), 
                 breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 50), 
                 fill = "#00A968") + # 頻度
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N_frame, fps = 100)


# アニメーション用のサンプルの密度を作図
anime_prop_graph <- ggplot() + # 
  geom_histogram(data = anime_freq_df, mapping = aes(x = x, y = ..density..), 
                 breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 50), 
                 fill = "#00A968") + # 密度
  geom_line(data = anime_dens_df, mapping = aes(x = x, y = density), 
            color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  ylim(c(0, max(dens_df[["density"]]) + 0.3)) + # y軸の表示範囲
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N_frame, fps = 100)


# 分布の生成 -------------------------------------------------------------------

# 超パラメータを指定
mu_prior <- 1
sigma_prior <- 2.5

# サンプルサイズを指定
N <- 10

# 1次元ガウス分布の平均パラメータを生成
mu_n <- rnorm(n = N, mean = mu_prior, sd = sigma_prior)

# 平均パラメータの期待値を計算
E_mu <- mu_prior

# 標準偏差パラメータを指定
sigma <- 1

# 作図用のxの点を作成
x_vals <- seq(from = E_mu - sigma*5, to = E_mu + sigma*5, length.out = 250)

# 平均パラメータの期待値による1次元ガウス分布を計算
res_dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = sigma), # 確率密度
  parameter = paste0("E[mu]=", E_mu) %>% 
    as.factor() # 色分け用のラベル
)

# サンプルごとに分布を計算
for(mu in mu_n) {
  # 1次元ガウス分布を計算
  tmp_dens_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = sigma), # 確率密度
    parameter = paste0("mu=", round(mu, 2)) %>% 
      as.factor() # 色分け用のラベル
  )
  
  # 結果を結合
  res_dens_df <- rbind(res_dens_df, tmp_dens_df)
}

# サンプルによる1次元ガウス分布を作図
ggplot() + 
  geom_line(data = res_dens_df, 
            mapping = aes(x = x, y = density, color = parameter, 
                          alpha = parameter, linetype = parameter), size = 0.8) + # 分布
  scale_linetype_manual(values = c("dashed", rep("solid", times = N))) + # 線の種類
  scale_alpha_manual(values = c(1, rep(0.5, times = N))) + # 透過度
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu_pri=", mu_prior, ", sigma_pri=", sigma_prior, ", sigma=", sigma), 
       x = expression(lambda)) # ラベル


