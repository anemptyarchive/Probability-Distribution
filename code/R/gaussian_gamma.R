
# ガウス-ガンマ分布の作図 ---------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)


# 確率密度の計算 -----------------------------------------------------------------

# 1次元ガウス分布のパラメータを指定
m <- 0

# 1次元ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 確率変数の値を指定
mu     <- 1.5
lambda <- 2.5


# 定義式により確率密度を計算
C_N      <- 1 / sqrt(2 * pi / beta / lambda)
dens_N   <- C_N * exp(-0.5 * beta * lambda * (mu - m)^2)
C_Gam    <- b^a / gamma(a)
dens_Gam <- C_Gam * lambda^(a - 1) * exp(-b * lambda)
dens     <- dens_N * dens_Gam
dens

# 対数をとった定義式により確率密度を計算
log_C_N      <- -0.5 * (log(2 * pi) - log(beta * lambda))
log_dens_N   <- log_C_N - 0.5 * beta * lambda * (mu - m)^2
log_C_Gam    <- a * log(b) - lgamma(a)
log_dens_Gam <- log_C_Gam + (a - 1) * log(lambda) - b * lambda
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens_N   <- dnorm(x = mu, mean = m, sd = sqrt(1 / beta / lambda))
dens_Gam <- dgamma(x = lambda, shape = a, rate = b)
dens     <- dens_N * dens_Gam
dens

# 対数をとった関数により確率密度を計算
log_dens_N   <- dnorm(x = mu, mean = m, sd = sqrt(1 / beta / lambda), log = TRUE)
log_dens_Gam <- dgamma(x = lambda, shape = a, rate = b, log = TRUE)
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens


# 統計量の計算 -----------------------------------------------------------------

# 1次元ガウス分布のパラメータを指定
m <- 0

# 1次元ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 平均を計算
E_mu <- m
E_lambda <- a / b
E_mu; E_lambda

# 分散を計算
V_mu <- 1 / beta / E_lambda
V_lambda <- a / b^2
V_mu; V_lambda

# 最頻値を計算
mode_lambda <- (a - 1) / b
mode_lambda


# 分布の可視化 -----------------------------------------------------------------

### ・ガウス-ガンマ分布の作図 -----

# 1次元ガウス分布の平均パラメータを指定
m <- 0

# 1次元ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 作図用のmuの値を作成
mu_vals <- seq(from = -3, to = 3, length.out = 201)

# 作図用のlambdaの値を作成
lambda_vals <- seq(from = 0.01, to = 2, length.out = 200)

# 作図用のmuとlambdaの点を作成
mu_lambda_points <- expand.grid(mu = mu_vals, lambda = lambda_vals) %>% 
  as.matrix()
mu_lambda_points <- tidyr::tibble(
  mu = rep(mu_vals, times = length(lambda_vals)), # muの値
  lambda = rep(lambda_vals, each = length(mu_vals)) # lambdaの値
) %>% 
  as.matrix()

# ガウス-ガンマ分布を計算
dens_df <- tidyr::tibble(
  mu = mu_lambda_points[, 1], # 確率変数mu
  lambda = mu_lambda_points[, 2], # 確率変数lambda
  N_dens = dnorm(x = mu, mean = m, sd = sqrt(1 / beta / lambda)), # muの確率密度
  Gam_dens = dgamma(x = lambda, shape = a, rate = b), # lambdaの確率密度
  density = N_dens * Gam_dens # 確率密度
)

# ガウス-ガンマ分布を作図
ggplot() + 
  geom_contour(data = dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 等高線図
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b), 
       x = expression(mu), y = expression(lambda), 
       color = "density") # ラベル


# 統計量を計算
E_lambda    <- a / b
s_lambda    <- sqrt(a / b^2)
mode_lambda <- (a - 1) / b
E_mu    <- m
E_s_mu  <- sqrt(1 / beta / E_lambda)
s_mu_df <- tidyr::tibble(
  lambda = lambda_vals, 
  s_minus = E_mu - sqrt(1 / beta / lambda_vals), # 平均 - 標準偏差
  s_plus = E_mu + sqrt(1 / beta / lambda_vals)   # 平均 + 標準偏差
)

# 統計量を重ねたガウス-ガンマ分布を作図
ggplot() + 
  geom_contour(data = dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 分布
  geom_vline(xintercept = m, color = "#00A968", size = 1.5, linetype = "dashed") + # 平均
  geom_line(data = s_mu_df, mapping = aes(x = s_minus, y = lambda), color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 - 標準偏差
  geom_line(data = s_mu_df, mapping = aes(x = s_plus, y = lambda), color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 + 標準偏差
  geom_hline(yintercept = E_lambda, color = "orange", size = 1.5, linetype = "dashed") + # 平均
  geom_hline(yintercept = E_lambda - s_lambda, color = "orange", size = 1.5, linetype = "dotted") + # 平均 - 標準偏差
  geom_hline(yintercept = E_lambda + s_lambda, color = "orange", size = 1.5, linetype = "dotted") + # 平均 + 標準偏差
  geom_hline(yintercept = mode_lambda, color = "chocolate", size = 1.5, linetype = "dashed") + # 最頻値
  xlim(c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b), 
       x = expression(mu), y = expression(lambda), 
       color = "density")


### ・mu軸側から見たグラフ -----

# 精度の期待値による1次元ガウス分布を計算
dens_N_df <- tidyr::tibble(
  mu = mu_vals, # 確率変数
  density = dnorm(x = mu_vals, mean = m, sd = sqrt(1 / beta / E_lambda)) # 確率密度
)

# 統計量を重ねた精度の期待値による1次元ガウス分布を作図
ggplot() + 
  geom_line(data = dens_N_df, mapping = aes(x = mu, y = density), color = "#00A968") + # 分布
  geom_vline(xintercept = E_mu, color = "#00A968", size = 1.5, linetype = "dashed") + # 平均
  geom_vline(xintercept = E_mu - E_s_mu, color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = E_mu + E_s_mu, color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 + 標準偏差
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", E[lambda]=", round(E_lambda, 2)), 
       x = expression(mu), y = expression(p(mu)))


# lambdaの値ごとに分布を計算
anime_dens_N_df <- tidyr::tibble()
anime_s_mu_df    <- tidyr::tibble()
for(lambda in lambda_vals) {
  # ラベル用のテキストを作成
  label_text <- paste0("m=", m, ", beta=", beta, ", lambda=", round(lambda, 2))
  
  # 1次元ガウス分布を計算
  tmp_dens_N_df <- tidyr::tibble(
    mu = mu_vals, # 確率変数
    density = dnorm(x = mu_vals, mean = m, sd = sqrt(1 / beta / lambda)), # 確率密度
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # 標準偏差を格納
  tmp_s_mu_df <- tidyr::tibble(
    s_minus = m - sqrt(1 / beta / lambda), # 平均 - 標準偏差
    s_plus = m + sqrt(1 / beta / lambda),  # 平均 + 標準偏差
    parameter = as.factor(label_text)      # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_dens_N_df <- rbind(anime_dens_N_df, tmp_dens_N_df)
  anime_s_mu_df   <- rbind(anime_s_mu_df, tmp_s_mu_df)
}

# アニメーション用の統計量を重ねた1次元ガウス分布を作図
anime_dens_N_graph <- ggplot() + 
  geom_line(data = anime_dens_N_df, mapping = aes(x = mu, y = density), color = "#00A968") + # 分布
  geom_vline(xintercept = E_mu, color = "#00A968", size = 1.5, linetype = "dashed") + # 平均
  geom_vline(data = anime_s_mu_df, mapping = aes(xintercept = s_minus), color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(data = anime_s_mu_df, mapping = aes(xintercept = s_plus), color = "#00A968", size = 1.5, linetype = "dotted") + # 平均 + 標準偏差
  gganimate::transition_manual(parameter) + # フレーム
  xlim(c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(p(mu)))

# gif画像を作成
gganimate::animate(anime_dens_N_graph, nframes = length(lambda_vals), fps = 100)


### ・lambda軸側から見たグラフ -----

# ガンマ分布を計算
dens_Gam_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)

# 統計量を重ねたガンマ分布を作図
ggplot() + 
  geom_line(data = dens_Gam_df, mapping = aes(x = lambda, y = density), color = "orange") + # 分布
  geom_vline(xintercept = E_lambda, color = "orange", size = 1.5, linetype = "dashed") + # 平均
  geom_vline(xintercept = E_lambda - s_lambda, color = "orange", size = 1.5, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = E_lambda + s_lambda, color = "orange", size = 1.5, linetype = "dotted") + # 平均 + 標準偏差
  geom_vline(xintercept = mode_lambda, color = "chocolate", size = 1.5, linetype = "dashed") + # 最頻値
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda), y = expression(p(lambda)))


# パラメータと分布の形状の関係 ----------------------------------------------------------

# パラメータとして利用する値を作成
m_vals    <- seq(from = -2, to = 2, by = 0.1)
beta_vals <- seq(from = 0.1, to = 10, by = 0.1)
a_vals    <- seq(from = 0.1, to = 10, by = 0.1)
b_vals    <- seq(from = 0.1, to = 10, by = 0.1)

# 固定するパラメータを指定
m    <- 0
beta <- 2
a    <- 5
b    <- 6

# 作図用の変数の値を作成
mu_vals     <- seq(from = -3, to = 3, length.out = 201)
lambda_vals <- seq(from = 0.01, to = 2, length.out = 200)

# フレーム数を設定
#frame_num <- length(m_vals)
#frame_num <- length(beta_vals)
#frame_num <- length(a_vals)
frame_num <- length(b_vals)

# パラメータの値ごとに分布を計算
anime_dens_df <- tidyr::tibble()
for(i in 1:frame_num) {
  # i番目の値を取得
  #m    <- m_vals[i]
  #beta <- beta_vals[i]
  #a    <- a_vals[i]
  b    <- b_vals[i]
  
  # ガウス-ガンマ分布を計算
  tmp_dens_df <- tidyr::tibble(
    mu = rep(mu_vals, times = length(lambda_vals)), # 確率変数mu
    lambda = rep(lambda_vals, each = length(mu_vals)), # 確率変数lambda
    N_dens = dnorm(x = mu, mean = m, sd = sqrt(1 / beta / lambda)), # muの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # lambdaの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0(
      "m=", round(m, 1), ", beta=", round(beta, 1), ", a=", round(a, 1), ", b=", round(b, 1)
    ) %>% 
      as.factor() # フレーム切替用のラベル
  )
  
  # 結果を結合
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
  
  # 途中経過を表示
  message("\r", appendLF = FALSE) # 前回の表示を消去
  message("\r", "i=", i, " (", round(i / frame_num * 100, 2), "%)", appendLF = FALSE)
}

# アニメーション用のガウス-ガンマ分布を作図
anime_dens_graph <- ggplot() + 
  geom_contour(data = anime_dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(lambda), 
       color = "density")

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = frame_num, fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# 1次元ガウス分布の平均パラメータを指定
m <- 0

# 1次元ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 作図用のmuの値を作成
mu_vals <- seq(from = -3, to = 3, length.out = 101)

# 作図用のlambdaの値を作成
lambda_vals <- seq(from = 0.02, to = 2, length.out = 100)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::tibble(
  mu = rep(mu_vals, times = length(lambda_vals)), # 確率変数mu
  lambda = rep(lambda_vals, each = length(mu_vals)), # 確率変数lambda
  N_dens = dnorm(x = mu, mean = m, sd = sqrt(1 / beta / lambda)), # muの確率密度
  Gam_dens = dgamma(x = lambda, shape = a, rate = b), # lambdaの確率密度
  density = N_dens * Gam_dens # 確率密度
)


# データ数を指定
N <- 10000

# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# 1次元ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = sqrt(1 / beta / lambda_n))

# サンプルを格納
data_df <- tidyr::tibble(
  mu = mu_n, # muのサンプル
  lambda = lambda_n # lambdaのサンプル
)


### ・乱数の可視化 -----

# サンプルの散布図を作成
ggplot() + 
  geom_point(data = data_df, mapping = aes(x = mu, y = lambda), color = "orange") + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", N), 
       x = expression(mu), y = expression(lambda), 
       color = "density") # ラベル


# サンプルのヒストグラムを作成
ggplot() + 
  geom_bin2d(data = data_df, mapping = aes(x = mu, y = lambda), alpha = 0.8) + # 頻度
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", N), 
       x = expression(mu), y = expression(lambda), 
       fill = "frequency", color = "density") # ラベル


# サンプルの密度を作図
ggplot() + 
  geom_density2d_filled(data = data_df, mapping = aes(x = mu, y = lambda, fill = ..level..), alpha = 0.8) + # 密度
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", N), 
       x = expression(mu), y = expression(lambda), 
       fill = "frequency", color = "density") # ラベル


### ・1データずつアニメーションで可視化 -----

# フレーム数(データ数)を指定
N_frame <- 100

# 乱数を1つずつ生成
#lambda_n <- rep(NA, times = N_frame)
#mu_n <- rep(NA, times = N_frame)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_dens_df <- tidyr::tibble()
for(n in 1:N_frame) {
  # ガンマ分布に従う乱数を生成
  #lambda_n[n] <- rgamma(n = 1, shape = a, rate = b)
  
  # 1次元ガウス分布に従う乱数を生成
  #mu_n[n] <- rnorm(n = 1, mean = m, sd = sqrt(1 / beta / lambda_n[n]))
  
  # ラベル用のテキストを作成
  label_text <- paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", n)
  
  # n個のサンプルを格納
  tmp_freq_df <- tidyr::tibble(
    mu = mu_n[1:n], # muのサンプル
    lambda = lambda_n[1:n], # lambdaのサンプル
    n = n, # データ数
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n番目のサンプルを格納
  tmp_data_df <- tidyr::tibble(
    mu = mu_n[n], # muのサンプル
    lambda = lambda_n[n], # lambdaのサンプル
    n = n, # データ数
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_dens_df <- dens_df %>% 
    dplyr::mutate(
      n = n, 
      parameter = as.factor(label_text)
    )
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
  
  # 途中経過を表示
  message("\r", appendLF = FALSE) # 前回の表示を消去
  message("\r", "n=", n, " (", round(n / N_frame * 100, 2), "%)", appendLF = FALSE)
}


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# アニメーション用のサンプルのヒストグラムを作成
anime_freq_graph <- ggplot() + 
  geom_bin2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             breaks = list(x = mu_breaks, y = lambda_breaks), alpha = 0.8) + # 頻度
  geom_point(data = anime_data_df, mapping = aes(x = mu, y = lambda), color = "orange", size = 5) + # サンプル
  geom_contour(data = anime_dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(lambda), 
       fill = "frequency", color = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N_frame, fps = 100)


# (データが少ないと密度が計算できないため)最初のフレームを除去
n_min <- 10
tmp_freq_df <- anime_freq_df %>% 
  dplyr::filter(n > n_min)
tmp_data_df <- anime_data_df %>% 
  dplyr::filter(n > n_min)
tmp_dens_df <- anime_dens_df %>% 
  dplyr::filter(n > n_min)

# (表示が上手くいかなくなるため)因子のレベルを再設定
tmp_freq_df[["parameter"]] <- factor(as.character(tmp_freq_df[["parameter"]]), levels = unique(tmp_freq_df[["parameter"]]))
tmp_data_df[["parameter"]] <- factor(as.character(tmp_data_df[["parameter"]]), levels = unique(tmp_data_df[["parameter"]]))
tmp_dens_df[["parameter"]] <- factor(as.character(tmp_dens_df[["parameter"]]), levels = unique(tmp_dens_df[["parameter"]]))

# アニメーション用のサンプルの密度を作図
anime_prop_graph <- ggplot() + 
  geom_density2d_filled(data = tmp_freq_df, mapping = aes(x = mu, y = lambda), alpha = 0.8) + # 密度
  geom_point(data = tmp_data_df, mapping = aes(x = mu, y = lambda), color = "orange", size = 5) + # サンプル
  geom_contour(data = tmp_dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  gganimate::transition_manual(parameter) + # フレーム
  xlim(c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  ylim(c(min(lambda_vals), max(lambda_vals))) + # y軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(lambda), 
       fill = "density", color = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N_frame - n_min, fps = 100)


### ・全データをアニメーションで可視化 -----

# フレーム数を指定
frame_num <- 100

# 1フレーム当たりのデータ数を計算
n_per_frame <- N %/% frame_num

# 乱数を1つずつ生成
anime_freq_df <- tidyr::tibble()
anime_dens_df <- tidyr::tibble()
for(i in 1:frame_num) {
  # ラベル用のテキストを作成
  label_text <- paste0(
    "m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", i * n_per_frame
  )
  
  # n個のサンプルを格納
  tmp_freq_df <- tidyr::tibble(
    mu = mu_n[1:(i*n_per_frame)], # muのサンプル
    lambda = lambda_n[1:(i*n_per_frame)], # lambdaのサンプル
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_dens_df <- dens_df %>% 
    dplyr::mutate( parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_dens_df <- rbind(anime_dens_df, tmp_dens_df)
  
  # 途中経過を表示
  message("\r", appendLF = FALSE) # 前回の表示を消去
  message("\r", "i=", i, " (", round(i / frame_num * 100, 2), "%)", appendLF = FALSE)
}


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# アニメーション用のサンプルのヒストグラムを作成
anime_freq_graph <- ggplot() + 
  geom_bin2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             breaks = list(x = mu_breaks, y = lambda_breaks), alpha = 0.8) + # 頻度
  geom_contour(data = anime_dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(lambda), 
       fill = "frequency", color = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num, fps = 100)


# アニメーション用のサンプルの密度を作図
anime_prop_graph <- ggplot() + 
  geom_density2d_filled(data = anime_freq_df, mapping = aes(x = mu, y = lambda), alpha = 0.8) + # 密度
  geom_contour(data = anime_dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  xlim(c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  ylim(c(min(lambda_vals), max(lambda_vals))) + # y軸の表示範囲
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = expression(lambda), 
       fill = "density", color = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = frame_num, fps = 100)


# 分布の生成 -------------------------------------------------------------------

# 1次元ガウス分布の平均パラメータを指定
m <- 0

# 1次元ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# サンプルサイズを指定
N <- 10

# 精度パラメータを生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# 平均パラメータを生成
mu_n <- rnorm(n = N, mean = m, sd = sqrt(1 / beta / lambda_n))


# 平均パラメータの期待値を計算
E_mu <- m

# 精度パラメータの期待値を計算
E_lambda <- a / b

# 標準偏差パラメータの期待値を計算
E_sigma <- sqrt(1 / E_lambda)

# 作図用のxの点を作成
x_vals <- seq(from = E_mu - E_sigma*5, to = E_mu + E_sigma*5, length.out = 250)

# パラメータの期待値による1次元ガウス分布を計算
res_dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = E_sigma), # 確率密度
  parameter = paste0("E[mu]=", E_mu, ", E[lambda]=", round(E_lambda, 2)) %>% 
    as.factor() # 色分け用のラベル
)


# サンプルごとに分布を計算
for(n in 1:N) {
  # n番目のパラメータを取得
  mu     <- mu_n[n]
  lambda <- lambda_n[n]
  
  # 1次元ガウス分布を計算
  tmp_dens_df <- tidyr::tibble(
    x = x_vals, # 確率変数
    density = dnorm(x = x_vals, mean = mu, sd = sqrt(1 / lambda)), # 確率密度
    parameter = paste0("mu=", round(mu, 2), ", lambda=", round(lambda, 2)) %>% 
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
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", N), 
       x = expression(x)) # ラベル


