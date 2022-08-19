
# ガウス-ガンマ分布 ---------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# チェック用
library(ggplot2)
library(patchwork)


# 確率密度の計算 -----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 確率変数の値を指定
mu     <- 1.5
lambda <- 2.5


# 定義式により確率密度を計算
C_N      <- sqrt(beta * lambda / 2 / pi)
dens_N   <- C_N * exp(-0.5 * beta * lambda * (mu - m)^2)
C_Gam    <- b^a / gamma(a)
dens_Gam <- C_Gam * lambda^(a - 1) * exp(-b * lambda)
dens     <- dens_N * dens_Gam
dens

# 対数をとった定義式により確率密度を計算
log_C_N      <- 0.5 * (log(beta * lambda) - log(2 * pi))
log_dens_N   <- log_C_N - 0.5 * beta * lambda * (mu - m)^2
log_C_Gam    <- a * log(b) - lgamma(a)
log_dens_Gam <- log_C_Gam + (a - 1) * log(lambda) - b * lambda
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens_N   <- dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda))
dens_Gam <- dgamma(x = lambda, shape = a, rate = b)
dens     <- dens_N * dens_Gam
dens

# 対数をとった関数により確率密度を計算
log_dens_N   <- dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda), log = TRUE)
log_dens_Gam <- dgamma(x = lambda, shape = a, rate = b, log = TRUE)
log_dens     <- log_dens_N + log_dens_Gam
dens         <- exp(log_dens)
dens; log_dens


# 統計量の計算 -----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 平均を計算
E_mu     <- m
E_lambda <- a / b
E_mu; E_lambda

# 分散を計算
V_mu     <- 1 / beta / E_lambda
V_lambda <- a / b^2
V_mu; V_lambda

# 最頻値を計算
mode_mu     <- m
mode_lambda <- (a - 1) / b
mode_mu; mode_lambda


# グラフの作成-----------------------------------------------------------------

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# 平均パラメータの期待値を計算
E_mu <- m

# 精度パラメータの期待値を計算
E_lambda <- a / b

# μの値を作成
mu_vals <- seq(
  from = E_mu - 1/sqrt(beta*E_lambda) * 4, 
  to = E_mu + 1/sqrt(beta*E_lambda) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = E_lambda * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )

# ガウス-ガンマ分布を作図
ggplot() + 
  geom_contour(data = dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 等高線
  #geom_contour_filled(data = dens_df, aes(x = mu, y = lambda, z = density, fill = ..level..), 
  #                    alpha = 0.8) + # 塗りつぶし等高線
  labs(
    title = "Gaussian-Gamma Distribution", 
    #subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b), # (文字列表記用)
    subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), # (数式表記用)
    color = "density", fill = "density", 
    x = expression(mu), y = expression(lambda)
  ) # ラベル


# μに関する統計量を格納
stat_mu_df <- tibble::tibble(
  lambda = lambda_vals, # 確率変数λ
  mean = m, # 期待値
  sd = 1 / sqrt(beta * lambda_vals) # 標準偏差
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !lambda, 
    names_to = "group", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(group, pattern = "sd_.*", replacement = "sd")
  ) # 期待値±標準偏差のカテゴリを統一

# λに関する統計量を格納
stat_lambda_df <- tibble::tibble(
  mean = a / b, # 期待値
  sd = sqrt(a / b^2), # 標準偏差
  mode = (a - 1) / b # 最頻値
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = dplyr::everything(), 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")
  ) # 期待値±標準偏差のカテゴリを統一

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねたガウス-ガンマ分布を作図
ggplot() + 
  geom_contour_filled(data = dens_df, aes(x = mu, y = lambda, z = density, fill = ..level..), 
                      alpha = 0.8) + # 分布
  geom_line(data = stat_mu_df, mapping = aes(x = statistic, y = lambda, color = type, linetype = type, group = group), 
            size = 1) + # μの統計量
  geom_hline(data = stat_lambda_df, mapping = aes(yintercept = statistic, color = type, linetype = type), 
             size = 1) + # λの統計量
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(xlim = c(min(mu_vals), max(mu_vals))) + # x軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), 
       color = "statistic", fill = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル


# 変数と形状の関係 ----------------------------------------------------------------

### ・パラメータの設定 -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# μの値を作成
mu_vals <- seq(
  from = m - 1/sqrt(beta*a/b) * 4, 
  to = m + 1/sqrt(beta*a/b) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = a/b * 3, length.out = 200)


### ・μ軸側から見たグラフ -----

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("lambda=", round(lambda, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b) |> 
      factor(levels = paste0("lambda=", round(lambda_vals, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b)) # フレーム切替用ラベル
  )

# μに関する統計量を格納
stat_mu_df <- tibble::tibble(
  lambda = lambda_vals, # 精度パラメータ
  mean = m, # 期待値
  sd = 1 / sqrt(beta * lambda_vals) # 標準偏差
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !lambda, 
    names_to = "group", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(group, pattern = "sd_.*", replacement = "sd"), # 期待値±標準偏差のカテゴリを統一
    parameter = paste0("lambda=", round(lambda, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b) |> 
      factor(levels = paste0("lambda=", round(lambda_vals, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b)) # フレーム切替用ラベル
  )


# μ軸側から見たガウス-ガンマ分布を作図
dens_mu_graph <- ggplot() + 
  geom_vline(data = stat_mu_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1, show.legend = FALSE) + # 統計量
  geom_line(data = dens_df, mapping = aes(x = mu, y = density, color = "NG"), 
            size = 1) + # ガウス-ガンマ分布
  geom_line(data = dens_df, mapping = aes(x = mu, y = N_dens, color = "N"), 
            size = 1) + # ガウス分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_manual(breaks = c("NG", "N", "mean", "sd"), 
                     values = c("#00A968", "purple", "blue", "orange"), 
                     labels = c("gaussian-gamma", "gaussian", expression(E(mu)), expression(E(mu) %+-% sqrt(V(mu)))), 
                     name = "distribution") + # 線の色
  scale_linetype_manual(breaks = c("mean", "sd"), values = c("dashed", "dotted")) + # 線の種類
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(xlim = c(min(mu_vals), max(mu_vals))) + # 軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(mu), y = "density")

# gif画像を作成
gganimate::animate(dens_mu_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600)


### ・λ軸側から見たグラフ -----

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("mu=", round(mu, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b) |> 
      factor(levels = paste0("mu=", round(mu_vals, 2), ", m=", m, ", beta=", beta, ", a=", a, ", b=", b)) # フレーム切替用ラベル
  )

# λに関する統計量を格納
stat_lambda_df <- tibble::tibble(
  mean = a / b, # 期待値
  sd = sqrt(a / b^2), # 標準偏差
  mode = (a - 1) / b # 最頻値
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = dplyr::everything(), 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")
  ) # 期待値±標準偏差のカテゴリを統一


# λ軸側から見たガウス-ガンマ分布を作図
dens_lambda_graph <- ggplot() + 
  geom_vline(data = stat_lambda_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1, show.legend = FALSE) + # 統計量
  geom_line(data = dens_df, mapping = aes(x = lambda, y = density, color = "NG"), 
            size = 1) + # ガウス-ガンマ分布
  geom_line(data = dens_df, mapping = aes(x = lambda, y = Gam_dens, color = "Gam"), 
            size = 1) + # ガンマ分布
  scale_color_manual(breaks = c("NG", "Gam", "mean", "sd", "mode"), 
                     values = c("#00A968", "purple", "blue", "orange", "chocolate"), 
                     labels = c("gaussian-gamma", "gaussian", expression(E(lambda)), expression(E(lambda) %+-% sqrt(V(lambda))), expression(mode(lambda))), 
                     name = "distribution") + # 線の色
  scale_linetype_manual(breaks = c("mean", "sd", "mode"), values = c("dashed", "dotted", "dashed")) + # 線の種類
  theme(legend.text.align = 0) + # 図の体裁:凡例
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(dens_lambda_graph, nframes = length(mu_vals), fps = 10, width = 800, height = 600)


# パラメータと分布の形状の関係 ----------------------------------------------------------

### ・パラメータの設定 -----

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

# 確率変数の値を作成
mu_vals     <- seq(from = -3, to = 3, length.out = 201)
lambda_vals <- seq(from = 0, to = 2, length.out = 201)


### ・mの影響 -----

# パラメータごとにガウス-ガンマ分布を計算
anime_dens_df <- tidyr:::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals, # 確率変数λ
  m = m_vals # パラメータm
) |> # 全ての組み合わせを作成
  dplyr::arrange(m, mu, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("m=", round(m, 1), ", beta=", beta, ", a=", a, ", b=", b) |> 
      factor(levels = paste0("m=", round(m_vals, 1), ", beta=", beta, ", a=", a, ", b=", b)) # フレーム切替用ラベル
  )


### ・βの影響 -----

# パラメータごとにガウス-ガンマ分布を計算
anime_dens_df <- tidyr:::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals, # 確率変数λ
  beta = beta_vals # パラメータβ
) |> # 全ての組み合わせを作成
  dplyr::arrange(beta, mu, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("m=", m, ", beta=", round(beta, 1), ", a=", a, ", b=", b) |> 
      factor(levels = paste0("m=", m, ", beta=", round(beta_vals, 1), ", a=", a, ", b=", b)) # フレーム切替用ラベル
  )


### ・aの影響 -----

# パラメータごとにガウス-ガンマ分布を計算
anime_dens_df <- tidyr:::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals, # 確率変数λ
  a = a_vals # パラメータa
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, mu, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("m=", m, ", beta=", beta, ", a=", round(a, 1), ", b=", b) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", round(a_vals, 1), ", b=", b)) # フレーム切替用ラベル
  )


### ・bの影響 -----

# パラメータごとにガウス-ガンマ分布を計算
anime_dens_df <- tidyr:::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals, # 確率変数λ
  b = b_vals # パラメータb
) |> # 全ての組み合わせを作成
  dplyr::arrange(b, mu, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens, # 確率密度
    parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", round(b, 1)) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", round(b_vals, 1))) # フレーム切替用ラベル
  )


### ・作図 -----

# ガウス-ガンマ分布のアニメーションを作図
anime_dens_graph <- ggplot() + 
  #geom_contour(data = anime_dens_df, aes(x = mu, y = lambda, z = density, color = ..level..)) + # 等高線
  geom_contour_filled(data = anime_dens_df, aes(x = mu, y = lambda, z = density, fill = ..level..), 
                      alpha = 0.8) + # 塗りつぶし等高線
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "density", 
       x = expression(mu), y = expression(lambda))

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(m_vals), fps = 10, width = 800, height = 600) # (mの影響用)
gganimate::animate(anime_dens_graph, nframes = length(beta_vals), fps = 10, width = 800, height = 600) # (βの影響用)
gganimate::animate(anime_dens_graph, nframes = length(a_vals), fps = 10, width = 800, height = 600) # (aの影響用)
gganimate::animate(anime_dens_graph, nframes = length(b_vals), fps = 10, width = 800, height = 600) # (bの影響用)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# データ数(サンプルサイズ)を指定
N <- 10000


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを格納
data_df <- tidyr::tibble(
  mu = mu_n, # μのサンプル
  lambda = lambda_n # λのサンプル
)


### ・乱数の可視化 -----

# μの値を作成
mu_vals <- seq(
  from = m - 1/sqrt(beta*a/b) * 4, 
  to = m + 1/sqrt(beta*a/b) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = a/b * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


# サンプルの散布図を作成
ggplot() + 
  geom_point(data = data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ", N==", N, ")")), 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# サンプルのヒストグラムを作成:度数
ggplot() + 
  geom_bin_2d(data = data_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ", N==", N, ")")), 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# サンプルのヒストグラムを作成:密度
ggplot() + 
  geom_density_2d_filled(data = data_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", N), 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

### ・パラメータの設定 -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# μの値を作成
mu_vals <- seq(
  from = m - 1/sqrt(beta*a/b) * 4, 
  to = m + 1/sqrt(beta*a/b) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = a/b * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


### ・1データずつ可視化 -----

# データ数(フレーム数)を指定
N <- 100


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを格納
anime_data_df <- tibble::tibble(
  n = 1:N, # データ番号
  mu = mu_n, # μのサンプル
  lambda = lambda_n, # λのサンプル
  parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N) |> 
    factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N)) # フレーム切替用ラベル
)

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame) |> # 各試行までのサンプルを抽出
  dplyr::mutate(
    mu = mu_n[n], # μのサンプル
    lambda = lambda_n[n], # λのサンプル
    parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", frame) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5, size = 3) + # n個のサンプル
  geom_point(data = anime_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin_2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
              breaks = list(x = mu_breaks, y = lambda_breaks), 
              alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # 
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# (データが少ないと密度を計算できないため)最初のフレームを除去
n_min <- 10
tmp_data_df <- anime_data_df|> 
  dplyr::filter(n > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )
tmp_freq_df <- anime_freq_df |> 
  dplyr::filter(frame > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )

# 密度の等高線のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density_2d_filled(data = tmp_freq_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                         alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = tmp_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # 
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N-n_min, fps = 10, width = 800, height = 600)


### ・複数データずつ可視化 -----

# データ数を指定
N <- 1000

# フレーム数を指定
frame_num <- 100

# 1フレーム当たりのデータ数を計算
n_per_frame <- N %/% frame_num


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:frame_num, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame*n_per_frame) |> # 各フレームで利用するサンプルを抽出
  dplyr::mutate(
    mu = mu_n[n], # μのサンプル
    lambda = lambda_n[n], # λのサンプル
    parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", frame*n_per_frame) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:frame_num*n_per_frame)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_point(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5, size = 3) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
             breaks = list(x = mu_breaks, y = lambda_breaks), 
             alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # 
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 密度の等高線のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density2d_filled(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                        alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # 
  scale_color_viridis_c(option = "D") + # 等高線の色
  coord_cartesian(xlim = c(min(mu_vals), max(mu_vals)), 
                  ylim = c(min(lambda_vals), max(lambda_vals))) + # 軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# 分布の数(サンプルサイズ)を指定
N <- 10


# ガンマ分布の精度パラメータを生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布の平均パラメータを生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# パラメータを格納
param_df <- tibble::tibble(
  mu = mu_n, 
  lambda = lambda_n, 
  parameter = paste0("mu=", round(mu_n, 2), ", lambda=", round(lambda_n, 3)) |> 
    factor() # 色分け用ラベル
)


# 平均パラメータの期待値を計算
E_mu <- m

# 精度パラメータの期待値を計算
E_lambda <- a / b


# μの値を作成
mu_vals <- seq(
  from = E_mu - 1/sqrt(beta*E_lambda) * 4, 
  to = E_mu + 1/sqrt(beta*E_lambda) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = E_lambda * 3, length.out = 200)

# ガウス-ガンマ分布を計算
gaussian_gamma_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数mu
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


# ガウス-ガンマ分布を作図
gaussian_gamma_graph <- ggplot() + 
  geom_contour_filled(data = gaussian_gamma_df, mapping = aes(x = mu, y = lambda, z = density, fill = ..level..), 
                      alpha = 0.6) + # パラメータの生成分布
  geom_point(mapping = aes(x = E_mu, y = E_lambda), 
             color = "red", size = 6, shape = 4) + # パラメータの期待値
  geom_point(data = param_df, mapping = aes(x = mu, y = lambda, color = parameter), 
             alpha = 0.8, size = 6, show.legend = FALSE) + # パラメータのサンプル
  labs(
    title = "Gaussian-Gamma Distribution", 
    subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ")")), 
    fill = "density", 
    x = expression(mu), y = expression(lambda)
  ) # ラベル
gaussian_gamma_graph


### ・分布の作図：1次元ガウス分布 -----

# xの値を作成
x_vals <- mu_vals

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = 1/sqrt(E_lambda)) # 確率密度
)

# パラメータのサンプルごとにガウス分布を計算
res_gaussian_df <- tidyr::expand_grid(
  x = x_vals, # 確率変数
  n = 1:N # パラメータ番号
) |> # 全ての組み合わせを作成
  dplyr::arrange(n, x) |> # パラメータのごとに並べ替え
  dplyr::mutate(
    mu = mu_n[n], # パラメータμ
    lambda = lambda_n[n], # パラメータλ
    density = dnorm(x = x, mean = mu, sd = 1/sqrt(lambda)), # 確率密度
    parameter = paste0("mu=", round(mu, 2), ", lambda=", round(lambda, 3)) |> 
      factor() # 色分け用ラベル
  )

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_gaussian_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") |> # 等号表示用の記法に変換
  (\(.){paste0("list(", ., ")")})() |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression化
names(label_vec) <- unique(res_gaussian_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換

# サンプルによる1次元ガウス分布を作図
gaussian_graph <- ggplot() + 
  geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布 
  geom_line(data = res_gaussian_df, mapping = aes(x = x, y = density, color = parameter), 
            alpha = 0.8, size = 1) + # サンプルによる分布
  scale_color_hue(labels = label_vec) + # 凡例テキスト:(数式表示用)
  guides(color = guide_legend(override.aes = list(alpha = 1))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(E(mu)==", round(E_mu, 2), ", E(lambda)==", round(E_lambda, 3), ")")), 
    x = expression(x), y = "density"
  ) # ラベル
gaussian_graph

# グラフを並べて描画
gaussian_gamma_graph / gaussian_graph + 
  patchwork::plot_layout(guides = "collect")


