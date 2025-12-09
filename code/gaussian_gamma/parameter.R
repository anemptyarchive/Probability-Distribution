
# ガウス-ガンマ分布 ---------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# チェック用
library(ggplot2)
library(patchwork)

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


