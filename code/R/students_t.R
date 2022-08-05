
# 1次元スチューデントのt分布 ----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(LaplacesDemon)

# チェック用
library(ggplot2)


# 確率密度の計算 -----------------------------------------------------------------

### ・パラメータの設定 -----

# 形状パラメータ(自由度)を指定
nu <- 5

# 位置パラメータを指定
mu <- 2

# スケールパラメータを指定
sigma <- 0.5

# 逆スケールパラメータを指定
lambda <- 4

# 確率変数を指定
x <- 1.5


### ・標準化t分布 -----

# 定義式により確率密度を計算
C    <- gamma(0.5 * (nu + 1)) / gamma(0.5 * nu) / sqrt(pi * nu)
term <- sqrt(1 + x^2 / nu)^(nu + 1)
dens <- C / term
dens

# 対数をとった定義式により確率密度を計算
log_C    <- lgamma(0.5 * (nu + 1)) - lgamma(0.5 * nu) - 0.5 * log(pi * nu)
log_term <- 0.5 * (nu + 1) * log(1 + x^2 / nu)
log_dens <- log_C - log_term
dens     <- exp(log_dens)
dens; log_dens

# 標準化t分布の関数により確率密度を計算
dens <- dt(x = x, df = nu)
dens

# 標準化t分布の対数をとった関数により確率密度を計算
log_dens <- dt(x = x, df = nu, log = TRUE)
dens     <- exp(log_dens)
dens; log_dens


### ・一般化t分布：スケールパラメータを使用 -----

# 対数をとった定義式により確率密度を計算
C    <- gamma(0.5 * (nu + 1)) / gamma(0.5 * nu) / sqrt(pi * nu) / sigma
term <- sqrt(1 + ((x - mu) / sigma)^2 / nu)^(nu + 1)
dens <- C / term
dens

# 対数をとった定義式により確率密度を計算
log_C    <- lgamma(0.5 * (nu + 1)) - lgamma(0.5 * nu) - 0.5 * log(pi * nu) - log(sigma)
log_term <- 0.5 * (nu + 1) * log(1 + ((x - mu) / sigma)^2 / nu)
log_dens <- log_C - log_term
dens     <- exp(log_dens)
dens; log_dens

# 一般化t分布の関数により確率密度を計算
dens <- LaplacesDemon::dst(x = x, mu = mu, sigma = sigma, nu = nu)
dens

# 一般化t分布の対数をとった関数により確率密度を計算
log_dens <- LaplacesDemon::dst(x = x, mu = mu, sigma = sigma, nu = nu, log = TRUE)
dens     <- exp(log_dens)

# 標準化t分布の関数により確率密度を計算
y    <- (x - mu) / sigma # 標準化
dens <- dt(x = y, df = nu) / sigma
dens


### ・一般化t分布：逆スケールパラメータを使用 -----

# 定義式により確率密度を計算
C    <- gamma(0.5 * (nu + 1)) / gamma(0.5 * nu) * sqrt(lambda / pi / nu)
term <- sqrt(1 + lambda * (x - mu)^2 / nu)^(nu + 1)
dens <- C / term
dens

# 対数をとった定義式により確率密度を計算
log_C    <- lgamma(0.5 * (nu + 1)) - lgamma(0.5 * nu) - 0.5 * (log(pi * nu) - log(lambda))
log_term <- 0.5 * (nu + 1) * log(1 + lambda / nu * (x - mu)^2)
log_dens <- log_C - log_term
dens     <- exp(log_dens)
dens; log_dens

# 一般化t分布の関数により確率密度を計算
dens <- LaplacesDemon::dst(x = x, mu = mu, sigma = 1/sqrt(lambda), nu = nu)
dens

# 一般化t分布の対数をとった関数により確率密度を計算
log_dens <- LaplacesDemon::dst(x = x, mu = mu, sigma = 1/sqrt(lambda), nu = nu, log = TRUE)
dens     <- exp(log_dens)

# 標準化t分布の関数により確率密度を計算
y    <- (x - mu) * sqrt(lambda) # 標準化
dens <- dt(x = y, df = nu) * sqrt(lambda)
dens


# 統計量の計算 ------------------------------------------------------------------

# 形状パラメータ(自由度)を指定
nu = 5

# 位置パラメータを指定
mu = 2

# スケールパラメータを指定
sigma = 0.5

# 逆スケールパラメータを指定
lmd = 4


# 期待値を計算:(nu > 1)
E_x <- mu
E_x

# sigmaを使って分散を計算:(nu > 2)
V_x <- sigma^2 * nu / (nu - 2)
V_x

# lambdaを使って分散を計算:(nu > 2)
V_x <- nu / (nu - 2) / lambda
V_x

# 最頻値を計算
mode_x <- mu
mode_x


# グラフの作成 ------------------------------------------------------------------

# 形状パラメータ(自由度)を指定
nu <- 5

# 位置パラメータを指定
mu <- 2

# スケールパラメータを指定
sigma <- 0.5


# xの値を作成
x_vals <- seq(from = mu - sigma*5, to = mu + sigma*5, length.out = 251)

# スチューデントのt分布を計算
dens_df <- tibble::tibble(
  x = x_vals, # 確率変数
  density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu) # 確率密度
)

# スチューデントのt分布のグラフを作成
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  labs(
    title = "Student's t Distribution", 
    subtitle = paste0("nu=", nu, ", mu=", mu, ", sigma=", sigma), # (文字列表記用)
    #subtitle = parse(text = paste0("list(nu==", nu, ", mu==", mu, ", sigma==", sigma, ")")), # (数式表記用)
    x = "x", y = "density"
  ) # ラベル


# 補助線用の統計量を計算
E_x <- mu # (nu > 1)
s_x <- sqrt(sigma^2 * nu / (nu - 2.0)) # (nu > 2)

# 統計量を重ねたt分布のグラフを作成:線のみ
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 分布
  geom_vline(xintercept = E_x, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = E_x-s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = E_x+s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  labs(
    title = "Student's t Distribution", 
    subtitle = parse(text = paste0("list(nu==", nu, ", mu==", mu, ", sigma==", sigma, ")")),     x = "x", y = "density"
  ) # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(mu, mu-sigma, mu+sigma), # 統計量
  type = c("mean", "sd", "sd") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange")
linetype_vec <- c(mean = "dashed", sd = "dotted")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))))

# 統計量を重ねたt分布のグラフを作成:凡例付き
ggplot() + 
  geom_line(data = dens_df, mapping = aes(x = x, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 期待値
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Student's t Distribution", 
       subtitle = parse(text = paste0("list(nu==", nu, ", mu==", mu, ", sigma==", sigma, ")")),        x = "x", y = "density") # ラベル


# パラメータと分布の関係：並べて比較 -------------------------------------------------------

### ・自由度の影響 -----

# 自由度として利用する値を指定
nu_vals <- c(1, 2, 3, 4, 5, 10, 100)

# 固定するパラメータを指定
mu <- 2
sigma <- 0.5


# xの値を作成
x_vals <- seq(from = mu - sigma*5, to = mu + sigma*5, length.out = 251)

# パラメータごとにt分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  nu = nu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(nu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("nu=", sort(nu_vals), ", mu=", mu, ", sigma=", sigma)) # 色分け用ラベル
  ) # 確率密度を計算


### ・位置パラメータの影響 -----

# 位置パラメータとして利用する値を指定
mu_vals <- c(-3.5, -2, -0.5, 1, 2.5)

# 固定するパラメータを指定
nu <- 1
sigma <- 0.5


# xの値を作成
x_vals <- seq(from = min(mu_vals)-sigma, to = max(mu_vals)+sigma, length.out = 251)

# パラメータごとにt分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("nu=", nu, ", mu=", sort(mu_vals), ", sigma=", sigma)) # 色分け用ラベル
  ) # 確率密度を計算


### ・スケールパラメータの影響 -----

# スケールパラメータとして利用する値を指定
sigma_vals <- c(0.5, 1, 2, 4, 8)

# 固定するパラメータを指定
nu <- 1
mu <- 2


# xの値を作成
x_vals <- seq(from = mu - max(sigma_vals), to = mu + max(sigma_vals), length.out = 251)

# パラメータごとにt分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  sigma = sigma_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(sigma, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("nu=", nu, ", mu=", mu, ", sigma=", sort(sigma_vals))) # 色分け用ラベル
  ) # 確率密度を計算


### ・逆スケールパラメータの影響 -----

# 逆スケールパラメータとして利用する値を指定
lambda_vals <- c(0.25, 0.5, 1, 2, 4)

# 固定するパラメータを指定
nu <- 1
mu <- 2


# xの値を作成
sigma <- 1 / sqrt(min(lambda_vals))
x_vals <- seq(from = mu - sigma*4, to = mu + sigma*4, length.out = 251)

# パラメータごとにt分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = 1/sqrt(lambda), nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", lambda=", lambda) |> 
      factor(levels = paste0("nu=", nu, ", mu=", mu, ", lambda=", sort(lambda_vals))) # 色分け用ラベル
  ) # 確率密度を計算


### ・作図 -----

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_dens_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") |> # 等号表示用の記法に変換
  (\(.){paste0("list(", ., ")")})() |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_dens_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換


# パラメータごとにt分布を作図
ggplot(data = res_dens_df, mapping = aes(x = x, y = density, color = parameter)) + # データ
  geom_line(size = 1) + # 折れ線グラフ
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Student's t Distribution", 
       x = "x", y = "density") # ラベル


# パラメータと分布の関係：アニメーションによる可視化 -------------------------------------------------------

### ・自由度の影響 -----

# 自由度として利用する値を指定
nu_vals <- 1:50 # (線の変化用)
nu_vals <- seq(from = 1, to = 25, by = 1) # (線の軌跡用)
length(nu_vals) # フレーム数

# 固定するパラメータを指定
mu <- 2
sigma <- 0.5


# xの値を作成
x_vals <- seq(from = mu - sigma*5, to = mu + sigma*5, length.out = 251)

# パラメータごとにt分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  nu = nu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(nu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", round(nu, 2), ", mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("nu=", round(nu_vals, 2), ", mu=", mu, ", sigma=", sigma)) # 色分け用ラベル
  ) # 確率密度を計算


### ・位置パラメータの影響 -----

# 位置パラメータとして利用する値を指定
mu_vals <- seq(from = -3, to = 3, by = 0.1) # (線の変化用)
mu_vals <- seq(from = -3, to = 3, by = 0.5) # (線の軌跡用)
length(mu_vals) # フレーム数

# 固定するパラメータを指定
nu <- 5
sigma <- 0.5


# xの値を作成
x_vals <- seq(from = min(mu_vals)-sigma, to = max(mu_vals)+sigma, length.out = 251)

# パラメータごとにt分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", round(mu, 2), ", sigma=", sigma) |> 
      factor(levels = paste0("nu=", nu, ", mu=", round(mu_vals, 2), ", sigma=", sigma)) # 色分け用ラベル
  ) # 確率密度を計算


### ・スケールパラメータの影響 -----

# スケールパラメータとして利用する値を指定
sigma_vals <- seq(from = 0.5, to = 5, by = 0.1) # (線の変化用)
sigma_vals <- seq(from = 0.5, to = 10, by = 0.5) # (線の軌跡用)
length(sigma_vals) # フレーム数

# 固定するパラメータを指定
nu <- 5
mu <- 2


# xの値を作成
x_vals <- seq(from = mu - max(sigma_vals), to = mu + max(sigma_vals), length.out = 251)

# パラメータごとにt分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  sigma = sigma_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(sigma, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = sigma, nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", sigma=", round(sigma, 2)) |> 
      factor(levels = paste0("nu=", nu, ", mu=", mu, ", sigma=", round(sigma_vals, 2))) # 色分け用ラベル
  ) # 確率密度を計算


### ・逆スケールパラメータの影響 -----

# 逆スケールパラメータとして利用する値を指定
lambda_vals <- seq(from = 0.1, to = 10, by = 0.1) # (線の変化用)
lambda_vals <- seq(from = 0.5, to = 10, by = 0.5) # (線の軌跡用)
length(lambda_vals) # フレーム数

# 固定するパラメータを指定
nu <- 5
mu <- 2


# xの値を作成
sigma <- 1 / sqrt(min(lambda_vals))
x_vals <- seq(from = mu - sigma*2, to = mu + sigma*2, length.out = 251)

# パラメータごとにt分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = LaplacesDemon::dst(x = x_vals, mu = mu, sigma = 1/sqrt(lambda), nu = nu), 
    parameter = paste0("nu=", nu, ", mu=", mu, ", lambda=", round(lambda, 2)) |> 
      factor(levels = paste0("nu=", nu, ", mu=", mu, ", lambda=", round(lambda_vals, 2))) # 色分け用ラベル
  ) # 確率密度を計算


### ・作図：線の変化 -----

# t分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Student's t Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(nu_vals), fps = 10, width = 800, height = 600) # (自由度の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 10, width = 800, height = 600) # (位置パラメータの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 10, width = 800, height = 600) # (スケールパラメータの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600) # (逆スケールパラメータの影響の場合)


### ・作図：線の軌跡 -----

## 自由度の影響用

# t分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(nu), group = nu)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(nu) + # フレーム
  labs(title = "Student's t Distribution", 
       subtitle = paste0("nu={frame_along}, mu=", mu, ", sigma=", sigma), 
       color = expression(nu), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(nu_vals)+10, end_pause = 10, fps = 10, width = 800, height = 600)


## 位置パラメータの影響用

# t分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(mu), group = mu)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(mu) + # フレーム
  labs(title = "Student's t Distribution", 
       subtitle = paste0("nu=", nu, ", mu={frame_along}, sigma=", sigma), 
       color = expression(mu), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals)+10, end_pause = 10, fps = 10, width = 800, height = 600)


## スケールパラメータの影響用

# t分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(sigma), group = sigma)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(sigma) + # フレーム
  labs(title = "Student's t Distribution", 
       subtitle = paste0("nu=", nu, ", mu=", mu, ", sigma={frame_along}"), 
       color = expression(sigma), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals)+10, end_pause=10, fps = 10, width = 800, height = 600)


## 逆スケールパラメータの影響用

# t分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(lambda), group = lambda)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(lambda) + # フレーム
  labs(title = "Student's t Distribution", 
       subtitle = paste0("nu=", nu, ", mu=", mu, ", lambda={frame_along}"), 
       color = expression(lambda), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals)+10, end_pause=10, fps = 10, width = 800, height = 600)


# パラメータと統計量の関係：アニメーションによる可視化 -------------------------------------------------------

## 「「分布の関係」のanime_dens_dfを使用

### ・自由度の影響 -----

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  nu = nu_vals, 
  mean = dplyr::if_else(
    nu > 1, true = mu, false = as.numeric(NA)
  ), # 期待値
  sd = dplyr::if_else(
    nu > 2, true = sqrt(sigma^2 * nu / (nu - 2)), false = as.numeric(NA)
  ), # 標準偏差
  parameter = paste0("nu=", round(nu_vals, 2), ", mu=", mu, ", sigma=", sigma) |> 
    factor(levels = paste0("nu=", round(nu_vals, 2), ", mu=", mu, ", sigma=", sigma)) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!c(nu, sd)) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・位置パラメータの影響 -----

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  nu = nu, 
  mu = mu_vals, 
  mean = dplyr::if_else(
    nu > 1, true = mu, false = as.numeric(NA)
  ), # 期待値
  sd = dplyr::if_else(
    nu > 2, true = sqrt(sigma^2 * nu / (nu - 2)), false = as.numeric(NA)
  ), # 標準偏差
  parameter = paste0("nu=", nu, ", mu=", round(mu_vals, 2), ", sigma=", sigma) |> 
    factor(levels = paste0("nu=", nu, ", mu=", round(mu_vals, 2), ", sigma=", sigma)) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!c(nu, mu, sd)) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・スケールパラメータの影響 -----

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  nu = nu, 
  sigma = sigma_vals, 
  mean = dplyr::if_else(
    nu > 1, true = mu, false = as.numeric(NA)
  ), # 期待値
  sd = dplyr::if_else(
    nu > 2, true = sqrt(sigma^2 * nu / (nu - 2)), false = as.numeric(NA)
  ), # 標準偏差
  parameter = paste0("nu=", nu, ", mu=", mu, ", sigma=", round(sigma_vals, 2)) |> 
    factor(levels = paste0("nu=", nu, ", mu=", mu, ", sigma=", round(sigma_vals, 2))) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!c(nu, sigma, sd)) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・逆スケールパラメータの影響 -----

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  nu = nu, 
  lambda = lambda_vals, 
  mean = dplyr::if_else(
    nu > 1, true = mu, false = as.numeric(NA)
  ), # 期待値
  sd = dplyr::if_else(
    nu > 2, true = nu / (nu - 2) / lambda, false = as.numeric(NA)
  ), # 標準偏差
  parameter = paste0("nu=", nu, ", mu=", mu, ", lambda=", round(lambda_vals, 2)) |> 
    factor(levels = paste0("nu=", nu, ", mu=", mu, ", lambda=", round(lambda_vals, 2))) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!c(nu, lambda, sd)) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・作図 -----

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange")
linetype_vec <- c(mean = "dashed", sd = "dotted")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))))


# 統計量を重ねたt分布のアニメーションを作図
anime_dens_graph <- ggplot() + 
  geom_line(data = anime_dens_df, mapping = aes(x = x, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(xlim = c(min(x_vals), max(x_vals))) + # 軸の表示範囲
  labs(title = "Student's t Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(nu_vals), fps = 10, width = 800, height = 600) # (自由度の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 10, width = 800, height = 600) # (位置パラメータの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 10, width = 800, height = 600) # (スケールパラメータの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600) # (逆スケールパラメータの影響の場合)


