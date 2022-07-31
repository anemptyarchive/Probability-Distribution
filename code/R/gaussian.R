
# 1次元ガウス分布 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# チェック用
library(ggplot2)


# 確率密度の計算 -----------------------------------------------------------------

### ・パラメータの設定 -----

# 平均パラメータを指定
mu <- 2

# 標準偏差パラメータを指定
sigma <- 0.5

# 精度パラメータを指定
lambda <- 4

# 確率変数の値を指定
x <- 1.5


### ・標準偏差を使用 -----

# 定義式により確率密度を計算
C    <- 1 / sqrt(2 * pi) / sigma
dens <- C * exp(-0.5 * (x - mu)^2 / sigma^2)
dens

# 対数をとった定義式により確率密度を計算
log_C    <- -0.5 * log(2 * pi) - log(sigma)
log_dens <- log_C - 0.5 * (x - mu)^2 / sigma^2
dens     <- exp(log_dens)
dens; log_dens

# ガウス分布の関数により確率密度を計算
dens <- dnorm(x = x, mean = mu, sd = sigma)
dens

# ガウス分布の対数をとった関数により確率密度を計算
log_dens <- dnorm(x = x, mean = mu, sd = sigma, log = TRUE)
dens     <- exp(log_dens)
dens; log_dens

# 標準正規分布の関数により確率密度を計算
y    <- (x - mu) / sigma # 標準化
dens <- dnorm(x = y, mean = 0, sd = 1) / sigma
dens


### ・精度を使用 -----

# 定義式により確率密度を計算
C    <- sqrt(lambda / 2 / pi)
dens <- C * exp(-0.5 * lambda * (x - mu)^2)
dens

# 対数をとった定義式により確率密度を計算
log_C    <- -0.5 * (log(2 * pi) - log(lambda))
log_dens <- log_C - 0.5 * lambda * (x - mu)^2
dens     <- exp(log_dens)
dens; log_dens

# ガウス分布の関数により確率密度を計算
dens <- dnorm(x = x, mean = mu, sd = 1/sqrt(lambda))
dens

# ガウス分布の対数をとった関数により確率密度を計算
log_dens <- dnorm(x = x, mean = mu, sd = 1/sqrt(lambda), log = TRUE)
dens     <- exp(log_dens)
dens; log_dens

# 標準正規分布の関数により確率密度を計算
y    <- (x - mu) * sqrt(lambda) # 標準化
dens <- dnorm(x = y, mean = 0, sd = 1) * sqrt(lambda)
dens


# 統計量の計算 ------------------------------------------------------------------

# 平均を指定
mu <- 2

# 標準偏差を指定
sigma <- 0.5


# 標準偏差から分散を計算
sigma2 <- sigma^2
sigma2

# 分散から精度を計算
lambda <- 1 / sigma2
lambda

# 精度から標準偏差を計算
sigma <- 1 / sqrt(lambda)
sigma


# グラフの作成 ------------------------------------------------------------------

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- seq(from = mu - sigma*4, to = mu + sigma*4, length.out = 251)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)


# ガウス分布のグラフを作成
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  labs(
    title = "Gaussian Distribution", 
    subtitle = paste0("mu=", mu, ", sigma=", sigma), # (文字列表記用)
    #subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), # (数式表記用)
    x = "x", y = "density"
  ) # ラベル


# 統計量を重ねたガウス分布のグラフを作成:線のみ
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 分布
  geom_vline(xintercept = mu, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = mu-sigma, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = mu+sigma, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), 
    x = "x", y = "density"
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

# 統計量を重ねたポアソン分布のグラフを作成:凡例付き
ggplot() + 
  geom_line(data = dens_df, mapping = aes(x = x, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 期待値
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gaussian Distribution", 
       subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), 
       x = "x", y = "density") # ラベル


# パラメータと分布の関係：並べて比較 ----------------------------------------------------------

### ・平均の影響 -----

# 平均パラメータとして利用する値を指定
mu_vals <- c(-3.5, -2, -0.5, 1, 2.5, 4)

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- seq(from = min(mu_vals) - sigma*2, to = max(mu_vals) + sigma*2, length.out = 251)

# パラメータごとにガウス分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("mu=", sort(mu_vals), ", sigma=", sigma)) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・標準偏差の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
sigma_vals <- c(0.5, 1, 2, 4)


# xの値を作成
x_vals <- seq(from = mu - max(sigma_vals)*1.5, to = mu + max(sigma_vals)*1.5, length.out = 250)

# パラメータごとにガウス分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  sigma = sigma_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(sigma, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", mu, ", sigma=", sigma) |> 
      factor(levels = paste0("mu=", mu, ", sigma=", sort(sigma_vals))) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・精度の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
lambda_vals <- c(0.25, 0.5, 1, 2, 4)


# xの値を作成
sigma <- 1 / sqrt(min(lambda_vals))
x_vals <- seq(from = mu - sigma*3, to = mu + sigma*3, length.out = 250)

# パラメータごとにガウス分布を計算
res_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = 1/sqrt(lambda)), 
    parameter = paste0("mu=", mu, ", lambda=", lambda) |> 
      factor(levels = paste0("mu=", mu, ", lambda=", sort(lambda_vals))) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・作図 -----

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_dens_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") |> # 等号表示用の記法に変換
  (\(.){paste0("list(", ., ")")})() |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_dens_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換


# ガウス分布のアニメーションを作図
ggplot(data = res_dens_df, mapping = aes(x = x, y = density, color = parameter)) + # データ
  geom_line(size = 1) + # 折れ線グラフ
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gaussian Distribution", 
       x = "x", y = "density") # ラベル


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------------------------------------

### ・平均の影響 -----

# 平均パラメータとして利用する値を指定
mu_vals <- seq(from = -5, to = 5, by = 0.1) # (線の変化用)
mu_vals <- seq(from = -5, to = 5, by = 0.5) # (線の軌跡用)
length(mu_vals) # フレーム数

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- seq(from = min(mu_vals)-sigma, to = max(mu_vals)+sigma, length.out = 251)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", round(mu, 2), ", sigma=", sigma) |> 
      factor(levels = paste0("mu=", round(mu_vals, 2), ", sigma=", sigma)) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・標準偏差の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
sigma_vals <- seq(from = 0.5, to = 5, by = 0.1) # (線の変化用)
sigma_vals <- seq(from = 0.5, to = 5, by = 0.5) # (線の軌跡用)
length(sigma_vals) # フレーム数


# xの値を作成
x_vals <- seq(from = mu - max(sigma_vals)*2, to = mu + max(sigma_vals)*2, length.out = 251)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  sigma = sigma_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(sigma, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", mu, ", sigma=", round(sigma, 2)) |> 
      factor(levels = paste0("mu=", mu, ", sigma=", round(sigma_vals, 2))) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・精度の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
lambda_vals <- seq(from = 0.1, to = 10, by = 0.1) # (線の変化用)
lambda_vals <- seq(from = 0.5, to = 10, by = 0.5) # (線の軌跡用)
length(lambda_vals) # フレーム数


# xの値を作成
sigma <- 1 / sqrt(min(lambda_vals))
x_vals <- seq(from = mu - sigma*2, to = mu + sigma*2, length.out = 250)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = 1/sqrt(lambda)), 
    parameter = paste0("mu=", mu, ", lambda=", round(lambda, 2)) |> 
      factor(levels = paste0("mu=", mu, ", lambda=", round(lambda_vals, 2))) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・作図：線の変化 -----

# ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 100, width = 800, height = 600) # (平均の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 100, width = 800, height = 600) # (標準偏差の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 100, width = 800, height = 600) # (精度の影響の場合)


### ・作図：線の軌跡 -----

## 平均の影響用

# ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(mu), group = mu)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(mu) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu={frame_along}, sigma=", sigma), 
       color = expression(mu), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals)+10, end_pause = 10, fps = 10, width = 800, height = 600)


## 標準偏差の影響用

# ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(sigma), group = sigma)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(sigma) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", sigma={frame_along}"), 
       color = expression(sigma), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals)+10, end_pause=10, fps = 10, width = 800, height = 600)


## 精度の影響用

# ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = x, y = density, color = factor(lambda), group = lambda)) + # データ
  geom_line() + # 折れ線グラフ
  gganimate::transition_reveal(lambda) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = paste0("mu=", mu, ", lambda={frame_along}"), 
       color = expression(lambda), 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals)+10, end_pause=10, fps = 10, width = 800, height = 600)


# パラメータと統計量の関係：アニメーションによる可視化 ----------------------------------------------------------

### ・平均の影響 -----

# 平均パラメータとして利用する値を指定
mu_vals <- seq(from = -5, to = 5, by = 0.1)
length(mu_vals) # フレーム数

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- seq(from = min(mu_vals)-sigma, to = max(mu_vals)+sigma, length.out = 251)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", round(mu, 2), ", sigma=", sigma) |> 
      factor(levels = paste0("mu=", round(mu_vals, 2), ", sigma=", sigma)) # フレーム切替用ラベル
  ) # 確率密度を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = mu_vals, # 期待値
  sd = sigma, # 標準偏差
  parameter = paste0("mu=", round(mu_vals, 2), ", sigma=", sigma) |> 
    factor(levels = paste0("mu=", round(mu_vals, 2), ", sigma=", sigma)) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・標準偏差の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
sigma_vals <- seq(from = 0.5, to = 5, by = 0.1)
length(sigma_vals) # フレーム数


# xの値を作成
x_vals <- seq(from = mu - max(sigma_vals)*2, to = mu + max(sigma_vals)*2, length.out = 251)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  sigma = sigma_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(sigma, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), 
    parameter = paste0("mu=", mu, ", sigma=", round(sigma, 2)) |> 
      factor(levels = paste0("mu=", mu, ", sigma=", round(sigma_vals, 2))) # フレーム切替用ラベル
  ) # 確率密度を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = mu, # 期待値
  sd = sigma_vals, # 標準偏差
  parameter = paste0("mu=", mu, ", sigma=", round(sigma_vals, 2)) |> 
    factor(levels = paste0("mu=", mu, ", sigma=", round(sigma_vals, 2))) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.*", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一


### ・精度の影響 -----

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータとして利用する値を指定
lambda_vals <- seq(from = 0.1, to = 10, by = 0.1)
length(lambda_vals) # フレーム数


# xの値を作成
sigma <- 1 / sqrt(min(lambda_vals))
x_vals <- seq(from = mu - sigma*2, to = mu + sigma*2, length.out = 250)

# パラメータごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = 1/sqrt(lambda)), 
    parameter = paste0("mu=", mu, ", lambda=", round(lambda, 2)) |> 
      factor(levels = paste0("mu=", mu, ", lambda=", round(lambda_vals, 2))) # フレーム切替用ラベル
  ) # 確率密度を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = mu, # 期待値
  sd = 1 / sqrt(lambda_vals), # 標準偏差
  parameter = paste0("mu=", mu, ", lambda=", round(lambda_vals, 2)) |> 
    factor(levels = paste0("mu=", mu, ", lambda=", round(lambda_vals, 2))) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_minus = mean - sd, 
    sd_plus = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
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


# 統計量を重ねた分布のアニメーションを作図
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
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 100, width = 800, height = 600) # (平均の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 100, width = 800, height = 600) # (標準偏差の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 100, width = 800, height = 600) # (精度の影響の場合)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# 平均パラメータを指定
mu <- 2

# 標準偏差パラメータを指定
sigma <- 2.5

# データ数(サンプルサイズ)を指定
N <- 1000


# ガウス分布に従う乱数を生成
x_n <- rnorm(n = N, mean = mu, sd = sigma)


### ・乱数の可視化 -----

# サンプルを格納
data_df <- tidyr::tibble(x = x_n)

# サンプルのヒストグラムを作成:度数
ggplot(data = data_df, mapping = aes(x = x, y = ..count..)) + # データ
  geom_histogram(bins = 30, fill = "#00A968") + # ヒストグラム
  labs(
    title = "Gaussian Distribution", 
    subtitle = paste0("mu=", mu, ", sigma=", sigma, ", N=", N), # (文字列表記用)
    #subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ", N==", N, ")")), # (数式表記用)
    x = "x", y = "frequency"
  ) # ラベル


# xの値を作成
x_vals <- seq(from = mu-sigma * 4, to = mu+sigma * 4, length.out = 251)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)

# サンプルのヒストグラムを作成:密度
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = x, y = ..density..), 
                 bins = 30, fill = "#00A968") + # ヒストグラム
  geom_line(data = dens_df, mapping = aes(x = x, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  labs(title = "Gaussian Distribution", 
       subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ", N==", N, ")")), # 数式で表示
       x = "x", y = "density") # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# 平均パラメータを指定
mu <- 2

# 標準偏差パラメータを指定
sigma <- 2.5

# データ数(フレーム数)を指定
N <- 100


# ガウス分布に従う乱数を生成
x_n <- rnorm(n = N, mean = mu, sd = sigma)

# サンプルを複製して格納
anime_freq_df <- tidyr::tibble(
  x = rep(x_n, times = N), # サンプル
  n = rep(1:N, times = N), # データ番号
  frame = rep(1:N, each = N) # フレーム番号
) |> 
  dplyr::filter(n <= frame) |> # サンプリング回数以前のサンプルを抽出
  dplyr::mutate(
    parameter = paste0("mu=", mu, ", sigma=", sigma, ", N=", frame) |> 
      factor(levels = paste0("mu=", mu, ", sigma=", sigma, ", N=", 1:N))
  ) # フレーム切替用ラベルを追加

# サンプルを格納
anime_data_df <- tidyr::tibble(
  x = x_n, # サンプル
  n = 1:N, # データ番号
  parameter = paste0("mu=", mu, ", sigma=", sigma, ", N=", 1:N) |> 
    factor(levels = paste0("mu=", mu, ", sigma=", sigma, ", N=", 1:N)) # フレーム切替用ラベル
)

# xの値を作成
x_vals <- seq(from = mu-sigma * 4, to = mu+sigma * 4, length.out = 251)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)


# サンプルのヒストグラムのアニメーションのアニメーションを作図:度数
anime_freq_graph <- ggplot() + # 
  geom_histogram(data = anime_freq_df, mapping = aes(x = x), 
                 breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 30), 
                 fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 6) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# サンプルのヒストグラムのアニメーションのアニメーションを作図:密度
anime_freq_graph <- ggplot() + # 
  geom_histogram(data = anime_freq_df, mapping = aes(x = x, y = ..density..), 
                 breaks = seq(from = min(x_vals), to = max(x_vals), length.out = 30), 
                 fill = "#00A968") + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 6) + # サンプル
  geom_line(data = dens_df, mapping = aes(x = x, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  coord_cartesian(ylim = c(0, max(dens_df[["density"]])*2)) + # 軸の表示範囲
  labs(title = "Gaussian Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# パラメータを指定
mu_gen    <- 5
sigma_gen <- 2.5

# 分布の数(サンプルサイズ)を指定
N <- 10


# ガウス分布の平均パラメータを生成
mu_n <- rnorm(n = N, mean = mu_gen, sd = sigma_gen) |> # ガウス分布の乱数を生成
  sort() # 昇順に並べ替え

# パラメータを格納
param_df <- tibble::tibble(
  mu = mu_n, 
  parameter = round(mu_n, 3) |> 
    factor() # 色分け用ラベル
)


# muの値を作成
mu_vals <- seq(from = mu_gen - sigma_gen*3, to = mu_gen + sigma_gen*3, length.out = 251)

# 生成分布を計算
gaussian_gen_df <- tibble::tibble(
  mu = mu_vals, # 確率変数
  density = dnorm(x = mu_vals, mean = mu_gen, sd = sigma_gen) # 確率密度
)

# 平均パラメータの期待値を計算
E_mu <- mu_gen


# 生成分布とパラメータのサンプルを作図
gaussian_gen_graph <- ggplot() + # データ
  geom_vline(xintercept = E_mu, 
             color = "red", size = 1, linetype = "dashed") + # パラメータの期待値
  geom_line(data = gaussian_gen_df, mapping = aes(x = mu, y = density), 
            color = "#00A968", size = 1) + # パラメータの生成分布
  geom_point(data = param_df, mapping = aes(x = mu, y = 0, color = parameter), 
             alpha = 0.8, size = 6, show.legend = FALSE) + # パラメータのサンプル
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) + # 凡例の体裁
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(
      text = paste0("list(mu[paste(g,e,n)]==", mu_gen, ", sigma[paste(g,e,n)]==", sigma_gen, ")"), 
    ), 
    color = expression(mu), 
    x = expression(mu), y = "density"
  ) # ラベル
gaussian_gen_graph


### ・分布の作図：1次元ガウス分布 -----

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- mu_vals

# パラメータのサンプルごとにガウス分布を計算
res_gaussian_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_n
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), # 確率密度
    parameter = round(mu, 3) |> 
      factor() # 色分け用ラベル
  )

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = sigma) # 確率密度
)


# サンプルと期待値によるガウス分布を作図
gaussian_graph <- ggplot() + 
  geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_line(data = res_gaussian_df, mapping = aes(x = x, y = density, color = parameter), 
            alpha = 0.8, size = 1) + # サンプルによる分布
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(E(mu)==", E_mu, ", sigma==", sigma, ")"), ), 
    color = expression(mu), 
    x = "x", y = "density"
  ) # ラベル
gaussian_graph

# グラフを並べて描画
gaussian_gen_graph / gaussian_graph + 
  patchwork::plot_layout(guides = "collect")


