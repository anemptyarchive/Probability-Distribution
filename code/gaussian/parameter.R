
# 1次元ガウス分布 --------------------------------------------------------------

# パラメータの影響


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


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
      factor(levels = paste0("mu=", sort(mu_vals), ", sigma=", sigma)) # 色分け用ラベル
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
      factor(levels = paste0("mu=", mu, ", sigma=", sort(sigma_vals))) # 色分け用ラベル
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
      factor(levels = paste0("mu=", mu, ", lambda=", sort(lambda_vals))) # 色分け用ラベル
  ) # 確率密度を計算


### ・作図 -----

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_dens_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") |> # 等号表示用の記法に変換
  (\(.){paste0("list(", ., ")")})() |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_dens_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換


# パラメータごとにガウス分布を作図
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
       subtitle = "{current_frame}", 
       x = "x", y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 10, width = 800, height = 600) # (平均の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 10, width = 800, height = 600) # (標準偏差の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600) # (精度の影響の場合)


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
gganimate::animate(anime_dens_graph, nframes = length(mu_vals), fps = 10, width = 800, height = 600) # (平均の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(sigma_vals), fps = 10, width = 800, height = 600) # (標準偏差の影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600) # (精度の影響の場合)


