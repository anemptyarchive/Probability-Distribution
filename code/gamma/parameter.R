
# ガンマ分布 -------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# チェック用
library(ggplot2)


# 確率密度の計算 -------------------------------------------------------------------

# パラメータを指定
a <- 5
b <- 2

# 確率変数の値を指定
lambda <- 2


# 定義式により確率密度を計算
C    <- b^a / gamma(a)
dens <- C * lambda^(a - 1) * exp(-b * lambda)
dens

# 対数をとった定義式により確率密度を計算
log_C    <- a * log(b) - lgamma(a)
log_dens <- log_C + (a - 1) * log(lambda) - b * lambda
dens     <- exp(log_dens)
dens; log_dens

# ガンマ分布の関数により確率密度を計算
dens <- dgamma(x = lambda, shape = a, rate = b)
dens

# ガンマ分布の関数により確率密度を計算
dens <- dgamma(x = lambda, shape = a, scale = 1/b)
dens

# ガンマ分布の対数をとった関数により確率密度を計算
log_dens <- dgamma(x = lambda, shape = a, rate = b, log = TRUE)
dens     <- exp(log_dens)
dens; log_dens

# ガンマ分布の対数をとった関数により確率密度を計算
log_dens <- dgamma(x = lambda, shape = a, scale = 1 / b, log = TRUE)
dens     <- exp(log_dens)
dens; log_dens


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
a <- 5
b <- 2


# 期待値を計算
E_lambda <- a / b
E_lambda

# 分散を計算
V_lambda <- a / b^2
V_lambda

# 最頻値を計算
mode_lambda <- (a - 1) / b
mode_lambda


# 歪度を計算
skewness <- 2 / sqrt(a)
skewness

# 尖度を計算
kurtosis <- 6 / a
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
a <- 5
b <- 2


# lambdaの値を作成
lambda_vals <- seq(from = 0, to = a/b * 4, length.out = 251)

# ガンマ分布を計算
dens_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)

# ガンマ分布のグラフを作成
ggplot(data = dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


# 補助線用の統計量を計算
E_lambda    <- a / b
s_lambda    <- sqrt(a / b^2)
mode_lambda <- (a - 1) / b

# 統計量を重ねたガンマ分布のグラフを作成:線のみ
ggplot(data = dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 分布
  geom_vline(xintercept = E_lambda, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = E_lambda - s_lambda, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = E_lambda + s_lambda, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  geom_vline(xintercept = mode_lambda, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda)) # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(E_lambda, E_lambda-s_lambda, E_lambda+s_lambda, mode_lambda), # 統計量
  type = c("mean", "sd", "sd", "mode") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねたガンマ分布のグラフを作成:凡例付き
ggplot() + 
  geom_line(data = dens_df, mapping = aes(x = lambda, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       x = expression(lambda), y = "density") # ラベル


# パラメータと分布の関係：並べて比較 ----------------------------------------------------------

# lambdaの値を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 501)


### ・aの影響 -----

# パラメータとして利用する値を指定
a_vals <- c(0.1, 0.5, 1, 2.5, 5, 10.5)

# 固定するパラメータを指定
b <- 5


# パラメータごとにガンマ分布を計算
res_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  a = a_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = paste0("a=", a, ", b=", b) |> 
      factor(levels = paste0("a=", sort(a_vals), ", b=", b)) # 色分け用ラベル
  ) # 確率密度を計算


### ・bの影響 -----

# 固定するパラメータを指定
a <- 5

# パラメータとして利用する値を指定
b_vals <- c(0.5, 1, 2.5, 5, 10.5, 25)


# パラメータごとにガンマ分布を計算
res_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  b = b_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(b, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = paste0("a=", a, ", b=", b) |> 
      factor(levels = paste0("a=", a, ", b=", sort(b_vals))) # 色分け用ラベル
  ) # 確率密度を計算


### ・aとbの影響 -----

# パラメータとして利用する値を指定
a_vals <- c(0.5, 6, 1, 2.5, 5.5)
b_vals <- c(0.1, 0.9, 1, 10, 5.5)


# パラメータごとにガンマ分布を計算
res_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  i = 1:length(a_vals) # パラメータ番号
) |> # 全ての組み合わせを作成
  dplyr::arrange(i, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    a = a_vals[i], 
    b = b_vals[i]
  ) |> # パラメータ列を追加
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = paste0("a=", a, ", b=", b) |> 
      factor() # 色分け用ラベル
  ) # 確率密度を計算


### ・作図 -----

# パラメータごとにガンマ分布のグラフを作成
ggplot(data = res_dens_df, mapping = aes(x = lambda, y = density, color = parameter)) + # データ
  geom_line(size = 1) + # 折れ線グラフ
  coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gamma Distribution", 
       x = expression(lambda), y = "density") # ラベル


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------------------------------------

# lambdaの値を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 250)


### ・aの影響 -----

# パラメータとして利用する値を指定
a_vals <- seq(from = 0.1, to = 10, by = 0.1)
length(a_vals) # フレーム数

# 固定するパラメータを指定
b <- 2


# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  a = a_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = paste0("a=", a, ", b=", b) |> 
      factor(levels = paste0("a=", a_vals, ", b=", b)) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・bの影響 -----

# 固定するパラメータを指定
a <- 2

# パラメータとして利用する値を指定
b_vals <- seq(from = 0.1, to = 10, by = 0.1)
length(a_vals) # フレーム数


# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  b = b_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(b, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = paste0("a=", a, ", b=", b) |> 
      factor(levels = paste0("a=", a, ", b=", b_vals)) # フレーム切替用ラベル
  ) # 確率密度を計算


### ・作図 -----

# ガンマ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = lambda, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  gganimate::transition_manual(parameter) + # フレーム
  #coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(a_vals), fps = 10, width = 800, height = 600) # (aの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(b_vals), fps = 10, width = 800, height = 600) # (bの影響の場合)


# パラメータの比較：アニメーションによる可視化 --------------------------------------------------

# lambdaの値を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 251)


### ・aの影響 -----

# 比較する値を指定
a_vals <- c(0.5, 1, 2, 4, 8, 16)

# 変化する値を指定
b_vals <- seq(from = 0.5, to = 10, by = 0.5)
length(b_vals) # フレーム数


# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  a = a_vals, 
  b = b_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, b, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dgamma(x = lambda_vals, shape = a, rate = b)
  ) # 確率密度を計算


# ガンマ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = lambda, y = dens, color = as.factor(b))) + # データ
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(b) + # フレーム
  facet_wrap(. ~ a, labeller = label_bquote(a==.(a))) + # グラフの分割
  coord_cartesian(ylim = c(0, 6)) + # 軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "b={frame_along}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(b_vals)+10, end_pause = 10, fps = 10, width = 1200, height = 600)


### ・bの影響 -----

# 変化する値を指定
a_vals <- seq(from = 0.5, to = 10, by = 0.5)
length(a_vals) # フレーム数

# 比較する値を指定
b_vals <- c(0.5, 1, 2, 4, 8, 12)


# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  a = a_vals, 
  b = b_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(b, a, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    dens = dgamma(x = lambda_vals, shape = a, rate = b)
  ) # 確率密度を計算


# ガンマ分布のアニメーションを作図
anime_dens_graph <- ggplot(data = anime_dens_df, mapping = aes(x = lambda, y = dens, color = as.factor(a))) + # データ
  geom_line(show.legend = FALSE) + # 折れ線グラフ
  gganimate::transition_reveal(a) + # フレーム
  facet_wrap(. ~ b, labeller = label_bquote(b==.(b))) + # グラフの分割
  coord_cartesian(ylim = c(0, 4)) + # 軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "a={frame_along}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(a_vals)+10, end_pause = 10, fps = 10, width = 1200, height = 600)


# 歪度と尖度の可視化 -------------------------------------------------------------------

# lambdaの値を作成
lambda_vals <- seq(from = 0, to = 5, length.out = 501)


### ・aの影響 -----

# パラメータとして利用する値を作成
a_vals <- seq(from = 1, to = 10, by = 0.1)
length(a_vals) # フレーム数

# 固定するパラメータを指定
b <- 2.5


# 歪度を計算
skewness_vec <- 2 / sqrt(a_vals)

# 尖度を計算
kurtosis_vec <- 6 / a_vals

# ラベル用のテキストを作成
param_vec <- paste0(
  "a=", a_vals, ", b=", b, 
  ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  a = a_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = rep(param_vec, each = length(lambda_vals)) |> 
      factor(levels = param_vec) # フレーム切替用ラベル
  ) # 確率密度を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = a_vals / b, # 期待値
  sd = sqrt(a_vals / b^2), # 標準偏差
  mode = (a_vals - 1) / b, # 最頻値
  parameter = factor(param_vec, levels = param_vec) # フレーム切替用のラベル
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


### ・bの影響 -----

# 固定するパラメータを指定
a <- 2.5

# パラメータとして利用する値を作成
b_vals <- seq(from = 1, to = 10, by = 0.1)
length(b_vals) # フレーム数


# 歪度を計算
skewness <- 2 / sqrt(a)

# 尖度を計算
kurtosis <- 6 / a

# ラベル用のテキストを作成
param_vec <- paste0(
  "a=", a, ", b=", b_vals, 
  ", skewness=", round(skewness, 3), ", kurtosis=", round(kurtosis, 3)
)

# パラメータごとにガンマ分布を計算
anime_dens_df <- tidyr::expand_grid(
  lambda = lambda_vals, 
  b = b_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(a, b, lambda) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dgamma(x = lambda, shape = a, rate = b), 
    parameter = rep(param_vec, each = length(lambda_vals)) |> 
      factor(levels = param_vec) # フレーム切替用ラベル
  ) # 確率密度を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = a / b_vals, # 期待値
  sd = sqrt(a / b_vals^2), # 標準偏差
  mode = (a - 1) / b_vals, # 最頻値
  parameter = factor(param_vec, levels = param_vec) # フレーム切替用のラベル
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
color_vec    <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))


# 統計量を重ねた分布のアニメーションを作図
anime_dens_graph <- ggplot() + 
  geom_line(data = anime_dens_df, mapping = aes(x = lambda, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  coord_cartesian(ylim = c(0, 2.5)) + # 軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = length(a_vals), fps = 10, width = 800, height = 600) # (aの影響の場合)
gganimate::animate(anime_dens_graph, nframes = length(b_vals), fps = 10, width = 800, height = 600) # (bの影響の場合)


