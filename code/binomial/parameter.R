
# 二項分布 ---------------------------------------------------------------------

# パラメータの影響の可視化


# ライブラリの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# パラメータと分布の関係:並べて比較 ----------------------------------------------------

### ・パラメータの影響 -----

# パラメータとして利用する値を指定
phi_vals <- c(0.1, 0.33, 0.5, 0.8, 0.9)

# 試行回数を指定
M <- 100


# xがとり得る値を作成
x_vals <- 0:M

# パラメータごとに二項分布を計算
res_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  phi = phi_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = M, prob = phi), 
    parameter = paste0("phi=", phi, ", M=", M) |> 
      factor(levels = paste0("phi=", sort(phi_vals), ", M=", M)) # 色分け用ラベル
  ) # 確率を計算


### ・試行回数の影響 -----

# パラメータを指定
phi <- 0.5

# 試行回数として利用する値を指定
M_vals <- c(5, 10, 20, 40)


# xがとり得る値を作成
x_vals <- 0:max(M_vals)

# パラメータごとに二項分布を計算
res_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  M = M_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(M, x) |> # 試行回数ごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = M, prob = phi), 
    parameter = paste0("phi=", phi, ", M=", M) |> 
      factor(levels = paste0("phi=", phi, ", M=", sort(M_vals))) # 色分け用ラベル
  ) |> # 確率を計算
  dplyr::filter(x <= M) # 実現可能な値を抽出


### ・作図 -----

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_prob_df[["parameter"]] |> 
  unique() |> # 重複を除去
  stringr::str_replace_all(pattern = "=", replacement = "==") %>% # 等号表示用の記法に変換
  paste0("list(", ., ")") |> # カンマ表示用の記法に変換
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_prob_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換


# パラメータごとに二項分布のグラフを作成
ggplot(data = res_prob_df, mapping = aes(x = x, y = probability, fill = parameter, color = parameter)) + # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  #scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  scale_fill_hue(labels = label_vec) + # 塗りつぶしの色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Binomial Distribution", 
       fill = "parameter", color = "parameter", 
       x = "x", y = "probability") # タイトル


# パラメータと分布の関係：アニメーションによる可視化 --------------------------------------------------

### ・パラメータの影響 -----

# パラメータとして利用する値を指定
phi_vals <- seq(from = 0, to = 1, by = 0.01)
length(phi_vals) # フレーム数

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M

# パラメータごとに二項分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  phi = phi_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = M, prob = phi), 
    parameter = paste0("phi=", phi, ", M=", M) |> 
      factor(levels = paste0("phi=", sort(phi_vals), ", M=", M)) # フレーム切替用ラベル
  ) # 確率を計算


### ・試行回数の影響 -----

# パラメータを指定
phi <- 0.3

# 試行回数の最大値を指定
M_max <- 100


# xがとり得る値を作成
x_vals <- 0:M_max

# 試行回数ごとに二項分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  M = 1:M_max
) |> # 全ての組み合わせを作成
  dplyr::arrange(M, x) |> # 試行回数ごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x_vals, size = M, prob = phi), 
    parameter = paste0("phi=", phi, ", M=", M) |> 
      factor(levels = paste0("phi=", phi, ", M=", 1:M_max)) # フレーム切替用ラベル
  ) |> # 確率を計算
  dplyr::filter(x <= M) # 実現可能な値を抽出


### ・作図 -----

# 二項分布のアニメーションを作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968", color = "#00A968") + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  #gganimate::view_follow(fixed_x = FALSE, fixed_y = TRUE) + # 表示範囲の調整
  #scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 10, width = 800, height = 600) # パラメータの影響用
gganimate::animate(anime_prob_graph, nframes = M_max, fps = 10, width = 800, height = 600) # 試行回数の影響用


# パラメータと統計量の関係：アニメーションによる可視化 --------------------------------------------------------------------

### ・パラメータの影響 -----

# パラメータとして利用する値を作成
phi_vals <- seq(from = 0.01, to = 0.99, by = 0.01)
length(phi_vals) # フレーム数

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M

# 歪度を計算
skewness_vec <- (1 - 2 * phi_vals) / sqrt(M * phi_vals * (1 - phi_vals))

# 尖度を計算
kurtosis_vec <- (1 - 6 * phi_vals * (1 - phi_vals)) / (M * phi_vals * (1 - phi_vals))

# ラベル用のテキストを作成
label_vec <- paste0(
  "phi=", phi_vals, ", M=", M, 
  ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとに二項分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  phi = phi_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = M, prob = phi), 
    parameter = rep(label_vec, each = length(x_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) # 確率を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = M * phi_vals, # 期待値
  sd = sqrt(M * phi_vals * (1 - phi_vals)), # 標準偏差
  mode = floor(phi_vals * (M + 1)), # 最頻値
  parameter = factor(label_vec, levels = label_vec) # フレーム切替用のラベル
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


### ・試行回数の影響 -----

# パラメータを指定
phi <- 0.35

# 試行回数の最大値を指定
M_vals <- 1:100


# xがとり得る値を作成
x_vals <- 0:max(M_vals)

# 歪度を計算
skewness_vec <- (1 - 2 * phi) / sqrt(M_vals * phi * (1 - phi))

# 尖度を計算
kurtosis_vec <- (1 - 6 * phi * (1 - phi)) / (M_vals * phi * (1 - phi))

# ラベル用のテキストを作成
label_vec <- paste0(
  "phi=", phi, ", M=", M_vals, 
  ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# 試行回数ごとに二項分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  M = M_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(M, x) |> # 試行回数ごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x_vals, size = M, prob = phi), 
    parameter = rep(label_vec, each = length(x_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) |> # 確率を計算
  dplyr::filter(x <= M) # 実現可能な値を抽出

# 試行回数ごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = M_vals * phi, # 期待値
  sd = sqrt(M_vals * phi * (1 - phi)), # 標準偏差
  mode = floor(phi * (M_vals + 1)), # 最頻値
  parameter = factor(label_vec, levels = label_vec) # フレーム切替用のラベル
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
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))


# 統計量を重ねた二項分布のアニメーションを作図
anime_prob_graph <- ggplot() + # データ
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968") + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  #gganimate::view_follow(fixed_x = FALSE, fixed_y = TRUE) + # 表示範囲の調整
  #scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 10, width = 800, height = 600) # パラメータの影響用
gganimate::animate(anime_prob_graph, nframes = length(M_vals), fps = 10, width = 800, height = 600) # 試行回数の影響用


