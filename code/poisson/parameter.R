
# ポアソン分布 ------------------------------------------------------------------

# パラメータの影響の可視化


# ライブラリの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------

# パラメータとして利用する値を指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)
length(lambda_vals) # フレーム数


# xの値を作成
x_vals <- seq(from = 0, to = ceiling(max(lambda_vals)) * 2)

# パラメータごとにポアソン分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dpois(x = x, lambda = lambda), 
    parameter = paste0("lambda=", lambda) |> 
      factor(levels = paste0("lambda=", sort(lambda_vals))) # フレーム切替用ラベル
  ) # 確率を計算


# ポアソン分布にアニメーションを作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  coord_cartesian(ylim = c(0, 0.5)) + # 軸の表示範囲
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600)


# パラメータと統計量の関係：アニメーションによる可視化 ----------------------------

# パラメータとして利用する値を指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)
length(lambda_vals) # フレーム数


# xの値を作成
x_vals <- seq(from = 0, to = ceiling(max(lambda_vals)) * 2)

# 歪度を計算
skewness_vec <- 1 / sqrt(lambda_vals)

# 尖度を計算
kurtosis_vec <- 1 / lambda_vals

# ラベル用のテキストを作成
label_vec <- paste0(
  "lambda=", lambda_vals, ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとにポアソン分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dpois(x = x, lambda = lambda), 
    parameter = rep(label_vec, each = length(x_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) # 確率を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = lambda_vals, # 期待値
  sd = sqrt(lambda_vals), # 標準偏差
  mode = floor(lambda_vals), # 最頻値
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

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))


# 統計量を重ねたポアソン分布のアニメーションを作図
anime_prob_graph <- ggplot() + 
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968", color = "#00A968") + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  coord_cartesian(ylim = c(0, 0.5)) + # 軸の表示範囲
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(lambda_vals), fps = 10, width = 800, height = 600)

