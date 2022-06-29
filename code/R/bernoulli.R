
# ベルヌーイ分布 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 確率変数の値を指定:(0, 1)
x <- 0

# ベクトルに変換
phi_v <- c(1 - phi, phi)
x_v <- c(1 - x, x)


# 定義式により確率を計算
prob <- phi^x * (1 - phi)^(1 - x)
prob

# 対数をとった定義式により確率を計算
log_prob <- x * log(phi) + (1 - x) * log(1 - phi)
prob <- exp(log_prob)
prob; log_prob

# 二項分布の関数により確率を計算
prob <- dbinom(x = x, size = 1, prob = phi)
prob

# 二項分布の対数をとった関数により確率を計算
log_prob <- dbinom(x = x, size = 1, prob = phi, log = TRUE)
prob <- exp(log_prob)
prob; log_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = 1, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
log_prob <- dmultinom(x = x_v, size = 1, prob = phi_v, log = TRUE)
prob <- exp(log_prob)
prob; log_prob

# インデックスにより確率を抽出
prob <- phi_v[x+1]
prob


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
phi <- 0.35


# 期待値を計算
E_x <- phi
E_x

# 分散を計算
V_x <- phi * (1 - phi)
V_x

# 最頻値を計算:(注:複数の場合も1つしか返さない)
mode_x <- which.max(c(1 - phi, phi)) - 1
mode_x


# 歪度を計算
skewness <- (1 - 2 * phi) / sqrt(phi * (1 - phi))
skewness

# 尖度を計算
kurtosis <- (1 - 6 * phi * (1 - phi)) / (phi * (1 - phi))
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
phi <- 0.35


# xがとり得る値を作成
x_vals <- 0:1

# ベルヌーイ分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = c(1 - phi, phi) # 確率
)


# ベルヌーイ分布を作図
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(title = "Bernoulli Distribution", 
       subtitle = paste0("phi=", phi), # (文字列表記用)
       #subtitle = parse(text = paste0("phi==", phi)), # (数式表記用)
       x = "x", y = "probability") # ラベル


# 補助線用の統計量を計算
E_x <- phi
s_x <- sqrt(phi * (1 - phi))
mode_x <- which.max(c(1 - phi, phi)) - 1

# ベルヌーイ分布を作図:線のみ
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  geom_vline(xintercept = E_x, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = E_x-s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = E_x+s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  geom_vline(xintercept = mode_x, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(title = "Bernoulli Distribution", 
       subtitle = parse(text = paste0("phi==", phi)), 
       x = "x", y = "probability") # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(E_x, E_x-s_x, E_x+s_x, mode_x), # 統計量
  type = c("mean", "sd", "sd", "mode") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))

# 統計量を重ねたベルヌーイ分布のグラフを作成:凡例付き
ggplot() + # データ
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968") + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(title = "Bernoulii Distribution", 
       subtitle = parse(text = paste0("phi==", phi)), 
       x = "x", y = "probability") # ラベル


# パラメータと分布の関係：アニメーションによる可視化 -----------------------------------

# パラメータとして利用する値を指定
phi_vals <- seq(from = 0, to = 1, by = 0.01)
length(phi_vals) # フレーム数


# xがとり得る値を作成
x_vals <- 0:1

# パラメータごとにベルヌーイ分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  phi = phi_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = 1, prob = phi), 
    parameter = paste0("phi=", phi) |> 
      factor(levels = paste0("phi=", phi_vals)) # フレーム切替用ラベル
  ) # 確率を計算


# ベルヌーイ分布のアニメーションを作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100, width = 600, height = 600)


# パラメータと統計量の関係：アニメーションによる可視化 --------------------------------------------------------------------

# パラメータとして利用する値を作成
phi_vals <- seq(from = 0, to = 1, by = 0.01)
length(phi_vals) # フレーム数


# xがとり得る値を作成
x_vals <- 0:1

# 歪度を計算
skewness_vec <- (1 - 2 * phi_vals) / sqrt(phi_vals * (1 - phi_vals))

# 尖度を計算
kurtosis_vec <- (1 - 6 * phi_vals * (1 - phi_vals)) / (phi_vals * (1 - phi_vals))

# ラベル用のテキストを作成
label_vec <- paste0(
  "phi=", phi_vals, ", skewness=", round(skewness_vec, 3), ", kurtosis=", round(kurtosis_vec, 3)
)

# パラメータごとにベルヌーイ分布を計算
anime_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  phi = phi_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(phi, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dbinom(x = x, size = 1, prob = phi), 
    parameter = rep(label_vec, each = length(x_vals)) |> 
      factor(levels = label_vec) # フレーム切替用ラベル
  ) # 確率を計算

# パラメータごとに統計量を計算
anime_stat_df <- tibble::tibble(
  mean = phi_vals, # 期待値
  sd = sqrt(phi_vals * (1 - phi_vals)), # 標準偏差
  mode = max.col(cbind(1 - phi_vals, phi_vals)) - 1, # 最頻値
  parameter = factor(label_vec, levels = label_vec) # フレーム切替用のラベル
) |> # 統計量を計算
  dplyr::mutate(
    sd_m = mean - sd, 
    sd_p = mean + sd
  ) |> # 期待値±標準偏差を計算
  dplyr::select(!sd) |> # 不要な列を削除
  tidyr::pivot_longer(
    cols = !parameter, 
    names_to = "type", 
    values_to = "statistic"
  ) |> # 統計量の列をまとめる
  dplyr::mutate(
    type = stringr::str_replace(type, pattern = "sd_.", replacement = "sd")) # 期待値±標準偏差のカテゴリを統一

# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", sd = "orange", mode = "chocolate")
linetype_vec <- c(mean = "dashed", sd = "dotted", mode = "dashed")
label_vec <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))), mode = expression(mode(x)))


# 統計量を重ねたベルヌーイ分布のアニメーションを作図
anime_prob_graph <- ggplot(data = anime_stat_df, ) + 
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), stat = "identity", fill = "#00A968", color = "#00A968") + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Bernoulii Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100, width = 700, height = 600)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング ----

# パラメータを指定
phi <- 0.35

# データ数を指定
N <- 1000


# ベルヌーイ分布に従う乱数を生成
x_n <- rbinom(n = N, size = 1, prob = phi)


### ・乱数の可視化 ----

# xがとり得る値を作成
x_vals <- 0:1

# ベルヌーイ分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = c(1 - phi, phi) # 確率
)

# サンプルを集計
freq_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  frequency = c(sum(x_n == 0), sum(x_n == 1)) # 度数
)


# サンプルのヒストグラムを作成:度数
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 度数
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = paste0("phi=", phi, ", N=", N), 
       x = "x", y = "frequency") # ラベル

# サンプルのヒストグラムを作成:相対度数
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = frequency/N), 
           stat = "identity", fill = "#00A968") + # 構成比
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(
    title = "Bernoulli Distribution", 
    subtitle = parse(
      text = paste0(
        "list(phi==", phi, ", N==(list(", paste0(freq_df[["frequency"]], collapse = ", "), ")))")
    ), 
    x = "x", y = "relative frequency"
  ) # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
phi <- 0.35

# # データ数(フレーム数)を指定
N <- 300


# ベルヌーイ分布に従う乱数を生成
x_n <- rbinom(n = N, size = 1, prob = phi)

# xがとり得る値を作成
x_vals <- 0:1

# サンプルを集計
freq_df <- tibble::tibble(
  x = x_n, # サンプル
  n = 1:N, # データ番号
  frequency = 1 # 集計用の値
) |> 
  dplyr::right_join(tidyr::expand_grid(x = x_vals, n = 1:N), by = c("x", "n")) |> # 全てのパターンに結合
  dplyr::mutate(frequency = tidyr::replace_na(frequency, replace = 0)) |> # サンプルにない場合の欠損値を0に置換
  dplyr::arrange(n, x) |> # 集計用に昇順に並べ替え
  dplyr::group_by(x) |> # 集計用にグループ化
  dplyr::mutate(frequency = cumsum(frequency)) |> # 累積和を計算
  dplyr::ungroup() # グループ化を解除

# フレーム切替用のラベルを作成
label_vec <- freq_df |> 
  tidyr::pivot_wider(
    id_cols = n, 
    names_from = x, 
    names_prefix = "x", 
    values_from = frequency
  ) |> # 度数列を展開
  tidyr::unite(col = "label", dplyr::starts_with("x"), sep = ", ") |> # 度数情報をまとめて文字列化
  dplyr::mutate(
    label = paste0("phi=", phi, ", N=", n, "=(", label, ")") %>% 
      factor(., levels = .)
  ) |> # パラメータ情報をまとめて因子化
  dplyr::pull(label) # ベクトルとして取得

# フレーム切替用のラベルを追加
anime_freq_df <- freq_df |> 
  tibble::add_column(parameter = rep(label_vec, each = length(x_vals)))

# サンプルを格納
anime_data_df <- tibble::tibble(
  x = x_n, # サンプル
  parameter = label_vec # フレーム切替用ラベルを追加
)

# ベルヌーイ分布の情報を複製
anime_prob_df <- tibble::tibble(
  x = x_vals, # 確率変数
  probability = c(1 - phi, phi), # 確率
  num = N # 複製数
) |> 
  tidyr::uncount(num) |> # データ数分に複製
  tibble::add_column(parameter = rep(label_vec, times = length(x_vals))) |> # フレーム切替用ラベルを追加
  dplyr::arrange(parameter) # サンプリング回数ごとに並べ替え


# ベルヌーイ乱数のヒストグラムのアニメーションを作図:度数
anime_hist_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_hist_graph, nframes = N, fps = 100, width = 600, height = 600)


# ベルヌーイ乱数のヒストグラムのアニメーションを作図:相対度数
anime_prop_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency/n), 
           stat = "identity", fill = "#00A968") + # 相対度数
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size= 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(0, 1)) + # y軸の表示範囲
  labs(title = "Bernoulli Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "relative frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100, width = 600, height = 600)


