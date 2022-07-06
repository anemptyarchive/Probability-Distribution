
# 二項分布 --------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10

# 確率変数の値を指定:(x <= M)
x <- 3

# ベクトルに変換
phi_v <- c(1 - phi, phi)
x_v <- c(M - x, x)


# 定義式により確率を計算
C <- gamma(M + 1) / gamma(M - x + 1) / gamma(x + 1)
prob <- C * phi^x * (1 - phi)^(M - x)
prob

# 対数をとった定義式により確率を計算
log_C <- lgamma(M + 1) - lgamma(M - x + 1) - lgamma(x + 1)
log_prob <- log_C + x * log(phi) + (M - x) * log(1 - phi)
prob <- exp(log_prob)
prob; log_prob

# 二項分布の関数により確率を計算
prob <- dbinom(x = x, size = M, prob = phi)
prob

# 二項分布の対数をとった関数により確率を計算
log_prob <- dbinom(x = x, size = M, prob = phi, log = TRUE)
prob <- exp(log_prob)
prob; log_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = M, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
log_prob <- dmultinom(x = x_v, size = M, prob = phi_v, log = TRUE)
prob <- exp(log_prob)
prob; log_prob


# 統計量の計算 -------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10


# 期待値を計算
E_x <- M * phi
E_x

# 分散を計算
V_x <- M * phi * (1 - phi)
V_x

# 最頻値を計算:(注:複数の場合も1つしか返さない)
mode_x <- floor(phi * (M + 1))
mode_x


# 歪度を計算
skewness <- (1 - 2 * phi) / sqrt(M * phi * (1 - phi))
skewness

# 尖度を計算
kurtosis <- (1 - 6 * phi * (1 - phi)) / (M * phi * (1 - phi))
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M

# 二項分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dbinom(x = x_vals, size = M, prob = phi) # 確率
)


# 二項分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M), # (文字列表記用)
       #subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), # (数式表記用)
       x = "x", y = "probability") # ラベル


# 補助線用の統計量を計算
E_x <- M * phi
s_x <- sqrt(M * phi * (1 - phi))
mode_x <- floor(phi * (M + 1))

# 統計量を重ねた二項分布のグラフを作成:線のみ
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  geom_vline(xintercept = E_x, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = c(E_x-s_x, E_x+s_x), color = "orange", size = 1, linetype = "dotted") + # 期待値 ± 標準偏差
  geom_vline(xintercept = mode_x, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), 
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

# 統計量を重ねた二項分布のグラフを作成:凡例付き
ggplot() + # データ
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968") + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Binomial Distribution", 
       subtitle = parse(text = paste0("list(phi==", phi, ", M==", M, ")")), 
       x = "x", y = "probability") # ラベル


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


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10

# データ数を指定
N <- 1000


# 二項分布に従う乱数を生成
x_n <- rbinom(n = N, size = M, prob = phi)


### ・乱数の可視化 -----

# xがとり得る値を作成
x_vals <- 0:M

# サンプルを集計
freq_df <- tidyr::tibble(x = x_n) |> # 乱数を格納
  dplyr::count(x, name = "frequency") |> # 度数を集計
  dplyr::right_join(tidyr::tibble(x = x_vals), by = "x") |> # 全てのパターンに追加
  dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) # サンプルにない場合の欠損値を0に置換

# 二項分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dbinom(x = x_vals, size = M, prob = phi) # 確率
)


# サンプルのヒストグラムを作成:度数
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 度数
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = paste0("phi=", phi, ", M=", M, ", N=", N), 
       x = "x", y = "frequency") # ラベル

# サンプルのヒストグラムを作成:相対度数
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = frequency/N), 
           stat = "identity", fill = "#00A968") + # 相対度数
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(
    title = "Binomial Distribution", 
    subtitle = parse(
      text = paste0(
        "list(phi==", phi, ", M==", M, ", N==", "(list(", paste0(freq_df[["frequency"]], collapse = ", "), "))", ")")
    ), 
    x = "x", y = "relative frequency"
  ) # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
phi <- 0.35

# 試行回数を指定
M <- 10

# データ数(フレーム数)を指定
N <- 300


# 二項分布に従う乱数を生成
x_n <- rbinom(n = N, size = M, prob = phi)

# xがとり得る値を作成
x_vals <- 0:M

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
    label = paste0("phi=", phi, ", M=", M, ", N=", n, "=(", label, ")") %>% 
      factor(., levels = .)
  ) |> # パラメータ情報をまとめて因子化
  dplyr::pull(label) # ベクトルとして取得

# フレーム切替用のラベルを追加
anime_freq_df <- freq_df |> 
  tibble::add_column(parameter = rep(label_vec, each = M+1))

# サンプルを格納
anime_data_df <- tibble::tibble(
  x = x_n, # サンプル
  parameter = label_vec # フレーム切替用ラベルを追加
)

# 二項分布の情報を複製
anime_prob_df <- tibble::tibble(
  x = x_vals, # 確率変数
  probability = dbinom(x = x_vals, size = M, prob = phi), # 確率
  num = N # 複製数
) |> 
  tidyr::uncount(num) |> # データ数分に複製
  tibble::add_column(parameter = rep(label_vec, times = length(x_vals))) |> # フレーム切替用ラベルを追加
  dplyr::arrange(parameter) # サンプリング回数ごとに並べ替え


# 二項乱数のヒストグラムのアニメーションを作図:度数
anime_hist_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_hist_graph, nframes = N, fps = 10, width = 800, height = 600)


# 二項乱数のヒストグラムのアニメーションを作図:相対度数
anime_hist_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency/n), 
           stat = "identity", fill = "#00A968") + # 相対度数
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, minor_breaks = FALSE) + # x軸目盛
  coord_cartesian(ylim = c(-0.01, 0.5)) + # 軸の表示範囲
  labs(title = "Binomial Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "relative frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_hist_graph, nframes = N, fps = 10, width = 800, height = 600)


