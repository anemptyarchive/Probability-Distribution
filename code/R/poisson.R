
# ポアソン分布 ------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
lambda <- 4.5

# 確率変数の値を指定
x <- 2


# 定義式により確率を計算
prob <- lambda^x / gamma(x + 1) * exp(-lambda)
prob

# 対数をとった定義式により確率を計算
log_prob <- x * log(lambda) - lgamma(x + 1) - lambda
prob <- exp(log_prob)
prob; log_prob

# ポアソン分布の関数により確率を計算
prob <- dpois(x = x, lambda = lambda)
prob

# ポアソン分布の対数をとった関数により確率を計算
log_prob <- dpois(x = x, lambda = lambda, log = TRUE)
prob <- exp(log_prob)
prob; log_prob


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
lambda <- 4.5


# 期待値を計算
E_x <- lambda
E_x

# 分散を計算
V_x <- lambda
V_x

# 最頻値を計算
mode_x <- floor(lambda)
mode_x


# 歪度を計算
skewness <- 1 / sqrt(lambda)
skewness

# 尖度を計算
kurtosis <- 1 / lambda
kurtosis


# グラフの作成 ------------------------------------------------------------------

# パラメータを指定
lambda <- 4.5


# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(lambda) * 3)

# ポアソン分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = lambda) # 確率
)


# ポアソン分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 棒グラフ
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda), # (文字列表記用)
       #subtitle = parse(text = paste0("lambda==", lambda)), # (数式表記用)
       x = "x", y = "probability") # ラベル


# 補助線用の統計量の計算
E_x <- lambda
s_x <- sqrt(lambda)
mode_x <- floor(lambda)

# 統計量を重ねたポアソン分布を作図:線のみ
ggplot(data = prob_df, mapping = aes(x = x, y = probability)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 分布
  geom_vline(xintercept = E_x, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = E_x-s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = E_x+s_x, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  geom_vline(xintercept = mode_x, color = "chocolate", size = 1, linetype = "dashed") + # 最頻値
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = parse(text = paste0("lambda==", lambda)), 
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

# 統計量を重ねたポアソン分布のグラフを作成:凡例付き
ggplot() + # データ
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", position = "dodge", fill = "#00A968") + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type), 
             size = 1, linetype = "dashed") + # 統計量
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Poisson Distribution", 
       subtitle = parse(text = paste0("lambda==", lambda)), 
       x = "x", y = "probability") # ラベル


# パラメータと分布の関係：並べて比較 ----------------------------------------------------------

# パラメータとして利用する値を指定
lambda_vals <- c(1, 5.5, 10, 15.5)


# 作図用のxの点を作成
x_vals <- seq(from = 0, to = ceiling(max(lambda_vals)) * 2)

# パラメータごとにポアソン分布を計算
res_prob_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_vals
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    probability = dpois(x = x, lambda = lambda), 
    parameter = paste0("lambda=", lambda) |> 
      factor(levels = paste0("lambda=", sort(lambda_vals))) # 色分け用ラベル
  ) # 確率を計算

# 凡例用のラベルを作成:(数式表示用)
label_vec <- res_prob_df[["parameter"]] |> 
  stringr::str_replace_all(pattern = "=", replacement = "==") %>% # 等号表示用の記法に変換
  paste0("list(", ., ")") |> # カンマ表示用の記法に変換
  unique() |> # 重複を除去
  parse(text = _) # expression関数化
names(label_vec) <- unique(res_prob_df[["parameter"]]) # ggplotに指定する文字列に対応する名前付きベクトルに変換


# パラメータごとにポアソン分布のグラフを作成:棒グラフ
ggplot(data = res_prob_df, mapping = aes(x = x, y = probability, fill = parameter, color = parameter)) + # データ
  geom_bar(stat = "identity", position = "dodge") + # 棒グラフ
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  scale_fill_hue(labels = label_vec) + # 塗りつぶしの色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Poisson Distribution", 
       fill = "parameter", color = "parameter", 
       x = "x", y = "probability") # タイトル

# パラメータごとにポアソン分布のグラフを作成:点グラフ
ggplot(data = res_prob_df, mapping = aes(x = x, y = probability, fill = parameter, color = parameter)) + # データ
  geom_point(size = 3) + # 散布図
  geom_line(size = 1) + # 折れ線グラフ
  #scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_color_hue(labels = label_vec) + # 線の色:(数式表示用)
  scale_fill_hue(labels = label_vec) + # 塗りつぶしの色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Poisson Distribution", 
       fill = "parameter", color = "parameter", 
       x = "x", y = "probability") # タイトル


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------

# パラメータとして利用する値を指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)
length(lambda_vals) # フレーム数


# 作図用のxの点を作成
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
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(lambda_vals), fps = 100, width = 800, height = 600)


# 歪度と尖度の可視化 ----------------------------

# パラメータとして利用する値を指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)
length(lambda_vals) # フレーム数


# 作図用のxの点を作成
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


# 統計量を重ねたポアソン分布のアニメーションを作図
anime_prob_graph <- ggplot() + 
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", fill = "#00A968", color = "#00A968") + # 分布
  geom_vline(data = anime_stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 統計量
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x = "x", y = "probability") # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(lambda_vals), fps = 100, width = 800, height = 600)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# パラメータを指定
lambda <- 4.5

# データ数を指定
N <- 1000


# ポアソン分布に従う乱数を生成
x_n <- rpois(n = N, lambda = lambda)


### ・乱数の可視化 -----

# 作図用のxの点を作成
x_vals <- seq(from = 0, to = max(x_n) + 3)

# サンプルを集計
freq_df <- tidyr::tibble(x = x_n) |> # 乱数を格納
  dplyr::count(x, name = "frequency") |> # 度数を測定
  dplyr::right_join(tidyr::tibble(x = x_vals), by = "x") |> # 全てのパターンに追加
  dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) # サンプルにない場合の欠損値を0に置換

# ポアソン分布を計算
prob_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = lambda) # 確率
)


# サンプルのヒストグラムを作成:度数
ggplot(data = freq_df, mapping = aes(x = x, y = frequency)) + # データ
  geom_bar(stat = "identity", fill = "#00A968") + # 度数
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = paste0("lambda=", lambda, ", N=", N), 
       x = "x", y = "frequency") # ラベル

# サンプルのヒストグラムを作成:相対度数
ggplot() + 
  geom_bar(data = freq_df, mapping = aes(x = x, y = frequency/N), 
           stat = "identity", fill = "#00A968") + # 相対度数
  geom_bar(data = prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(
    title = "Bernoulli Distribution", 
    subtitle = parse(
      text = paste0(
        "list(lambda==", lambda, ", N==(list(", paste0(freq_df[["frequency"]], collapse = ", "), ")))")
    ), 
    x = "x", y = "relative frequency"
  ) # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
lambda <- 4.5

# データ数を指定
N <- 1000


# ポアソン分布に従う乱数を生成
x_n <- rpois(n = N, lambda = lambda)


# 作図用のxの点を作成
x_vals <- seq(from = 0, to = max(x_n) + 3)

# サンプルを集計
freq_df <- tidyr::tibble(
  x = x_n, # サンプル
  n = 1:N, # データ番号
  frequency = 1 # 集計用の値
) |> # 乱数を格納
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
    label = paste0("lambda=", lambda, ", N=", n, "=(", label, ")") %>% 
      factor(., levels = .)
  ) |> # パラメータ情報をまとめて因子化
  dplyr::pull(label) # ベクトルとして取得

# フレーム切替用のラベルを追加
anime_freq_df <- freq_df |> 
  tibble::add_column(parameter = rep(label_vec, each = length(x_vals)))

# サンプルを格納
anime_data_df <- tibble::tibble(x = x_n) |> 
  tibble::add_column(parameter = label_vec) # フレーム切替用のラベルを追加

# ポアソン分布の情報を複製
anime_prob_df <- tibble::tibble(
  x = x_vals, # 確率変数
  probability = dpois(x = x_vals, lambda = lambda), # 確率
  num = N # 複製数
) |> 
  tidyr::uncount(num) |> # データ数分に複製
  tibble::add_column(parameter = rep(label_vec, times = length(x_vals))) # フレーム切替用のラベルを追加


# ポアソン乱数のヒストグラムのアニメーションを作図:度数
anime_freq_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency), 
           stat = "identity", fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x ="x", y = "frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 100, width = 800, height = 600)


# ポアソン乱数のヒストグラムのアニメーションを作図:相対度数
anime_prop_graph <- ggplot() + 
  geom_bar(data = anime_freq_df, mapping = aes(x = x, y = frequency/n), 
           stat = "identity", fill = "#00A968") + # 相対度数
  geom_bar(data = anime_prob_df, mapping = aes(x = x, y = probability), 
           stat = "identity", alpha = 0, color = "darkgreen", linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = x_vals, labels = x_vals) + # x軸目盛
  ylim(c(-0.01, 0.5)) + # y軸の表示範囲
  labs(title = "Poisson Distribution", 
       subtitle = "{current_frame}", 
       x ="x", y = "relative frequency") # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100, width = 800, height = 600)


