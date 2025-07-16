
# 二項分布 ---------------------------------------------------------------------

# 乱数の可視化


# ライブラリの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


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


