
# 1次元ガウス分布 --------------------------------------------------------------

# 乱数の可視化


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


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


