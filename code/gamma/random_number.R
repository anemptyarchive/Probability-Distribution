
# ガンマ分布 -------------------------------------------------------------------

# 乱数の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# パッケージ名の省略用
library(ggplot2)


# 乱数の生成 -------------------------------------------------------------------

# パラメータを指定
a <- 5
b <- 2

# データ数(サンプルサイズ)を指定
N <- 1000


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# サンプルを格納
data_df <- tidyr::tibble(lambda = lambda_n)


# サンプルのヒストグラムを作成:度数
ggplot(data = data_df, mapping = aes(x = lambda, y = ..count..)) + # データ
  geom_histogram(fill = "#00A968", bins = 30) + # 度数
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b, ", N=", N), 
       x = expression(lambda), y = "frequency") # ラベル


# lambdaの値を作成
lambda_vals <- seq(from = 0, to = max(lambda_n)+1, length.out = 250)

# ガンマ分布を計算
dens_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)

# サンプルのヒストグラムを作成:密度
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = lambda, y = ..density..), 
                 fill = "#00A968", bins = 30) + # 密度
  geom_line(data = dens_df, mapping = aes(x = lambda, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b, ", N=", N), 
       x = expression(lambda), y = "density") # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
a <- 5
b <- 2

# データ数(フレーム数)を指定
N <- 100


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# サンプルを複製して格納
anime_freq_df <- tibble::tibble(
  lambda = rep(lambda_n, times = N), # サンプル
  n = rep(1:N, times = N), # データ番号
  frame = rep(1:N, each = N) # フレーム番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame) |> # サンプリング回数以前のサンプルを抽出
  dplyr::mutate(
    parameter = paste0("a=", a, ", b=", b, ", N=", frame) |> 
      factor(levels = paste0("a=", a, ", b=", b, ", N=", 1:N))
  ) # フレーム切替用ラベルを追加

# サンプルを格納
anime_data_df <- tidyr::tibble(
  lambda = lambda_n, # サンプル
  parameter = paste0("a=", a, ", b=", b, ", N=", 1:N) |> 
    factor(levels = paste0("a=", a, ", b=", b, ", N=", 1:N)) # フレーム切替用ラベル
)

# lambdaの値を作成
lambda_vals <- seq(from = 0, to = max(lambda_n)+1, length.out = 500)

# ガンマ分布を計算
dens_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)


# サンプルのヒストグラムを作成:度数
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = lambda, y = ..count..), 
                 breaks = seq(from = 0, to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = lambda, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda), y = "frequency") # ラベル

# サンプルのヒストグラムを作成:密度
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = lambda, y = ..density..), 
                 breaks = seq(from = 0, to = max(lambda_vals), length.out = 30), 
                 fill = "#00A968") + # 密度
  geom_line(data = dens_df, mapping = aes(x = lambda, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = lambda, y = 0), 
             color = "orange", size = 5) + # サンプル
  gganimate::transition_manual(parameter) + # フレーム
  coord_cartesian(ylim = c(NA, max(dens_df[["density"]]*2))) + # 軸の表示範囲
  labs(title = "Gamma Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


