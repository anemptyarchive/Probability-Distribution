
# ベータ分布 -------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(MCMCpack)
library(gganimate)
library(patchwork)

# チェック用
library(magrittr)
library(ggplot2)
library(patchwork)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# パラメータを指定
alpha <- 5
beta  <- 2

# データ数(サンプルサイズ)を指定
N <- 1000


# ベータ分布に従う乱数を生成
phi_n <- rbeta(n = N, shape1 = alpha, shape2 = beta)


### ・乱数の可視化 -----

# サンプルを格納
data_df <- tidyr::tibble(phi = phi_n)

# サンプルのヒストグラムを作成：頻度
ggplot(data = data_df, mapping = aes(x = phi)) + # データ
  geom_histogram(fill = "#00A968", bins = 30) + # 度数
  labs(title = "Beta Distribution", 
       subtitle = paste0("alpha=", alpha, ", beta=", beta, ", N=", N), # (文字列表記用)
       #subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ", N==", N, ")")), # (数式表記用)
       x = expression(phi), y = "frequency") # ラベル


# phiのとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
dens_df <- tidyr::tibble(
  phi = phi_vals, # x軸の値
  dens = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)

# サンプルのヒストグラムを作成：密度
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = phi, y = ..density..), 
                 fill = "#00A968", bins = 30) + # 密度
  geom_line(data = dens_df, mapping = aes(x = phi, y = dens), 
            color = "darkgreen", size = 1,linetype = "dashed") + # 元の分布
  labs(title = "Beta Distribution", 
       subtitle = parse(text = paste0("list(alpha==", alpha, ", beta==", beta, ", N==", N, ")")), 
       x = expression(phi), y = "density") # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

# パラメータを指定
alpha <- 5
beta  <- 2

# データ数(フレーム数)を指定
N <- 100


# ベータ分布に従う乱数を生成
phi_n <- rbeta(n = N, shape1 = alpha, shape2 = beta)


# サンプルを複製して格納
anime_freq_df <- tibble::tibble(
  phi = rep(phi_n, times = N), # サンプル
  n = rep(1:N, times = N), # データ番号
  frame = rep(1:N, each = N) # フレーム番号
) |> 
  dplyr::filter(n <= frame) |> # サンプリング回数以前のサンプルを抽出
  dplyr::mutate(
    parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", frame) |> 
      factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N))
  ) # フレーム切替用ラベルを追加

# サンプルを格納
anime_data_df <- tidyr::tibble(
  phi = phi_n, 
  n = 1:N, 
  parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", n) |> 
    factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N)) # フレーム切替用ラベル
)
anime_data_df <- tidyr::tibble(
  phi = phi_n, 
  parameter = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N) |> 
    factor(levels = paste0("alpha=", alpha, ", beta=", beta, ", N=", 1:N))
)

# phiのとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 501)

# ベータ分布を計算
dens_df <- tidyr::tibble(
  phi = phi_vals, # x軸の値
  density = dbeta(x = phi_vals, shape1 = alpha, shape2 = beta) # 確率密度
)


# サンプルのヒストグラムを作成：頻度
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = phi), 
                 breaks = seq(from = 0, to = 1, length.out = 30), 
                 fill = "#00A968") + # 度数
  geom_point(data = anime_data_df, mapping = aes(x = phi, y = 0), 
             color = "orange", size = 6) + # 
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "frequency") # ラベル

# サンプルのヒストグラムを作成：密度
anime_freq_graph <- ggplot() + 
  geom_histogram(data = anime_freq_df, mapping = aes(x = phi, y = ..density..), 
                 breaks = seq(from = 0, to = 1, length.out = 30), 
                 fill = "#00A968") + # 度数
  geom_line(data = dens_df, mapping = aes(x = phi, y = density), 
            color = "darkgreen", size = 1, linetype = "dashed") + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = phi, y = 0), 
             color = "orange", size = 6) + # 
  gganimate::transition_manual(parameter) + # フレーム
  coord_cartesian(ylim = c(0, 5)) + # 軸の表示範囲
  labs(title = "Beta Distribution", 
       subtitle = "{current_frame}", 
       x = expression(phi), y = "density") # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


