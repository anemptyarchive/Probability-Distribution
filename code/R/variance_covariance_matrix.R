
# 分散共分散行列 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(mvnfast)
library(ggrepel)
library(patchwork)

# チェック用
library(ggplot2)
library(patchwork)


# 相関行列との関係 ----------------------------------------------------------------

### ・分散共分散行列の設定 -----

# 次元数を指定
D <- 3

# 分散共分散行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換


### ・標準偏差と相関係数の計算 -----

# 次元を指定
i <- 1
j <- 2

# 分散を抽出
sigma2_i <- sigma_dd[i, i]
sigma2_j <- sigma_dd[j, j]
sigma2_i; sigma2_j

# 全ての分散を抽出
diag(sigma_dd)

# 共分散を抽出
sigma_ij <- sigma_dd[i, j]
sigma_ji <- sigma_dd[j, i]
sigma_ij; sigma_ji

# 標準偏差を計算
sigma_i <- sqrt(sigma2_i)
sigma_j <- sqrt(sigma2_j)
sigma_i; sigma_j

# 相関係数を計算
rho_ii <- sigma2_i / sigma_i / sigma_i
rho_ij <- sigma_ij / sigma_i / sigma_j
rho_ji <- sigma_ji / sigma_j / sigma_i
rho_jj <- sigma2_j / sigma_j / sigma_j
rho_ii; rho_ij; rho_ji; rho_jj


### ・分散共分散行列と相関行列の計算 -----

# 標準偏差が対角成分の行列を作成
s_dd <- sigma_dd |> 
  diag() |> # 対角成分(分散)を抽出
  (\(.){sqrt(.)})() |> # 標準偏差を計算
  diag() # 対角行列を作成
s_dd

# 標準偏差の逆数が対角成分の行列を作成
s_inv_dd <- sigma_dd |> 
  diag() |> # 対角成分(分散)を抽出
  (\(.){1 / sqrt(.)})() |> # 標準偏差の逆数を計算
  diag() # 対角行列を作成
s_inv_dd

# 標準偏差の逆数が対角成分の行列を計算
s_inv_dd <- solve(s_dd)
s_inv_dd

# 標準偏差が対角成分の行列を計算
s_dd <- solve(s_inv_dd)
s_dd

# 相関行列を計算
rho_dd <- s_inv_dd %*% sigma_dd %*% s_inv_dd
rho_dd

# 分散共分散行列を計算
sigma_dd <- s_dd %*% rho_dd %*% s_dd
sigma_dd


# マハラノビス距離との関係 ------------------------------------------------------------

### ・距離の計算 -----

# 次元数を指定
D <- 3

# 平均ベクトルを指定
mu_d <- c(1, 2, 4)

# 分散共分散行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換

# 変数の値を指定
x_d <- c(3, 2, 1)

# ユークリッド距離を計算
delta_euclid <- sum((x_d - mu_d)^2) |> 
  sqrt()
delta_euclid

# マハラノビス距離を計算
delta_mahal <- t(x_d - mu_d) %*% solve(sigma_dd) %*% (x_d - mu_d) |> 
  as.numeric() |> 
  sqrt()
delta_mahal


### ・生成分布(多次元ガウス分布)の設定 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 1.5), nrow = 2, ncol = 2)


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, # x軸の値
  x_2 = x_2_vals # y軸の値
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# 多次元ガウス分布を計算
dens_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)

# 点ごとに距離を計算
dist_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  euclidean = apply((t(x_mat) - mu_d)^2, 2, sum) |> 
    sqrt(), # ユークリッド距離
  mahalanobis = t(t(x_mat)-mu_d) %*% solve(sigma_dd) %*% (t(x_mat)-mu_d) |> 
    diag() |> 
    sqrt() # マハラノビス距離
)


### ・マハラノビス距離の可視化 -----

# データ数(サンプルサイズ)を指定
N <- 10

# 多次元ガウス分布に従う乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)

# サンプルごとに距離を計算
data_df <- tibble::tibble(
  n = factor(1:N), # データ番号
  x_1 = x_nd[, 1], # x軸の値
  x_2 = x_nd[, 2], # y軸の値
  euclidean = apply((t(x_nd) - mu_d)^2, 2, sum) |> 
    sqrt(), # ユークリッド距離
  mahalanobis = t(t(x_nd)-mu_d) %*% solve(sigma_dd) %*% (t(x_nd)-mu_d) |> 
    diag() |> 
    sqrt(), # マハラノビス距離
  coord_label = paste0("x=(", round(x_1, 1), ", ", round(x_2, 1), ")"), # 座標ラベル
  dist_label = paste0("ED=", round(euclidean, 2), "\nMD=", round(mahalanobis, 2)) # 距離ラベル
)


# 距離の等高線のプロット位置を指定
dist_vals <- seq(from = 1, to = 10, by = 1)

# パラメータラベルを作成:(数式表示用)
math_text <- paste0(
  "list(", 
  "mu==group('(', list(", paste0(mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# サンプルごとのユークリッド距離とマハラノビス距離の可視化
ggplot() + 
  geom_contour_filled(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.5) + # 2次元ガウス分布
  geom_contour(data = dist_df, mapping = aes(x = x_1, y = x_2, z = euclidean, linetype = "ed"), 
               breaks = dist_vals, color = "pink", size = 1) + # ユークリッド距離
  geom_contour(data = dist_df, mapping = aes(x = x_1, y = x_2, z = mahalanobis, linetype = "md"), 
               breaks = dist_vals, color = "skyblue", size = 1) + # マハラノビス距離
  geom_segment(data = data_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = x_1, yend = x_2, color = n), 
               arrow = arrow(length = unit(5, "pt"), type = "closed"), show.legend = FALSE) + # 期待値とサンプルの線分
  geom_point(data = data_df, mapping = aes(x = x_1, y = x_2, color = n), 
             alpha = 0.8, size = 5, show.legend = FALSE) + # サンプル
  ggrepel::geom_label_repel(data = data_df, mapping = aes(x = x_1, y = x_2, color = n, label = paste0(coord_label, "\n", dist_label)), 
                            alpha = 0.8, size = 2.5, label.padding = unit(3, "pt"), 
                            box.padding = unit(10, "pt"), point.padding = unit(10, "pt"), 
                            min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE) + # サンプルラベル
  coord_fixed(ratio = 1) + # アスペクト比
  scale_linetype_manual(breaks = c("ed", "md"), values = c("solid", "solid"), 
                        labels = c("euclidean", "mahalanobis"), name = "distance") + # (凡例表示用の黒魔術)
  guides(linetype = guide_legend(override.aes = list(color = c("pink", "skyblue")))) + # (凡例表示用の黒魔術)
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), # (数式表示用)
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))


# 距離を指定
dist <- 2

# 指定した範囲の内外を判定
data_df2 <- data_df |> 
  dplyr::mutate(
    color_flag = dplyr::case_when(
      euclidean <= dist & mahalanobis <= dist ~ "both", 
      euclidean <= dist ~ "euclid", 
      mahalanobis <= dist ~ "mahal", 
      TRUE ~ "none"
    ) # 色分け用フラグ
  )

# ユークリッド距離とマハラノビス距離の比較
ggplot() + 
  geom_contour_filled(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.5) + # 2次元ガウス分布
  geom_contour(data = dist_df, mapping = aes(x = x_1, y = x_2, z = euclidean, linetype = "ed"), 
               breaks = dist, color = "pink", size = 1) + # ユークリッド距離
  geom_contour(data = dist_df, mapping = aes(x = x_1, y = x_2, z = mahalanobis, linetype = "md"), 
               breaks = dist, color = "skyblue", size = 1) + # マハラノビス距離
  geom_point(data = data_df2, mapping = aes(x = x_1, y = x_2, color = color_flag), 
             alpha = 0.8, size = 5, show.legend = FALSE) + # サンプル
  geom_label(mapping = aes(x = min(x_1_vals), y = max(x_2_vals), label = paste0("dist:", dist)), 
             hjust = 0) + # 距離ラベル
  coord_fixed(ratio = 1) + # アスペクト比
  scale_color_manual(breaks = c("both", "euclid", "mahal", "none"), 
                     values = c("#00A968", "pink", "skyblue", "gray")) + # サンプルの色
  scale_linetype_manual(breaks = c("ed", "md"), values = c("solid", "solid"), 
                        labels = c("euclidean", "mahalanobis"), name = "distance") + # (凡例表示用の黒魔術)
  guides(linetype = guide_legend(override.aes = list(color = c("pink", "skyblue")))) + # (凡例表示用の黒魔術)
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))


# マハラノビス距離との関係をアニメーションで可視化 ------------------------------------------------------------

### ・分散(1軸)の影響 -----

# 平均ベクトルを指定
mu_d <- c(0, 0)

# x軸の分散として利用する値を指定
sigma2_1_vals <- seq(from = 0.5, to = 5, by = 0.1) |> 
  round(2)
length(sigma2_1_vals) # フレーム数

# y軸の分散を指定
sigma2_2 <- 2

# 共分散を指定
sigma_12 <- 0.6


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(max(sigma2_1_vals)) * 2, 
  to = mu_d[1] + sqrt(max(sigma2_1_vals)) * 2, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma2_2) * 3, 
  to = mu_d[2] + sqrt(sigma2_2) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma2_1 = sigma2_1_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_1) |> # 確率密度の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに距離を計算
anime_dist_df <- tidyr::expand_grid(
  sigma2_1 = sigma2_1_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_1) |> # 距離の計算用にグループ化
  dplyr::mutate(
    euclidean = apply((t(x_mat) - mu_d)^2, 2, sum) |> 
      sqrt(), # ユークリッド距離
    mahalanobis = t(t(x_mat)-mu_d) %*% solve(matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2)) %*% (t(x_mat)-mu_d) |> 
      diag() |> 
      sqrt(), # マハラノビス距離
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_longer(
    cols = c(euclidean, mahalanobis), 
    names_to = "type", 
    values_to = "distance"
  ) # 距離の列をまとめる


### ・分散(2軸)の影響 -----

# 平均ベクトルを指定
mu_d <- c(0, 0)

# x軸の分散を指定
sigma2_1 <- 2

# y軸の分散として利用する値を指定
sigma2_2_vals <- seq(from = 0.5, to = 5, by = 0.1) |> 
  round(2)
length(sigma2_2_vals) # フレーム数

# 共分散を指定
sigma_12 <- 0.6


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma2_1) * 3, 
  to = mu_d[1] + sqrt(sigma2_1) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(max(sigma2_2_vals)) * 2, 
  to = mu_d[2] + sqrt(max(sigma2_2_vals)) * 2, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma2_2 = sigma2_2_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_2) |> # 確率密度の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに距離を計算
anime_dist_df <- tidyr::expand_grid(
  sigma2_2 = sigma2_2_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_2) |> # 距離の計算用にグループ化
  dplyr::mutate(
    euclidean = apply((t(x_mat) - mu_d)^2, 2, sum) |> 
      sqrt(), # ユークリッド距離
    mahalanobis = t(t(x_mat)-mu_d) %*% solve(matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)) %*% (t(x_mat)-mu_d) |> 
      diag() |> 
      sqrt(), # マハラノビス距離
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_longer(
    cols = c(euclidean, mahalanobis), 
    names_to = "type", 
    values_to = "distance"
  ) # 距離の列をまとめる


### ・共分散の影響 -----

# 平均ベクトルを指定
mu_d <- c(0, 0)

# 分散を指定
sigma2_1 <- 3
sigma2_2 <- 4.5

# 共分散を指定として利用する値を指定
sigma_12_vals <- seq(from = -2, to = 2, by = 0.1) |> 
  round(2)
length(sigma_12_vals) # フレーム数


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma2_1) * 3, 
  to = mu_d[1] + sqrt(sigma2_1) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma2_2) * 3, 
  to = mu_d[2] + sqrt(sigma2_2) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_12) |> # 確率密度の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12_vals, ", ", sigma_12_vals, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに距離を計算
anime_dist_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_12) |> # 距離の計算用にグループ化
  dplyr::mutate(
    euclidean = apply((t(x_mat) - mu_d)^2, 2, sum) |> 
      sqrt(), # ユークリッド距離
    mahalanobis = t(t(x_mat)-mu_d) %*% solve(matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2)) %*% (t(x_mat)-mu_d) |> 
      diag() |> 
      sqrt(), # マハラノビス距離
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12_vals, ", ", sigma_12_vals, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_longer(
    cols = c(euclidean, mahalanobis), 
    names_to = "type", 
    values_to = "distance"
  ) # 距離の列をまとめる


### ・作図 -----

# 距離の等高線のプロット位置を指定
dist_vals <- seq(from = 1, to = 10, by = 1)

# ユークリッド距離とマハラノビス距離のアニメーションを作図
anime_dist_graph <- ggplot() + 
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.5) + # 2次元ガウス分布
  geom_contour(data = anime_dist_df, mapping = aes(x = x_1, y = x_2, z = distance, color = type), 
               breaks = dist_vals, size = 1) + # 距離
  gganimate::transition_manual(parameter) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  scale_color_manual(breaks = c("euclidean", "mahalanobis"), values = c("pink", "skyblue"), name = "distance") + # 線の色
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_dist_graph, nframes = length(sigma2_1_vals), fps = 10, width = 800, height = 800) # (分散(1軸)の影響用)
gganimate::animate(anime_dist_graph, nframes = length(sigma2_2_vals), fps = 10, width = 800, height = 800) # (分散(2軸)の影響用)
gganimate::animate(anime_dist_graph, nframes = length(sigma_12_vals), fps = 10, width = 800, height = 800) # (共分散の影響用)


# 固有値・固有ベクトルとの関係 -------------------------------------------------------------


### ・パラメータの設定 -----

# 次元数を指定
D <- 3

# 分散共分散行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換


# 固有値と固有ベクトルを計算
res_eigen <- eigen(sigma_dd)
res_eigen

# 固有値を取得
lambda_d <- res_eigen[["values"]]

# 固有ベクトルを取得
u_dd <- res_eigen[["vectors"]] |> 
  t()


### ・固有ベクトルの性質の計算 ----

# インデックスを指定
i <- 1
j <- 2


# 固有ベクトルを取得
u_id <- u_dd[i, ]
u_jd <- u_dd[j, ]


# 固有ベクトルの内積を計算:式(2.47)
I_ii <- t(u_id) %*% u_id |> 
  as.numeric()
I_ij <- t(u_id) %*% u_jd |> 
  as.numeric()
I_ji <- t(u_jd) %*% u_id |> 
  as.numeric()
I_jj <- t(u_jd) %*% u_jd |> 
  as.numeric()
I_ii; round(I_ij, 5); round(I_ji, 5); I_jj


# 固有ベクトルをまとめた行列の積を計算
I <- u_dd %*% t(u_dd)
round(I, 5)

# 固有ベクトルをまとめた行列の積を計算
I <- t(u_dd) %*% u_dd
round(I, 5)

# 固有ベクトルの積の総和を計算
I <- matrix(rep(0, times = D*D), nrow = D, ncol = D)
for(i in 1:D) {
  # i番目の固有ベクトルを抽出
  u_id <- u_dd[i, ]
  
  # 固有ベクトルの積を計算
  tmp_uu_dd <- u_id %*% t(u_id)
  
  # D個の和を計算
  I <- I + tmp_uu_dd
}
round(I, 5)


# 転置行列を計算
u_t_dd <- t(u_dd)
u_t_dd

# 逆行列を計算
u_inv_dd <- solve(u_dd)
u_inv_dd


### ・分散共分散行列と固有値・固有ベクトルの関係の計算 ----

# 分散共分散行列と固有ベクトルの行列の積:式(2.45)の左辺
sigma_u_d <- sigma_dd %*% u_id |> 
  as.numeric()

# 固有値と固有ベクトルの積:式(2.45)の右辺
lambda_u_d <- lambda_d[i] * u_id

# 分散共分散行列の積と固有値の積の比較:式(2.45)
dplyr::near(sigma_u_d, lambda_u_d)


# 分散共分散を計算:式(2.48)
res_sigma_dd <- matrix(rep(0, times = D*D), nrow = D, ncol = D)
for(i in 1:D) {
  # i番目の固有値・固有ベクトルを抽出
  lambda_i <- lambda_d[i]
  u_id     <- u_dd[i, ]
  
  # 固有値と2つの固有ベクトルの積を計算
  tmp_sigma_dd <- lambda_i * u_id %*% t(u_id)
  
  # D個の和を計算
  res_sigma_dd <- res_sigma_dd + tmp_sigma_dd
}
res_sigma_dd

# 分散共分散を計算:式(2.48')
res_sigma_dd <- t(u_dd) %*% diag(lambda_d) %*% u_dd
res_sigma_dd

# 分散共分散を計算:式(2.48')
res_sigma_dd <- t(lambda_d * u_dd) %*% u_dd
res_sigma_dd


# 精度行列を計算
sigma_inv_dd <- solve(sigma_dd)
sigma_inv_dd

# 精度行列を計算:式(2.49)
res_sigma_inv_dd <- matrix(rep(0, times = D*D), nrow = D, ncol = D)
for(i in 1:D) {
  # i番目の固有値・固有ベクトルを抽出
  lambda_i <- lambda_d[i]
  u_id     <- u_dd[i, ]
  
  # 固有値と2つの固有ベクトルの積を計算
  tmp_sigma_inv_dd <- 1/lambda_i * u_id %*% t(u_id)
  
  # D個の和を計算
  res_sigma_inv_dd <- res_sigma_inv_dd + tmp_sigma_inv_dd
}
res_sigma_inv_dd

# 精度行列を計算:式(2.49')
res_sigma_inv_dd <- t(u_dd) %*% diag(1/lambda_d) %*% u_dd
res_sigma_inv_dd

# 精度行列を計算:式(2.49')
res_sigma_inv_dd <- t(1/lambda_d * u_dd) %*% u_dd
res_sigma_inv_dd




# 分散共分散行列の行列式の平方根を計算:式(2.55)の左辺
res1 <- sqrt(det(sigma_dd))

# 固有値の平方根の総乗を計算:式(2.55)の右辺
res2 <- prod(sqrt(lambda_d))

# 比較:式(2.55)
dplyr::near(res1, res2)


# 分散共分散行列の固有ベクトルによるガウス分布の回転 --------------------------------------------------------------------

### ・パラメータの設定 -----

# 次元数を指定
D <- 2

# 平均ベクトルを指定
mu_d <- c(-4, 2)

# 分散共分散行列を指定
sigma_dd <- matrix(c(2, -0.6, -0.6, 1.5), nrow = D, ncol = D)

# データ数(サンプルサイズ)を指定
N <- 10


# 固有値と固有ベクトルを計算
res_eigen <- eigen(sigma_dd)
res_eigen

# 固有値を取得
lambda_d <- res_eigen[["values"]]

# 固有ベクトルを取得
u_dd <- res_eigen[["vectors"]] |> 
  t()


### ・分布の計算 -----

# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
dens_x_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)


# 多次元ガウス分布に従う乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)

# サンプルを格納
data_x_df <- tibble::tibble(
  n = factor(1:N), # データ番号
  x_1 = x_nd[, 1], # x軸の値
  x_2 = x_nd[, 2] # y軸の値
)


# 断面図の軸を計算
axis_x_df <- tibble::tibble(
  xstart = mu_d[1] - u_dd[, 1] * sqrt(lambda_d), 
  ystart = mu_d[2] - u_dd[, 2] * sqrt(lambda_d), 
  xend = mu_d[1] + u_dd[, 1] * sqrt(lambda_d), 
  yend = mu_d[2] + u_dd[, 2] * sqrt(lambda_d)
)


# パラメータラベルを作成:(数式表記用)
param_x_text <- paste0(
  "list(", 
  "mu==group('(', list(", paste0(mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# 確率密度の最大値を計算
max_dens <- mvnfast::dmvn(X = mu_d, mu = mu_d, sigma = sigma_dd)

# 2次元ガウス分布のグラフを作成
dens_x_graph <- ggplot() + 
  geom_contour_filled(data = dens_x_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 分布
  geom_contour(data = dens_x_df, mapping = aes(x = x_1, y = x_2, z = density), 
               breaks = max_dens*exp(-0.5), color = "red", size = 1, linetype = "dashed") + # 分布の断面図
  geom_segment(data = axis_x_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "blue", size = 1, arrow = arrow(length = unit(10, "pt"))) + # 断面図の軸
  geom_point(data = data_x_df, mapping = aes(x = x_1, y = x_2, color = n), 
             alpha = 0.8, size = 5, show.legend = FALSE) + # サンプル
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_x_text), 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))
dens_x_graph


### ・回転後の分布の計算 -----

# 平均ベクトルを作成
mu_y_d <- rep(0, times = D)

# 分散共分散行列を計算
lambda_dd <- diag(lambda_d)

# yの値を作成
y_1_vals <- seq(
  from = -sqrt(lambda_dd[1, 1]) * 3, 
  to = sqrt(lambda_dd[1, 1]) * 3, 
  length.out = 100
)
y_2_vals <- seq(
  from = -sqrt(lambda_dd[2, 2]) * 3, 
  to = sqrt(lambda_dd[2, 2]) * 3, 
  length.out = 100
)

# yの点を作成
y_mat <- tidyr::expand_grid(
  y_1 = y_1_vals, 
  y_2 = y_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
dens_y_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = y_mat, mu = mu_y_d, sigma = lambda_dd) # 確率密度
)


# サンプルを回転
y_nd <- t(t(x_nd) - mu_d) %*% t(u_dd)

# 回転したサンプルを格納
data_y_df <- tibble::tibble(
  n = factor(1:N), 
  y_1 = y_nd[, 1], 
  y_2 = y_nd[, 2]
)


# 固有ベクトルを回転
u_y_dd <- u_dd %*% t(u_dd)

# 断面図の軸を計算
axis_y_df <- tibble::tibble(
  xstart = - u_y_dd[, 1] * sqrt(lambda_d), 
  ystart = - u_y_dd[, 2] * sqrt(lambda_d), 
  xend = u_y_dd[, 1] * sqrt(lambda_d), 
  yend = u_y_dd[, 2] * sqrt(lambda_d)
)


# パラメータラベルを作成:(数式表記用)
param_y_text <- paste0(
  "list(", 
  "mu[y]==group('(', list(", paste0(round(mu_y_d, 2), collapse = ", "), "), ')')", 
  ", Lambda[y]==group('(', list(", paste0(round(lambda_dd, 2), collapse = ", "), "), ')')", 
  ")"
)

# 回転後の2次元ガウス分布のグラフを作成
dens_y_graph <- ggplot() + 
  geom_contour_filled(data = dens_y_df, mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 塗りつぶし等高線
  geom_contour(data = dens_y_df, mapping = aes(x = y_1, y = y_2, z = density), 
               breaks = max_dens*exp(-0.5), color = "red", size = 1, linetype = "dashed") + # 分布の断面図
  geom_segment(data = axis_y_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "blue", size = 1, arrow = arrow(length = unit(10, "pt"))) + # 断面図の軸
  geom_point(data = data_y_df, mapping = aes(x = y_1, y = y_2, color = n), 
             alpha = 0.8, size = 5, show.legend = FALSE) + 
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_y_text), 
       fill = "density", 
       x = expression(y[1]), y = expression(y[2]))
dens_y_graph

# グラフを並べて描画
dens_x_graph + dens_y_graph


### ・固有ベクトルによる分布の回転 ----

# xの点を回転
x_to_y_mat <- t(t(x_mat) - mu_d) %*% t(u_dd)

# yの値を作成
y_1_vals <- seq(from = min(x_to_y_mat[, 1]), to = max(x_to_y_mat[, 1]), length.out = 100)
y_2_vals <- seq(from = min(x_to_y_mat[, 2]), to = max(x_to_y_mat[, 2]), length.out = 100)

# yの点を作成
y_mat <- tidyr::expand_grid(
  y_1 = y_1_vals, 
  y_2 = y_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# yの点を回転
y_to_x_mat = y_mat %*% solve(t(u_dd))

# 多次元ガウス分布を計算
dens_y_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = y_to_x_mat, mu = rep(0,2), sigma = sigma_dd) # 確率密度
)


# サンプルを回転
y_nd <- t(t(x_nd) - mu_d) %*% t(u_dd)

# 回転したサンプルを格納
data_y_df <- tibble::tibble(
  n = factor(1:N), 
  y_1 = y_nd[, 1], 
  y_2 = y_nd[, 2]
)


# 軸を計算
axis_y_df <- dplyr::bind_cols(
  value_1 = c(axis_x_df[["xstart"]], axis_x_df[["xend"]]) - mu_d[1], 
  value_2 = c(axis_x_df[["ystart"]], axis_x_df[["yend"]]) - mu_d[2]
) |> 
  as.matrix() |> 
  (\(.){. %*% t(u_dd)})() |> 
  as.vector() |> 
  tibble::as_tibble() |> 
  tibble::add_column(
    name = paste0(rep(c("x", "y"), each = 4), rep(c("start", "end"), each = 2, times = 2)), 
    axis = rep(c("y_1", "y_2"), times = 4)
  ) |> 
  tidyr::pivot_wider(
    id_cols = axis, 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


# パラメータラベルを作成:(数式表記用)
param_y_text <- paste0(
  "list(", 
  "lambda==group('(', list(", paste0(round(lambda_d, 2), collapse = ", "), "), ')')", 
  ", U==group('(', list(", paste0(round(u_dd, 2), collapse = ", "), "), ')')", 
  ")"
)

# 固有ベクトルを重ねた2次元ガウス分布のグラフを作成
dens_y_graph <- ggplot() + 
  geom_contour_filled(data = dens_y_df, mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 塗りつぶし等高線
  geom_contour(data = dens_y_df, mapping = aes(x = y_1, y = y_2, z = density), 
               breaks = max_dens*exp(-0.5), color = "red", size = 1, linetype = "dashed") + # 分布の断面図
  geom_segment(data = axis_y_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "blue", size = 1, arrow = arrow(length = unit(10, "pt"))) + # 断面図の軸
  geom_point(data = data_y_df, mapping = aes(x = y_1, y = y_2, color = n), 
             alpha = 0.8, size = 5, show.legend = FALSE) + 
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_y_text), 
       fill = "density", 
       x = expression(y[1]), y = expression(y[2]))
dens_y_graph

# グラフを並べて描画
dens_x_graph + dens_y_graph


