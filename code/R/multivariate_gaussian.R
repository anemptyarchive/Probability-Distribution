
# 多次元ガウス分布 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(mvnfast)
library(patchwork)

# チェック用
library(ggplot2)
library(patchwork)


# 確率密度の計算 -----------------------------------------------------------------

### ・パラメータの設定 -----

# 次元数を指定
D <- 3

# 平均ベクトルを指定
mu_d <- c(10, -6, 1.5)

# 分散共分散行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換
det(sigma_dd) # 確認

# 分散共分散行列をランダムに設定
sigma_dd <- rWishart(n = 1, df = D, Sigma = diag(D)) |> # ウィシャート分布の乱数を生成
  (\(.){.[, , 1]})() |> # 1番目のデータを抽出
  as.matrix() # マトリクスに変換
det(sigma_dd) # 確認

# 精度行列を指定
lambda_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換

# 精度行列を計算
lambda_dd <- solve(sigma_dd)

# 確率変数の値を指定
x_d = c(11.5, -5, 0)


### ・分散共分散行列を使用 -----

# 定義式により確率密度を計算
C_N  <- 1 / sqrt((2 * pi)^D * det(sigma_dd))
dens <- C_N * exp(-0.5 * t(x_d - mu_d) %*% solve(sigma_dd) %*% (x_d - mu_d)) |> 
  as.numeric()
dens

# 対数をとった定義式により確率密度を計算
log_C_N  <- -0.5 * (D * log(2 * pi) + log(det(sigma_dd)))
log_dens <- log_C_N - 0.5 * t(x_d - mu_d) %*% solve(sigma_dd) %*% (x_d - mu_d) |> 
  as.numeric()
dens <- exp(log_dens)
dens; log_dens

# 多次元ガウス分布の関数により確率密度を計算
dens <- mvnfast::dmvn(X = x_d, mu = mu_d, sigma = sigma_dd)
dens

# 多次元ガウス分布の対数をとった関数により確率密度を計算
log_dens <- mvnfast::dmvn(X = x_d, mu = mu_d, sigma = sigma_dd, log = TRUE)
dens <- exp(log_dens)
dens; log_dens


### ・精度行列を使用 -----

# 定義式により確率密度を計算
C_N  <- sqrt(det(lambda_dd) / (2 * pi)^D)
dens <- C_N * exp(-0.5 * t(x_d - mu_d) %*% lambda_dd %*% (x_d - mu_d)) |> 
  as.numeric()
dens

# 対数をとった定義式により確率密度を計算
log_C_N  <- -0.5 * (D * log(2 * pi) - log(det(lambda_dd)))
log_dens <- log_C_N - 0.5 * t(x_d - mu_d) %*% lambda_dd %*% (x_d - mu_d) |> 
  as.numeric()
dens <- exp(log_dens)
dens; log_dens

# 多次元ガウス分布の関数により確率密度を計算
dens <- mvnfast::dmvn(X = x_d, mu = mu_d, sigma = solve(lambda_dd))
dens

# 多次元ガウス分布の対数をとった関数により確率密度を計算
log_dens <- mvnfast::dmvn(X = x_d, mu = mu_d, sigma = solve(lambda_dd), log = TRUE)
dens <- exp(log_dens)
dens; log_dens


# 確率分布の作図 ----------------------------------------------------------------------

### ・パラメータの設定 -----

# 平均ベクトルを指定
mu_d <- c(0, 0)
mu_d <- c(6, 10)

# 分散共分散行列を指定
sigma_dd <- diag(2) # 単位行列
sigma_dd <- c(1, 0.6, 0.6, 4) |> # 値を指定
  matrix(nrow = 2, ncol = 2) # マトリクスに変換


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 101
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
dens_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)


### ・グラフの作成 -----

# パラメータラベルを作成:(数式表示用)
math_text <- paste0(
  "list(", 
  "mu==group('(', list(", paste0(mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# 2次元ガウス分布のグラフを作成:等高線図
ggplot() + 
  #geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 等高線
  geom_contour_filled(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), alpha = 0.8) + # 塗りつぶし等高線
  labs(
    title ="Bivariate Gaussian Distribution", 
    subtitle = paste0("mu=(", paste0(mu_d, collapse = ', '), "), Sigma=(", paste0(sigma_dd, collapse = ', '), ")"), # (文字列表記用)
    #subtitle = parse(text = math_text), # (数式表示用)
    color = "density", # (等高線用)
    fill = "density", # (塗りつぶし等高線用)
    x = expression(x[1]), y = expression(x[2])
  )

# 2次元ガウス分布のグラフを作成:ヒートマップ
ggplot() + 
  geom_tile(data = dens_df, mapping = aes(x = x_1, y = x_2, fill = density, color = density), alpha = 0.8) + # ヒートマップ
  scale_color_viridis_c(option = "D") + # タイルの色
  scale_fill_viridis_c(option = "D") + # 枠線の色
  #scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # タイルの色
  #scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # 枠線の色
  labs(
    title ="Bivariate Gaussian Distribution", 
    #subtitle = paste0("mu=(", paste0(mu_d, collapse = ', '), "), Sigma=(", paste0(sigma_dd, collapse = ', '), ")"), # (文字列表記用)
    subtitle = parse(text = math_text), # (数式表示用)
    fill = "density", color = "density", 
    x = expression(x[1]), y = expression(x[2])
  )


### ・統計量の可視化 -----

# 確率密度の最大値を計算
max_dens <- mvnfast::dmvn(X = mu_d, mu = mu_d, sigma = sigma_dd)

# 固有値と固有ベクトルを計算
res_eigen <- eigen(sigma_dd)

# 固有値を取得
lambda_d <- res_eigen[["values"]]

# 固有ベクトルを取得
u_dd <- res_eigen[["vectors"]] |> 
  t()


# 固有ベクトル
eigen_df <- tibble::tibble(
  xstart = 0, 
  ystart = 0, 
  xend = u_dd[, 1], 
  yend = u_dd[, 2]
)

# 固有ベクトル(軸の方向)のグラフを作成
ggplot() + 
  geom_segment(data = eigen_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               size = 1, arrow = arrow()) + # 軸の方向
  #coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Eigenvector", 
       subtitle = parse(text = paste0("U==(list(", paste0(round(u_dd, 2), collapse = ", "), "))")), 
       x = expression(x[1]), y = expression(x[2]))


# 断面図の軸を計算
axis_df <- tibble::tibble(
  xstart = mu_d[1] - u_dd[, 1] * sqrt(lambda_d), 
  ystart = mu_d[2] - u_dd[, 2] * sqrt(lambda_d), 
  xend = mu_d[1] + u_dd[, 1] * sqrt(lambda_d), 
  yend = mu_d[2] + u_dd[, 2] * sqrt(lambda_d)
)

# 固有値・固有ベクトルラベルを作成:(数式表示用)
eigen_text <- paste0(
  "list(", 
  "lambda==group('(', list(", paste0(round(lambda_d, 2), collapse = ", "), "), ')')", 
  ", U==group('(', list(", paste0(round(u_dd, 2), collapse = ", "), "), ')')", 
  ")"
)

# 固有ベクトルを重ねた2次元ガウス分布のグラフを作成
ggplot() + 
  #geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # ヒートマップ
  geom_contour_filled(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 塗りつぶし等高線
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density), breaks = max_dens*exp(-0.5), 
               color = "red", size = 1, linetype = "dashed") + # 分布の断面図
  #geom_segment(data = axis_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = xend, yend = yend), 
  #             color = "blue", size = 1, arrow = arrow(length = unit(10, "pt"))) + # 断面図の軸:サイズが固有値の固有ベクトル
  geom_segment(data = axis_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "blue", size = 1, arrow = arrow(length = unit(10, "pt"))) + # 断面図の軸:サイズが固有値の倍の固有ベクトル
  #geom_label(mapping = aes(x = mu_d[1], y = mu_d[2]+sqrt(sigma_dd[2, 2])*2, label = eigen_text), 
  #           parse = TRUE, hjust = 0.5, vjust = 0) + # 固有値・固有ベクトルラベル:(等高線用)
  geom_label(mapping = aes(x = min(x_1_vals), y = max(x_2_vals), label = eigen_text), 
             parse = TRUE, hjust = 0, vjust = 0) + # 固有値・固有ベクトルラベル:(塗りつぶし等高線用)
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title ="Bivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), 
       color = "density", fill = "density", 
       x = expression(x[1]), y = expression(x[2]))


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------------------------------------

### ・平均(1軸)の影響 -----

# x軸の平均として利用する値を指定
mu_1_vals <- seq(from = -2, to = 2, by = 0.04) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(mu_1_vals)

# y軸の平均を指定
mu_2 <- 10

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = 2, ncol = 2)


# xの値を作成
x_1_vals <- seq(
  from = min(mu_1_vals) - sqrt(sigma_dd[1, 1]) * 2, 
  to = max(mu_1_vals) + sqrt(sigma_dd[1, 1]) * 2, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_2 - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_2 + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  mu_1 = mu_1_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(mu_1) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd
    ), # 確率密度
    parameter = paste0("mu=(", mu_1, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("mu=(", mu_1_vals, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・平均(2軸)の影響 -----

# x軸の平均を指定
mu_1 <- 6

# y軸の平均として利用する値を指定
mu_2_vals <- seq(from = -2, to = 2, by = 0.04) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(mu_2_vals)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = 2, ncol = 2)


# xの値を作成
x_1_vals <- seq(
  from = mu_1 - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_1 + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = min(mu_2_vals) - sqrt(sigma_dd[2, 2]) * 2, 
  to = max(mu_2_vals) + sqrt(sigma_dd[2, 2]) * 2, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  mu_2 = mu_2_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(mu_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd
    ), # 確率密度
    parameter = paste0("mu=(", mu_1, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("mu=(", mu_1, ", ", mu_2_vals, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・平均(1,2軸)の影響 -----

# 平均ベクトルとして利用する値を指定
mu_1_vals <- seq(from = -3, to = 1, by = 0.04) |> 
  round(digits = 2)
mu_2_vals <- seq(from = -2, to = 2, by = 0.04) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(mu_1_vals)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = 2, ncol = 2)


# xの値を作成
x_1_vals <- seq(
  from = min(mu_1_vals) - sqrt(sigma_dd[1, 1]) * 2, 
  to = max(mu_1_vals) + sqrt(sigma_dd[1, 1]) * 2, 
  length.out = 101
)
x_2_vals <- seq(
  from = min(mu_2_vals) - sqrt(sigma_dd[2, 2]) * 2, 
  to = max(mu_2_vals) + sqrt(sigma_dd[2, 2]) * 2, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  idx = seq_along(mu_1_vals), # パラメータ番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(idx) |> # 分布の計算用にグループ化
  dplyr::mutate(
    mu_1 = mu_1_vals[idx], 
    mu_2 = mu_2_vals[idx], 
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd
    ), # 確率密度
    parameter = paste0("mu=(", mu_1, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("mu=(", mu_1_vals, ", ", mu_2_vals, "), Sigma=(", paste0(sigma_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・分散(1軸)の影響 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# x軸の分散として利用する値を指定
sigma2_1_vals <- seq(from = 0.5, to = 6, by = 0.1) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(sigma2_1_vals)

# y軸の分散を指定
sigma2_2 <- 4

# 共分散を指定
sigma_12 <- 0.6


# x軸とy軸の値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(max(sigma2_1_vals)) * 2, 
  to = mu_d[1] + sqrt(max(sigma2_1_vals)) * 2, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma2_2) * 3, 
  to = mu_d[2] + sqrt(sigma2_2) * 3, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma2_1 = sigma2_1_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_1) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・分散(2軸)の影響 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# x軸の分散を指定
sigma2_1 <- 1

# y軸の分散として利用する値を指定
sigma2_2_vals <- seq(from = 0.5, to = 5, by = 0.1) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(sigma2_2_vals)

# 共分散を指定
sigma_12 <- 0.6


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma2_1) * 3, 
  to = mu_d[1] + sqrt(sigma2_1) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(max(sigma2_2_vals)) * 2, 
  to = mu_d[2] + sqrt(max(sigma2_2_vals)) * 2, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma2_2 = sigma2_2_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma2_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・分散(1,2軸)の影響 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# 分散として利用する値を指定
sigma2_1_vals <- seq(from = 1, to = 4.5, by = 0.1) |> 
  round(digits = 2)
sigma2_2_vals <- seq(from = 1, to = 4.5, by = 0.1) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(sigma2_1_vals)

# 共分散を指定
sigma_12 <- 0.6

# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(max(sigma2_1_vals)) * 2, 
  to = mu_d[1] + sqrt(max(sigma2_1_vals)) * 2, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(max(sigma2_2_vals)) * 2, 
  to = mu_d[2] + sqrt(max(sigma2_2_vals)) * 2, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  idx = seq_along(sigma2_1_vals), # パラメータ番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(idx) |> # 分布の計算用にグループ化
  dplyr::mutate(
    sigma2_1 = sigma2_1_vals[idx], 
    sigma2_2 = sigma2_2_vals[idx], 
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma2_1), sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・共分散の影響 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# 分散を指定
sigma2_1 <- 1
sigma2_2 <- 1

# 共分散として利用する値を指定
sigma_12_vals <- seq(from = -0.8, to = 0.8, by = 0.02) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(sigma_12_vals)


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma2_1) * 3, 
  to = mu_d[1] + sqrt(sigma2_1) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma2_2) * 3, 
  to = mu_d[2] + sqrt(sigma2_2) * 3, 
  length.out = 101
)

# パラメータごとに多次元ガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_12) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12_vals, ", ", sigma_12_vals, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・作図 -----

# 2次元ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot() + 
  #geom_contour(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 等高線
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), alpha = 0.8) + # 塗りつぶし等高線
  gganimate::transition_manual(parameter) + # フレーム
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# 2次元ガウス分布のアニメーションを作図
anime_dens_graph <- ggplot() + 
  geom_tile(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, fill = density), alpha = 0.8) + # ヒートマップ
  gganimate::transition_manual(parameter) + # フレーム
  scale_fill_viridis_c(option = "D") + # 等高線の色
  labs(title ="Bivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = frame_num, fps = 10, width = 800, height = 600)


# パラメータと軸の関係をアニメーションで可視化 -----------------------------------------------------------------

## 「パラメータと分布の関係」のanime_dens_dfを利用

### ・分散(1軸)の影響 -----

# パラメータごとに楕円を計算
anime_ellipse_df <- anime_dens_df |> 
  dplyr::group_by(sigma2_1) |> # 分布の計算用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度の最大値
    density = dplyr::if_else(
      density >= max_dens * exp(-0.5), 
      true = max_dens * exp(-0.5), 
      false = -1
    ) # 断面に変換
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに楕円の軸を計算
anime_axis_df <- tidyr::expand_grid(
  sigma2_1 = sigma2_1_vals, # パラメータごとに
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(sigma2_1) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    mu = rep(mu_d, each = 2, times = 2), # 平均値
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    u = matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["vectors"]]})() |> 
      t() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    lambda = matrix(c(unique(sigma2_1), sigma_12, sigma_12, sigma2_2), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    value = mu + sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> 
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


### ・分散(2軸)の影響 -----

# パラメータごとに楕円を計算
anime_ellipse_df <- anime_dens_df |> 
  dplyr::group_by(sigma2_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)
    ), # 確率密度の最大値
    density = dplyr::if_else(
      density >= max_dens * exp(-0.5), 
      true = max_dens * exp(-0.5), 
      false = 0
    ) # 断面に変換
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに楕円の軸を計算
anime_axis_df <- tidyr::expand_grid(
  sigma2_2 = sigma2_2_vals, # パラメータごとに
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(sigma2_2) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    mu = rep(mu_d, each = 2, times = 2), # 平均値
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    u = matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["vectors"]]})() |> 
      t() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    lambda = matrix(c(sigma2_1, sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    value = mu + sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> 
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


### ・分散(1,2軸)の影響 -----

# パラメータごとに楕円を計算
anime_ellipse_df <- anime_dens_df |> 
  dplyr::group_by(sigma2_1, sigma2_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma2_1), sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2)
    ), # 確率密度の最大値
    density = dplyr::if_else(
      density >= max_dens * exp(-0.5), 
      true = max_dens * exp(-0.5), 
      false = 0
    ) # 断面に変換
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに楕円の軸を計算
anime_axis_df <- tidyr::expand_grid(
  idx = seq_along(sigma2_1_vals), # パラメータ番号
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(idx) |> # 軸の計算用にグループ化
  dplyr::mutate(
    sigma2_1 = sigma2_1_vals[idx], 
    sigma2_2 = sigma2_2_vals[idx], 
    axis = rep(c("y_1", "y_2"), times = 4), # 受け皿の確保とpivot_wider用の列
    mu = rep(mu_d, each = 2, times = 2), # 平均値
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    u = matrix(c(unique(sigma2_1), sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["vectors"]]})() |> 
      t() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    lambda = matrix(c(unique(sigma2_1), sigma_12, sigma_12, unique(sigma2_2)), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    value = mu + sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> 
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


### ・共分散の影響 -----

# パラメータごとに楕円を計算
anime_ellipse_df <- anime_dens_df |> 
  dplyr::group_by(sigma_12) |> # 分布の計算用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2)
    ), # 確率密度の最大値
    density = dplyr::if_else(
      density >= max_dens * exp(-0.5), 
      true = max_dens * exp(-0.5), 
      false = 0
    ) # 断面に変換
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに楕円の軸を計算
anime_axis_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, # パラメータごとに
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(sigma_12) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    mu = rep(mu_d, each = 2, times = 2), # 平均値
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    u = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["vectors"]]})() |> 
      t() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    lambda = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = 2, ncol = 2) |> 
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    value = mu + sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12, ", ", sigma_12, ", ", sigma2_2, ")") |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma2_1, ", ", sigma_12_vals, ", ", sigma_12_vals, ", ", sigma2_2, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> 
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


### ・作図 -----

# 楕円の軸のアニメーションを作図
anime_eigen_graph <- ggplot() + 
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 2次元ガウス分布
  geom_contour(data = anime_ellipse_df, mapping = aes(x = x_1, y = x_2, z = density), 
               bins = 2, color = "red", size = 1, linetype = "dotted") + # 分布の断面
  #geom_segment(data = anime_axis_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = xend, yend = yend), 
  #             color = "blue", size = 1, arrow = arrow()) + # 断面の軸:サイズが固有値の固有ベクトル
  geom_segment(data = anime_axis_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "blue", size = 1, arrow = arrow()) + # 断面の軸:サイズが固有値の倍の固有ベクトル
  gganimate::transition_manual(parameter) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title ="Bivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_eigen_graph, nframes = frame_num, fps = 10, width = 800, height = 600)

warnings()


# 乱数の生成 -------------------------------------------------------------------

### ・パラメータの設定 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 1.5), nrow = 2, ncol = 2)

# データ数(サンプルサイズ)を指定
N <- 10000
N <- 20


# 多次元ガウス分布に従う乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)

# サンプルを格納
data_df <- tibble::tibble(
  x_1 = x_nd[, 1], # x軸の値
  x_2 = x_nd[, 2] # y軸の値
)


### ・乱数の可視化 -----

# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 101
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
dens_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)


# パラメータラベルを作成:(数式表示用)
math_text <- paste0(
  "list(", 
  "mu==group('(', list(", paste0(mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# サンプルの散布図を作成
ggplot() + 
  geom_point(data = data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", alpha = 0.3) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), 
       color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# サンプルの度数のヒートマップを作成
ggplot() + 
  geom_bin_2d(data = data_df, mapping = aes(x = x_1, y = x_2, fill = ..count..), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), 
       fill = "frequency", color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# サンプルの密度の等高線図を作成
ggplot() + 
  geom_density_2d_filled(data = data_df, mapping = aes(x = x_1, y = x_2, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = parse(text = math_text), 
       fill = "density", color = "density", 
       x = expression(x[1]), y = expression(x[2]))


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

### ・パラメータの設定 -----

# 平均ベクトルを指定
mu_d <- c(6, 10)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = 2, ncol = 2)


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(sigma_dd[1, 1]) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(sigma_dd[2, 2]) * 3, 
  length.out = 101
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
dens_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)


### ・1データずつ可視化 -----

# データ数(フレーム数)を指定
N <- 100


# 多次元ガウス分布に従う乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)

# サンプルを格納
anime_data_df <- tibble::tibble(
  n = 1:N, # データ番号
  x_1 = x_nd[, 1], # x軸の値
  x_2 = x_nd[, 2], # y軸の値
  parameter = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", n) |> 
    factor(levels = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", 1:N)) # フレーム切替用ラベル
)

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame) |> # 各試行までのサンプルを抽出
  dplyr::mutate(
    x_1 = x_nd[n, 1], 
    x_2 = x_nd[n, 2], 
    parameter = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", frame) |> 
      factor(levels = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", 1:N)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_point(data = anime_freq_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", alpha = 0.5, size = 3) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N+10, end_pause = 10, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
x_1_breaks <- seq(from = min(x_1_vals), to = max(x_1_vals), length.out = 30)
x_2_breaks <- seq(from = min(x_2_vals), to = max(x_2_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin_2d(data = anime_freq_df, mapping = aes(x = x_1, y = x_2, fill = ..count..), 
              breaks = list(x = x_1_breaks, y = x_2_breaks), 
              alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 除去するフレーム数を指定
n_min <- 10

# (データが少ないと密度を計算できないため)最初のフレームを除去
tmp_data_df <- anime_data_df|> 
  dplyr::filter(n > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )
tmp_freq_df <- anime_freq_df |> 
  dplyr::filter(frame > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )

# 密度の等高線図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density_2d_filled(data = tmp_freq_df, mapping = aes(x = x_1, y = x_2, fill = ..level..), 
                         alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  geom_point(data = tmp_data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N-n_min+10, end_pause = 10, fps = 10, width = 800, height = 600)


### ・複数データずつ可視化 -----

# データ数を指定
N <- 10000

# フレーム数を指定
frame_num <- 100

# 1フレーム当たりのデータ数を計算
n_per_frame <- N %/% frame_num


# 多次元ガウス分布に従う乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:frame_num, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame*n_per_frame) |> # 各フレームで利用するサンプルを抽出
  dplyr::mutate(
    x_1 = x_nd[n, 1], 
    x_2 = x_nd[n, 2], 
    parameter = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", frame*n_per_frame) |> 
      factor(levels = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), N=", 1:frame_num*n_per_frame)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_point(data = anime_freq_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", alpha = 0.3, size = 3) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
x_1_breaks <- seq(from = min(x_1_vals), to = max(x_1_vals), length.out = 30)
x_2_breaks <- seq(from = min(x_2_vals), to = max(x_2_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin_2d(data = anime_freq_df, mapping = aes(x = x_1, y = x_2, fill = ..count..), 
              breaks = list(x = x_1_breaks, y = x_2_breaks), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 密度の等高線図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density_2d_filled(data = anime_freq_df, mapping = aes(x = x_1, y = x_2, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 分布の生成 -----------------------------------------------------------------

### ・パラメータの設定 -----

# (生成分布の)平均ベクトルを指定
mu_gen_d <- c(6, 10)

# (生成分布の)分散共分散行列を指定
sigma_gen_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = 2, ncol = 2)

# 分布の数(サンプルサイズ)を指定
N <- 9


# 多次元ガウス分布の平均ベクトルμを生成
mu_nd <- mvnfast::rmvn(n = N, mu = mu_gen_d, sigma = sigma_gen_dd)

# パラメータを格納
param_df <- tibble::tibble(
  mu_1 = mu_nd[, 1], # x軸の値
  mu_2 = mu_nd[, 2] # y軸の値
) |> # 値を格納
  dplyr::arrange(mu_1, mu_2) |> # 作図時の配置調整用に並べ替え
  dplyr::mutate(
    parameter = paste0("(", round(mu_1, 2), ", ", round(mu_2, 2), ")") |> 
      factor() # 色分け用ラベル
  )


# 確率変数μの値を作成
mu_1_vals <- seq(
  from = mu_gen_d[1] - sqrt(sigma_gen_dd[1, 1]) * 3, 
  to = mu_gen_d[1] + sqrt(sigma_gen_dd[1, 1]) * 3, 
  length.out = 101
)
mu_2_vals <- seq(
  from = mu_gen_d[2] - sqrt(sigma_gen_dd[2, 2]) * 3, 
  to = mu_gen_d[2] + sqrt(sigma_gen_dd[2, 2]) * 3, 
  length.out = 101
)

# 確率変数μの点を作成
mu_mat <- tidyr::expand_grid(
  mu_1 = mu_1_vals, 
  mu_2 = mu_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 多次元ガウス分布を計算
gaussian_generator_df <- tibble::tibble(
  mu_1 = mu_mat[, 1], # x軸の値
  mu_2 = mu_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = mu_mat, mu = mu_gen_d, sigma = sigma_gen_dd) # 確率密度
)

# パラメータの期待値を計算
E_mu_d <- mu_gen_d


# パラメータラベルを作成:(数式表示用)
generator_param_text <- paste0(
  "list(", 
  "mu[paste(g,e,n)]==group('(', list(", paste0(mu_gen_d, collapse = ", "), "), ')')", 
  ", Sigma[paste(g,e,n)]==group('(', list(", paste0(sigma_gen_dd, collapse = ", "), "), ')')", 
  ")"
)

# 2次元ガウス分布を作図
gaussian_generator_graph <- ggplot() + 
  geom_contour_filled(data = gaussian_generator_df, mapping = aes(x = mu_1, y = mu_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # パラメータの生成分布
  geom_point(mapping = aes(x = E_mu_d[1], y = E_mu_d[2]), 
             color = "red", size = 6, shape = 4) + # パラメータの期待値
  geom_point(data = param_df, mapping = aes(x = mu_1, y = mu_2, color = parameter), 
             alpha = 0.8, size = 6) + # パラメータのサンプル
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = generator_param_text), 
       color = expression(mu), fill = "density", 
       x = expression(mu[1]), y = expression(mu[2]))
gaussian_generator_graph


### ・分布の作図：多次元ガウス分布 -----

# (生成された分布の)分散共分散行列を指定
sigma_dd <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)


# 確率変数xの値を作成
x_1_vals <- mu_1_vals
x_2_vals <- mu_2_vals

# 確率変数xの点を作成
x_mat <- mu_mat


# パラメータの期待値による多次元ガウス分布を計算
E_gaussian_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = E_mu_d, sigma = sigma_dd) # 確率密度
)

# パラメータのサンプルごとに多次元ガウス分布を計算
gaussian_sample_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに確率変数xを複製
  dplyr::mutate(
    mu_1 = mu_nd[n, 1], 
    mu_2 = mu_nd[n, 2], 
  ) |> # パラメータのサンプルを抽出
  dplyr::arrange(mu_1, mu_2, x_1, x_2) |> # 作図時の配置調整用に並べ替え
  dplyr::group_by(n) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd
    ) # 確率密度
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::mutate(
    parameter = paste0("(", round(mu_1, 2), ", ", round(mu_2, 2), ")") |> 
      (\(.){factor(., levels = unique(.))})() # 色分け用ラベル
  )


# パラメータラベルを作成:(数式表示用)
sample_param_text <- paste0(
  "list(", 
  "Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# サンプルによる2次元ガウス分布を作図:分割して表示
gaussian_sample_graph <- ggplot() + 
  geom_contour_filled(data = gaussian_sample_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # サンプルによる分布
  geom_point(data = param_df, mapping = aes(x = mu_1, y = mu_2, color = parameter), 
             size = 6, shape = 4, show.legend = FALSE) + # サンプルによる分布の期待値
  facet_wrap(. ~ parameter, dir = "v", labeller = label_bquote(mu==.((as.character(parameter))))) + # グラフの分割
  coord_cartesian(xlim = c(min(mu_1_vals), max(mu_1_vals)), ylim = c(min(mu_2_vals), max(mu_2_vals))) + # 軸の表示範囲
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = sample_param_text), 
       color = expression(mu), fill = "density", 
       x = expression(x[1]), y = expression(x[2]))
gaussian_sample_graph

# グラフを並べて描画
gaussian_generator_graph + gaussian_sample_graph


# 確率密度の最大値を計算
max_dens <- mvnfast::dmvn(X = E_mu_d, mu = E_mu_d, sigma = sigma_dd)

# パラメータラベルを作成:(数式表示用)
sample_param_text <- paste0(
  "list(", 
  "E(mu)==group('(', list(", paste0(E_mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ")"
)

# サンプルによる2次元ガウス分布を作図:重ねて表示
gaussian_sample_graph <- ggplot() + 
  geom_contour(data = E_gaussian_df, mapping = aes(x = x_1, y = x_2, z = density), 
               breaks = max_dens*exp(-0.5), color = "red", linetype ="dashed") + # 期待値による分布
  geom_point(mapping = aes(x = E_mu_d[1], y = E_mu_d[2]), 
             color = "red", size = 6, shape = 4) + # 期待値による分布の期待値
  geom_contour(data = gaussian_sample_df, mapping = aes(x = x_1, y = x_2, z = density, color = parameter), 
               breaks = max_dens*exp(-0.5), show.legend = FALSE) + # サンプルによる分布
  geom_point(data = param_df, mapping = aes(x = mu_1, y = mu_2, color = parameter), 
             size = 6, shape = 4, show.legend = FALSE) + # サンプルによる分布の期待値
  coord_cartesian(xlim = c(min(mu_1_vals), max(mu_1_vals)), ylim = c(min(mu_2_vals), max(mu_2_vals))) + # 軸の表示範囲
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = sample_param_text), 
       color = expression(mu), 
       x = expression(x[1]), y = expression(x[2]))
gaussian_sample_graph

# グラフを並べて描画
gaussian_generator_graph + gaussian_sample_graph + 
  patchwork::plot_layout(guides = "collect") # 凡例の位置をまとめる


# 多次元ガウス分布の可視化 ------------------------------------------------------------

### ・分布の作図 -----

# 次元数を指定
D <- 4

# 平均ベクトルをランダムに設定
mu_d <- rnorm(n = D, mean = 0, sd = 1)

# 分散共分散行列をランダムに設定
sigma_dd <- rWishart(n = 1, df = D+1, Sigma = diag(D))[, , 1] |> 
  solve()


# 処理用のベクトルを作成
sigma_ii_vals <- diag(sigma_dd)
sigma_ij_vals <- as.vector(sigma_dd)

# xの値を作成
x_vals <- seq(
  from = - sqrt(max(sigma_ii_vals)) * 4, 
  to = sqrt(max(sigma_ii_vals)) * 4, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_i = x_vals, 
  x_j = x_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# 次元ごとに2次元ガウス分布を計算
dens_df <- tidyr::expand_grid(
  i = 1:D, # 行番号
  j = 1:D, # 列番号
  x_i = x_vals, # x軸の値
  x_j = x_vals  # y軸の値
) |> # 次元ごとに格子点を複製
  dplyr::filter(i < j) |> # 重複を除去
  dplyr::group_by(i, j) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = c(mu_d[unique(i)], mu_d[unique(j)]), 
      sigma = c(
        sigma_ii_vals[unique(i)], sigma_ij_vals[D*(unique(j)-1) + unique(i)], 
        sigma_ij_vals[D*(unique(j)-1) + unique(i)], sigma_ii_vals[unique(j)]
      ) |> 
        matrix(nrow = 2, ncol = 2)
    )
  ) |> # 確率密度を計算
  dplyr::ungroup() # グループ化を解除

# 次元ごとにパラメータラベルを作成
label_df <- tidyr::expand_grid(
  i = 1:D, # 行番号
  j = 1:D  # 列番号
) |> 
  dplyr::filter(i < j) |> # 重複を除去
  dplyr::group_by(i, j) |> # ラベルの作成用にグループ化
  dplyr::mutate(
    parameter = paste0(
      "list(", 
      "mu==(list(", round(mu_d[i], 2), ", ", round(mu_d[j], 2), "))", 
      ", Sigma==(list(", round(sigma_ii_vals[i], 2), ", ", round(sigma_ij_vals[D*(j-1) + i], 2), 
      ", ", round(sigma_ij_vals[D*(j-1) + i], 2), ", ", round(sigma_ii_vals[j], 2), "))", 
      ")"
    )
  ) |> # パラメータラベルを作成
  dplyr::ungroup() # グループ化を解除


# パラメータラベルを作成
param_text <- paste0(
  "list(", 
  "mu==(list(mu[1],...,mu[D]))", 
  ", Sigma==(list(sigma[1]^2,...,sigma[ij],...,sigma[D]^2))", 
  ")"
)

# 多次元ガウス分布を作図
ggplot() + 
  geom_contour_filled(data = dens_df, mapping = aes(x = x_i, y = x_j, z = density, fill = ..level..), 
                      alpha = 0.8) + # 2次元ガウス分布
  geom_label(data = label_df, mapping = aes(x = min(x_vals), y = max(x_vals), label = parameter), 
             parse = TRUE, hjust = "inward", vjust = "inward") + # パラメータラベル
  facet_grid(i ~ j, labeller = label_bquote(rows = i==.(i), cols = j==.(j))) + # グラフの分割
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = expression(x[i]), y = expression(x[j]))


### ・乱数の作図 -----

# データ数を指定
N <- 100

# 多次元ガウス分布の乱数を生成
x_nd <- mvnfast::rmvn(n = N, mu = mu_d, sigma = sigma_dd)
x_nd


# 次元ごとに乱数を格納
data_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  i = 1:D, # 行番号
  j = 1:D  # 列番号
) |> # 全ての組み合わせを作成
  dplyr::filter(i < j) |> # 重複を除去
  dplyr::group_by(n, i, j) |> # 分布の計算用にグループ化
  dplyr::mutate(
    x_i = x_nd[n, i], # x軸の値
    x_j = x_nd[n, j]  # y軸の値
  ) |> # 確率密度を計算
  dplyr::ungroup() # グループ化を解除


# 多次元ガウス分布を作図
ggplot() + 
  geom_contour_filled(data = dens_df, mapping = aes(x = x_i, y = x_j, z = density, fill = ..level..), 
                      alpha = 0.8) + # 2次元ガウス分布
  geom_point(data = data_df, mapping = aes(x = x_i, y = x_j), 
             color = "orange", alpha = 0.5) + 
  geom_label(data = label_df, mapping = aes(x = min(x_vals), y = max(x_vals), label = parameter), 
             parse = TRUE, hjust = "inward", vjust = "inward") + # パラメータラベル
  facet_grid(i ~ j, labeller = label_bquote(rows = i==.(i), cols = j==.(j))) + # グラフの分割
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = expression(x[i]), y = expression(x[j]))


### ・乱数のアニメーションの作図 -----

# フレームごとに分布を複製
anime_dens_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  dens_df
)

# フレーム列を追加
anime_data_df <- data_df |> 
  dplyr::mutate(frame = n)

# 次元ごとに乱数を複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N, # データ番号
  i = 1:D, # 行番号
  j = 1:D  # 列番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n < frame, i < j) |> # 重複を除去
  dplyr::group_by(frame, n, i, j) |> # 分布の計算用にグループ化
  dplyr::mutate(
    x_i = x_nd[n, i], # x軸の値
    x_j = x_nd[n, j]  # y軸の値
  ) |> # 確率密度を計算
  dplyr::ungroup() # グループ化を解除

# サンプルラベルを作成
anime_label_df <- tibble::tibble(
  frame = 1:N, # フレーム番号
  i = 2, # 行番号
  j = 2  # 列番号
) |> 
  dplyr::group_by(frame) |> # ラベル作成用にグループ化
  dplyr::mutate(
    x = paste0("x=(", paste0(round(x_nd[frame, ], 2), collapse = ", "), ")")
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 多次元ガウス分布のアニメーションを作図
anime_graph <- ggplot() + 
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x_i, y = x_j, z = density, fill = ..level..), 
                      alpha = 0.8) + # 2次元ガウス分布
  geom_point(data = anime_freq_df, mapping = aes(x = x_i, y = x_j), 
             color = "orange", alpha = 0.5) + 
  geom_point(data = anime_data_df, mapping = aes(x = x_i, y = x_j), 
             color = "orange", size = 3) + 
  geom_label(data = label_df, mapping = aes(x = min(x_vals), y = max(x_vals), label = parameter), 
             parse = TRUE, hjust = "inward", vjust = "inward") + # パラメータラベル
  geom_label(data = anime_label_df, mapping = aes(x = median(x_vals), y = median(x_vals), label = x), 
             color = "orange") + # パラメータラベル
  gganimate::transition_manual(frame) + # 
  facet_grid(i ~ j, labeller = label_bquote(rows = i==.(i), cols = j==.(j))) + # グラフの分割
  labs(title ="Multivariate Gaussian Distribution", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = expression(x[i]), y = expression(x[j]))

# gif画像を作成
gganimate::animate(anime_graph, nframes = N+10, end_pause = 10, fps = 10, width = 900, height = 900)


