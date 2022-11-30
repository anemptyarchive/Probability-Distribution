
# 多次元スチューデントのt分布 ----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(mvnfast)
library(LaplacesDemon)
library(gganimate)

# チェック用
library(ggplot2)


# 確率密度の計算 -----------------------------------------------------------------

# 次元数を指定
D <- 3

# 自由度を指定
nu <- 5

# 位置ベクトルを指定
mu_d <- c(10, -6, 1.5)

# スケール行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換
det(sigma_dd) # 確認

# スケール行列をランダムに設定
sigma_dd <- rWishart(n = 1, df = D, Sigma = diag(D))[, , 1]

# 逆スケール行列を指定
lambda_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換

# 逆スケール行列を計算
lambda_dd <- solve(sigma_dd)

# 確率変数の値を指定
x_d = c(11.5, -5, 0)


### ・スケール行列を使用 -----

# 定義式により確率密度を計算
C_St1    <- gamma(0.5 * (nu + D)) / gamma(0.5 * nu)
C_St2    <- 1 / sqrt(pi * nu)^D / sqrt(det(sigma_dd))
tmp_term <- (t(x_d - mu_d) %*% solve(sigma_dd) %*% (x_d - mu_d)) |> 
  as.numeric()
term     <- 1 / sqrt(1 + tmp_term / nu)^(nu + D)
dens     <- C_St1 * C_St2 * term
dens


# 対数をとった定義式により確率密度を計算
log_C_St1 <- lgamma(0.5 * (nu + D)) - lgamma(0.5 * nu)
log_C_St2 <- -D * 0.5 * log(pi * nu) - 0.5 * log(det(sigma_dd))
tmp_term  <- (t(x_d - mu_d) %*% solve(sigma_dd) %*% (x_d - mu_d)) |> 
  as.numeric()
log_term  <- -(nu + D) * 0.5 * log(1 + tmp_term / nu)
log_dens  <- log_C_St1 + log_C_St2 + log_term
dens      <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens <- mvnfast::dmvt(X = x_d, mu = mu_d, sigma = sigma_dd, df = nu)
dens

# 関数により確率密度を計算
dens <- LaplacesDemon::dmvt(x = x_d, mu = mu_d, S = sigma_dd, df = nu)
dens


### ・逆スケール行列を使用 -----

# 定義式により確率密度を計算
C_St1    <- gamma(0.5 * (nu + D)) / gamma(0.5 * nu)
C_St2    <- 1 / sqrt(pi * nu)^D / sqrt(det(solve(lambda_dd)))
tmp_term <- (t(x_d - mu_d) %*% lambda_dd %*% (x_d - mu_d)) |> 
  as.numeric()
term     <- 1 / sqrt(1 + tmp_term / nu)^(nu + D)
dens     <- C_St1 * C_St2 * term
dens

# 対数をとった定義式により確率密度を計算
log_C_St1 <- lgamma(0.5 * (nu + D)) - lgamma(0.5 * nu)
log_C_St2 <- -D * 0.5 * log(pi * nu) - 0.5 * log(det(solve(lambda_dd)))
tmp_term  <- (t(x_d - mu_d) %*% lambda_dd %*% (x_d - mu_d)) |> 
  as.numeric()
term      <- -(nu + D) * 0.5 * log(1 + tmp_term / nu)
log_dens  <- log_C_St1 + log_C_St2 + log_term
dens      <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens <- mvnfast::dmvt(X = x_d, mu = mu_d, sigma = solve(lambda_dd), df = nu)
dens




### ・標準化t分布により計算 -----

# 固有値・固有ベクトルを計算
res_eigen <- eigen(sigma_dd)
lambda_d <- res_eigen[["values"]]
u_dd <- res_eigen[["vectors"]] |> 
  t()

# 変数を変換
y_d <- (u_dd %*% (x_d - mu_d) / sqrt(lambda_d)) |> 
  as.vector()
y_d

# Σを用いて標準化t分布により確率密度を計算
dens <- mvnfast::dmvt(X = y_d, mu = rep(0, times = D), sigma = diag(D), df = nu) / sqrt(det(sigma_dd))
dens

# Λを用いて標準化t分布により確率密度を計算
dens <- sqrt(det(lambda_dd)) * mvnfast::dmvt(X = y_d, mu = rep(0, times = D), sigma = diag(D), df = nu)
dens



# 統計量の計算 ------------------------------------------------------------------

# 次元数を指定
D <- 3

# 自由度を指定
nu <- 5

# 位置ベクトルを指定
mu_d <- c(10, -6, 1.5)

# スケール行列を指定
sigma_dd <- c(
  4, 1.8, -0.1, 
  1.8, 9, 2.4, 
  -0.1, 2.4, 1
) |> # 値を指定
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換

# 逆スケール行列を計算
lambda_dd <- solve(sigma_dd)


# 期待値を計算:(ν > 1)
E_x_d <- mu_d

# Σを使って共分散を計算:(ν > 2)
cov_x_dd <- nu / (nu - 2) * sigma_dd
cov_x_dd

# Λを使って共分散を計算:(ν > 2)
cov_x_dd <- nu / (nu - 2) * solve(lambda_dd)
cov_x_dd

# 最頻値を計算
mode_x_d <- mu_d


# グラフの作成 ------------------------------------------------------------------

### ・パラメータの設定 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# 位置ベクトルを指定
mu_d <- c(0, 0)
mu_d <- c(6, 10)

# スケール行列を指定
sigma_dd <- diag(D) # 単位行列
sigma_dd <- c(1, 0.6, 0.6, 4) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換


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

# 多次元t分布を計算
dens_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvt(X = x_mat, mu = mu_d, sigma = sigma_dd, df = nu) # 確率密度
)


### ・作図 -----

# パラメータラベルを作成:(数式表示用)
param_text <- paste0(
  "list(", 
  "mu==group('(', list(", paste0(mu_d, collapse = ", "), "), ')')", 
  ", Sigma==group('(', list(", paste0(sigma_dd, collapse = ", "), "), ')')", 
  ", nu==", nu, 
  ")"
)

# 2次元t分布のグラフを作成:等高線図
ggplot() + 
  #geom_contour(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 等高線
  geom_contour_filled(data = dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), alpha = 0.8) + # 塗りつぶし等高線
  labs(
    title ="Bivariate Student's t-Distribution", 
    subtitle = paste0("mu=(", paste0(mu_d, collapse = ', '), "), Sigma=(", paste0(sigma_dd, collapse = ', '), "), nu=", nu), # (文字列表記用)
    #subtitle = parse(text = param_text), # (数式表示用)
    color = "density", # (等高線用)
    fill = "density", # (塗りつぶし等高線用)
    x = expression(x[1]), y = expression(x[2])
  )

# 2次元t分布のグラフを作成:ヒートマップ
ggplot() + 
  geom_tile(data = dens_df, mapping = aes(x = x_1, y = x_2, fill = density, color = density), alpha = 0.8) + # ヒートマップ
  scale_color_viridis_c(option = "D") + # タイルの色
  scale_fill_viridis_c(option = "D") + # 枠線の色
  #scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # タイルの色
  #scale_color_gradientn(colors = c("blue", "green", "yellow", "red")) + # 枠線の色
  labs(
    title ="Bivariate Student's t-Distribution", 
    #subtitle = paste0("mu=(", paste0(mu_d, collapse = ', '), "), Sigma=(", paste0(sigma_dd, collapse = ', '), "), nu=", nu), # (文字列表記用)
    subtitle = parse(text = param_text), # (数式表示用)
    fill = "density", color = "density", 
    x = expression(x[1]), y = expression(x[2])
  )


# パラメータと分布の関係：アニメーションによる可視化 ----------------------------------------------------------

### ・自由度の影響 -----

# 次元数を設定
D <- 2

# 自由度パラメータとして利用する値を指定
nu_vals <- seq(from = 0.1, to = 15, by = 0.1) |> 
  round(digits = 1)

# フレーム数を設定
frame_num <- length(nu_vals)
frame_num

# 位置ベクトルを指定
mu_d <- c(6, 10)

# スケール行列を指定
sigma_dd <- c(1, 0.6, 0.6, 4) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換


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


# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  nu = nu_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(nu) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = sigma_dd, 
      df = unique(nu)
    ), # 確率密度
    parameter = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu) |> 
      factor(levels = paste0("mu=(", paste0(mu_d, collapse = ", "), "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu_vals)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・位置ベクトル(1軸)の影響 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# x軸の位置パラメータとして利用する値を指定
mu_1_vals <- seq(from = -2, to = 2, by = 0.04) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(mu_1_vals)
frame_num

# y軸の位置パラメータを指定
mu_2 <- 10

# スケール行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = D, ncol = D)


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


# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  mu_1 = mu_1_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(mu_1) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd, 
      df = nu
    ), # 確率密度
    parameter = paste0("mu=(", mu_1, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu) |> 
      factor(levels = paste0("mu=(", mu_1_vals, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・位置ベクトル(2軸)の影響 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# x軸の位置パラメータを指定
mu_1 <- 6

# y軸の位置パラメータとして利用する値を指定
mu_2_vals <- seq(from = -2, to = 2, by = 0.04) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(mu_2_vals)
frame_num

# スケール行列を指定
sigma_dd <- matrix(c(1, 0.6, 0.6, 4), nrow = D, ncol = D)


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


# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  mu_2 = mu_2_vals, 
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(mu_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = unique(cbind(mu_1, mu_2)), 
      sigma = sigma_dd, 
      df = nu
    ), # 確率密度
    parameter = paste0("mu=(", mu_1, ", ", mu_2, "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu) |> 
      factor(levels = paste0("mu=(", mu_1, ", ", mu_2_vals, "), Sigma=(", paste0(sigma_dd, collapse = ", "), "), nu=", nu)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(1,1成分)の影響 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# 位置ベクトルを指定
mu_d <- c(6, 10)

# x軸のスケールパラメータとして利用する値を指定
sigma_11_vals <- seq(from = 0.5, to = 6, by = 0.1) |> 
  round(digits = 1)

# フレーム数を設定
frame_num <- length(sigma_11_vals)
frame_num

# y軸のスケールパラメータを指定
sigma_22 <- 4

# x・y軸のスケールパラメータを指定
sigma_12 <- 0.6


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(max(sigma_11_vals)) * 2, 
  to = mu_d[1] + sqrt(max(sigma_11_vals)) * 2, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_22) * 3, 
  to = mu_d[2] + sqrt(sigma_22) * 3, 
  length.out = 101
)


# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_11 = sigma_11_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_11) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(unique(sigma_11), sigma_12, sigma_12, sigma_22), nrow = D, ncol = D), 
      df = nu
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11, ", ", sigma_12, ", ", sigma_12, ", ", sigma_22, "), nu=", nu) |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11_vals, ", ", sigma_12, ", ", sigma_12, ", ", sigma_22, "), nu=", nu)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(2,2成分)の影響 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# 位置ベクトルを指定
mu_d <- c(6, 10)

# x軸のスケールパラメータを指定
sigma_11 <- 1

# y軸のスケールパラメータとして利用する値を指定
sigma_22_vals <- seq(from = 0.5, to = 6, by = 0.1) |> 
  round(digits = 1)

# フレーム数を設定
frame_num <- length(sigma_22_vals)
frame_num

# x・y軸のスケールパラメータを指定
sigma_12 <- 0.6


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_11) * 3, 
  to = mu_d[1] + sqrt(sigma_11) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(max(sigma_22_vals)) * 2, 
  to = mu_d[2] + sqrt(max(sigma_22_vals)) * 2, 
  length.out = 101
)

# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_22 = sigma_22_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_22) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(sigma_11, sigma_12, sigma_12, unique(sigma_22)), nrow = D, ncol = D), 
      df = nu
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11, ", ", sigma_12, ", ", sigma_12, ", ", sigma_22, "), nu=", nu) |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11, ", ", sigma_12, ", ", sigma_12, ", ", sigma_22_vals, "), nu=", nu)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(2,2成分)の影響 -----

# 次元数を設定
D <- 2

# 自由度を指定
nu <- 3

# 位置ベクトルを指定
mu_d <- c(6, 10)

# x軸・y軸のスケールパラメータを指定
sigma_11 <- 4
sigma_22 <- 10

# x・y軸のスケールパラメータとして利用する値を指定
sigma_12_vals <- seq(from = -5, to = 5, by = 0.1) |> 
  round(digits = 1)

# フレーム数を設定
frame_num <- length(sigma_12_vals)
frame_num


# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(sigma_11) * 3, 
  to = mu_d[1] + sqrt(sigma_11) * 3, 
  length.out = 101
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(sigma_22) * 2, 
  to = mu_d[2] + sqrt(sigma_22) * 2, 
  length.out = 101
)


# パラメータごとに多次元t分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(sigma_12) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvt(
      X = as.matrix(cbind(x_1, x_2)), 
      mu = mu_d, 
      sigma = matrix(c(sigma_11, unique(sigma_12), unique(sigma_12), sigma_22), nrow = D, ncol = D), 
      df = nu
    ), # 確率密度
    parameter = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11, ", ", sigma_12, ", ", sigma_12, ", ", sigma_22, "), nu=", nu) |> 
      factor(levels = paste0("mu=(", mu_d[1], ", ", mu_d[2], "), Sigma=(", sigma_11, ", ", sigma_12_vals, ", ", sigma_12_vals, ", ", sigma_22, "), nu=", nu)) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・作図 -----

# 2次元t分布のアニメーションを作図:等高線図
anime_dens_graph <- ggplot() + 
  #geom_contour(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, color = ..level..)) + # 等高線
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), alpha = 0.8) + # 塗りつぶし等高線
  gganimate::transition_manual(parameter) + # フレーム
  labs(title ="Bivariate Student's t-Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# 2次元t分布のアニメーションを作図:ヒートマップ
anime_dens_graph <- ggplot() + 
  geom_tile(data = anime_dens_df, mapping = aes(x = x_1, y = x_2, fill = density), alpha = 0.8) + # ヒートマップ
  gganimate::transition_manual(parameter) + # フレーム
  scale_fill_viridis_c(option = "D") + # 等高線の色
  labs(title ="Bivariate Student's t-Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = frame_num, fps = 10, width = 800, height = 600)


