
# ウィシャート分布 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(mvnfast)

# チェック用
library(ggplot2)


# 確率密度の計算 -----------------------------------------------------------------

# 次元数を指定
D <- 3

# 自由度を指定
nu <- D

# 逆スケール行列を指定
w_dd <- diag(D) # 単位行列
w_dd <- diag(c(10, 6)) # 対角行列
w_dd <- c(
  1.2, 0.6, -1.5, 
  0.6, 2.4, 0.4, 
  -1.5, 0.4, 3.6
) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換

# 確率変数の値を指定
lambda_dd <- diag(D) * 0.9
lambda_dd <- c(
  2, 0, -1, 
  0, 2.4, 0.4, 
  -1, 0.4, 4
) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換


# 定義式により確率密度を計算
C_w1 <- 1 / sqrt(det(w_dd))^nu / sqrt(2)^(nu*D) / sqrt(pi)^(0.5*D*(D-1))
C_w2 <- 1 / prod(gamma(0.5 * (nu + 1 - 1:D)))
dens <- C_w1 * C_w2 * sqrt(det(lambda_dd))^(nu-D-1) * exp(-0.5 * sum(diag(solve(w_dd) %*% lambda_dd)))
dens

# 対数をとった定義式により確率密度を計算
log_C_w1 <- -0.5 * (nu*log(det(w_dd)) + nu*D*log(2) + 0.5*D*(D-1)*log(pi))
log_C_w2 <- -sum(lgamma(0.5 * (nu + 1 - 1:D)))
log_dens <- log_C_w1 + log_C_w2 + 0.5 * ((nu - D - 1) * log(det(lambda_dd)) - sum(diag(solve(w_dd) %*% lambda_dd)))
dens <- exp(log_dens)
dens; log_dens

# ウィシャート分布の関数により確率密度を計算
dens <- MCMCpack::dwish(W = lambda_dd, v = nu, S = w_dd)
dens


# 統計量の計算 ------------------------------------------------------------------

# 次元数を指定
D <- 3

# 自由度を指定
nu <- D + 2

# 逆スケール行列を指定
w_dd <- c(
  1.2, 0.6, -1.5, 
  0.6, 2.4, 0.4, 
  -1.5, 0.4, 3.6
) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換


# 期待値を計算
E_lambda_dd <- nu * w_dd
E_lambda_dd


# 成分を指定
i <- 2
j <- 1

# 指定した成分の分散を計算
V_lambda_ij <- nu * (w_dd[i, j]^2 + w_dd[i, i] * w_dd[j, j])
V_lambda_ij


# 対角成分を行方向に複製した行列を作成
w_ii <- w_dd |> 
  diag() |> # 対角成分を抽出
  rep(each = D) |> # 対角成分を複製
  matrix(nrow = D, ncol = D, byrow = TRUE) # マトリクスに変換

# 全ての成分の分散を計算
V_lambda_dd <- nu * (w_dd^2 + w_ii * t(w_ii))
V_lambda_dd


# 最頻値を計算:(ν > D+1)
mode_lambda_dd <- (nu - D - 1) * w_dd
mode_lambda_dd


# 乱数の生成 -----------------------------------------------------------------

### ・サンプリング -----

# 次元数を指定
D <- 4

# 自由度を指定
nu <- D + 2

# 逆スケール行列を指定
w_dd <- diag(D) # 単位行列
w_dd <- c(
  6, 0, 1, 4, 
  0, 2.4, -1, 0, 
  1, -1, 3.6, -1.7, 
  4, 0, -1.7, 4.8
) |> # 値を指定
  matrix(nrow = D, ncol = D) # マトリクスに変換

# 逆スケール行列をランダムに設定
rWishart(n = 1, df = nu, Sigma = diag(D))

# 逆スケール行列をランダムに設定
w_dd <- rWishart(n = 1, df = D, Sigma = diag(D))[, , 1]
round(w_dd, 2)

# データ数(サンプルサイズ)を指定
N <- 5000


# ウィシャート分布に従う乱数を生成
lambda_ddn <- rWishart(n = N, df = nu, Sigma = w_dd)

# サンプルを格納
data_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # サンプルごとに成分番号を複製
  #dplyr::filter(i <= j) |> # 重複を削除
  dplyr::group_by(n, i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    lambda = lambda_ddn[i, j, n]
  ) |> # 次元ごとに値を抽出
  dplyr::ungroup() # グループ化を解除


### ・乱数の可視化 -----

# 統計量を計算
stat_df <- tidyr::expand_grid(
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # 成分番号を作成
  #dplyr::filter(i <= j) |> # 重複を削除
  dplyr::group_by(i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    mean = nu * w_dd[i, j], # 期待値
    mode = dplyr::if_else(
      condition = nu > D + 1, 
      true = (nu - D - 1) * w_dd[i, j], 
      false = as.numeric(NA)
    ) # 最頻値
  ) |> # 次元ごとに統計量を計算
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_longer(
    cols = c(mean, mode), 
    names_to = "type", 
    values_to = "statistic"
  )

# 逆スケール行列ラベルを作成
label_df <- tidyr::expand_grid(
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # 成分番号を作成
  #dplyr::filter(i <= j) |> # 重複を削除
  dplyr::group_by(i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    label = paste0("w[", i, j, "]==", round(w_dd[i, j], 2))
  ) |> # 次元ごとにラベルを作成
  dplyr::ungroup() # グループ化を解除


# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", mode = "chocolate")
label_vec <- c(mean = expression(E(lambda[ij])), mode = expression(mode(lambda[ij])))

# サンプルのヒストグラムを作成
ggplot() + 
  geom_histogram(data = data_df, mapping = aes(x = lambda, y = ..count..), 
                 bins = 50, fill = "#00A968") + # サンプル
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type), 
             linetype = "dashed") + # 統計量
  geom_label(data = label_df, mapping = aes(x = max(lambda_ddn), y = N*0.01, label = label), 
             parse = TRUE, hjust = "inward", vjust = "inward", alpha = 0.5) + # パラメータラベル
  facet_grid(i ~ j, labeller = label_bquote(rows = i==.(i), cols = j==.(j))) + # グラフの分割
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Wishart Distribution", 
       subtitle = parse(text = paste0("list(D==", D, ", nu==", nu, ", W==(list(w[11], ..., w[DD])), N==", N, ")")), 
       x = expression(lambda[ij]), y = "frequency")


# 乱数と分布の関係：アニメーションによる可視化 -----------------------------------------------------------

# 次元数を指定
D <- 4

# 自由度を指定
nu <- D + 2

# 逆スケール行列を指定
w_dd <- diag(D)
w_dd <- rWishart(n = 1, df = D, Sigma = diag(D))[, , 1]
round(w_dd, 2)  

# データ数(フレーム数)を指定
N <- 250


# ウィシャート分布に従う乱数を生成
lambda_ddn <- rWishart(n = N, df = nu, Sigma = w_dd)

# サンプルを格納
anime_data_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # サンプルごとに成分番号を複製
  dplyr::group_by(n, i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    frame = n, # フレーム番号
    lambda = lambda_ddn[i, j, n], # サンプルの値
    parameter = paste0("D=", D, ", nu=", nu, ", W=(w_11, ..., w_DD)", ", n=", n) |> 
      factor(levels = paste0("D=", D, ", nu=", nu, ", W=(w_11, ..., w_DD)", ", n=", 1:N)) # フレーム切替用ラベル
  ) |> # 次元ごとに値を抽出
  dplyr::ungroup() # グループ化を解除

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N, # データ番号
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # フレームとサンプルごとに成分番号を複製
  dplyr::filter(n <= frame) |> # 各試行までのサンプルを抽出
  dplyr::group_by(frame, n, i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    lambda = lambda_ddn[i, j, n], # サンプルの値
    parameter = paste0("D=", D, ", nu=", nu, ", W=(w_11, ..., w_DD)", ", n=", frame) |> 
      factor(levels = paste0("D=", D, ", nu=", nu, ", W=(w_11, ..., w_DD)", ", n=", 1:N)) # フレーム切替用ラベル
  ) |> # 次元ごとに値を抽出
  dplyr::ungroup() # グループ化を解除

# 統計量を計算
stat_df <- tidyr::expand_grid(
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # 成分番号を作成
  #dplyr::filter(i <= j) |> # 重複を削除
  dplyr::group_by(i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    mean = nu * w_dd[i, j], # 期待値
    mode = dplyr::if_else(
      condition = nu > D + 1, 
      true = (nu - D - 1) * w_dd[i, j], 
      false = as.numeric(NA)
    ) # 最頻値
  ) |> # 次元ごとに統計量を計算
  dplyr::ungroup() |> # グループ化を解除
  tidyr::pivot_longer(
    cols = c(mean, mode), 
    names_to = "type", 
    values_to = "statistic"
  )

# 逆スケール行列ラベルを作成
label_df <- tidyr::expand_grid(
  i = 1:D, # 行インデックス
  j = 1:D  # 列インデックス
) |> # 成分番号を作成
  dplyr::group_by(i, j) |> # 値の抽出用にグループ化
  dplyr::mutate(
    label = paste0("w[", i, j, "]==", round(w_dd[i, j], 2))
  ) |> # 次元ごとにラベルを作成
  dplyr::ungroup() # グループ化を解除


# 凡例用の設定を作成:(数式表示用)
color_vec <- c(mean = "blue", mode = "chocolate")
label_vec <- c(mean = expression(E(lambda[ij])), mode = expression(mode(lambda[ij])))

# ヒストグラムのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_label(data = label_df, mapping = aes(x = max(lambda_ddn), y = N*0.01, label = label), 
             parse = TRUE, hjust = "inward", vjust = "inward", alpha = 0.5) + # 逆スケール行列ラベル
  geom_histogram(data = anime_freq_df, mapping = aes(x = lambda, y = ..count..), 
                 bins = 30, fill = "#00A968") + # n個のサンプル
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type), 
             linetype = "dashed") + # 統計量
  geom_point(data = anime_data_df, mapping = aes(x = lambda, y = 0), 
             color = "orange", size = 3) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  facet_grid(i ~ j, labeller = label_bquote(rows = i==.(i), cols = j==.(j))) + # グラフの分割
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(数式表示用)
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Wishart Distribution", 
       subtitle = "{current_frame}", 
       x = expression(lambda[ij]), y = "frequency")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N+10, end_pause = 10, fps = 10, width = 1000, height = 1000)


# 確率分布の生成 -----------------------------------------------------------------

### ・生成分布のパラメータの設定 -----

# 次元数を指定
D <- 2

# 自由度を指定
nu <- D + 9

# パラメータを指定
w_dd <- diag(D) # 単位行列
w_dd <- diag(c(10, 11)) # 対角行列
w_dd <- matrix(c(6, 0.5, 0.5, 10), nrow = D, ncol = D)

# 分布の数(サンプルサイズ)を指定
N <- 9


# 多次元ガウス分布の分散共分散行列を生成
lambda_ddn <- rWishart(n = N, df = nu, Sigma = w_dd)


### ・生成された分布の設定：多次元ガウス分布 -----

# (生成された分布の)平均ベクトルを指定
mu_d <- c(0, 0)


# 分散共分散行列の期待値を計算
E_sigma_dd <- solve(nu * w_dd)

# xの値を作成
x_1_vals <- seq(
  from = mu_d[1] - sqrt(E_sigma_dd[1, 1]) * 3, 
  to = mu_d[1] + sqrt(E_sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = mu_d[2] - sqrt(E_sigma_dd[2, 2]) * 3, 
  to = mu_d[2] + sqrt(E_sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = E_sigma_dd) # 確率密度
)


# 分散共分散行列の期待値の固有値・固有ベクトルを計算
res_eigen    <- eigen(E_sigma_dd)
E_lambda_d <- res_eigen[["values"]]
E_u_dd     <- res_eigen[["vectors"]] |> 
  t()

# パラメータの期待値によるガウス分布の長軸を計算
E_axis_df <- tibble::tibble(
  xend = mu_d[1] + E_u_dd[1, 1] * sqrt(E_lambda_d[1]), 
  yend = mu_d[2] + E_u_dd[1, 2] * sqrt(E_lambda_d[1])
)


### ・分布の作図：多次元ガウス分布 -----

# パラメータごとにガウス分布を計算
gaussian_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals 
) |> # パラメータごとに格子点を複製
  dplyr::group_by(n) |> # 分布の計算用にグループ化
  dplyr::mutate(
    density = mvnfast::dmvn(
      X = x_mat, 
      mu = mu_d, 
      sigma = solve(lambda_ddn[, , unique(n)])
    ), # 確率密度
    parameter = paste0("(", paste0(round(lambda_ddn[, , unique(n)], 2), collapse = ", "), ")") |> 
      as.factor() # 色分け用ラべル
  ) |> 
  dplyr::ungroup() # グループ化を解除


# パラメータごとに断面図の軸を計算
axis_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    mu = rep(mu_d, each = 2, times = 2), # 平均値
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    u = lambda_ddn[, , unique(n)] |> 
      solve() |> 
      (\(.){eigen(.)[["vectors"]]})() |> 
      t() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    lambda = lambda_ddn[, , unique(n)] |> 
      solve() |> 
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値の平方根
    value = mu + sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("(", paste0(round(lambda_ddn[, , unique(n)], 2), collapse = ", "), ")") |> 
      as.factor() # 色分け用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(n, axis, name, value, parameter) |> 
  tidyr::pivot_wider(
    id_cols = c(n, axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割

# N個のガウス分布を分割して作図
ggplot() + 
  geom_contour_filled(data = gaussian_df, mapping = aes(x = x_1, y = x_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # サンプルによる分布
  geom_segment(data = axis_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = xend, yend = yend), 
               color = "pink", size = 1, arrow = arrow(length = unit(10, "pt"))) + # サンプルによる分布の軸
  facet_wrap(. ~ parameter, labeller = label_bquote(Lambda==.((as.character(parameter))))) + # グラフの分割
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Maltivariate Gaussian Distribution", 
       subtitle = parse(text = paste0("list(nu==", nu, ", W==(list(", paste0(round(w_dd, 2), collapse = ", "), ")))")), 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))


# 期待値による分布の確率密度の最大値を計算
max_dens <- mvnfast::dmvn(X = mu_d, mu = mu_d, sigma = E_sigma_dd)
max_dens <- 30
# N+1個のガウス分布の楕円を作図
ggplot() + 
  geom_contour(data = E_gaussian_df, mapping = aes(x = x_1, y = x_2, z = density), 
               breaks = max_dens*exp(-0.5), color ="red", linetype = "dashed") + # 期待値による分布
  geom_contour(data = gaussian_df, mapping = aes(x = x_1, y = x_2, z = density, color = parameter), 
               breaks = max_dens*exp(-0.5), alpha = 1) + # サンプルによる分布
  coord_cartesian(xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # 描画範囲
  labs(title = "Maltivariate Gaussian Distribution", 
       subtitle = parse(text = paste0("list(nu==", nu, ", W==(list(", paste0(round(w_dd, 2), collapse = ", "), ")))")), 
       color = expression(Lambda), 
       x = expression(x[1]), y = expression(x[2]))


# パラメータごとに断面図の長軸を計算
axis_df <- tibble::tibble(
  n = 1:N # データ番号
) |> 
  dplyr::group_by(n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    lambda_1 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){.[["values"]][1]})(), # 固有値
    u_11 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){t(.[["vectors"]])[1, 1]})(), # 固有ベクトルの成分
    u_12 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){t(.[["vectors"]])[1, 2]})(), # 固有ベクトルの成分
    xend = mu_d[1] + u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = mu_d[2] + u_12 * sqrt(lambda_1), # 終点のy軸の値
    parameter = paste0("(", paste0(round(lambda_ddn[, , n], 2), collapse = ", "), ")") |> 
      as.factor() # 色分け用ラべル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# N+1個のガウス分布の長軸を作図
ggplot() + 
  geom_contour(data = E_gaussian_df, mapping = aes(x = x_1, y = x_2, z = density), 
               breaks = max_dens*exp(-0.5), color ="red", linetype = "dashed") + # 期待値による分布
  geom_segment(data = E_axis_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = xend, yend = yend), 
               color = "red", size = 1, linetype = "dashed", arrow = arrow(length = unit(10, "pt"))) + # 期待値による分布の長軸
  geom_segment(data = axis_df, mapping = aes(x = mu_d[1], y = mu_d[2], xend = xend, yend = yend, color = parameter), 
               alpha = 1, arrow = arrow(length = unit(10, "pt"))) + # サンプルによる分布の長軸
  coord_fixed(ratio = 1, xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # アスペクト比
  labs(title = "Maltivariate Gaussian Distribution", 
       subtitle = parse(text = paste0("list(nu==", nu, ", W==(list(", paste0(round(w_dd, 2), collapse = ", "), ")))")), 
       color = expression(Lambda), 
       x = expression(x[1]), y = expression(x[2]))


# マハラノビス距離(楕円)による可視化 ------------------------------------------------------------------

### ・パラメータの設定 -----

# 次元数を指定
D <- 2

# 自由度を指定
nu <- D + 8

# 逆スケール行列を指定
w_dd <- matrix(c(8, 0.6, 0.6, 12), nrow = D, ncol = D)

# パラメータの数(サンプルサイズ)を指定
N <- 100


# 分散共分散行列の期待値を計算
E_sigma_dd <- solve(nu * w_dd)

# xの値を作成
x_1_vals <- seq(
  from = - sqrt(E_sigma_dd[1, 1]) * 3, 
  to = sqrt(E_sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = - sqrt(E_sigma_dd[2, 2]) * 3, 
  to = sqrt(E_sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 期待値によるマハラノビス距離(楕円)を計算
E_delta_df <- tibble::tibble(
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  delta = mahalanobis(x = x_mat, center = 0, cov = E_sigma_dd) |> 
    sqrt() # マハラノビス距離
)


# 期待値の固有値・固有ベクトルを計算
res_eigen  <- eigen(E_sigma_dd)
E_lambda_d <- res_eigen[["values"]]
E_u_dd     <- res_eigen[["vectors"]] |> 
  t()

# 期待値による楕円の長軸を計算
E_axis_df <- tibble::tibble(
  xend = E_u_dd[1, 1] * sqrt(E_lambda_d[1]), 
  yend = E_u_dd[1, 2] * sqrt(E_lambda_d[1])
)


### ・生成したマハラノビス距離(楕円)の作図 -----

# 分散共分散行列を生成
lambda_ddn <- rWishart(n = N, df = nu, Sigma = w_dd)

# サンプルごとにマハラノビス距離(楕円)を計算
res_delta_df <- tidyr::expand_grid(
  n = 1:N, # データ番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals 
) |> # パラメータごとに格子点を複製
  dplyr::group_by(n) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(x = x_mat, center = 0, cov = lambda_ddn[, , unique(n)], inverted = TRUE) |> 
      sqrt() # マハラノビス距離
  ) |> 
  dplyr::ungroup() # グループ化を解除


# サンプルごとに楕円の長軸を計算
res_axis_df <- tibble::tibble(
  n = 1:N # データ番号
) |> 
  dplyr::group_by(n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    lambda_1 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){.[["values"]][1]})(), # 固有値
    u_11 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){t(.[["vectors"]])[1, 1]})(), # 固有ベクトルの成分
    u_12 = lambda_ddn[, , n] |> 
      solve() |> 
      eigen() |> 
      (\(.){t(.[["vectors"]])[1, 2]})(), # 固有ベクトルの成分
    xend = u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = u_12 * sqrt(lambda_1)  # 終点のy軸の値
  ) |> 
  dplyr::ungroup() # グループ化を解除


# 等高線を引く距離を指定
delta <- 1

# サンプルごとにマハラノビス距離(楕円)を作図
ggplot() + 
  geom_contour(data = E_delta_df, mapping = aes(x = x_1, y = x_2, z = delta), 
               breaks = delta, color ="red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_contour(data = res_delta_df, mapping = aes(x = x_1, y = x_2, z = delta, color = factor(n)), 
               breaks = delta, alpha = 0.5, show.legend = FALSE) + # サンプルによる分布
  coord_fixed(ratio = 1, xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # アスペクト比
  labs(title = "Mahalabobis Distance", 
       subtitle = parse(text = paste0("list(nu==", nu, ", W==(list(", paste0(round(w_dd, 2), collapse = ", "), ")), N==", N, ")")), 
       x = expression(x[1]), y = expression(x[2]))

# サンプルごとに楕円の長軸を作図
ggplot() + 
  geom_contour(data = E_delta_df, mapping = aes(x = x_1, y = x_2, z = delta), 
               breaks = delta, color ="red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_segment(data = E_axis_df, mapping = aes(x = 0, y = 0, xend = xend, yend = yend), 
               color = "red", size = 1, linetype = "dashed", arrow = arrow(length = unit(10, "pt"))) + # 期待値による分布の長軸
  geom_segment(data = res_axis_df, mapping = aes(x = 0, y = 0, xend = xend, yend = yend), 
               color = "orange", alpha = 0.5, arrow = arrow(length = unit(5, "pt"))) + # サンプルによる分布の長軸
  coord_fixed(ratio = 1, xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # アスペクト比
  labs(title = "Mahalanobis Distance", 
       subtitle = parse(text = paste0("list(nu==", nu, ", W==(list(", paste0(round(w_dd, 2), collapse = ", "), ")), N==", N, ")")), 
       x = expression(x[1]), y = expression(x[2]))


# 期待値によるマハラノビス距離と軸のサンプルのアニメーションで可視化 ------------------------------------------------------------------

### ・自由度の影響 -----

# 次元数を指定
D <- 2

# 自由度として利用する値を指定
nu_vals <- seq(from = D, to = 50, by = 1)

# 逆スケール行列を指定
w_dd <- matrix(c(6, -0.6, -0.6, 10), nrow = D, ncol = D)

# フレーム数を設定
frame_num <- length(nu_vals)


# 作図用の共分散行列を作成
E_sigma_dd <- solve(max(nu_vals) * w_dd)

# xの値を作成
x_1_vals <- seq(
  from = - sqrt(E_sigma_dd[1, 1]) * 3, 
  to = sqrt(E_sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = - sqrt(E_sigma_dd[2, 2]) * 3, 
  to = sqrt(E_sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに共分散行列の期待値によるマハラノビス距離(楕円)を計算
anime_E_delta_df <- tidyr::expand_grid(
  nu = nu_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(nu) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = unique(nu) * w_dd, # 精度行列の期待値を指定
      inverted = TRUE
    ), # 確率密度
    parameter = paste0("nu=", unique(nu), ", W=(", paste0(w_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("nu=", nu_vals, ", W=(", paste0(w_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに共分散行列の期待値による楕円の軸を計算
anime_E_axis_df <- tidyr::expand_grid(
  nu = nu_vals, # パラメータ
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(nu) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    lambda = solve(unique(nu) * w_dd) |> # 共分散行列の期待値を計算
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    u = solve(unique(nu) * w_dd) |> # 共分散行列の期待値を計算
      (\(.){t(eigen(.)[["vectors"]])})() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    value = sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("nu=", nu, ", W=(", paste0(w_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("nu=", nu_vals, ", W=(", paste0(w_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> # 利用する列を選択
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割

# 楕円の数(サンプルサイズ)を指定
N <- 25

# パラメータごとに楕円のサンプルを計算
anime_sample_delta_df <- tidyr::expand_grid(
  nu = nu_vals, # パラメータ
  n = 1:N, # サンプル番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータとサンプルごとに格子点を複製
  dplyr::group_by(nu, n) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = rWishart(n = 1, df = nu, Sigma = w_dd)[, , 1], # 精度行列を生成
      inverted = TRUE
    ), 
    parameter = paste0("nu=", nu, ", W=(", paste0(w_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("nu=", nu_vals, ", W=(", paste0(w_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 軸の数(サンプルサイズ)を指定
N <- 100

# パラメータごとに楕円の長軸のサンプルを計算
anime_sample_axis_df <- tidyr::expand_grid(
  nu = nu_vals, # パラメータ
  n = 1:N # サンプル番号
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(nu, n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    eigen_lt = rWishart(n = 1, df = nu, Sigma = w_dd)[, , 1] |> # 精度行列を生成
      solve() |> # 分散共分散行列に変換
      eigen() |> # 固有値・固有ベクトルを計算
      list(), # リストに格納
    lambda_1 = eigen_lt[[1]][["values"]][1], # 固有値
    u_11 = eigen_lt[[1]][["vectors"]][1, 1], # 固有ベクトルの成分
    u_12 = eigen_lt[[1]][["vectors"]][1, 2], # 固有ベクトルの成分
    xend = u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = u_12 * sqrt(lambda_1), # 終点のy軸の値
    parameter = paste0("nu=", nu, ", W=(", paste0(w_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("nu=", nu_vals, ", W=(", paste0(w_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに精度行列の期待値のラベルを作成
anime_label_df <- tibble::tibble(
  nu = nu_vals # パラメータ
) |> 
  dplyr::group_by(nu) |> # 期待値の計算用にグループ化
  dplyr::mutate(
    label = paste0("E[Lambda]=(", paste0(round(unique(nu) * w_dd, 2), collapse = ", "), ")"), # 精度行列ラベル
    parameter = paste0("nu=", nu, ", W=(", paste0(w_dd, collapse = ", "), ")") |> 
      factor(levels = paste0("nu=", nu_vals, ", W=(", paste0(w_dd, collapse = ", "), ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(1,1成分)の影響 -----

# 次元数を指定
D <- 2

# 自由度を指定
nu <- D + 18

# 逆スケール行列を指定
w_11_vals <- seq(from = 1, to = 10, by = 0.1) |> 
  round(digits = 2)
w_12 <- 0.6
w_22 <- 5

# フレーム数を設定
frame_num <- length(w_11_vals)


# 作図用の共分散行列を作成
E_sigma_dd <- solve(nu * matrix(c(min(w_11_vals), w_12, w_12, w_22), nrow = D, ncol = D))

# xの値を作成
x_1_vals <- seq(
  from = - sqrt(E_sigma_dd[1, 1]) * 1.5, 
  to = sqrt(E_sigma_dd[1, 1]) * 1.5, 
  length.out = 100
)
x_2_vals <- seq(
  from = - sqrt(E_sigma_dd[2, 2]) * 3, 
  to = sqrt(E_sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに共分散行列の期待値によるマハラノビス距離(楕円)を計算
anime_E_delta_df <- tidyr::expand_grid(
  w_11 = w_11_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # サンプルごとに格子点を複製
  dplyr::group_by(w_11) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = nu * matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D), # 精度行列の期待値を指定
      inverted = TRUE
    ), # 確率密度
    parameter = paste0("nu=", nu, ", W=(", unique(w_11), ", ", w_12, ", ", w_12, ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11_vals, ", ", w_12, ", ", w_12, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに共分散行列の期待値による楕円の軸を計算
anime_E_axis_df <- tidyr::expand_grid(
  w_11 = w_11_vals, # パラメータごとに
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(w_11) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    lambda = solve(nu * matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    u = solve(nu * matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){t(eigen(.)[["vectors"]])})() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    value = sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("nu=", nu, ", W=(", unique(w_11), ", ", w_12, ", ", w_12, ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11_vals, ", ", w_12, ", ", w_12, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> # 利用する列を選択
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割

# 楕円の数(サンプルサイズ)を指定
N <- 25

# パラメータごとに楕円のサンプルを計算
anime_sample_delta_df <- tidyr::expand_grid(
  w_11 = w_11_vals, # パラメータ
  n = 1:N, # サンプル番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータとサンプルごとに格子点を複製
  dplyr::group_by(w_11, n) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = rWishart(
        n = 1, df = nu, Sigma = matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D)
      )[, , 1], # 精度行列を生成
      inverted = TRUE
    ), 
    parameter = paste0("nu=", nu, ", W=(", unique(w_11), ", ", w_12, ", ", w_12, ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11_vals, ", ", w_12, ", ", w_12, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 軸の数(サンプルサイズ)を指定
N <- 100

# パラメータごとに楕円の長軸のサンプルを計算
anime_sample_axis_df <- tidyr::expand_grid(
  w_11 = w_11_vals, # パラメータ
  n = 1:N # サンプル番号
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(w_11, n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    eigen_lt = rWishart(
      n = 1, 
      df = nu, 
      Sigma = matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D)
    )[, , 1] |> # 精度行列を生成
      solve() |> # 分散共分散行列に変換
      eigen() |> # 固有値・固有ベクトルを計算
      list(), # リストに格納
    lambda_1 = eigen_lt[[1]][["values"]][1], # 固有値
    u_11 = eigen_lt[[1]][["vectors"]][1, 1], # 固有ベクトルの成分
    u_12 = eigen_lt[[1]][["vectors"]][2, 1], # 固有ベクトルの成分
    xend = u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = u_12 * sqrt(lambda_1), # 終点のy軸の値
    parameter = paste0("nu=", nu, ", W=(", unique(w_11), ", ", w_12, ", ", w_12, ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11_vals, ", ", w_12, ", ", w_12, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに共分散行列の期待値のラベルを作成
anime_label_df <- tibble::tibble(
  w_11 = w_11_vals # パラメータ
) |> 
  dplyr::group_by(w_11) |> # 期待値の計算用にグループ化
  dplyr::mutate(
    label = paste0("E[Lambda]=(", paste0(round(unique(nu) * matrix(c(unique(w_11), w_12, w_12, w_22), nrow = D, ncol = D), 2), collapse = ", "), ")"), # 分散共分散行列ラベル
    parameter = paste0("nu=", nu, ", W=(", unique(w_11), ", ", w_12, ", ", w_12, ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11_vals, ", ", w_12, ", ", w_12, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(1,2成分)の影響 -----

# 次元数を指定
D <- 2

# 自由度を指定
nu <- D + 18

# 逆スケール行列を指定
w_11 <- 6
w_12_vals <- seq(from = -4, to = 4, by = 0.1) |> 
  round(digits = 2)
w_22 <- 5

# フレーム数を設定
frame_num <- length(w_12_vals)


# 作図用の共分散行列を作成
E_sigma_dd <- solve(nu * matrix(c(w_11, median(w_12_vals), median(w_12_vals), w_22), nrow = D, ncol = D))

# xの値を作成
x_1_vals <- seq(
  from = - sqrt(E_sigma_dd[1, 1]) * 3, 
  to = sqrt(E_sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = - sqrt(E_sigma_dd[2, 2]) * 3, 
  to = sqrt(E_sigma_dd[2, 2]) * 3, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに共分散行列の期待値によるマハラノビス距離(楕円)を計算
anime_E_delta_df <- tidyr::expand_grid(
  w_12 = w_12_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(w_12) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = nu * matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D), # 精度行列の期待値を計算
      inverted = TRUE
    ), # 確率密度
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", unique(w_12), ", ", unique(w_12), ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12_vals, ", ", w_12_vals, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに共分散行列の期待値による楕円の軸を計算
anime_E_axis_df <- tidyr::expand_grid(
  w_12 = w_12_vals, # パラメータ
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(w_12) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    lambda = solve(nu * matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    u = solve(nu * matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){t(eigen(.)[["vectors"]])})() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    value = sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", unique(w_12), ", ", unique(w_12), ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12_vals, ", ", w_12_vals, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> # 利用する列を選択
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割


# 楕円の数(サンプルサイズ)を指定
N <- 25

# パラメータごとに楕円のサンプルを計算
anime_sample_delta_df <- tidyr::expand_grid(
  w_12 = w_12_vals, # パラメータ
  n = 1:N, # サンプル番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータとサンプルごとに格子点を複製
  dplyr::group_by(w_12, n) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = rWishart(
        n = 1, df = nu, Sigma = matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D)
      )[, , 1], # 精度行列を生成
      inverted = TRUE
    ), 
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", unique(w_12), ", ", unique(w_12), ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12_vals, ", ", w_12_vals, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 軸の数(サンプルサイズ)を指定
N <- 100

# パラメータごとに楕円の長軸のサンプルを計算
anime_sample_axis_df <- tidyr::expand_grid(
  w_12 = w_12_vals, # パラメータ
  n = 1:N # サンプル番号
) |> 
  dplyr::group_by(w_12, n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    eigen_lt = rWishart(
      n = 1, 
      df = nu, 
      Sigma = matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D)
    )[, , 1] |> # 精度行列を生成
      solve() |> # 分散共分散行列に変換
      eigen() |> # 固有値・固有ベクトルを計算
      list(), # リストに格納
    lambda_1 = eigen_lt[[1]][["values"]][1], # 固有値
    u_11 = eigen_lt[[1]][["vectors"]][1, 1], # 固有ベクトルの成分
    u_12 = eigen_lt[[1]][["vectors"]][2, 1], # 固有ベクトルの成分
    xend = u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = u_12 * sqrt(lambda_1), # 終点のy軸の値
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", unique(w_12), ", ", unique(w_12), ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12_vals, ", ", w_12_vals, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに精度行列の期待値のラベルを作成
anime_label_df <- tibble::tibble(
  w_12 = w_12_vals # パラメータ
) |> 
  dplyr::group_by(w_12) |> # 期待値の計算用にグループ化
  dplyr::mutate(
    label = paste0("E[Lambda]=(", paste0(round(nu * matrix(c(w_11, unique(w_12), unique(w_12), w_22), nrow = D, ncol = D), 2), collapse = ", "), ")"), # 分散共分散行列ラベル
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", unique(w_12), ", ", unique(w_12), ", ", w_22, ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12_vals, ", ", w_12_vals, ", ", w_22, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・スケール行列(2,2成分)の影響 -----

# 次元数を指定
D <- 2

# 自由度を指定
nu <- D + 18

# 逆スケール行列を指定
w_11 <- 5
w_12 <- 0.6
w_22_vals <- seq(from = 1, to = 10, by = 0.1) |> 
  round(digits = 2)

# フレーム数を設定
frame_num <- length(w_22_vals)


# 作図用の共分散行列を作成
E_sigma_dd <- solve(nu * matrix(c(w_11, w_12, w_12, min(w_22_vals)), nrow = D, ncol = D))

# xの値を作成
x_1_vals <- seq(
  from = - sqrt(E_sigma_dd[1, 1]) * 3, 
  to = sqrt(E_sigma_dd[1, 1]) * 3, 
  length.out = 100
)
x_2_vals <- seq(
  from = - sqrt(E_sigma_dd[2, 2]) * 1.5, 
  to = sqrt(E_sigma_dd[2, 2]) * 1.5, 
  length.out = 100
)

# xの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換


# パラメータごとに共分散行列の期待値によるマハラノビス距離(楕円)を計算
anime_E_delta_df <- tidyr::expand_grid(
  w_22 = w_22_vals, # パラメータ
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータごとに格子点を複製
  dplyr::group_by(w_22) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = nu * matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D), # 精度行列の期待値を指定
      inverted = TRUE
    ), # 確率密度
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", unique(w_22), ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", w_22_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに共分散行列の期待値による楕円の軸を計算
anime_E_axis_df <- tidyr::expand_grid(
  w_22 = w_22_vals, # パラメータごとに
  name = paste0(rep(c("x", "y"), each = 2, times = 2), rep(c("start", "end"), each = 4)) # 列名
) |> # パラメータごとに受け皿を複製
  dplyr::group_by(w_22) |> # 軸の計算用にグループ化
  dplyr::mutate(
    axis = rep(c("y_1", "y_2"), times = 4), # pivot_wider用の列
    sign = rep(c(-1, 1), each = 4), # 始点・終点の計算用の符号
    lambda = solve(nu * matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){eigen(.)[["values"]]})() |> 
      (\(.){rep(sqrt(.), times = 4)})(), # 固有値
    u = solve(nu * matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D)) |> # 共分散行列の期待値を計算
      (\(.){t(eigen(.)[["vectors"]])})() |> 
      (\(.){rep(as.vector(.), times = 2)})(), # 固有ベクトル
    value = sign * u * lambda, # 軸の始点・終点を計算
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", unique(w_22), ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", w_22_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(axis, name, value, parameter) |> # 利用する列を選択
  tidyr::pivot_wider(
    id_cols = c(axis, parameter), 
    names_from = name, 
    values_from = value
  ) # 軸の視点・終点の列を分割

# 楕円の数(サンプルサイズ)を指定
N <- 25

# パラメータごとに楕円のサンプルを計算
anime_sample_delta_df <- tidyr::expand_grid(
  w_22 = w_22_vals, # パラメータ
  n = 1:N, # サンプル番号
  x_1 = x_1_vals, 
  x_2 = x_2_vals
) |> # パラメータとサンプルごとに格子点を複製
  dplyr::group_by(w_22, n) |> # 距離の計算用にグループ化
  dplyr::mutate(
    delta = mahalanobis(
      x = x_mat, 
      center = 0, 
      cov = rWishart(
        n = 1, df = nu, Sigma = matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D)
      )[, , 1], # 精度行列を生成
      inverted = TRUE
    ), 
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", unique(w_22), ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", w_22_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 軸の数(サンプルサイズ)を指定
N <- 100

# パラメータごとに楕円の長軸のサンプルを計算
anime_sample_axis_df <- tidyr::expand_grid(
  w_22 = w_22_vals, # パラメータ
  n = 1:N # サンプル番号
) |> 
  dplyr::group_by(w_22, n) |> # 軸の計算用にグループ化
  dplyr::mutate(
    eigen_lt = rWishart(
      n = 1, 
      df = nu, 
      Sigma = matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D)
    )[, , 1] |> # 精度行列を生成
      solve() |> # 分散共分散行列に変換
      eigen() |> # 固有値・固有ベクトルを計算
      list(), # リストに格納
    lambda_1 = eigen_lt[[1]][["values"]][1], # 固有値
    u_11 = eigen_lt[[1]][["vectors"]][1, 1], # 固有ベクトルの成分
    u_12 = eigen_lt[[1]][["vectors"]][2, 1], # 固有ベクトルの成分
    xend = u_11 * sqrt(lambda_1), # 終点のx軸の値
    yend = u_12 * sqrt(lambda_1), # 終点のy軸の値
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", unique(w_22), ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", w_22_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータごとに分散共分散行列の期待値のラベルを作成
anime_label_df <- tibble::tibble(
  w_22 = w_22_vals # パラメータ
) |> 
  dplyr::group_by(w_22) |> # 期待値の計算用にグループ化
  dplyr::mutate(
    label = paste0("E[Lambda]=(", paste0(round(nu * matrix(c(w_11, w_12, w_12, unique(w_22)), nrow = D, ncol = D), 2), collapse = ", "), ")"), # 分散共分散行列ラベル
    parameter = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", unique(w_22), ")") |> 
      factor(levels = paste0("nu=", nu, ", W=(", w_11, ", ", w_12, ", ", w_12, ", ", w_22_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


### ・作図 -----

# 等高線を引く距離を指定
delta <- 1

# マハラノビス距離(楕円)のアニメーションを作図
anime_delta_graph <- ggplot() + 
  geom_contour(data = anime_E_delta_df, mapping = aes(x = x_1, y = x_2, z = delta), 
               breaks = delta, color ="red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_contour(data = anime_sample_delta_df, mapping = aes(x = x_1, y = x_2, z = delta, color = factor(n)), 
               breaks = delta, alpha = 0.5, show.legend = FALSE) + # サンプルによる分布
  geom_label(data = anime_label_df, mapping = aes(x = min(x_1_vals), y = max(x_2_vals), label = label), 
             hjust = 0, vjust = 0, alpha = 0.5) + # 分散共分散行列の期待値ラベル
  gganimate::transition_manual(parameter) + # フレーム
  coord_fixed(ratio = 1, xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # アスペクト比
  labs(title = "Mahalanobis Distance", 
       subtitle = "{current_frame}", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_delta_graph, nframes = frame_num, fps = 10, width = 800, height = 600)


# 楕円の軸のアニメーションを作図
anime_axis_graph <- ggplot() + 
  geom_contour_filled(data = anime_E_delta_df, mapping = aes(x = x_1, y = x_2, z = delta, fill = ..level..), 
                      alpha = 0.8) + # 期待値による分布
  geom_contour(data = anime_E_delta_df, mapping = aes(x = x_1, y = x_2, z = delta), 
               breaks = 1, color = "red", size = 1, linetype = "dotted") + # 期待値による分布の断面
  geom_segment(data = anime_E_axis_df, mapping = aes(x = xstart, y = ystart, xend = xend, yend = yend), 
               color = "red", size = 1, linetype = "dashed", arrow = arrow(length = unit(10, "pt"))) + # 期待値による断面の軸
  geom_segment(data = anime_sample_axis_df, mapping = aes(x = 0, y = 0, xend = xend, yend = yend), 
               color = "orange", alpha = 0.5, size = 1, arrow = arrow(length = unit(5, "pt"))) + # サンプルによる分布の長軸
  geom_label(data = anime_label_df, mapping = aes(x = min(x_1_vals), y = max(x_2_vals), label = label), 
             hjust = 0, vjust = 0, alpha = 0.5) + # 分散共分散行列の期待値ラベル
  gganimate::transition_manual(parameter) + # フレーム
  coord_fixed(ratio = 1, xlim = c(min(x_1_vals), max(x_1_vals)), ylim = c(min(x_2_vals), max(x_2_vals))) + # アスペクト比
  labs(title ="Mahalanobis Distance", 
       subtitle = "{current_frame}", 
       fill = "distance", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(anime_axis_graph, nframes = frame_num, fps = 10, width = 800, height = 600)


