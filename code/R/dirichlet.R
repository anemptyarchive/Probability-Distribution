
# ディリクレ分布 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(MCMCpack)
library(patchwork)

# チェック用
library(ggplot2)
library(patchwork)


# 確率密度の計算 -----------------------------------------------------------------

# パラメータを指定
beta_v <- c(4, 2, 3)

# 確率変数の値を指定
phi_v <- c(0.5, 0.3, 0.2)


# 定義式により確率密度を計算
C    <- gamma(sum(beta_v)) / prod(gamma(beta_v))
dens <- C * prod(phi_v^(beta_v - 1))
dens

# 対数をとった定義式により確率密度を計算
log_C    <- lgamma(sum(beta_v)) - sum(lgamma(beta_v))
log_dens <- log_C + sum((beta_v - 1) * log(phi_v))
dens     <- exp(log_dens)
dens; log_dens

# 関数により確率密度を計算
dens <- MCMCpack::ddirichlet(x = phi_v, alpha = beta_v)
dens


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
beta_v <- c(4, 2, 3)

# 次元数を設定
V <- length(beta_v)


# 期待値を計算
E_phi_v <- beta_v / sum(beta_v)
E_phi_v

# 対数の期待値を計算
E_log_phi_v <- digamma(beta_v) - digamma(sum(beta_v))
E_log_phi_v

# 分散を計算
V_phi_v <- beta_v * (sum(beta_v) - beta_v) / sum(beta_v)^2 / (sum(beta_v) + 1)
V_phi_v

# インデックスを指定:(i ≠ j)
i <- 1
j <- 2

# 共分散を計算
Cov_phi_ij <- -beta_v[i] * beta_v[j] / sum(beta_v)^2 / (sum(beta_v) + 1)
Cov_phi_ij

# 共分散を計算
Cov_phi_vv <- -beta_v %*% t(beta_v) / sum(beta_v)^2 / (sum(beta_v) + 1)
diag(Cov_phi_vv) <- as.numeric(NA)
Cov_phi_vv

# 最頻値を計算:(β_v > 1)
mode_phi_v <- (beta_v - 1) / (sum(beta_v) - V)
mode_phi_v


# 三角座標の設定 -----------------------------------------------------------------

### ・座標の作成 -----

# 軸目盛の位置を指定
axis_vals <- seq(from = 0, to = 1, by = 0.1)

# 枠線用の値を作成
ternary_axis_df <- tibble::tibble(
  y_1_start = c(0.5, 0, 1),         # 始点のx軸の値
  y_2_start = c(0.5*sqrt(3), 0, 0), # 始点のy軸の値
  y_1_end = c(0, 1, 0.5),           # 終点のx軸の値
  y_2_end = c(0, 0, 0.5*sqrt(3)),   # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3")     # 元の軸
)

# グリッド線用の値を作成
ternary_grid_df <- tibble::tibble(
  y_1_start = c(
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
  ), # 始点のx軸の値
  y_2_start = c(
    sqrt(3) * 0.5 * axis_vals, 
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals)
  ), # 始点のy軸の値
  y_1_end = c(
    axis_vals, 
    0.5 * axis_vals + 0.5, 
    0.5 * rev(axis_vals)
  ), # 終点のx軸の値
  y_2_end = c(
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals), 
    sqrt(3) * 0.5 * rev(axis_vals)
  ), # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3") |> 
    rep(each = length(axis_vals)) # 元の軸
)

# 軸ラベル用の値を作成
ternary_axislabel_df <- tibble::tibble(
  y_1 = c(0.25, 0.5, 0.75),               # x軸の値
  y_2 = c(0.25*sqrt(3), 0, 0.25*sqrt(3)), # y軸の値
  label = c("phi[1]", "phi[2]", "phi[3]"),      # 軸ラベル
  h = c(3, 0.5, -2),  # 水平方向の調整用の値
  v = c(0.5, 3, 0.5), # 垂直方向の調整用の値
  axis = c("x_1", "x_2", "x_3") # 元の軸
)

# 軸目盛ラベル用の値を作成
ternary_ticklabel_df <- tibble::tibble(
  y_1 = c(
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
  ), # x軸の値
  y_2 = c(
    sqrt(3) * 0.5 * axis_vals, 
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals)
  ), # y軸の値
  label = c(
    rev(axis_vals), 
    axis_vals, 
    rev(axis_vals)
  ), # 軸目盛ラベル
  h = c(
    rep(1.5, times = length(axis_vals)), 
    rep(1.5, times = length(axis_vals)), 
    rep(-0.5, times = length(axis_vals))
  ), # 水平方向の調整用の値
  v = c(
    rep(0.5, times = length(axis_vals)), 
    rep(0.5, times = length(axis_vals)), 
    rep(0.5, times = length(axis_vals))
  ), # 垂直方向の調整用の値
  angle = c(
    rep(-60, times = length(axis_vals)), 
    rep(60, times = length(axis_vals)), 
    rep(0, times = length(axis_vals))
  ), # ラベルの表示角度
  axis = c("x_1", "x_2", "x_3") |> 
    rep(each = length(axis_vals)) # 元の軸
)


### ・格子点の作成 -----

# 三角座標の値を作成
y_1_vals <- seq(from = 0, to = 1, length.out = 301)
y_2_vals <- seq(from = 0, to = 0.5*sqrt(3), length.out = 300)

# 格子点を作成
y_mat <- tidyr::expand_grid(
  y_1 = y_1_vals, 
  y_2 = y_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 3次元変数に変換
phi_mat <- tibble::tibble(
  phi_2 = y_mat[, 1] - y_mat[, 2] / sqrt(3), 
  phi_3 = 2 * y_mat[, 2] / sqrt(3)
) |> # 元の座標に変換
  dplyr::mutate(
    phi_2 = dplyr::if_else(phi_2 >= 0 & phi_2 <= 1, true = phi_2, false = as.numeric(NA)), 
    phi_3 = dplyr::if_else(phi_3 >= 0 & phi_3 <= 1 & !is.na(phi_2), true = phi_3, false = as.numeric(NA)), 
    phi_1 = 1 - phi_2 - phi_3, 
    phi_1 = dplyr::if_else(phi_1 >= 0 & phi_1 <= 1, true = phi_1, false = as.numeric(NA))
  ) |> # 範囲外の値をNAに置換
  dplyr::select(phi_1, phi_2, phi_3) |> # 順番を変更
  as.matrix() # マトリクスに変換


# グラフの作成 ------------------------------------------------------------------

### ・パラメータの設定 -----

# パラメータを指定
beta_v <- c(4, 2, 3)


### ・散布図によるヒートマップ -----

# Φがとり得る値を作成
phi_vals <- seq(from = 0, to = 1, length.out = 51)

# Φがとり得る点を作成
phi_scatter_mat <- tidyr::expand_grid(
  phi_1 = phi_vals, 
  phi_2 = phi_vals, 
  phi_3 = phi_vals
) |> # 格子点を作成
  dplyr::mutate(
    sum_phi = rowSums(cbind(phi_1, phi_2, phi_3)), 
    phi_1 = phi_1 / sum_phi, 
    phi_2 = phi_2 / sum_phi, 
    phi_3 = phi_3 / sum_phi
  ) |> # 正規化
  dplyr::distinct(phi_1, phi_2, phi_3) |> # 重複を除去
  as.matrix() # マトリクスに変換

# ディリクレ分布を計算
dens_scatter_df <- tibble::tibble(
  phi_1 = phi_scatter_mat[, 1], # 三次元座標のx軸の値
  phi_2 = phi_scatter_mat[, 2], # 三次元座標のy軸の値
  phi_3 = phi_scatter_mat[, 3], # 三次元座標のz軸の値
  y_1 = phi_scatter_mat[, 2] + 0.5 * phi_scatter_mat[, 3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * phi_scatter_mat[, 3],      # 三角座標のy軸の値
  density = MCMCpack::ddirichlet(x = phi_scatter_mat, alpha = beta_v) # 確率密度
)

# ディリクレ分布の(散布図による)ヒートマップを作成
ggplot() + 
  geom_point(data = dens_scatter_df, 
             mapping = aes(x = y_1, y = y_2, color = density)) + # 確率密度の散布図
  scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(0.5, 0), guide = "none") + # 透過
  scale_color_viridis_c() + # グラデーション
  #scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # グラデーション
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", alpha = 0.5, linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = paste0("beta=(", paste0(beta_v, collapse = ", "), ")"), # (文字列表示用)
       #subtitle = parse(text = paste0("beta==(list(", paste0(beta_v, collapse = ", "), "))")), # (数式表示用)
       fill = "density", 
       x = "", y = "")


### ・等高線図 -----

# ディリクレ分布の確率密度を計算
dens_contour_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = MCMCpack::ddirichlet(x = phi_mat, alpha = beta_v), # 確率密度
) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外の値をNAに置換


# ディリクレ分布の等高線図を作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = dens_contour_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 確率密度の等高線
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = paste0("beta=(", paste0(beta_v, collapse = ", "), ")"), # (文字列表示用)
       #subtitle = parse(text = paste0("beta==(list(", paste0(beta_v, collapse = ", "), "))")), # (数式表示用)
       fill = "density", 
       x = "", y = "")


### ・ヒートマップと等高線図 -----

# 範囲外の要素を除去
dens_heatmap_df <- dens_contour_df |> 
  dplyr::filter(fill_flg)

# ディリクレ分布のヒートマップを作成
ggplot() + 
  geom_tile(data = dens_heatmap_df, 
            mapping = aes(x = y_1, y = y_2, fill = density)) + # 確率密度のヒートマップ
  #geom_tile(data = dens_contour_df, 
  #          mapping = aes(x = y_1, y = y_2, fill = density, alpha = fill_flg)) + # 確率密度のヒートマップ
  #scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(0.8, 0), guide = "none") + # 透過
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  scale_fill_viridis_c() + # グラデーション
  #scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # グラデーション
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = paste0("beta=(", paste0(beta_v, collapse = ", "), ")"), # (文字列表示用)
       #subtitle = parse(text = paste0("beta==(list(", paste0(beta_v, collapse = ", "), "))")), # (数式表示用)
       fill = "density", 
       x = "", y = "")


### ・統計量の可視化 -----

# 補助線用の統計量を計算
E_phi_v    <- beta_v / sum(beta_v)
mode_phi_v <- (beta_v - 1) / (sum(beta_v) - length(beta_v))

# 期待値のグリッド線用の値を作成
stat_grid_df <- tibble::tibble(
  y_1_start = c(
    0.5 * (1 - c(E_phi_v[1], mode_phi_v[1])), 
    c(E_phi_v[2], mode_phi_v[2]), 
    0.5 * (1 - c(E_phi_v[3], mode_phi_v[3])) + 0.5
  ), # 始点のx軸の値
  y_2_start = c(
    sqrt(3) * 0.5 * (1 - c(E_phi_v[1], mode_phi_v[1])), 
    c(0, 0), 
    sqrt(3) * 0.5 * c(E_phi_v[3], mode_phi_v[3])
  ), # 始点のy軸の値
  y_1_end = c(
    1 - c(E_phi_v[1], mode_phi_v[1]), 
    0.5 * c(E_phi_v[2], mode_phi_v[2]) + 0.5, 
    0.5 * c(E_phi_v[3], mode_phi_v[3])
  ), # 終点のx軸の値
  y_2_end = c(
    c(0, 0), 
    sqrt(3) * 0.5 * (1 - c(E_phi_v[2], mode_phi_v[2])), 
    sqrt(3) * 0.5 * c(E_phi_v[3], mode_phi_v[3])
  ), # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3") |> 
    rep(each = 2), # 軸ラベル
  statistic = c("mean", "mode") |> 
    rep(times = 3) # 色分け用ラベル
)


# 統計量を重ねたディリクレ分布の等高線図を作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = dens_contour_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 確率密度の等高線
  geom_segment(data = stat_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end, color = statistic), 
               size = 1, linetype ="dashed") + # 統計量のグリッド線
  scale_color_manual(breaks = c("mean", "mode"), 
                     values = c("blue", "chocolate"), 
                     labels = c(expression(E(phi[v])), expression(mode(phi[v]))), 
                     name = "statistic") + # 線の色
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = parse(text = paste0("beta==(list(", paste0(beta_v, collapse = ", "), "))")), 
       fill = "density", 
       x = "", y = "")


# パラメータの比較：並べて比較 --------------------------------------------------

# パラメータとして利用する値を指定
beta_1_vals <- c(1, 0.9, 3, 10, 4, 3)
beta_2_vals <- c(1, 0.9, 3, 10, 2, 0.9)
beta_3_vals <- c(1, 0.9, 3, 10, 3, 2)


# パラメータごとにディリクレ分布を計算
res_dens_df <- tidyr::expand_grid(
  param_i = seq(beta_1_vals), # パラメータ
  phi_i = 1:nrow(y_mat) # 点番号
) |> # パラメータごとに格子点を複製
  dplyr::group_by(param_i) |> # 分布の計算用にグループ化
  dplyr::mutate(
    y_1 = y_mat[phi_i, 1], 
    y_2 = y_mat[phi_i, 2], 
    beta_1 = beta_1_vals[unique(param_i)], 
    beta_2 = beta_2_vals[unique(param_i)], 
    beta_3 = beta_3_vals[unique(param_i)], 
    density = MCMCpack::ddirichlet(
      x = phi_mat, alpha = c(unique(beta_1), unique(beta_2), unique(beta_3))
    ), # 確率密度
    beta = paste0("(", unique(beta_1), ", ", unique(beta_2), ", ", unique(beta_3), ")") |> 
      factor(levels = paste0("(", beta_1_vals, ", ", beta_2_vals, ", ", beta_3_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外を非表示化

# パラメータごとにディリクレ分布の等高線図を作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = res_dens_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 確率密度の等高線
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  facet_wrap(. ~ beta, nrow = 2, labeller = label_bquote(beta==.(as.character(beta)))) + # グラフを分割
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       fill = "density", 
       x = "", y = "")


# パラメータの比較：アニメーションによる可視化 --------------------------------------------------

### ・第1成分の影響 -----

# パラメータとして利用する値を指定
beta_1_vals <- seq(from = 1, to = 10, by = 0.2) |> 
  round(digits = 1)

# 固定するパラメータを指定
beta_2 <- 2
beta_3 <- 3

# フレーム数の設定
frame_num <- length(beta_1_vals)


# パラメータごとにディリクレ分布を計算
anime_dens_df <- tidyr::expand_grid(
  beta_1 = beta_1_vals, # パラメータ
  i = 1:nrow(y_mat) # 点番号
) |> # パラメータごとに格子点を複製
  dplyr::group_by(beta_1) |> # 分布の計算用にグループ化
  dplyr::mutate(
    y_1 = y_mat[i, 1], 
    y_2 = y_mat[i, 2], 
    density = MCMCpack::ddirichlet(
      x = phi_mat, alpha = c(unique(beta_1), beta_2, beta_3)
    ), # 確率密度
    parameter = paste0("beta=(", unique(beta_1), ", ", beta_2, ", ", beta_3, ")") |> 
      factor(levels = paste0("beta=(", beta_1_vals, ", ", beta_2, ", ", beta_3, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外を非表示化


### ・第2成分の影響 -----

# パラメータとして利用する値を指定
beta_2_vals <- seq(from = 1, to = 10, by = 0.2) |> 
  round(digits = 1)

# 固定するパラメータを指定
beta_1 <- 2
beta_3 <- 3

# フレーム数の設定
frame_num <- length(beta_2_vals)


# パラメータごとにディリクレ分布を計算
anime_dens_df <- tidyr::expand_grid(
  beta_2 = beta_2_vals, # パラメータ
  i = 1:nrow(y_mat) # 点番号
) |> # パラメータごとに格子点を複製
  dplyr::group_by(beta_2) |> # 分布の計算用にグループ化
  dplyr::mutate(
    y_1 = y_mat[i, 1], 
    y_2 = y_mat[i, 2], 
    density = MCMCpack::ddirichlet(
      x = phi_mat, alpha = c(beta_1, unique(beta_2), beta_3)
    ), # 確率密度
    parameter = paste0("beta=(", beta_1, ", ", unique(beta_2), ", ", beta_3, ")") |> 
      factor(levels = paste0("beta=(", beta_1, ", ", beta_2_vals, ", ", beta_3, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外を非表示化


### ・第3成分の影響 -----

# パラメータとして利用する値を指定
beta_3_vals <- seq(from = 1, to = 10, by = 0.2) |> 
  round(digits = 1)

# 固定するパラメータを指定
beta_1 <- 2
beta_2 <- 3

# フレーム数の設定
frame_num <- length(beta_3_vals)


# パラメータごとにディリクレ分布を計算
anime_dens_df <- tidyr::expand_grid(
  beta_3 = beta_3_vals, # パラメータ
  i = 1:nrow(y_mat) # 点番号
) |> # パラメータごとに格子点を複製
  dplyr::group_by(beta_3) |> # 分布の計算用にグループ化
  dplyr::mutate(
    y_1 = y_mat[i, 1], 
    y_2 = y_mat[i, 2], 
    density = MCMCpack::ddirichlet(
      x = phi_mat, alpha = c(beta_1, beta_2, unique(beta_3))
    ), # 確率密度
    parameter = paste0("beta=(", beta_1, ", ", beta_2, ", ", unique(beta_3), ")") |> 
      factor(levels = paste0("beta=(", beta_1, ", ", beta_2, ", ", beta_3_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外を非表示化


### ・3つの成分の影響 -----

# フレーム番号を指定
frame_num <- 51

# パラメータとして利用する値を指定
beta_1_vals <- seq(from = 1, to = 16, length.out = frame_num) |> 
  round(digits = 2)
beta_2_vals <- seq(from = 2, to = 10, length.out = frame_num) |> 
  round(digits = 2)
beta_3_vals <- seq(from = 3, to = 4, length.out = frame_num) |> 
  round(digits = 2)

# パラメータごとにディリクレ分布を計算
anime_dens_df <- tidyr::expand_grid(
  param_i = 1:frame_num, # パラメータ
  point_i = 1:nrow(y_mat) # 点番号
) |> # パラメータごとに格子点を複製
  dplyr::group_by(param_i) |> # 分布の計算用にグループ化
  dplyr::mutate(
    y_1 = y_mat[point_i, 1], 
    y_2 = y_mat[point_i, 2], 
    beta_1 = beta_1_vals[unique(param_i)], 
    beta_2 = beta_2_vals[unique(param_i)], 
    beta_3 = beta_3_vals[unique(param_i)], 
    density = MCMCpack::ddirichlet(
      x = phi_mat, alpha = c(unique(beta_1), unique(beta_2), unique(beta_3))
    ), # 確率密度
    parameter = paste0("beta=(", unique(beta_1), ", ", unique(beta_2), ", ", unique(beta_3), ")") |> 
      factor(levels = paste0("beta=(", beta_1_vals, ", ", beta_2_vals, ", ", beta_3_vals, ")")) # フレーム切替用ラベル
  ) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外を非表示化


### ・作図 -----

# ディリクレ分布のアニメーションを作成:ヒートマップ
anime_dens_graph <- ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_tile(data = anime_dens_df, 
            mapping = aes(x = y_1, y = y_2, fill = density, alpha = fill_flg)) + # 確率密度のヒートマップ
  gganimate::transition_manual(parameter) + # フレーム
  scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(0.8, 0), guide = "none") + # 透過
  scale_fill_viridis_c() + # グラデーション
  #scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # グラデーション
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = "", y = "")

# ディリクレ分布のアニメーションを作成:等高線図
anime_dens_graph <- ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = anime_dens_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 確率密度の等高線
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_dens_graph, nframes = frame_num, fps = 10, width = 600, height = 600)


# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# パラメータを指定
beta_v <- c(4, 2, 3)

# データ数(サンプルサイズ)を指定
N <- 10000


# ディリクレ分布に従う乱数を生成
phi_nv <- MCMCpack::rdirichlet(n = N, alpha = beta_v)


### ・乱数の可視化 -----

# サンプルを三角座標に変換して格納
data_df <- tibble::tibble(
  y_1 = phi_nv[, 2] + 0.5 * phi_nv[, 3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * phi_nv[, 3] # 三角座標のy軸の値
)

# ディリクレ分布の確率密度を計算
dens_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = MCMCpack::ddirichlet(x = phi_mat, alpha = beta_v), # 確率密度
) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外の値をNAに置換


# パラメータラベル用の文字列を作成
param_text <- paste0(
  "list(", 
  "beta==(list(", paste0(beta_v, collapse = ", "), "))", 
  ", N==", N, 
  ")"
)

# サンプルの散布図を作成
ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_point(data = data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", alpha = 0.3) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = parse(text = param_text), 
       color = "density", 
       x = "", y = "")

# サンプルの度数のヒートマップを作成
ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_bin_2d(data = data_df, 
              mapping = aes(x = y_1, y = y_2, fill = ..count..), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = parse(text = param_text), 
       color = "density", fill = "frequency", 
       x = "", y = "")

# サンプルの密度の等高線図を作成
ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_density_2d_filled(data = data_df, 
                         mapping = aes(x = y_1, y = y_2, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = parse(text = param_text), 
       color = "density", fill = "density", 
       x = "", y = "")



# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

### ・1データずつ可視化 -----

# パラメータを指定
beta_v <- c(4, 2, 3)

# データ数(フレーム数)を指定
N <- 100


# ディリクレ分布に従う乱数を生成
phi_nv <- MCMCpack::rdirichlet(n = N, alpha = beta_v)

# サンプルを三角座標に変換して格納
anime_data_df <- tibble::tibble(
  n = 1:N, # データ番号
  y_1 = phi_nv[, 2] + 0.5 * phi_nv[, 3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * phi_nv[, 3], # 三角座標のy軸の値
  parameter = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", n) |> 
    factor(levels = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", 1:N)) # フレーム切替用ラベル
)

# サンプルを複製して三角座標に変換して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame) |> # 各試行までのサンプルを抽出
  dplyr::mutate(
    y_1 = phi_nv[n, 2] + 0.5 * phi_nv[n, 3], # 三角座標のx軸の値
    y_2 = sqrt(3) * 0.5 * phi_nv[n, 3], # 三角座標のy軸の値
    parameter = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", frame) |> 
      factor(levels = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", 1:N)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  geom_point(data = anime_freq_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", alpha = 0.5) + # n個のサンプル
  geom_point(data = anime_data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", size = 3) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_bin_2d(data = anime_freq_df, 
              mapping = aes(x = y_1, y = y_2, fill = ..count..), 
              alpha = 0.8) + # n個のサンプル
  geom_point(data = anime_data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", size = 3) + # n番目のサンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "frequency", 
       x = "", y = "")

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
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_density_2d_filled(data = tmp_freq_df, 
                         mapping = aes(x = y_1, y = y_2, fill = ..level..), 
                         alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  geom_point(data = tmp_data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", size = 3) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_viridis_c(option = "D") + # 等高線の色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "density", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N+10, end_pause = 10, fps = 10, width = 800, height = 600)


### ・複数データずつ可視化 -----

# パラメータを指定
beta_v <- c(4, 2, 3)

# データ数を指定
N <- 10000

# フレーム数を指定
frame_num <- 100

# 1フレーム当たりのデータ数を計算
n_per_frame <- N %/% frame_num


# ディリクレ分布に従う乱数を生成
phi_nv <- MCMCpack::rdirichlet(n = N, alpha = beta_v)

# サンプルを複製して三角座標に変換して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:frame_num, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame*n_per_frame) |> # 各フレームで利用するサンプルを抽出
  dplyr::mutate(
    y_1 = phi_nv[n, 2] + 0.5 * phi_nv[n, 3], # 三角座標のx軸の値
    y_2 = sqrt(3) * 0.5 * phi_nv[n, 3], # 三角座標のy軸の値
    parameter = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", frame*n_per_frame) |> 
      factor(levels = paste0("beta=(", paste0(beta_v, collapse = ", "), "), n=", 1:frame_num*n_per_frame)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_point(data = anime_freq_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", alpha = 0.3) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_bin_2d(data = anime_freq_df, 
              mapping = aes(x = y_1, y = y_2, fill = ..count..), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "frequency", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 密度の等高線図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_density_2d_filled(data = anime_freq_df, 
                         mapping = aes(x = y_1, y = y_2, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, 
               mapping = aes(x = y_1, y = y_2, z = density, color = ..level..)) + # 分布
  gganimate::transition_manual(parameter) + # フレーム
  scale_color_viridis_c(option = "D") + # 等高線の色
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = "{current_frame}", 
       color = "density", fill = "density", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# パラメータを指定
beta_v <- c(4, 2, 3)

# データ数(サンプルサイズ)を指定
N <- 9


# カテゴリ分布・多項分布のパラメータを生成
phi_nv <- MCMCpack::rdirichlet(n = N, alpha = beta_v)

# パラメータのサンプルを格納
param_df <- tibble::tibble(
  phi_1 = phi_nv[, 1], # 元の座標のx軸の値
  phi_2 = phi_nv[, 2], # 元の座標のy軸の値
  phi_3 = phi_nv[, 3], # 元の座標のz軸の値
  y_1 = phi_nv[, 2] + 0.5 * phi_nv[, 3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * phi_nv[, 3],     # 三角座標のy軸の値
  phi = paste0("(", apply(round(phi_nv, 2), 1, paste0, collapse = ", "), ")") # 色分け用ラベル
) |> 
  dplyr::arrange(-round(phi_3, 1), round(phi_2, 1), -round(phi_1, 1)) |> # グラフの配置調整用に並べ替え
  dplyr::mutate(
    phi = factor(phi, levels = phi) # 色分け用ラベル
  )

# ディリクレ分布を計算
dir_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = MCMCpack::ddirichlet(x = phi_mat, alpha = beta_v), # 確率密度
) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(phi_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外の値をNAに置換

# パラメータの期待値を計算
E_phi_v <- beta_v / sum(beta_v)

# パラメータの期待値を格納
E_param_df <- tibble::tibble(
  y_1 = E_phi_v[2] + 0.5 * E_phi_v[3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * E_phi_v[3]     # 三角座標のy軸の値
)

# 期待値のグリッド線用の値を作成
E_grid_df <- tibble::tibble(
  y_1_start = c(
    0.5 * (1 - E_phi_v[1]), 
    E_phi_v[2], 
    0.5 * (1 - E_phi_v[3]) + 0.5
  ), # 始点のx軸の値
  y_2_start = c(
    sqrt(3) * 0.5 * (1 - E_phi_v[1]), 
    0, 
    sqrt(3) * 0.5 * E_phi_v[3]
  ), # 始点のy軸の値
  y_1_end = c(
    1 - E_phi_v[1], 
    0.5 * E_phi_v[2] + 0.5, 
    0.5 * E_phi_v[3]
  ), # 終点のx軸の値
  y_2_end = c(
    0, 
    sqrt(3) * 0.5 * (1 - E_phi_v[2]), 
    sqrt(3) * 0.5 * E_phi_v[3]
  ), # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3") # 色分け用ラベル
)

# パラメータラベル用の文字列を作成
dir_param_text <- paste0(
  "beta==(list(", paste0(beta_v, collapse = ", "), "))"
)

# サンプルの散布図を作成
dir_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = dir_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # パラメータの生成分布
  geom_segment(data = E_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end, linetype = axis), 
               color = c("red", "green4", "blue"), size = 1) + # パラメータの期待値のグリッド線
  scale_linetype_manual(breaks = c("x_1", "x_2", "x_3"), 
                        values = c("dashed", "dashed", "dashed"), 
                        labels = c(expression(phi[1]), expression(phi[2]), expression(phi[3])), 
                        name = "axis") + 
  guides(linetype = guide_legend(override.aes = list(color = c("red", "green4", "blue"), size = 0.5))) + # 凡例の体裁
  geom_point(data = E_param_df, mapping = aes(x = y_1, y = y_2), 
             color = "hotpink", size = 5, shape = 4, stroke = 2) + # パラメータの期待値
  geom_point(data = param_df, 
             mapping = aes(x = y_1, y = y_2, color = phi), 
             alpha = 0.8, size = 5) + # パラメータのサンプル
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸の体裁
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸の体裁
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Dirichlet Distribution", 
       subtitle = parse(text = dir_param_text), 
       color = expression(phi), fill = "density", 
       x = "", y = "")
dir_graph


### ・分布の作図：カテゴリ分布 -----

# パラメータのサンプルごとにカテゴリ分布を計算
res_cat_df <- tidyr::expand_grid(
  n = 1:N, # サンプル番号
  v = 1:3  # 次元番号
) |> # サンプルごとに次元番号を複製
  dplyr::group_by(n) |> # 確率の計算用に
  dplyr::mutate(
    probability = phi_nv[unique(n), ] |> 
      as.vector(), # 確率
    phi = paste0("(", paste0(round(phi_nv[unique(n), ], 2), collapse = ", "), ")") |> 
      factor(levels = levels(param_df[["phi"]])) # 色分け用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータの期待値によるカテゴリ分布を計算
E_cat_df <- tibble::tibble(
  v = 1:3, # 次元番号
  probability = E_phi_v, # 確率
  axis = c("x_1", "x_2", "x_3") # 色分け用ラベル
)


# パラメータラベル用の文字列を作成
cat_param_text <- paste0(
  "E(phi)==(list(", paste0(round(E_phi_v, 2), collapse = ", "), "))"
)

# サンプルによるカテゴリ分布を作図
cat_graph <- ggplot() + 
  geom_bar(data = E_cat_df, mapping = aes(x = v, y = probability, color = axis), 
           stat = "identity", alpha = 0, size = 1, linetype ="dashed", show.legend = FALSE) + # 期待値による分布
  geom_bar(data = res_cat_df, mapping = aes(x = v, y = probability, fill = phi), 
           stat = "identity", alpha = 0.8, show.legend = FALSE) + # サンプルによる分布
  facet_wrap(. ~ phi, labeller = label_bquote(phi==.(as.character(phi)))) + # グラフの分割
  scale_color_manual(breaks = c("x_1", "x_2", "x_3"), values = c("red", "green4", "blue")) + # 各次元の期待値の色
  labs(title = "Categorical Distribution", 
       subtitle = parse(text = cat_param_text), 
       fill = expression(phi), 
       x = "v", y = "probability")
cat_graph

# グラフを並べて描画
dir_graph + cat_graph + 
  patchwork::plot_layout(guides = "collect")


### ・分布の作図：多項分布 -----

# 試行回数を指定
M <- 10


# xがとり得る値を作成
x_vals <- 0:M |> 
  as.numeric()

# 作図用のxの点を作成
x_mat <- tidyr::expand_grid(
  x_1 = x_vals, # x軸の値
  x_2 = x_vals, # y軸の値
  x_3 = x_vals  # z軸の値
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# パラメータの期待値による多項分布を計算
E_mult_df <- tibble::tibble(
  i = 1:nrow(x_mat), # 点番号
  x_1 = x_mat[, 1], # x軸の値
  x_2 = x_mat[, 2], # y軸の値
  x_3 = x_mat[, 3]  # z軸の値
) |> 
  dplyr::filter(x_1+x_2+x_3 == M) |> # 範囲外の点を除去
  dplyr::group_by(i) |> # 確率の計算用にグループ化
  dplyr::mutate(
    probability = dmultinom(x = x_mat[i, ], size = M, prob = E_phi_v) # 確率
  ) |> 
  dplyr::ungroup() # グループ化を解除

# パラメータのサンプルごとに多項分布を計算
res_mult_df <- tidyr::expand_grid(
  n = 1:N, # パラメータ番号
  i = 1:nrow(x_mat), # 点番号
) |> # サンプルごとに格子点を複製
  dplyr::mutate(
    x_1 = x_mat[i, 1], # x軸の値
    x_2 = x_mat[i, 2], # y軸の値
    x_3 = x_mat[i, 3]  # z軸の値
  ) |> 
  dplyr::filter(x_1+x_2+x_3 == M) |> # 範囲外の非表示化用のフラグ
  dplyr::group_by(n, i) |> # 確率の計算用にグループ化
  dplyr::mutate(
    probability = dmultinom(x = x_mat[i, ], size = M, prob = phi_nv[n, ]), # 確率
    phi = paste0("(", paste0(round(phi_nv[n, ], 2), collapse = ", "), ")") |> 
      factor(levels = levels(param_df[["phi"]])) # 色分け用ラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除


# パラメータラベル用の文字列を作成
mult_param_text <- paste0(
  "list(", 
  "E(phi)==(list(", paste0(round(E_phi_v, 2), collapse = ", "), "))", 
  ", M==", M, 
  ")"
)

# 期待値による多項分布のグラフを作成
E_mult_graph <- ggplot() + # データ
  geom_tile(data = E_mult_df, mapping = aes(x = x_2, y = x_3, fill = probability, alpha = fill_flg), 
            alpha = 0.8) + # 期待値による分布
  geom_point(mapping = aes(x = M*E_phi_v[2], y = M*E_phi_v[3]), 
             color = "hotpink", size = 5, shape = 4, stroke = 2) + # パラメータの期待値
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  coord_fixed(ratio = 1) + # アスペクト比
  scale_x_continuous(breaks = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M) + # y軸目盛
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Multinomial Distribution", 
       subtitle = parse(text = mult_param_text), 
       fill = "probability", 
       x = expression(x[2]), y = expression(x[3]))
E_mult_graph

# サンプルによる多項分布のグラフを作成
res_mult_graph <- ggplot() + # データ
  geom_tile(data = res_mult_df, mapping = aes(x = x_2, y = x_3, fill = probability), 
            alpha = 0.8) + # サンプルによる分布
  geom_point(data = param_df, mapping = aes(x = M*phi_2, y = M*phi_3, color = phi), 
             alpha = 0.8, size = 5, shape = 4, stroke = 2, show.legend = FALSE) + # サンプルによる分布の期待値
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  facet_wrap(. ~ phi, nrow = 2, labeller = label_bquote(phi==.(as.character(phi)))) + # グラフを分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Multinomial Distribution", 
       subtitle = parse(text = paste0("M==", M)), 
       color = expression(phi), fill = "probability", 
       x = expression(x[2]), y = expression(x[3]))
res_mult_graph

# グラフを並べて描画
(dir_graph + E_mult_graph) / res_mult_graph


