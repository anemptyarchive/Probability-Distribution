
# 混合ポアソン分布 --------------------------------------------------------------

# パラメータの影響の可視化


# ライブラリの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(MCMCpack)

# パッケージ名の省略用
library(ggplot2)


# パラメータの影響 ----------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num <- 66


# クラスタ数を指定
K <- 3

# K個のパラメータを指定
lambda_mat <- cbind(
  rep(20, times = frame_num), 
  1:frame_num, 
  rep(40, times = frame_num)
) # フレームごとに値を指定
lambda_mat <- c(20, 30, 40) |> # 値を指定
  rep(each = frame_num) |> # 値を固定して複製
  matrix(nrow = frame_num, ncol = K)

# 混合比率を指定
pi_1 <- 0.35 # 固定する値を指定
pi_mat <- cbind(
  rep(pi_1, times = frame_num), 
  seq(from = 0, to = (1-pi_1), length.out = frame_num), 
  (1-pi_1) - seq(from = 0, to = (1-pi_1), length.out = frame_num)
) # フレームごとに値を指定
pi_mat <- c(0.35, 0.25, 0.4) |> # 値を指定
  rep(each = frame_num) |> # 値を固定して複製
  matrix(nrow = frame_num, ncol = K)
rowSums(pi_mat)


# x軸の最大値を指定
x_max <- frame_num
x_max <- max(lambda_mat) * 2

# x軸の値を作成
x_vec <- seq(from = 0, to = x_max, by = 1)


### 分布の計算 -----

# クラスタごとの重み付け確率を計算
anim_cluster_prob_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  k = 1:K,  # クラスタ番号
  x = x_vec # 確率変数の値
) |> # フレームごとにクラスタと変数を複製
  dplyr::mutate(
    lambda = lambda_mat[unique(frame_i), k], # パラメータ
    pi     = pi_mat[unique(frame_i), k],     # 混合比率
    prob   = pi * dpois(x = x, lambda = lambda), # 重み付け確率
    .by = frame_i
  )


### 分布の作図 -----

# パラメータを格納
anim_param_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  k = 1:K # クラスタ番号
) |> # フレームごとにクラスタを複製
  dplyr::mutate(
    lambda = lambda_mat[unique(frame_i), k], # パラメータ
    pi     = pi_mat[unique(frame_i), k],     # 混合比率
    .by = frame_i
  ) |> 
  dplyr::mutate(
    param_lbl = paste0(
      "list(", 
      "K == ", K, ", ", 
      "lambda == (list(", paste0(round(lambda, digits = 2), collapse = ", "), ")), ", 
      "pi == (list(", paste0(round(pi, digits = 2), collapse = ", "), "))", 
      ")"
    ), # パラメータラベル
    .by = frame_i
  )

# クラスタのごとの分布を作図
graph <- ggplot() + 
  geom_bar(
    data = anim_cluster_prob_df, 
    mapping = aes(x = x, y = prob, fill = factor(k)), 
    stat = "identity", position = "stack", 
    alpha = 0.5
  ) + # 混合分布
  geom_line(
    data = anim_cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k)), 
    linewidth = 1
  ) + # クラスタごとの重み付け分布
  geom_point(
    data = anim_cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k)), 
    size = 2.5
  ) + # クラスタごとの重み付け分布
  geom_vline(
    data = anim_param_df, 
    mapping = aes(xintercept = lambda, color = factor(k)), 
    linetype = "dashed"
  ) + # クラスタごとの期待値
  geom_text(
    data = anim_param_df, 
    mapping = aes(x = lambda, y = Inf, label = paste0("lambda[", k, "]")), 
    hjust = 0.5, vjust = 1, parse = TRUE
  ) + # クラスタごとの期待値ラベル
  geom_segment(
    data = anim_param_df, 
    mapping = aes(
      x = lambda-sqrt(lambda), y = -Inf, 
      xend = lambda+sqrt(lambda), yend = -Inf, 
      color = factor(k)
    )
  ) + # クラスタごとの標準偏差
  geom_text(
    data = anim_param_df, 
    mapping = aes(x = lambda-sqrt(lambda), y = -Inf, label = "|", color = factor(k)), 
    size = 3
  ) + # クラスタごとの標準偏差の始点
  geom_text(
    data = anim_param_df, 
    mapping = aes(x = lambda+sqrt(lambda), y = -Inf, label = "|", color = factor(k)), 
    size = 3
  ) + # クラスタごとの標準偏差の終点
  geom_text(
    data = anim_param_df, 
    mapping = aes(x = lambda, y = -Inf, label = paste0("lambda[", k, "] %+-% sqrt(lambda[", k, "])")), 
    hjust = 0.5, vjust = 0, parse = TRUE
  ) + # クラスタごとの標準偏差ラベル
  geom_text(
    data = anim_param_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム切替
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 50) # (パラメータラベルの表示用)
  ) + 
  guides(
    fill = guide_legend(override.aes = list(alpha = 1)), 
    color = FALSE
  ) + # 凡例の体裁
  coord_cartesian(xlim = c(0, x_max), clip = "off") + # (パラメータラベルの表示用)
  labs(
    title = "mixture Poisson distribution", 
    subtitle = " ", # (パラメータラベルの表示用)
    fill = "cluster", 
    x = expression(x), 
    y = "probability"
  )

# 動画を作成
gganimate::animate(
  plot = graph, nframes = frame_num, fps = 10, 
  width = 12, height = 9, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/paramter.mp4")
)


