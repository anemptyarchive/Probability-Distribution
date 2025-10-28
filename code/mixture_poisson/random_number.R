
# 混合ポアソン分布 -------------------------------------------------------------

# 乱数の可視化


# ライブラリの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# サンプルサイズの影響 ---------------------------------------------------------

### パラメータの設定 -----

# クラスタごとのパラメータを指定
lambda_k <- c(1, 5.5, 10, 16.8)

# 混合比率を指定
pi_k <- c(0.2, 0.3, 0.1, 0.4)

# クラスタ数を設定
K <- length(lambda_k)


### 乱数の生成 -----

# サンプルサイズを指定
N <- 3000

# クラスタを生成
s_nk <- rmultinom(n =  N, size = 1, prob = pi_k) |> 
  t()

# クラスタ番号を抽出
s_n <- which(t(s_nk) == 1, arr.ind = TRUE) |> 
  _[, "row"]

# サンプルを生成
x_n <- rpois(n = N, lambda = lambda_k[s_n])


### 変数の設定 -----

# x軸の範囲を設定
u <- 5
x_min <- 0
x_max <- x_n |> 
  #(\(.) {.[1:frame_num]})() |> # 「1サンプルずつ」の場合
  #(\(.) {.[1:(smp_per_frame*frame_num)]})() |> # 「複数サンプルずつ」の場合
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, by = 1)


### 分布の計算 -----

# 混合ポアソン分布の重み付け確率を計算
cluster_prob_df <- tidyr::expand_grid(
  k = 1:K,  # クラスタ番号
  x = x_vec # 確率変数
) |> # クラスタごとに変数を複製
  dplyr::mutate(
    lambda = lambda_k[k], # パラメータ
    pi     = pi_k[k],     # 混合比率
    prob   = pi * dpois(x = x, lambda = lambda) # 重み付け確率
  )


### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num <- 300


##### サンプルの集計 -----

# サンプルを格納
anim_sample_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  n       = frame_i,     # サンプル番号
  s       = s_n[n],      # クラスタ番号
  x       = x_n[n]       # サンプル値
)

# サンプルを集計
anim_cluster_freq_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  N       = frame_i      # サンプル数
) |> 
  dplyr::reframe(
    n = 1:N, # サンプル番号
    .by = c(frame_i, N)
  ) |> # 過去のサンプルを複製
  dplyr::mutate(
    s = s_n[n], # クラスタ番号
    x = x_n[n]  # サンプル値
  ) |> 
  dplyr::count(
    frame_i, N, s, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / N # 相対度数
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = frame_i
    ), 
    s = 1:K, 
    x = x_vec, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- anim_cluster_freq_df |> 
  dplyr::summarise(
    freq = sum(freq), 
    .by = c(frame_i, N, s)
  ) |> 
  tidyr::pivot_wider(
    id_cols      = c(frame_i, N), 
    names_from   = s, 
    names_prefix = "s", 
    values_from  = freq
  ) |> # 度数列を展開
  tidyr::unite(
    col = "freq_str", dplyr::starts_with("s"), sep = ", "
  ) |> # 度数情報を結合
  dplyr::mutate(
    param_lbl = paste0(
      "list(", 
      "N == ", N, ", ", 
      "lambda == (list(", paste0(round(lambda_k, digits = 2), collapse = ", "), ")), ", 
      "pi == (list(", paste0(round(pi_k, digits = 2), collapse = ", "), "))", 
      ")"
    ), # パラメータラベル
    freq_lbl = paste0(
      "N == (list(", freq_str, "))"
    ) # 度数ラベル
  ) |> 
  dplyr::select(-freq_str)


# ラベル用の文字列を作成
cluster_lbl_vec <- paste0(
  "k == ", 1:K
) |> 
  parse(text = _)


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_cluster_freq_df, 
    mapping = aes(x = x, y = freq, fill = factor(s)), 
    stat = "identity", position = "stack"
  ) + # 度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf, color = factor(s)), 
    size = 4
  ) + # サンプル
  geom_text(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = param_lbl),
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = freq_lbl),
    parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5
  ) + # 度数のラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_fill_hue(label = cluster_lbl_vec) + # クラスタ番号
  guides(color = "none") + 
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/freq_1smp.mp4")
)


##### 相対度数の作図 -----

# 確率軸の範囲を指定
u <- 0.2
relfreq_max <- cluster_prob_df |> 
  dplyr::summarise(prob = sum(prob), .by = x) |> # 周辺化
  dplyr::pull(prob) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#relfreq_max <- 0.2

# サンプルの相対度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k), linetype = "generator"), 
    stat = "identity", position = "stack", 
    fill = NA, linewidth = 1
  ) + # 生成確率
  geom_bar(
    data    = anim_cluster_freq_df, 
    mapping = aes(x = x, y = rel_freq, fill = factor(s), linetype = "sample"), 
    stat = "identity", position = "stack", 
    alpha = 0.5
  ) + # 相対度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf, color = factor(s)), 
    size = 4
  ) + # サンプル
  geom_text(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = param_lbl),
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = freq_lbl),
    parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5
  ) + # 度数のラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_fill_hue(label = cluster_lbl_vec) + # クラスタ番号
  scale_linetype_manual(
    breaks = c("generator", "sample"), 
    values = c("dashed", "solid"), 
    labels = c("generator", "random number"), 
    name   = "distribution"
  ) + # 凡例の表示用
  guides(
    color    = "none", 
    linetype = guide_legend(
      override.aes = list(
        color     = "gray35", 
        linetype  = c("dashed", "blank"), 
        linewidth = 0.5
      )
    )
  ) + 
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, relfreq_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "relative frequency, probability"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/relfreq_1smp.mp4")
)


#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num <- 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame <- N %/% frame_num


##### サンプルの集計 -----

# サンプルを集計
anim_cluster_freq_df <- tibble::tibble(
  frame_i = 1:frame_num,          # フレーム番号
  N       = smp_per_frame*frame_i # サンプル数
) |> 
  dplyr::reframe(
    n = 1:N, # サンプル番号
    .by = c(frame_i, N)
  ) |> # 過去のサンプルを複製
  dplyr::mutate(
    s = s_n[n], # クラスタ番号
    x = x_n[n]  # サンプル値
  ) |> 
  dplyr::count(
    frame_i, N, s, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / N # 相対度数
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = smp_per_frame*frame_i
    ), 
    s = 1:K, 
    x = x_vec, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- anim_cluster_freq_df |> 
  dplyr::summarise(
    freq = sum(freq), 
    .by = c(frame_i, N, s)
  ) |> 
  tidyr::pivot_wider(
    id_cols      = c(frame_i, N), 
    names_from   = s, 
    names_prefix = "s", 
    values_from  = freq
  ) |> # 度数列を展開
  tidyr::unite(
    col = "freq_str", dplyr::starts_with("s"), sep = ", "
  ) |> # 度数情報を結合
  dplyr::mutate(
    param_lbl = paste0(
      "list(", 
      "N == ", N, ", ", 
      "lambda == (list(", paste0(round(lambda_k, digits = 2), collapse = ", "), ")), ", 
      "pi == (list(", paste0(round(pi_k, digits = 2), collapse = ", "), "))", 
      ")"
    ), # パラメータラベル
    freq_lbl = paste0(
      "N == (list(", freq_str, "))"
    ) # 度数ラベル
  ) |> 
  dplyr::select(-freq_str)


# ラベル用の文字列を作成
cluster_lbl_vec <- paste0(
  "k == ", 1:K
) |> 
  parse(text = _)


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_cluster_freq_df, 
    mapping = aes(x = x, y = freq, fill = factor(s)), 
    stat = "identity", position = "stack"
  ) + # 度数
  geom_text(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = param_lbl),
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = freq_lbl),
    parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5
  ) + # 度数のラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  gganimate::view_follow(fixed_y = FALSE) + # 描画領域制御
  scale_fill_hue(label = cluster_lbl_vec) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/freq_nsmp.mp4")
)


##### 相対度数の作図 -----

# 確率軸の範囲を指定
u <- 0.2
relfreq_max <- cluster_prob_df |> 
  dplyr::summarise(prob = sum(prob), .by = x) |> # 周辺化
  dplyr::pull(prob) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#relfreq_max <- 0.2

# サンプルの相対度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k), linetype = "generator"), 
    stat = "identity", position = "stack", 
    fill = NA, linewidth = 1
  ) + # 生成確率
  geom_bar(
    data    = anim_cluster_freq_df, 
    mapping = aes(x = x, y = rel_freq, fill = factor(s), linetype = "sample"), 
    stat = "identity", position = "stack", 
    alpha = 0.5
  ) + # 相対度数
  geom_text(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = param_lbl),
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_label_df,
    mapping = aes(x = -Inf, y = Inf, label = freq_lbl),
    parse = TRUE, hjust = 0, vjust = 1, alpha = 0.5
  ) + # 度数のラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_fill_hue(label = cluster_lbl_vec) + # クラスタ番号
  scale_linetype_manual(
    breaks = c("generator", "sample"), 
    values = c("dashed", "solid"), 
    labels = c("generator", "random number"), 
    name   = "distribution"
  ) + # 凡例の表示用
  guides(
    color    = "none", 
    linetype = guide_legend(
      override.aes = list(
        color     = "gray35", 
        linetype  = c("dashed", "blank"), 
        linewidth = 0.5
      )
    )
  ) + 
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, relfreq_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", color = "cluster", 
    x = expression(x), 
    y = "relative frequency, probability"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/relfreq_nsmp.mp4")
)


