
# 混合ポアソン分布 -------------------------------------------------------------

# 乱数の生成


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

# クラスタ数を取得
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


### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num <- N
frame_num <- 300

# x軸の範囲を指定
u <- 5
x_max <- x_n[1:frame_num] |> # 集計対象を抽出
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# x軸の値を作成
x_vec <- seq(from = 0, to = x_max, by = 1)


# サンプルを格納
anim_sample_df <- tibble::tibble(
  frame_i = 1:frame_num,  # サンプル番号
  s       = s_n[frame_i], # クラスタ番号
  x       = x_n[frame_i]  # サンプル値
)

# サンプルを集計
anim_freq_df <- tibble::tibble(
  frame_i = 1:frame_num # サンプルサイズ
) |> 
  dplyr::reframe(
    n = 1:frame_i, # サンプル番号
    .by = frame_i
  ) |> 
  dplyr::mutate(
    s = s_n[n], # クラスタ番号
    x = x_n[n]  # サンプル値
  ) |> 
  dplyr::count(
    frame_i, s, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / frame_i # 相対度数
  ) |> 
  tidyr::complete(
    frame_i = 1:frame_num, s = 1:K, x = 0:x_max, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- anim_freq_df |> 
  dplyr::summarise(
    freq = sum(freq), 
    .by = c(frame_i, s)
  ) |> 
  tidyr::pivot_wider(
    id_cols      = frame_i, # サンプルサイズ
    names_from   = s, 
    names_prefix = "s", 
    values_from  = freq
  ) |> # 度数列を展開
  tidyr::unite(
    col = "freq_txt", dplyr::starts_with("s"), sep = ", "
  ) |> # 度数情報を結合
  dplyr::mutate(
    param_lbl = paste0(
      "list(", 
      "N == ", frame_i, ", ", 
      "lambda == (list(", paste0(round(lambda_k, digits = 2), collapse = ", "), ")), ", 
      "pi == (list(", paste0(round(pi_k, digits = 2), collapse = ", "), "))", 
      ")"
    ), # パラメータラベル
    freq_lbl = paste0("N == (list(", freq_txt, "))") # 度数ラベル
  ) |> 
  dplyr::select(-freq_txt)

# 混合ポアソン分布を計算
prob_df <- tidyr::expand_grid(
  k = 1:K,  # クラスタ番号
  x = x_vec # 確率変数
) |> # クラスタごとに変数を複製
  dplyr::mutate(
    lambda = lambda_k[k], # パラメータ
    pi     = pi_k[k],     # 混合比率
    prob   = pi * dpois(x = x, lambda = lambda) # 重み付け確率
  )


# サンプルの度数を作図
graph <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = freq, fill = factor(s)), 
    stat = "identity", position = "stack"
  ) + # 度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = 0), 
    color = "orange", size = 4
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
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(0, x_max), 
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
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/freq_1.mp4")
)

# 確率軸の範囲を指定
prob_max <- 0.2

# サンプルの相対度数を作図
graph <- ggplot() + 
  geom_bar(
    data = prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "stack", 
    fill = NA, color = "darkgreen", linetype = "dashed"
  ) + # 生成分布
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = rel_freq, fill = factor(s)), 
    stat = "identity", position = "stack", 
    alpha = 0.5
  ) + # 度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = 0), 
    color = "orange", size = 4
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
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(0, x_max), 
    ylim = c(0, prob_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "relative frequency"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/rel_freq_1.mp4")
)


### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num <- 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame <- N %/% frame_num

# x軸の範囲を指定
u <- 5
x_max <- x_n[1:(smp_per_frame*frame_num)] |> # 集計対象を抽出
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# x軸の値を作成
x_vec <- seq(from = 0, to = x_max, by = 1)


# サンプルを集計
anim_freq_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  tmp_N   = smp_per_frame*frame_i # サンプル数
) |> 
  dplyr::reframe(
    n = 1:tmp_N, # サンプル番号
    .by = c(frame_i, tmp_N)
  ) |> 
  dplyr::mutate(
    s = s_n[n], # クラスタ番号
    x = x_n[n]  # 観測値
  ) |> 
  dplyr::count(
    frame_i, tmp_N, s, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / tmp_N # 相対度数
  ) |> 
  tidyr::complete(
    frame_i = 1:frame_num, s = 1:K, x = 0:x_max, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- anim_freq_df |> 
  dplyr::summarise(
    freq = sum(freq), 
    .by = c(frame_i, s)
  ) |> 
  tidyr::pivot_wider(
    id_cols      = frame_i, # サンプルサイズ
    names_from   = s, 
    names_prefix = "s", 
    values_from  = freq
  ) |> # 度数列を展開
  tidyr::unite(
    col = "freq_txt", dplyr::starts_with("s"), sep = ", "
  ) |> # 度数情報を結合
  dplyr::mutate(
    tmp_N = smp_per_frame*frame_i, # サンプル数
    param_lbl = paste0(
      "list(", 
      "N == ", tmp_N, ", ", 
      "lambda == (list(", paste0(round(lambda_k, digits = 2), collapse = ", "), ")), ", 
      "pi == (list(", paste0(round(pi_k, digits = 2), collapse = ", "), "))", 
      ")"
    ), # パラメータラベル
    freq_lbl = paste0("N == (list(", freq_txt, "))") # 度数ラベル
  ) |> 
  dplyr::select(-freq_txt)

# 混合ポアソン分布を計算
prob_df <- tidyr::expand_grid(
  k = 1:K,  # クラスタ番号
  x = x_vec # 確率変数
) |> # クラスタごとに変数を複製
  dplyr::mutate(
    lambda = lambda_k[k], # パラメータ
    pi     = pi_k[k],     # 混合比率
    prob   = pi * dpois(x = x, lambda = lambda) # 重み付け確率
  )


# サンプルの度数を作図
graph <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
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
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(0, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  gganimate::view_follow(fixed_y = FALSE) + # 描画領域
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/freq_n.mp4")
)

# サンプルの相対度数を作図
graph <- ggplot() + 
  geom_bar(
    data = prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "stack", 
    fill = NA, color = "darkgreen", linetype = "dashed"
  ) + # 生成分布
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = rel_freq, fill = factor(s)), 
    stat = "identity", position = "stack", 
    alpha = 0.5
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
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(0, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "mixture Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    fill = "cluster", 
    x = expression(x), 
    y = "relative frequency"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/mixture_poisson/random_number/rel_freq_n.mp4")
)


