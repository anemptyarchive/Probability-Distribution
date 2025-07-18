
# 1次元ガウス分布 --------------------------------------------------------------

# 乱数の可視化


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# サンプルサイズの影響 ---------------------------------------------------------

### パラメータの設定 -----

# 平均パラメータを指定
mu <- 2

# 標準偏差パラメータを指定
sigma <- 2.5


### 乱数の生成 -----

# サンプルサイズを指定
N <- 3000

# ガウス分布の乱数を生成
x_n <- rnorm(n = N, mean = mu, sd = sigma)


### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を指定
frame_num <- N
frame_num <- 300


# x軸の範囲を設定
u <- 5
x_size <- (x_n[1:frame_num] - mu) |> # 集計対象を抽出
  abs() |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
sgm_num <- 2
x_size <- max(x_size, sgm_num*sigma)
x_min <- mu - x_size
x_max <- mu + x_size

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, length.out = 1001)


# サンプルを格納
anim_sample_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  n       = frame_i,     # サンプル番号
  x       = x_n[n]       # サンプル値
)

# 集計用のサンプルを格納
anim_freq_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  N       = frame_i      # サンプル数
) |> 
  dplyr::reframe(
    n = 1:N, # サンプル番号
    .by = c(frame_i, N)
  ) |> # 過去のサンプルを複製
  dplyr::mutate(
    x = x_n[n] # サンプル値
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  param_lbl = paste0(
    "list(", 
    "N == ", frame_i, ", ", 
    "mu == ", mu, ", ", 
    "sigma == ", sigma, 
    ")"
  ), # パラメータラベル
)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x    = x_vec, # 確率変数
  dens = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
)


# サンプルの度数を作図
anim <- ggplot() + 
  geom_histogram(
    data    = anim_freq_df, 
    mapping = aes(x = x), 
    breaks = seq(from = x_min, to = x_max, length.out = 30), 
    fill = "#00A968"
  ) + # 度数
  geom_point(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.5, size = 2
  ) + # 過去のサンプル
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # 新規のサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + # 描画範囲
  labs(
    title = "Gaussian distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/gaussian/random_number/freq_1smp.mp4")
)


# 確率確率軸の範囲を指定
dens_max <- 0.25
u <- 0.1
dens_max <- dens_df |> 
  dplyr::pull(dens) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# サンプルの密度を作図
anim <- ggplot() + 
  geom_histogram(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = ..density..), 
    breaks = seq(from = x_min, to = x_max, length.out = 30), 
    fill = "#00A968"
  ) + # 密度
  geom_line(
    data    = dens_df, 
    mapping = aes(x = x, y = dens), 
    color = "darkgreen", linewidth = 1, linetype = "dashed"
  ) + # 生成分布
  geom_point(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.5, size = 2
  ) + # 過去のサンプル
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # 新規のサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, dens_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + # 描画範囲
  labs(
    title = "Gaussian distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "density"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/gaussian/random_number/dens_1smp.mp4")
)


#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num <- 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame <- N %/% frame_num

# x軸の範囲を設定
u <- 5
x_size <- (x_n[1:(smp_per_frame*frame_num)] - mu) |> # 集計対象を抽出
  abs() |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
sgm_num <- 2
x_size <- max(x_size, sgm_num*sigma)
x_min <- mu - x_size
x_max <- mu + x_size

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, length.out = 1001)


# 集計用のサンプルを格納
anim_freq_df <- tibble::tibble(
  frame_i = 1:frame_num,          # フレーム番号
  N       = smp_per_frame*frame_i # サンプル数
) |> 
  dplyr::reframe(
    n = 1:N, # サンプル番号
    .by = c(frame_i, N)
  ) |> # 過去のサンプルを複製
  dplyr::mutate(
    x = x_n[n] # サンプル値
  )

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  param_lbl = paste0(
    "list(", 
    "N == ", smp_per_frame*frame_i, ", ", 
    "mu == ", mu, ", ", 
    "sigma == ", sigma, 
    ")"
  ), # パラメータラベル
)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x    = x_vec, # 確率変数
  dens = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
)


# サンプルの度数を作図
anim <- ggplot() + 
  geom_histogram(
    data    = anim_freq_df, 
    mapping = aes(x = x), 
    breaks = seq(from = x_min, to = x_max, length.out = 30), 
    fill = "#00A968"
  ) + # 度数
  geom_point(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.5, size = 2
  ) + # 過去のサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  gganimate::view_follow(fixed_y = FALSE) + # 描画領域制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + # 描画範囲
  labs(
    title = "Gaussian distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/gaussian/random_number/freq_nsmp.mp4")
)


# 確率確率軸の範囲を指定
dens_max <- 0.25
u <- 0.1
dens_max <- dens_df |> 
  dplyr::pull(dens) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# サンプルの密度を作図
anim <- ggplot() + 
  geom_histogram(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = ..density..), 
    breaks = seq(from = x_min, to = x_max, length.out = 30), 
    fill = "#00A968"
  ) + # 密度
  geom_line(
    data    = dens_df, 
    mapping = aes(x = x, y = dens), 
    color = "darkgreen", linewidth = 1, linetype = "dashed"
  ) + # 生成分布
  geom_point(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.5, size = 2
  ) + # 過去のサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, dens_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + # 描画範囲
  labs(
    title = "Gaussian distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "density"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/gaussian/random_number/dens_nsmp.mp4")
)


