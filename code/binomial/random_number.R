
# 二項分布 ---------------------------------------------------------------------

# 乱数の可視化


# ライブラリの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# サンプルサイズの影響 ---------------------------------------------------------

### パラメータの設定 -----

# 試行回数を指定
M <- 9

# パラメータを指定
phi <- 0.4


### 乱数の生成 -----

# サンプルサイズを指定
N <- 3000

# 二項分布の乱数を生成
x_n <- rbinom(n = N, size = M, prob = phi)


### 変数の設定 -----

# x軸の範囲を設定
x_min <- 0
x_max <- M

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, by = 1)


### 分布の計算 -----

# 二項分布の確率を計算
prob_df <- tidyr::tibble(
  x    = x_vec, # 確率変数
  prob = dbinom(x = x, size = M, prob = phi) # 確率
)


### 乱数の可視化 -----

#### 1サンプルずつ集計 -----

# フレーム数を設定
frame_num <- 300


##### サンプルの集計 -----

# サンプルを格納
anim_sample_df <- tibble::tibble(
  frame_i = 1:frame_num, # フレーム番号
  n       = frame_i,     # サンプル番号
  x       = x_n[n],      # サンプル値
  param_lbl = paste0(
    "list(", 
    "N == ", n, ", ", 
    "M == ", M, ", ", 
    "phi == ", phi, 
    ")"
  ) # パラメータラベル
)

# サンプルを集計
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
  ) |> 
  dplyr::count(
    frame_i, N, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / N # 相対度数
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = frame_i
    ), 
    x = x_vec, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = freq), 
    stat = "identity", 
    fill = "#00A968"
  ) + # 度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # サンプル
  geom_text(
    data    = anim_sample_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(0, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/binomial/random_number/freq_1smp.mp4")
)


##### 相対度数の作図 -----

# 相対度数軸の範囲を設定
u <- 0.25
relfreq_max <- prob_df |> 
  dplyr::pull(prob) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#relfreq_max <- 0.25

# サンプルの相対度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = prob_df, 
    mapping = aes(x = x, y = prob, linetype = "generator"), 
    stat = "identity", position = "identity", 
    fill = NA, color = "darkgreen", linewidth = 1
  ) + # 生成確率
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = rel_freq, linetype = "sample"), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # 相対度数
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # サンプル
  geom_text(
    data    = anim_sample_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  scale_linetype_manual(
    breaks = c("generator", "sample"), 
    values = c("dashed", "solid"), 
    labels = c("generator", "random number"), 
    name   = "distribution"
  ) + # 凡例の表示用
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5)), 
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
    title = "Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "relative frequency, probability"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/binomial/random_number/relfreq_1smp.mp4")
)


#### 複数サンプルずつ集計 -----

# フレーム数を指定
frame_num <- 300

# 1フレーム当たりのサンプル数を設定
smp_per_frame <- N %/% frame_num


##### サンプルの集計 -----

# サンプルを集計
anim_freq_df <- tibble::tibble(
  frame_i = 1:frame_num,          # フレーム番号
  N       = smp_per_frame*frame_i # サンプル数
) |> 
  dplyr::reframe(
    n = 1:N, # サンプル番号
    .by = c(frame_i, N)
  ) |> # 過去のサンプルを複製
  dplyr::mutate(
    x = x_n[n] # 観測値
  ) |> 
  dplyr::count(
    frame_i, N, x, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    rel_freq = freq / N # 相対度数
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = smp_per_frame*frame_i
    ), 
    x = x_vec, 
    fill = list(freq = 0, rel_freq = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num,           # フレーム番号
  N       = smp_per_frame*frame_i, # サンプル数
  param_lbl = paste0(
    "list(", 
    "N == ", N, ", ", 
    "M == ", M, ", ", 
    "phi == ", phi, 
    ")"
  ) # パラメータラベル
)


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = freq), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 度数
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  gganimate::view_follow(fixed_y = FALSE) + # 描画領域制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(0, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "frequency"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/binomial/random_number/freq_nsmp.mp4")
)


##### 相対度数の作図 -----

# 相対度数軸の範囲を設定
u <- 0.25
relfreq_max <- prob_df |> 
  dplyr::pull(prob) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#relfreq_max <- 0.25

# サンプルの相対度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = prob_df, 
    mapping = aes(x = x, y = prob, linetype = "generator"), 
    stat = "identity", position = "identity", 
    fill = NA, color = "darkgreen", linewidth = 1
  ) + # 生成確率
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = x, y = rel_freq, linetype = "sample"), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # 相対度数
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  scale_linetype_manual(
    breaks = c("generator", "sample"), 
    values = c("dashed", "solid"), 
    labels = c("generator", "random number"), 
    name   = "distribution"
  ) + # 凡例の表示用
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5)), 
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
  labs(
    title = "Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "relative frequency, probability"
  )

# 動画を作成
gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/binomial/random_number/relfreq_nsmp.mp4")
)


