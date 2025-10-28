
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


### 変数の設定 -----

# x軸の範囲を設定
u <- 5
sgm_num <- 2
x_size <- x_n |> 
  #(\(.) {.[1:frame_num]})() |> # 「1サンプルずつ」の場合
  #(\(.) {.[1:(smp_per_frame*frame_num)]})() |> # 「複数サンプルずつ」の場合
  (\(.) {. - mu})() |> # パラメータの影響を除去
  abs() |> 
  max() |> 
  (\(.) {max(., sgm_num*sigma)})() |> # パラメータと比較
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
x_min  <- mu - x_size
x_max  <- mu + x_size

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, length.out = 1001)


# 階級幅を指定
bin_size <- 1

# 階級数を計算
bin_num <- (x_max - x_min) %/% bin_size

# 境界値の範囲を設定
bin_min <- x_min - 0.5*bin_size
bin_max <- x_max + 0.5*bin_size


### 分布の計算 -----

# ガウス分布の確率密度を計算
dens_df <- tidyr::tibble(
  x    = x_vec, # 確率変数
  dens = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
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
  x       = x_n[n],      # サンプル値
  param_lbl = paste0(
    "list(", 
    "N == ", n, ", ", 
    "mu == ", mu, ", ", 
    "sigma == ", sigma, 
    ")"
  ) # パラメータラベル
)

# 各試行までのサンプルを格納
anim_obs_df <- tibble::tibble(
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

# サンプルを集計
anim_freq_df <- anim_obs_df |> 
  dplyr::mutate(
    bin_i  = (x - bin_min) %/% bin_size,         # 階級番号(計算用)
    lower  = bin_min + bin_i * bin_size,         # 階級の下限値(確認用)
    center = bin_min + (bin_i + 0.5) * bin_size, # 階級値
    upper  = bin_min + (bin_i + 1) * bin_size    # 階級の上限値(確認用)
  ) |> 
  dplyr::count(
    frame_i, N, lower, center, upper, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    dens = freq / (bin_size * N) # 密度
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = frame_i
    ), 
    tibble::tibble(
      lower  = seq(from = bin_min, to = bin_max-bin_size, by = bin_size), 
      center = seq(from = x_min, to = x_max, by = bin_size), 
      upper  = seq(from = bin_min+bin_size, to = bin_max, by = bin_size)
    ), 
    fill = list(freq = 0, rel_freq = 0, dens = 0)
  ) # 未観測値を補完


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = center, y = freq), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 度数
  geom_point(
    data    = anim_obs_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.2, size = 2
  ) + # 各試行までのサンプル
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # 新規のサンプル
  geom_text(
    data    = anim_sample_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
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


##### 密度の作図 -----

# 密度軸の範囲を指定
u <- 0.25
dens_max <- dens_df |> 
  dplyr::pull(dens) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#dens_max <- 0.25

# サンプルの密度を作図
anim <- ggplot() + 
  geom_line(
    data    = dens_df, 
    mapping = aes(x = x, y = dens, linetype = "generator"), 
    color = "darkgreen", linewidth = 1
  ) + # 生成分布
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = center, y = dens, linetype = "sample"), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # 密度
  geom_point(
    data    = anim_obs_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.2, size = 2
  ) + # 各試行までのサンプル
  geom_point(
    data    = anim_sample_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", size = 4
  ) + # 新規のサンプル
  geom_text(
    data    = anim_sample_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
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
    ylim = c(0, dens_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
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


##### サンプルの集計 -----

# 各試行までのサンプルを格納
anim_obs_df <- tibble::tibble(
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

# サンプルを集計
anim_freq_df <- anim_obs_df |> 
  dplyr::mutate(
    bin_i  = (x - bin_min) %/% bin_size,         # 階級番号(計算用)
    lower  = bin_min + bin_i * bin_size,         # 階級の下限値(確認用)
    center = bin_min + (bin_i + 0.5) * bin_size, # 階級値
    upper  = bin_min + (bin_i + 1) * bin_size    # 階級の上限値(確認用)
  ) |> 
  dplyr::count(
    frame_i, N, lower, center, upper, name = "freq" # 度数
  ) |> 
  dplyr::mutate(
    dens = freq / (bin_size * N) # 密度
  ) |> 
  tidyr::complete(
    tibble::tibble(
      frame_i = 1:frame_num, 
      N       = smp_per_frame*frame_i
    ), 
    tibble::tibble(
      lower  = seq(from = bin_min, to = bin_max-bin_size, by = bin_size), 
      center = seq(from = x_min, to = x_max, by = bin_size), 
      upper  = seq(from = bin_min+bin_size, to = bin_max, by = bin_size)
    ), 
    fill = list(freq = 0, rel_freq = 0, dens = 0)
  ) # 未観測値を補完

# ラベル用の文字列を作成
anim_label_df <- tibble::tibble(
  frame_i = 1:frame_num,           # フレーム番号
  N       = smp_per_frame*frame_i, # サンプル数
  param_lbl = paste0(
    "list(", 
    "N == ", N, ", ", 
    "mu == ", mu, ", ", 
    "sigma == ", sigma, 
    ")"
  ) # パラメータラベル
)


##### 度数の作図 -----

# サンプルの度数を作図
anim <- ggplot() + 
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = center, y = freq), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 度数
  geom_point(
    data    = anim_obs_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.2, size = 2
  ) + # 各試行までのサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  gganimate::view_follow(fixed_y = FALSE) + # 描画領域制御
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + 
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
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


##### 密度の作図 -----

# 密度軸の範囲を指定
u <- 0.25
dens_max <- dens_df |> 
  dplyr::pull(dens) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
#dens_max <- 0.25

# サンプルの密度を作図
anim <- ggplot() + 
  geom_line(
    data    = dens_df, 
    mapping = aes(x = x, y = dens, linetype = "generator"), 
    color = "darkgreen", linewidth = 1
  ) + # 生成確率
  geom_bar(
    data    = anim_freq_df, 
    mapping = aes(x = center, y = dens, linetype = "sample"), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # 度数
  geom_point(
    data    = anim_obs_df, 
    mapping = aes(x = x, y = -Inf), 
    color = "orange", alpha = 0.2, size = 2
  ) + # 各試行までのサンプル
  geom_text(
    data    = anim_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
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
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, dens_max), 
    clip = "off" # (パラメータラベル用の枠外描画設定)
  ) + 
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


