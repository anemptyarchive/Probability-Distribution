
# ポアソン分布 -----------------------------------------------------------------

# パラメータの可視化


# パッケージの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# パラメータの影響 -------------------------------------------------------------

### パラメータの設定 -----

# フレームごとのパラメータを指定
lambda_vals <- seq(from = 0, to = 10, by = 0.1)

# フレーム数を設定
frame_num <- length(lambda_vals)


### 変数の設定 -----

# x軸の範囲を設定
x_min <- 0
u <- 5
x_max <- lambda_vals |> 
  max() |> 
  (\(.) {. * 1.5})() |> # 倍率を指定
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, by = 1)


### 分布の計算 -----

# ポアソン分布の確率を計算
anime_prob_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, # フレーム番号
  x       = x_vec # 確率変数
) |> # フレームごとに変数を複製
  dplyr::mutate(
    lambda = lambda_vals[frame_i], # パラメータ
    prob   = dpois(x = x, lambda = lambda) # 確率
  )


### パラメータと形状の関係 -----

# ラベル用の設定を作成
anime_label_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  lambda  = lambda_vals, 
  param_lbl = paste0("lambda == ", round(lambda, digits = 2))
)

# ポアソン分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anime_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 確率
  geom_text(
    data    = anime_label_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  theme(
    plot.subtitle = element_text(size = 50) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(clip = "off") + # (パラメータラベル用の枠外表示設定)
  labs(
    title = "Poisson Distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/poisson/parameter/parameter.mp4")
)


### パラメータと統計量の関係 -----

# 統計量を計算
anim_param_df <- tibble::tibble(
  frame_i = 1:frame_num,   # フレーム番号
  lambda  = lambda_vals,   # パラメータ
  mu      = lambda,        # 期待値
  sigma   = sqrt(lambda),  # 標準偏差
  mode    = floor(lambda), # 最頻値
  param_lbl = paste0("lambda == ", round(lambda, digits = 2)), 
  stat_lbl  = paste0(
    "mean: ", sprintf(fmt = '%.02f', domain = mu), "\n", 
    "sd:      ", sprintf(fmt = '%.02f', domain = sigma), "\n", # (スペースによる位置調整)
    "mode: ", sprintf(fmt = '%.02f', domain = mode)
  )
)

# 標準偏差の範囲を計算
anim_sd_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  sign    = c(-1, 1) # 符号
) |> 
  dplyr::mutate(
    lambda  = lambda_vals[frame_i], # パラメータ
    mu      = lambda,       # 期待値
    sigma   = sqrt(lambda), # 標準偏差
    label_x = mu + sign*sigma, # 描画位置
    sd_lbl = dplyr::if_else(
      condition = sign == 1, 
      true      = "+ sigma", 
      false     = "- sigma"
    )
  )


# ラベル用の文字列を作成
label_vec    <- c(
  mean = expression(E(x) == lambda), 
  sd   = expression(sqrt(V(x)) == sqrt(lambda)), 
  mode = expression(mode(x) == group(lfloor, lambda, rfloor))
)

# ポアソン分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anime_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 確率
  geom_vline(
    data    = anim_param_df, 
    mapping = aes(xintercept = mu, linetype = "mean"), 
    linewidth = 1
  ) + # 期待値の位置
  geom_vline(
    data    = anim_sd_df, 
    mapping = aes(xintercept = label_x, linetype = "sd"), 
    linewidth = 1
  ) + # 標準偏差の位置
  geom_segment(
    data    = anim_param_df, 
    mapping = aes(x = mu-sigma, y = -Inf, xend = mu+sigma, yend = -Inf), 
    linewidth = 1
  ) + # 標準偏差の範囲
  geom_text(
    data    = anim_sd_df,
    mapping = aes(x = label_x, y = -Inf), 
    label = "|", size = 3
  ) + # 標準偏差の指示線
  geom_vline(
    data    = anim_param_df, 
    mapping = aes(xintercept = mode, linetype = "mode"), 
    linewidth = 1
  ) + # 最頻値の位置
  geom_label(
    data    = anim_param_df, 　
    mapping = aes(x = lambda, y = Inf, label = "lambda"), 
    parse = TRUE, hjust = 0.5, vjust = 1.5, 
    fill = "gray92", label.padding = unit(0, units = "lines"), label.size = 0, 
    size = 4
  ) + # パラメータのラベル
  geom_label(
    data    = anim_param_df, 　
    mapping = aes(x = mu, y = -Inf, label = "E(x) %+-% sqrt(V(x))"), 
    parse = TRUE, hjust = 0.5, vjust = -0.2, 
    fill = "gray92", label.padding = unit(0, units = "lines"), label.size = 0, 
    size = 3
  ) + # 統計量のラベル
  geom_text(
    data    = anim_param_df, 　
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_param_df, 
    mapping = aes(x = -Inf, y = Inf, label = stat_lbl), 
    hjust = 0, vjust = 1, alpha = 0.5
  ) + # 統計量のラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  scale_linetype_manual(
    breaks = c("mean", "sd", "mode"), 
    values = c("dashed", "dotted", "dotdash"), 
    labels = label_vec, 
    name   = "statistics"
  ) + # (凡例表示用)
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  ) + # 凡例の体裁
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(clip = "off") + # (パラメータラベル用の枠外描画設定)
  labs(
    title = "Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/poisson/parameter/stats.mp4")
)


### パラメータとモーメントの関係 -----

# ガウス分布の確率密度を計算
anime_norm_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  x       = seq(from = min(x_vec), to = max(x_vec), length.out = 1001)
) |> # フレームごとに複製
  dplyr::mutate(
    lambda = lambda_vals[frame_i], # パラメータ
    mu     = lambda,       # 期待値
    sigma  = sqrt(lambda), # 標準偏差
    dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
  )


# 統計量・モーメントを計算
anim_param_df <- tibble::tibble(
  frame_i = 1:frame_num, 
  lambda  = lambda_vals,    # パラメータ
  mu      = lambda,         # 期待値
  sigma   = sqrt(lambda),   # 標準偏差
  skew    = 1/sqrt(lambda), # 歪度
  kurt    = 1/lambda,       # 尖度
  param_lbl = paste0(
    "list(", 
    "lambda == ", round(lambda, digits = 2), ", ", 
    "mu == ", round(mu, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), 
    ")"
  ), 
  moment_lbl = paste0(
    "skewness: ", sprintf(fmt = '%.03f', domain = skew), "\n", 
    "kurtosis:     ", sprintf(fmt = '%.03f', domain = kurt) # (スペースによる位置調整)
  )
)

# 標準偏差の範囲用の設定を作成
anim_sd_df <- tidyr::expand_grid(
  frame_i = 1:frame_num, 
  sign    = c(-1, 1) # 符号
) |> 
  dplyr::mutate(
    lambda  = lambda_vals[frame_i], # パラメータ
    mu      = lambda,       # 期待値
    sigma   = sqrt(lambda), # 標準偏差
    label_x = mu + sign*sigma, # 描画位置
    sd_lbl = dplyr::if_else(
      condition = sign == 1, 
      true      = "+ sigma", 
      false     = "- sigma"
    )
  )


# ポアソン分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anime_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # ポアソン分布の確率
  geom_point(
    data    = anime_prob_df, 
    mapping = aes(x = x, y = prob), 
    color = "#00A968", size = 3
  ) + # ポアソン分布の確率
  geom_segment(
    data    = anim_param_df, 
    mapping = aes(x = mu, y = 0, xend = mu, yend = Inf), 
    linewidth = 0.8, linetype = "dashed"
  ) + # 期待値の位置
  geom_segment(
    data    = anim_param_df, 
    mapping = aes(x = mu-sigma, y = -Inf, xend = mu+sigma, yend = -Inf), 
    linewidth = 0.8
  ) + # 標準偏差の範囲
  geom_text(
    data    = anim_sd_df,
    mapping = aes(x = label_x, y = -Inf, label = "|")
  ) + # 標準偏差の指示線
  geom_text(
    data    = anim_param_df, 
    mapping = aes(x = mu, y = -Inf, label = "mu"), 
    parse = TRUE, hjust = 0.5, vjust = -0.7, 
    size = 4
  ) + # 期待値のラベル
  geom_text(
    data    = anim_sd_df, 
    mapping = aes(x = label_x, y = -Inf, label = sd_lbl), 
    parse = TRUE, hjust = 0.5, vjust = -0.7, 
    size = 4
  ) + # 標準偏差のラベル
  geom_line(
    data    = anime_prob_df, 
    mapping = aes(x = x, y = prob, color = "pois"), 
    linewidth = 1
  ) + # ポアソン分布の確率
  geom_line(
    data    = anime_norm_df, 
    mapping = aes(x = x, y = dens, color = "norm"), 
    linewidth = 1, linetype = "dashed"
  ) + # ガウス分布の確率密度
  geom_text(
    data    = anim_param_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  geom_label(
    data    = anim_param_df, 
    mapping = aes(x = -Inf, y = Inf, label = moment_lbl), 
    hjust = 0, vjust = 1, alpha = 0.5
  ) + # モーメントのラベル
  gganimate::transition_manual(frames = frame_i) + # フレーム制御
  #scale_x_continuous(breaks = x_vec, minor_breaks = FALSE) + # x軸目盛
  scale_color_manual(
    breaks = c("norm", "pois"), 
    values = c("red", "#00A968"), 
    labels = c("Gaussian", "Poisson"), 
    name   = "distribution"
  ) + # (凡例の表示用)
  guides(
    color    = guide_legend(override.aes = list(linewidth = 0.5)), 
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  ) + # 凡例の体裁
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(clip = "off") + # (パラメータラベル用の枠外描画設定)
  labs(
    title = "Poisson distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability, density"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/poisson/parameter/moment.mp4")
)


