
# 負の二項分布 ---------------------------------------------------------------------

# パラメータの可視化


# ライブラリの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージ名の省略用
library(ggplot2)


# 編集メモ ---------------------------------------------------------------------

## Φ = 0 のときにInfになり意図しない装飾になる問題に未対応


# パラメータの影響 -------------------------------------------------------------

### パラメータの設定 -----

# フレームごとのパラメータを指定
#r_vals   <- 0:100
phi_vals <- seq(from = 0, to = 1, by = 0.01)

# フレーム数を設定
#frame_num <- length(phi_vals)
frame_num <- length(r_vals)

# 固定するパラメータを作成
r_vals   <- rep(5, times = frame_num)
#phi_vals <- rep(0.5, times = frame_num)


### 変数の設定 -----

# x軸の余白を指定:(「モーメントとの関係」用)
x_margin <- 0

# x軸の範囲を設定
u <- 5
x_min <- 0 - x_margin # 余白を追加
x_max <- (r_vals * (1-phi_vals) / phi_vals) |> # 基準値を指定
  (\(.) {ifelse(test = is.infinite(.), yes = NA, no = .)})() |> # 0除算を除去
  max(na.rm = TRUE) |> 
  (\(.) {. * 0.1})() |> # 倍率を指定
  (\(.) {ceiling(. /u)*u})() |> # u単位で切り上げ
  (\(.) {. + x_margin})() # 余白を追加
x_min; x_max

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, by = 1)


### 分布の計算 -----

# 確率を計算
anim_prob_df <- tidyr::expand_grid(
  i = 1:frame_num, # フレーム番号
  x = x_vec        # 確率変数
) |> # フレームごとに変数を複製
  dplyr::mutate(
    r    = r_vals[i],   # 成功回数
    phi  = phi_vals[i], # 成功確率
    prob = dnbinom(x = x, size = r, prob = phi) # 確率
  )


### 分布の作図 -----


#### パラメータと形状の関係 -----

# ラベル用の文字列を作成
anim_param_df <- tibble::tibble(
  i   = 1:frame_num, # フレーム番号
  r   = r_vals,      # 成功回数
  phi = phi_vals,    # 成功確率
  param_lbl = paste0(
    "list(", 
    "r == ", r, ", ", 
    "phi == ", round(phi, digits = 2), ", ", 
    "paste(psi == 1-phi, {} == ", round(1-phi, digits = 2), ")", 
    ")"
  ) # パラメータラベル
)

# 負の二項分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anim_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 確率
  geom_text(
    data    = anim_param_df, 
    mapping = aes(x = -Inf, y = Inf, label = param_lbl), 
    parse = TRUE, hjust = 0, vjust = -0.5
  ) + # パラメータのラベル
  gganimate::transition_manual(frames = i) + # フレーム制御
  theme(
    plot.subtitle = element_text(size = 50) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(clip = "off") + # (パラメータラベル用の枠外表示設定)
  labs(
    title = "Negative Binomial Distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/negative_binomial/parameter/parameter.mp4")
)


#### パラメータと統計量の関係 -----

# ラベル用の文字列を作成
anim_param_df <- tibble::tibble(
  i   = 1:frame_num, # フレーム番号
  r   = r_vals,      # 成功回数
  phi = phi_vals,    # 成功確率
  mean_x = r * (1 - phi) / phi,         # 期待値
  sd_x   = sqrt(r * (1 - phi) / phi^2), # 標準偏差
  mode_x = ifelse(
    test = r > 1, 
    yes  = floor((r - 1) * (1 - phi) / phi), 
    no   = 1
  ), # 最頻値
  param_lbl = paste0(
    "list(", 
    "r == ", r, ", ", 
    "phi == ", round(phi, digits = 2), 
    ")"
  ), # パラメータラベル
  stat_lbl  = paste0(
    "mean: ", sprintf(fmt = '%.02f', domain = mean_x), "\n", 
    "sd:      ", sprintf(fmt = '%.02f', domain = sd_x), "\n", # (スペースによる位置調整)
    "mode: ", sprintf(fmt = '%.02f', domain = mode_x)
  ) # 統計量ラベル
)

# 標準偏差の範囲を計算
anim_sd_df <- tidyr::expand_grid(
  i    = 1:frame_num, # フレーム番号
  sign = c(-1, 1)     # 符号
) |> # パラメータごとに符号を複製
  dplyr::mutate(
    r   = r_vals[i],   # 成功回数
    phi = phi_vals[i], # 成功確率
    mean_x  = r * (1 - phi) / phi,         # 期待値
    sd_x    = sqrt(r * (1 - phi) / phi^2), # 標準偏差
    label_x = mean_x + sign*sd_x # 描画位置
  )


# ラベル用の文字列を作成
stat_lbl_vec <- c(
  mean = expression(E(x) == frac(r * (1 - phi), phi)), 
  sd   = expression(sqrt(V(x)) == sqrt(frac(r * (1 - phi), phi^2))), 
  mode = expression(mode(x) == group(lfloor, frac((r - 1) * (1 - phi), phi), rfloor))
)

# 負の二項分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anim_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968"
  ) + # 確率
  geom_vline(
    data    = anim_param_df, 
    mapping = aes(xintercept = mean_x, linetype = "mean"), 
    linewidth = 1
  ) + # 期待値の位置
  geom_vline(
    data    = anim_sd_df, 
    mapping = aes(xintercept = label_x, linetype = "sd"), 
    linewidth = 1
  ) + # 標準偏差の位置
  geom_vline(
    data    = anim_param_df, 
    mapping = aes(xintercept = mode_x, linetype = "mode"), 
    linewidth = 1
  ) + # 最頻値の位置
  geom_segment(
    data    = anim_param_df, 
    mapping = aes(x = mean_x-sd_x, y = -Inf, xend = mean_x+sd_x, yend = -Inf), 
    linewidth = 1
  ) + # 標準偏差の範囲
  geom_text(
    data    = anim_sd_df,
    mapping = aes(x = label_x, y = -Inf), 
    label = "|", size = 3
  ) + # 標準偏差の指示線
  geom_label(
    data    = anim_param_df, 　
    mapping = aes(x = mean_x, y = -Inf, label = "E(x) %+-% sqrt(V(x))"), 
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
  gganimate::transition_manual(frames = i) + # フレーム制御
  scale_linetype_manual(
    breaks = c("mean", "sd", "mode"), 
    values = c("dashed", "dotted", "dotdash"), 
    labels = stat_lbl_vec, 
    name   = "statistics"
  ) + # (凡例表示用)
  guides(
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  ) + # 凡例の体裁
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    clip = "off" # (パラメータラベル用の枠外表示設定)
  ) + # 描画範囲
  labs(
    title = "Negative Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/negative_binomial/parameter/stats.mp4")
)


#### パラメータとモーメントの関係 -----

# ラベル用の文字列を作成
anim_param_df <- tibble::tibble(
  i     = 1:frame_num, # フレーム番号
  r     = r_vals,      # 成功回数
  phi   = phi_vals,    # 成功確率
  mu    = r * (1 - phi) / phi,         # 期待値
  sigma = sqrt(r * (1 - phi) / phi^2), # 標準偏差
  skew  = (2 - phi) / sqrt((1 - phi) * r), # 歪度
  kurt  = 6 / r + phi^2 / (1 - phi) / r,   # 尖度
  param_lbl = paste0(
    "list(", 
    "r == ", r, ", ", 
    "phi == ", round(phi, digits = 2), ", ", 
    "mu == ", round(mu, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), 
    ")"
  ), # パラメータラベル
  moment_lbl = paste0(
    "skewness: ", sprintf(fmt = '%.03f', domain = skew), "\n", 
    "kurtosis:     ", sprintf(fmt = '%.03f', domain = kurt) # (スペースによる位置調整)
  ) # モーメントラベル
)

# 標準偏差の範囲を計算
anim_sd_df <- tidyr::expand_grid(
  i    = 1:frame_num, # フレーム番号
  sign = c(-1, 1)     # 符号
) |> # パラメータごとに符号を複製
  dplyr::mutate(
    r   = r_vals[i],   # 成功回数
    phi = phi_vals[i], # 成功確率
    mu      = r * (1 - phi) / phi,         # 期待値
    sigma   = sqrt(r * (1 - phi) / phi^2), # 標準偏差
    label_x = mu + sign*sigma, # 描画位置
    sd_lbl  = (sign == 1) |> 
      dplyr::if_else(
        true  = "+ sigma", 
        false = "- sigma"
      ) # 標準偏差ラベル
  )

# ガウス分布の確率密度を計算
anim_norm_df <- tidyr::expand_grid(
  i = 1:frame_num, # フレーム番号
  x = seq(from = x_min, to = x_max, length.out = 1001) # 確率変数
) |> # フレームごとに変数を複製
  dplyr::mutate(
    r     = r_vals[i],   # 成功回数
    phi   = phi_vals[i], # 成功確率
    mu    = r * (1 - phi) / phi,         # 期待値
    sigma = sqrt(r * (1 - phi) / phi^2), # 標準偏差
    dens  = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
  )


# 確率密度軸の範囲を設定
dens_max <- 1

# 負の二項分布のアニメーションを作図
graph <- ggplot() + 
  geom_bar(
    data    = anim_prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity", position = "identity", 
    fill = "#00A968", alpha = 0.5
  ) + # 負の二項分布の確率
  geom_point(
    data    = anim_prob_df, 
    mapping = aes(x = x, y = prob), 
    color = "#00A968", size = 3
  ) + # 負の二項分布の確率
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
    data    = anim_prob_df, 
    mapping = aes(x = x, y = prob, color = "nb"), 
    linewidth = 1
  ) + # 負の二項分布の確率
  geom_line(
    data    = anim_norm_df, 
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
  gganimate::transition_manual(frames = i) + # フレーム制御
  scale_color_manual(
    breaks = c("nb", "norm"), 
    values = c("#00A968", "red"), 
    labels = c("Negative Binomial", "Gaussian"), 
    name   = "distribution"
  ) + # (凡例の表示用)
  guides(
    color    = guide_legend(override.aes = list(linewidth = 0.5)), 
    linetype = guide_legend(override.aes = list(linewidth = 0.5))
  ) + # 凡例の体裁
  theme(
    plot.subtitle = element_text(size = 40) # (パラメータラベル用の空行サイズ)
  ) + # 図の体裁
  coord_cartesian(
    xlim = c(x_min, x_max), 
    ylim = c(0, dens_max), 
    clip = "off" # (パラメータラベル用の枠外表示設定)
  ) + # 描画範囲
  labs(
    title = "Binomial distribution", 
    subtitle = "", # (パラメータラベル用の空行)
    x = expression(x), 
    y = "probability, density"
  )

# 動画を作成
gganimate::animate(
  plot = graph, 
  nframes = frame_num, fps = 10, 
  width = 12, height = 8, units = "in", res = 100, 
  renderer = gganimate::av_renderer(file = "figure/negative_binomial/parameter/moment.mp4")
)

warnings()


