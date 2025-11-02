
# ガンマ分布 -------------------------------------------------------------------

# 1次元ガウス分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(cowplot)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# ハイパーパラメータの影響 -----------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num <- 100

# 生成分布のパラメータを指定
a_vals <- seq(from = 0, to = 20, length.out = frame_num+1)[-1]
b_vals <- seq(from = 2, to = 2, length.out = frame_num)
a_vals <- seq(from = 10, to = 10, length.out = frame_num)
b_vals <- seq(from = 0, to = 20, length.out = frame_num+1)[-1]

# サンプル分布のパラメータを指定
mu <- 0


### 変数の設定 -----

# λ軸の範囲を設定
u <- 5
lambda_min <- 0
lambda_max <- (a_vals / b_vals) |> # 基準値を指定
  max() |> 
  (\(.) {. * 0.2})() |> # 倍率を指定
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
lambda_min; lambda_max

# λ軸の値を作成
lambda_vec <- seq(from = lambda_min, to = lambda_max, length.out = 1001)
lambda_vec[lambda_vec == 0] <- 1e-10 # 値を調整:(infの回避用)
head(lambda_vec)


# x軸の範囲を設定
u <- 2
x_size <- sqrt(1/lambda_max) |> # 基準値を指定
  (\(.) {. * 2})() |> # 倍率を指定
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
x_min  <- mu - x_size
x_max  <- mu + x_size
x_min; x_max

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, length.out = 1001)
head(x_vec)


### 対応関係の作図 -----

#### 精度パラメータとの関係 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"

# サンプルサイズを指定
N <- 6


# 確率密度軸の範囲を設定
u <- 0.05
gen_dens_max <- 2.5
gen_dens_max <- dgamma(
  x     = (a_vals-1) / b_vals, # 最頻値
  shape = a_vals, 
  rate  = b_vals
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
smp_dens_max <- 1.25
smp_dens_max <- dnorm(
  x    = mu, # 最頻値
  mean = mu, 
  sd   = sqrt(1/lambda_max) # 最大値
) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
gen_dens_max; smp_dens_max

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ##### パラメータの生成 -----
  
  # 生成分布のパラメータを取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # 期待値を計算
  E_lambda <- a / b
  E_x      <- mu
  
  # サンプル分布のパラメータを生成
  pctl_min <- pgamma(q = lambda_min, shape = a, rate = b)         # 描画範囲の最小値を取得
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)         # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔に累積確率を設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)             # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  lambda_n[lambda_n == 0]         <- 1e-10
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # サンプル分布のパラメータを格納
  E_data_df <- tibble::tibble(
    lambda   = E_lambda,       # 生成分布の期待値
    sigma    = sqrt(1/lambda), # 標準偏差
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 最頻値における確率密度
  )
  smp_data_df <- tibble::tibble(
    n        = 1:N,            # サンプル番号
    lambda   = lambda_n,       # サンプル値
    sigma    = sqrt(1/lambda), # 標準偏差パラメータ
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 最頻値における確率密度
  )
  
  ##### 生成分布の作図 -----
  
  # 生成分布の確率密度を計算
  gen_dens_df <- tidyr::tibble(
    lambda = lambda_vec, # 確率変数
    dens   = dgamma(x = lambda, shape = a, rate = b) # 確率密度
  )
  
  # 生成分布のラベルを作成
  gen_param_lbl <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "b == ", round(b, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  gen_stats_lbl <- paste0(
    "list(", 
      "paste(", 
        "E(lambda) == frac(a, b), {} == ", round(E_lambda, digits = 2), 
      "), ", 
      "paste(", 
        "V(lambda) == frac(a, b^2), {} == ", round(a/b^2, digits = 2), 
      ")", 
    ")"
  ) |> 
    parse(text = _)

  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(lambda))
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "lambda[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "lambda[", 1:N, "] == ", round(lambda_n, digits = 2)
  ) |> 
    parse(text = _)
  
  # 生成分布を作図
  gen_graph <- ggplot() + 
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_line(
      data    = gen_dens_df, 
      mapping = aes(x = lambda, y = dens, linetype = "generator"), 
      linewidth = 1
    ) + # 生成分布
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = 0, color = factor(n)), 
      size = 4
    ) + # サンプル
    scale_x_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_lambda, lambda_n), 
        labels = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_color_hue(labels = smp_param_lbl) + # (凡例の表示用)
    scale_linetype_manual(
      breaks = "generator", 
      values = "solid", 
      labels = gen_param_lbl, 
      name   = "generator                 " # (スペースによる表示枠の固定)
    ) + # (凡例の表示用)
    guides(
      color    = "none", 
      #color    = guide_legend(override.aes = list(size = 4)), 
      linetype = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min, lambda_max), 
      ylim = c(0, gen_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gamma distribution", 
      subtitle = gen_stats_lbl, 
      color = "sample", 
      x = expression(lambda), 
      y = expression(p(lambda ~"|"~ a, b))
    )
  
  ##### サンプル分布の作図 -----
  
  # 期待値による分布の確率密度を計算
  E_dens_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = mu, sd = sqrt(1/E_lambda)) # 確率密度
  )
  
  # サンプル分布の確率密度を計算
  smp_dens_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n],    # 精度パラメータ
      sigma  = sqrt(1/lambda), # 標準偏差パラメータ
      dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
    )
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(
    p(x == mu ~'|'~ mu, E(lambda)^{-1})
  )
  E_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "E(lambda) == ", round(E_lambda, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  E_stats_lbl <- paste0(
    "list(", 
        "paste(", 
      "E(x) == mu, {} == ", round(E_x, digits = 2), 
      "), ", 
        "paste(", 
      "V(x) == frac(b, a), {} == ", round(1/E_lambda, digits = 2), 
      ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルのラベルを作成
  smp_data_lbl  <- paste0(
    "p(x == mu ~'|'~ mu, lambda[", 1:N, "]^{-1})"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "lambda[", 1:N, "] == ", round(lambda_n, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプル分布を作図
  smp_graph <- ggplot() + 
    geom_hline(
      data    = E_data_df, 
      mapping = aes(yintercept = dens_max), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値との対応
    geom_hline(
      data    = smp_data_df, 
      mapping = aes(yintercept = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
    geom_line(
      data    = E_dens_df, 
      mapping = aes(x = x, y = dens, color = "expected"), 
      linewidth = 1, linetype = "dotdash"
    ) + # 期待値による分布
    geom_line(
      data    = smp_dens_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプル分布
    scale_y_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_data_df[["dens_max"]], smp_data_df[["dens_max"]]), 
        labels = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_color_manual(
      breaks = c("expected", 1:N), 
      values = c("red", scales::hue_pal()(n = N)), 
      labels = c(E_param_lbl, smp_param_lbl), 
      name   = "sample                    " # (スペースによる表示枠の固定)
    ) + # (凡例の表示用)
    guides(
      color = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(x_min, x_max), 
      ylim = c(0, smp_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gaussian distribution", 
      subtitle = E_stats_lbl, 
      x = expression(x), 
      y = expression(p(x ~"|"~ mu, lambda^{-1}))
    )
  
  ##### 軸変換の作図 -----
  
  # 変換曲線の座標を計算
  adapt_line_df <- tibble::tibble(
    lambda   = lambda_vec,     # 生成分布の確率変数
    sigma    = sqrt(1/lambda), # 標準偏差
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 最頻値における確率密度
  )
  
  # 軸変換を作図
  adapt_graph <- ggplot() + 
    geom_line(
      data    = adapt_line_df, 
      mapping = aes(x = lambda, y = dens_max), 
      linewidth = 1
    ) + # 変換曲線
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = dens_max), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 生成分布との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = dens_max, xend = Inf, yend = dens_max), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # サンプル分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 生成分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = dens_max, xend = Inf, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプル分布との対応
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = dens_max, color = factor(n)), 
      size = 4
    ) + # 変換点
    guides(color = "none") + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min, lambda_max), 
      ylim = c(0, smp_dens_max)
    ) + # (軸の対応用)
    labs(
      x = expression(lambda), 
      y = expression(p(x == mu ~"|"~ mu, lambda^{-1}))
    )
  
  ##### 対応関係の作図 -----
  
  # 凡例を取得
  gen_legend <- cowplot::get_legend(gen_graph)
  smp_legend <- cowplot::get_legend(smp_graph)
  
  # 凡例の表示用の図を作成
  legend_graph <- cowplot::plot_grid(
    gen_legend, smp_legend, NULL, 
    nrow = 1, ncol = 3
  ) + 
    theme(
      plot.background = element_rect(fill = "white", color = NA) # (透過背景の対策用)
    )
  
  # 凡例を除去
  tmp_gen_graph <- gen_graph + 
    theme(legend.position = "none")
  tmp_smp_graph <- smp_graph + 
    theme(legend.position = "none")
  
  # グラフを並べて描画
  comb_graph <- cowplot::plot_grid(
    tmp_gen_graph, legend_graph, 
    adapt_graph,   tmp_smp_graph, 
    nrow = 2, ncol = 2, 
    align = "hv" # (グラフ位置のズレ対策用)
  )
  
  ##### グラフの出力 -----
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = comb_graph, 
    width = 16, height = 12, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gamma/generate_gaussian/gam_to_gauss_dens.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


#### 標準偏差パラメータとの関係 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"

# サンプルサイズを指定
N <- 6


# 確率密度軸の範囲を設定
u <- 0.05
gen_dens_max <- 2.5
gen_dens_max <- dgamma(
  x     = (a_vals-1) / b_vals, # 最頻値
  shape = a_vals, 
  rate  = b_vals
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
smp_dens_max <- 1.25
smp_dens_max <- dnorm(
  x    = mu, # 最頻値
  mean = mu, 
  sd   = sqrt(1/lambda_max) # 最大値
) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
gen_dens_max; smp_dens_max

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ##### パラメータの生成 -----
  
  # 生成分布のパラメータを取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # 期待値を計算
  E_lambda <- a / b
  E_x      <- mu
  
  # サンプル分布のパラメータを生成
  pctl_min <- pgamma(q = lambda_min, shape = a, rate = b)         # 描画範囲の最小値を取得
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)         # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔に累積確率を設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)             # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  lambda_n[lambda_n == 0]         <- 1e-5 # (微小値が小さすぎると'出力後'の軸変換図で意図しない線が生じる)
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # サンプル分布のパラメータを格納
  E_data_df <- tibble::tibble(
    lambda     = E_lambda,       # 生成分布の期待値
    sigma      = 1/sqrt(lambda), # 標準偏差
    x          = mu + sigma,     # 期待値から標準偏差1つ分離れたx座標
    dens_sigma = dnorm(x = x, mean = mu, sd = sigma) # 標準偏差1つ分における確率密度
  )
  smp_data_df <- tibble::tibble(
    n          = 1:N,            # サンプル番号
    lambda     = lambda_n,       # サンプル値
    sigma      = sqrt(1/lambda), # 標準偏差パラメータ
    x          = mu + sigma,     # 期待値から標準偏差1つ分離れたx座標
    dens_sigma = dnorm(x = x, mean = mu, sd = sigma) # 標準偏差1つ分における確率密度
  )
  
  ##### 生成分布の作図 -----
  
  # 生成分布の確率密度を計算
  gen_dens_df <- tidyr::tibble(
    lambda = lambda_vec, # 確率変数
    dens   = dgamma(x = lambda, shape = a, rate = b) # 確率密度
  )
  
  # 生成分布のラベルを作成
  gen_param_lbl <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "b == ", round(b, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  gen_stats_lbl <- paste0(
    "list(", 
    "paste(", 
    "E(lambda) == frac(a, b), {} == ", round(E_lambda, digits = 2), 
    "), ", 
    "paste(", 
    "V(lambda) == frac(a, b^2), {} == ", round(a/b^2, digits = 2), 
    ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(lambda))
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "lambda[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "lambda[", 1:N, "] == ", round(lambda_n, digits = 2)
  ) |> 
    parse(text = _)
  
  # 生成分布を作図
  gen_graph <- ggplot() + 
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_line(
      data    = gen_dens_df, 
      mapping = aes(x = lambda, y = dens, linetype = "generator"), 
      linewidth = 1
    ) + # 生成分布
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = 0, color = factor(n)), 
      size = 4
    ) + # サンプル
    scale_x_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_lambda, lambda_n), 
        labels = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_color_hue(labels = smp_param_lbl) + # (凡例の表示用)
    scale_linetype_manual(
      breaks = "generator", 
      values = "solid", 
      labels = gen_param_lbl, 
      name   = "generator                 " # (スペースによる表示枠の固定)
    ) + # (凡例の表示用)
    guides(
      color    = "none", 
      #color    = guide_legend(override.aes = list(size = 4)), 
      linetype = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min, lambda_max), 
      ylim = c(0, gen_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gamma distribution", 
      subtitle = gen_stats_lbl, 
      color = "sample", 
      x = expression(lambda), 
      y = expression(p(lambda ~"|"~ a, b))
    )
  
  ##### サンプル分布の作図 -----
  
  # 期待値によ分布の確率密度を計算
  E_dens_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = mu, sd = 1/sqrt(E_lambda)) # 確率密度
  )
  
  # サンプル分布の確率密度を計算
  smp_dens_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n],    # 精度パラメータ
      sigma  = sqrt(1/lambda), # 標準偏差パラメータ
      dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
    )
  
  # 期待値のラベルを作成
  E_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "E(lambda) == ", round(E_lambda, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  E_stats_lbl <- paste0(
    "list(", 
    "paste(", 
    "E(x) == mu, {} == ", round(E_x, digits = 2), 
    "), ", 
    "paste(", 
    "V(x) == frac(b, a), {} == ", round(1/E_lambda, digits = 2), 
    ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルのラベルを作成
  smp_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "lambda[", 1:N, "] == ", round(lambda_n, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプル分布を作図
  smp_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = mu, y = -Inf, xend = mu, yend = Inf), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # y軸線
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = x, y = -Inf, xend = x, yend = dens_sigma), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = mu, y = dens_sigma, xend = x, yend = dens_sigma), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 標準偏差の範囲
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = x, y = -Inf, xend = x, yend = dens_sigma, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = mu, y = dens_sigma, xend = x, yend = dens_sigma, color = factor(n)), 
      linewidth = 1, linetype = "twodash", show.legend = FALSE
    ) + # 標準偏差の範囲
    geom_line(
      data    = E_dens_df, 
      mapping = aes(x = x, y = dens, color = "expected"), 
      linewidth = 1, linetype = "dotdash"
    ) + # 期待値による分布
    geom_line(
      data = smp_dens_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプル分布
    scale_color_manual(
      breaks = c("expected", 1:N), 
      values = c("red", scales::hue_pal()(n = N)), 
      labels = c(E_param_lbl, smp_param_lbl), 
      name   = "sample                    " # (スペースによる表示枠の固定)
    ) + # (凡例の表示用)
    guides(
      color = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(x_min, x_max), 
      ylim = c(0, smp_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gaussian distribution", 
      subtitle = E_stats_lbl, 
      x = expression(x), 
      y = expression(p(x ~"|"~ mu, lambda^{-1}))
    )
  
  ##### 恒等関数の作図 -----
  
  # 変換曲線の座標を計算
  adapt_line_df <- tibble::tibble(
    lambda   = lambda_vec,     # 生成分布の確率変数
    sigma    = sqrt(1/lambda), # 標準偏差
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 最頻値における確率密度
  )
  
  # 恒等関数を作図
  identity_graph <- ggplot() + 
    geom_line(
      data    = adapt_line_df, 
      mapping = aes(x = lambda, y = lambda), 
      linewidth = 1
    ) + # 恒等関数
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = lambda), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 生成分布との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = lambda, xend = Inf, yend = lambda), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 変換曲線との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 生成分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = lambda, xend = Inf, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 変換曲線との対応
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = lambda, color = factor(n)), 
      size = 4
    ) + # 軸変換の点
    guides(color = "none") + 
    theme(
      axis.title = element_text(size = 10)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min, lambda_max), 
      ylim = c(lambda_min, lambda_max)
    ) + # (軸の対応用)
    labs(
      x = expression(lambda), 
      y = expression(lambda)
    )
  
  ##### 軸変換の作図 -----
  
  # 期待値のラベルを作成
  E_data_lbl <- expression(E(sigma))
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "sigma[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "sigma[", 1:N, "] == ", round(1/sqrt(lambda_n), digits = 2)
  ) |> 
    parse(text = _)
  
  # 軸変換を作図
  adapt_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = 0, y = -Inf, xend = 0, yend = Inf), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # y軸線
    geom_line(
      data    = adapt_line_df, 
      mapping = aes(x = sigma, y = lambda), 
      linewidth = 1
    ) + # 変換曲線
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = -Inf, y = lambda, xend = 0, yend = lambda), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 恒等関数との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = 0, y = lambda, xend = sigma, yend = lambda), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 標準偏差の範囲
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = sigma, y = lambda, xend = sigma, yend = Inf), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # サンプル分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = -Inf, y = lambda, xend = 0, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 恒等関数との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = 0, y = lambda, xend = sigma, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "twodash"
    ) + # 標準偏差の範囲
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = sigma, y = lambda, xend = sigma, yend = Inf, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプル分布との対応
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = sigma, y = lambda, color = factor(n)), 
      size = 4, show.legend = FALSE
    ) + # 変換点
    scale_x_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(1/sqrt(E_lambda), 1/sqrt(lambda_n)), 
        labels = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_color_hue(labels = smp_param_lbl) + # (凡例の表示用)
    guides(
      color = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title = element_text(size = 10)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(-x_size, x_size), 
      ylim = c(lambda_min, lambda_max)
    ) + # (軸の対応用)
    labs(
      color = "sample", 
      x = expression(sigma == frac(1, sqrt(lambda))), 
      y = expression(lambda == frac(1, sigma^2))
    )
  
  ##### 対応関係の作図 -----
  
  # 凡例を取得
  gen_legend   <- cowplot::get_legend(gen_graph)
  smp_legend   <- cowplot::get_legend(smp_graph)
  adapt_legend <- cowplot::get_legend(adapt_graph)
  
  # 凡例の表示用の図を作成
  legend_graph <- cowplot::plot_grid(
    gen_legend, smp_legend, adapt_legend, 
    NULL, # (凡例の位置の調整用)
    nrow = 4, ncol = 1
  ) + 
    theme(
      plot.background = element_rect(fill = "white", color = NA) # (透過背景の対策用)
    )
  
  # 凡例を除去
  tmp_gen_graph   <- gen_graph + 
    theme(legend.position = "none")
  tmp_smp_graph   <- smp_graph + 
    theme(legend.position = "none")
  tmp_adapt_graph <- adapt_graph + 
    theme(legend.position = "none")
  
  # グラフを並べて描画
  tmp_comb_graph <- cowplot::plot_grid(
    tmp_gen_graph,  tmp_smp_graph, 
    identity_graph, tmp_adapt_graph, 
    nrow = 2, ncol = 2, 
    align = "hv" # (グラフ位置のズレ対策用)
  )
  comb_graph <- cowplot::plot_grid(
    tmp_comb_graph, legend_graph, 
    nrow = 1, ncol = 2, 
    rel_widths = c(1, 0.2) # (凡例の位置の調整用)
  )
  
  ##### グラフの出力 -----
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = comb_graph, 
    width = 16, height = 12, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gamma/generate_gaussian/gam to_gauss_sd.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


