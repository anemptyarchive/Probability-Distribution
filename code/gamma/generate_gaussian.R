
# ガンマ分布 -------------------------------------------------------------------

# 1次元ガウス分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# ハイパラとサンプル分布の形状の関係 -------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num <- 100

# ハイパーパラメータを指定
a_vals <- seq(from = 0, to = 20, length.out = frame_num+1)[-1]
b_vals <- seq(from = 2, to = 2, length.out = frame_num)

# 平均パラメータを指定
mu <- 0

# サンプルサイズを指定
N <- 11


# λ軸の範囲を指定
u <- 5
lambda_max <- (a_vals / b_vals) |> # 期待値
  max() |> 
  (\(.) {. * 2})() |> # 倍率を指定
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# λ軸の値を作成
lambda_vec <- seq(from = 0, to = lambda_max, length.out = 1001)
lambda_vec[lambda_vec == 0] <- 1e-5 # 作図用に値を調整

# x軸の範囲を指定
x_size <- sqrt(1/lambda_max) * 4

# x軸の値を作成
x_vec <- seq(from = mu-x_size, to = mu+x_size, length.out = 1001)


### サンプル(精度パラメータ)の作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"


# 確率密度軸の範囲を設定
u <- 0.05
gamma_dens_max <- ((a_vals - 1) / b_vals) |>  # 最頻値
  dgamma(x = _,  shape = a_vals, rate  = b_vals) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
gauss_dens_max <- dnorm(x = mu, mean = mu, sd = sqrt(1/lambda_max))

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ## パラメータの生成
  
  # パラメータを取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # サンプルの期待値を計算
  E_lambda <- a / b
  E_sigma  <- sqrt(1/E_lambda)
  
  # パラメータを生成
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)  # 描画範囲の最大値を取得
  pctl_n   <- seq(from = 0, to = pctl_max, length.out = N) # 等間隔の累積確率に設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)      # 分布の形状に応じて設定
  
  # 作図用に値を調整
  lambda_n[lambda_n == 0]         <- 1e-10
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # パラメータを格納
  param_df <- tibble::tibble(
    n      = 1:N,      # サンプル番号
    lambda = lambda_n, # 確率変数(精度パラメータ)
    sigma  = sqrt(1/lambda), # 標準偏差パラメータ
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 期待値における確率密度
  )
  
  ## サンプルの生成分布の作図
  
  # ガンマ分布を計算
  gamma_df <- tidyr::tibble(
    lambda = lambda_vec, # 確率変数
    dens   = dgamma(x = lambda, shape = a, rate = b) # 確率密度
  )
  
  # ラベル用の文字列を作成
  gamma_param_lbl <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "b == ", round(b, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  sample_lbl <- paste0("lambda == ", round(lambda_n, digits = 2)) |> 
    parse(text = _)
  
  # ガンマ分布を作図
  gamma_graph <- ggplot() + 
    geom_line(
      data    = gamma_df, 
      mapping = aes(x = lambda, y = dens), 
      color = "#00A968", linewidth = 1
    ) + # パラメータの生成分布
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = param_df, 
      mapping = aes(x = lambda, y = 0, color = factor(n)), 
      size = 3
    ) + # パラメータのサンプル
    scale_color_hue(labels = sample_lbl) + # サンプルのラベル
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, lambda_max), 
      ylim = c(0, gamma_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gamma distribution", 
      subtitle = gamma_param_lbl, 
      color = "sample", 
      x = expression(lambda), 
      y = expression(p(lambda ~"|"~ a, b))
    )
  
  ## サンプルによる分布の作図
  
  # パラメータの期待値によるガウス分布を計算
  E_gauss_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = mu, sd = E_sigma) # 確率密度
  )
  
  # サンプルごとにガウス分布を計算
  res_gauss_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n],    # 精度パラメータ
      sigma  = sqrt(1/lambda), # 標準偏差パラメータ
      dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
    )
  
  # ラベル用の文字列を作成
  gauss_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "E(lambda) == ", round(E_lambda, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルごとにガウス分布を作図
  gauss_graph <- ggplot() + 
    geom_line(
      data    = E_gauss_df, 
      mapping = aes(x = x, y = dens), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値による分布
    geom_line(
      data = res_gauss_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプルによる分布
    geom_segment(
      data    = param_df, 
      mapping = aes(x = -Inf, y = dens_max, xend = mu, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
    scale_color_hue(labels = sample_lbl) + # パラメータのラベル
    coord_cartesian(
      xlim = c(mu-x_size, mu+x_size), 
      ylim = c(0, gauss_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gaussian distribution", 
      subtitle = gauss_param_lbl, 
      color = "parameter", 
      x = expression(x), 
      y = expression(p(x ~"|"~ mu, lambda^{-1}))
    )
  
  ## 軸変換の作図
  
  # 軸変換曲線を計算
  adapt_df <- tibble::tibble(
    lambda = lambda_vec,     # 精度パラメータ
    sigma  = sqrt(1/lambda), # 標準偏差パラメータ
    dens   = dnorm(x = mu, mean = mu, sd = sigma) # 期待値における確率密度
  )
  
  # 軸変換を作図
  lambda_to_dens_graph <- ggplot() + 
    geom_line(
      data    = adapt_df, 
      mapping = aes(x = lambda, y = dens), 
      linewidth = 1
    ) + # 軸変換曲線
    geom_segment(
      data    = param_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_segment(
      data    = param_df, 
      mapping = aes(x = lambda, y = dens_max, xend = Inf, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = param_df, 
      mapping = aes(x = lambda, y = dens_max, color = factor(n)), 
      size = 3
    ) + # 軸変換の点
    scale_color_hue(labels = sample_lbl) + # パラメータのラベル
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, lambda_max), 
      ylim = c(0, gauss_dens_max)
    ) + # 描画範囲
    labs(
      color = "parameter", 
      x = expression(lambda), 
      y = expression(p(x == mu ~"|"~ mu, lambda^{-1}))
    )
  
  ## グラフの出力
  
  # グラフを並べて描画
  wrap_lambda_graph <- patchwork::wrap_plots(
    gamma_graph, patchwork::plot_spacer(), 
    lambda_to_dens_graph, gauss_graph, 
    nrow = 2, ncol = 2, guides = "collect"
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_lambda_graph, 
    width = 16, height = 12, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gamma/generate_gaussian/gauss_lambda.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


### サンプル(標準偏差パラメータ)の作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"


# 確率密度軸の範囲を設定
u <- 0.05
gamma_dens_max <- ((a_vals - 1) / b_vals) |>  # 最頻値
  dgamma(x = _,  shape = a_vals, rate  = b_vals) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
gauss_dens_max <- dnorm(x = mu, mean = mu, sd = sqrt(1/lambda_max))

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ## パラメータの生成
  
  # パラメータを取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # サンプルの期待値を計算
  E_lambda <- a / b
  E_sigma  <- sqrt(1/E_lambda)
  
  # パラメータを生成
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)  # 描画範囲の最大値を取得
  pctl_n   <- seq(from = 0, to = pctl_max, length.out = N) # 等間隔の累積確率に設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)      # 分布の形状に応じて設定
  
  # 作図用に値を調整
  lambda_n[lambda_n == 0]         <- 1e-5 # (微小値が小さすぎると'出力後'の軸変換図で意図しない線が生じる)
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # パラメータを格納
  param_df <- tibble::tibble(
    n      = 1:N,      # サンプル番号
    lambda = lambda_n, # 確率変数(精度パラメータ)
    sigma  = sqrt(1/lambda), # 標準偏差パラメータ
    dens_sigma = dnorm(x = mu+sigma, mean = mu, sd = sigma) # 期待値から標準偏差1つ分離れた値における確率密度
  )
  
  ## サンプルの生成分布の作図
  
  # ガンマ分布を計算
  gamma_df <- tidyr::tibble(
    lambda = lambda_vec, # 確率変数
    dens   = dgamma(x = lambda, shape = a, rate = b) # 確率密度
  )
  
  # ラベル用の文字列を作成
  gamma_param_lbl <- paste0(
    "list(", 
    "a == ", round(a, digits = 2), ", ", 
    "b == ", round(b, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  sample_lambda_lbl <- paste0("lambda == ", round(lambda_n, digits = 2)) |> 
    parse(text = _)
  
  # ガンマ分布を作図
  gamma_graph <- ggplot() + 
    geom_line(
      data    = gamma_df, 
      mapping = aes(x = lambda, y = dens), 
      color = "#00A968", linewidth = 1
    ) + # パラメータの生成分布
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = param_df, 
      mapping = aes(x = lambda, y = 0, color = factor(n)), 
      size = 3
    ) + # パラメータのサンプル
    scale_color_hue(labels = sample_lambda_lbl) + # サンプルのラベル
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, lambda_max), 
      ylim = c(0, gamma_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gamma distribution", 
      subtitle = gamma_param_lbl, 
      color = "sample", 
      x = expression(lambda), 
      y = expression(p(lambda ~"|"~ a, b))
    )
  
  ## サンプルによる分布の作図
  
  # パラメータの期待値によるガウス分布を計算
  E_gauss_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = mu, sd = E_sigma) # 確率密度
  )
  
  # サンプルごとにガウス分布を計算
  res_gauss_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n],    # 精度パラメータ
      sigma  = sqrt(1/lambda), # 標準偏差パラメータ
      dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
    )
  
  # ラベル用の文字列を作成
  gauss_param_lbl <- paste0(
    "list(", 
    "mu == ", mu, ", ", 
    "E(lambda) == ", round(E_lambda, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルごとにガウス分布を作図
  gauss_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = mu, y = -Inf, xend = mu, yend = Inf), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # y軸線
    geom_line(
      data    = E_gauss_df, 
      mapping = aes(x = x, y = dens), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値による分布
    geom_line(
      data = res_gauss_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプルによる分布
    geom_segment(
      data    = param_df, 
      mapping = aes(x = mu+sigma, y = -Inf, xend = mu+sigma, yend = dens_sigma, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_segment(
      data    = param_df, 
      mapping = aes(x = mu, y = dens_sigma, xend = mu+sigma, yend = dens_sigma, color = factor(n)), 
      linewidth = 1, linetype = "twodash", show.legend = FALSE
    ) + # 標準偏差パラメータの範囲
    scale_color_hue(labels = sample_lambda_lbl) + # サンプルのラベル
    coord_cartesian(
      xlim = c(mu-x_size, mu+x_size), 
      ylim = c(0, gauss_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gaussian distribution", 
      subtitle = gauss_param_lbl, 
      color = "parameter", 
      x = expression(x), 
      y = expression(p(x ~"|"~ mu, lambda^{-1}))
    )
  
  ## 軸変換の作図
  
  # 軸変換曲線を計算
  adapt_df <- tibble::tibble(
    lambda = lambda_vec,     # 精度パラメータ
    sigma  = sqrt(1/lambda), # 標準偏差パラメータ
    dens   = dnorm(x = mu, mean = mu, sd = sigma) # 期待値における確率密度
  )
  
  # 恒等関数を作図
  identity_graph <- ggplot() + 
    geom_line(
      data = adapt_df, 
      mapping = aes(x = lambda, y = lambda), 
      linewidth = 1
    ) + # 恒等関数
    geom_segment(
      data    = param_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_segment(
      data    = param_df, 
      mapping = aes(x = lambda, y = lambda, xend = Inf, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = param_df, 
      mapping = aes(x = lambda, y = lambda, color = factor(n)), 
      size = 3
    ) + # 軸変換の点
    scale_color_hue(labels = sample_lambda_lbl) + # サンプルのラベル
    guides(color = "none") + # 凡例の体裁
    labs(
      color = "parameter", 
      x = expression(lambda), 
      y = expression(lambda)
    )
  
  # ラベル用の文字列を作成
  sample_sigma_lbl <- paste0("sigma == ", round(sqrt(1/lambda_n), digits = 2)) |> 
    parse(text = _)
  
  # 軸変換を作図
  lambda_to_sigma_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = 0, y = -Inf, xend = 0, yend = Inf),
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # y軸線
    geom_line(
      data    = adapt_df,
      mapping = aes(x = sigma, y = lambda),
      linewidth = 1
    ) + # 軸変換曲線
    geom_segment(
      data    = param_df,
      mapping = aes(x = -Inf, y = lambda, xend = 0, yend = lambda, color = factor(n)),
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_segment(
      data    = param_df,
      mapping = aes(x = sigma, y = lambda, xend = sigma, yend = Inf, color = factor(n)),
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_segment(
      data    = param_df,
      mapping = aes(x = 0, y = lambda, xend = sigma, yend = lambda, color = factor(n)),
      linewidth = 1, linetype = "twodash"
    ) + # 標準偏差パラメータの範囲
    geom_point(
      data    = param_df,
      mapping = aes(x = sigma, y = lambda, color = factor(n)),
      size = 3
    ) + # 軸変換の点
    scale_color_hue(labels = sample_sigma_lbl) + # サンプルのラベル
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(-x_size, x_size), 
      ylim = c(0, lambda_max)
    ) + # x軸との対応用
    labs(
      color = "parameter", 
      x = expression(sigma == sqrt(frac(1, lambda))), 
      y = expression(lambda == frac(1, sigma^2))
    )
  
  ## グラフの出力
  
  # グラフを並べて描画
  wrap_sigma_graph <- patchwork::wrap_plots(
    gamma_graph, gauss_graph, 
    identity_graph, lambda_to_sigma_graph, 
    nrow = 2, ncol = 2, guides = "collect"
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_sigma_graph, 
    width = 16, height = 12, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gamma/generate_gaussian/gauss_sigma.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


