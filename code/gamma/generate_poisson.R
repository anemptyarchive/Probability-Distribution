
# ガンマ分布 -------------------------------------------------------------------

# ポアソン分布の生成


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

# x軸の値を作成
x_vec <- seq(from = 0, to = lambda_max, by = 1)


### サンプルの作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"


# 確率密度軸の範囲を設定
u <- 0.05
dens_max <- ((a_vals - 1) / b_vals) |>  # 最頻値
  dgamma(x = _,  shape = a_vals, rate  = b_vals) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ## パラメータの生成
  
  # パラメータを取得
  a <- a_vals[i]
  b <- b_vals[i]
  
  # サンプル(パラメータ)の期待値を計算
  E_lambda <- a / b
  
  # パラメータを生成
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)  # 描画範囲の最大値を取得
  pctl_n   <- seq(from = 0, to = pctl_max, length.out = N) # 等間隔の累積確率に設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)      # 分布の形状に応じて設定
  
  # 作図用に値を調整
  lambda_n[lambda_n == 0]         <- 1e-10
  lambda_n[lambda_n > lambda_max] <- lambda_max

  # パラメータを格納
  param_df <- tibble::tibble(
    n      = 1:N,     # サンプル番号
    lambda = lambda_n # 確率変数
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
      ylim = c(0, dens_max)
    ) + # 描画範囲
    labs(
      title = "Gamma distribution", 
      subtitle = gamma_param_lbl, 
      color = "sample", 
      x = expression(lambda), 
      y = expression(p(lambda ~"|"~ a, b))
    )
  
  ## サンプルによる分布の作図
  
  # パラメータの期待値によるガンマ分布を計算
  E_pois_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    prob = dpois(x = x, lambda = E_lambda) # 確率
  )
  
  # サンプルごとにガウス分布を計算
  res_pois_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n], # パラメータ
      prob   = dpois(x = x, lambda = lambda) # 確率
    )
  
  # ラベル用の文字列を作成
  pois_param_lbl <- paste0("E(lambda) == ", round(E_lambda, digits = 2)) |> 
    parse(text = _)
  
  # サンプルごとにポアソン分布を作図
  pois_graph <- ggplot() + 
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype ="dashed"
    ) + # パラメータの期待値の位置
    geom_bar(
      data    = E_pois_df, 
      mapping = aes(x = x, y = prob), 
      stat = "identity", position = "identity", 
      fill = NA, color = "red", linetype = "dashed"
    ) + # 期待値による分布
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルごとの期待値の位置
    geom_line(
      data    = res_pois_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      linewidth = 1
  ) + # サンプルによる分布
    geom_point(
      data    = res_pois_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      size = 3, shape = "circle open", show.legend = FALSE
    ) + # サンプルによる分布
    scale_color_hue(labels = sample_lbl) + # サンプルのラベル
    #guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, lambda_max), 
      ylim = c(0, 1)
    ) + # 描画範囲
    labs(
      title = "Poisson istribution", 
      subtitle = pois_param_lbl, 
      color = "parameter", 
      x = expression(x), 
      y = expression(p(x ~"|"~ lambda))
    )
  
  # グラフの出力
  
  # グラフを並べて描画
  wrap_graph <- patchwork::wrap_plots(
    gamma_graph, pois_graph, 
    nrow = 2, guides = "collect"
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_graph, 
    width = 12, height = 9, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gamma/generate_poisson/pois.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


