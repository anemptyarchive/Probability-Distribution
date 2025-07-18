
# 1次元ガウス分布 --------------------------------------------------------------

# 1次元ガウス分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(cowplot)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# ハイパラとサンプル分布の形状の関係 -------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num <- 100

# 生成分布のパラメータを指定
mu_vals    <- seq(from = 0, to = 0, length.out = frame_num)
sigma_vals <- seq(from = 0, to = 10, length.out = frame_num+1)[-1]

# サンプル分布のパラメータを指定
s <- 1

# サンプルサイズを指定
N <- 11


# m軸の範囲を設定
sigma_num <- 2
u <- 5
m_min <- (mu_vals - sigma_num*sigma_vals) |> 
  min() |> 
  (\(.) {floor(. /u)*u})() # u単位で切り下げ
m_max <- (mu_vals + sigma_num*sigma_vals) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# m軸の値を作成
m_vec <- seq(from = m_min, to = m_max, length.out = 1001)

# x軸の範囲を設定
x_min <- m_min
x_max <- m_max

# x軸の値を作成
x_vec <- m_vec


### サンプル(平均パラメータ)の作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"


# 確率密度軸の範囲を設定
u <- 0.05
generator_dens_max <- dnorm(x = 0, mean = 0, sd = min(sigma_vals)) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
generator_dens_max <- 0.5
sample_dens_max <- dnorm(x = 0, mean = 0, sd = s) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  #### パラメータの生成 -----
  
  # パラメータを取得
  mu    <- mu_vals[i]
  sigma <- sigma_vals[i]
  
  # サンプルの期待値を計算
  E_m <- mu
  
  # パラメータを生成
  pctl_min <- pnorm(q = m_min, mean = mu, sd = sigma)             # 描画範囲の最小値を取得
  pctl_max <- pnorm(q = m_max, mean = mu, sd = sigma)             # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔の累積確率に設定
  m_n      <- qnorm(p = pctl_n, mean = mu, sd = sigma)            # 分布の形状に応じて設定
  
  # 作図用に値を調整
  m_n[m_n < m_min] <- m_min
  m_n[m_n > m_max] <- m_max
  
  # パラメータを格納
  param_df <- tibble::tibble(
    n = 1:N, # サンプル番号
    m = m_n, # 確率変数(平均パラメータ)
    dens_max = dnorm(x = m, mean = m, sd = s) # 最頻値における確率密度
  )
  
  #### 生成分布の作図 -----
  
  # ガウス分布を計算
  generator_df <- tidyr::tibble(
    m    = m_vec, # 確率変数
    dens = dnorm(x = m, mean = mu, sd = sigma) # 確率密度
  )
  
  # ラベル用の文字列を作成
  generator_param_lbl <- paste0(
    "list(", 
    "mu == ", round(mu, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # ガウス分布を作図
  generator_graph <- ggplot() + 
    geom_vline(
      xintercept = E_m, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = m, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_line(
      data    = generator_df, 
      mapping = aes(x = m, y = dens), 
      linewidth = 1
    ) + # 生成分布
    geom_point(
      data    = param_df, 
      mapping = aes(x = m, y = 0, color = factor(n)), 
      size = 3
    ) + # サンプル
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # 図の体裁
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(m_min, m_max), 
      ylim = c(0, generator_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gaussian distribution", 
      subtitle = generator_param_lbl, 
      x = expression(m), 
      y = expression(p(m ~"|"~ mu, sigma))
    )
  
  #### サンプル分布の作図 -----
  
  # サンプルの期待値によるガウス分布を計算
  E_sample_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = E_m, sd = s) # 確率密度
  )
  
  # サンプルごとにガウス分布を計算
  res_sample_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      m    = m_n[n], # 平均パラメータ
      dens = dnorm(x = x, mean = m, sd = s) # 確率密度
    )
  
  # ラベル用の文字列を作成
  sample_param_lbl <- paste0(
    "list(", 
    "E(m) == ", round(E_m, digits = 2), ", ", 
    "s == ", s, 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルごとにガウス分布を作図
  sample_graph <- ggplot() + 
    geom_vline(
      xintercept = E_m, 
      color = "red", linewidth = 1, linetype ="dashed"
    ) + # 期待値との対応
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = m, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
    geom_line(
      data    = E_sample_df, 
      mapping = aes(x = x, y = dens), 
      color = "red", linewidth = 1, linetype = "dotdash"
    ) + # 期待値による分布
    geom_line(
      data    = res_sample_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプルによる分布
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # 図の体裁
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(x_min, x_max), 
      ylim = c(0, sample_dens_max)
    ) + # 描画範囲
    labs(
      title = "Gaussian distribution", 
      subtitle = sample_param_lbl, 
      x = expression(x), 
      y = expression(p(x ~"|"~ m, s))
    )
  
  #### 凡例の作図 -----
  
  # ラベル用の文字列を作成
  sample_lbl <- paste0("m == ", round(m_n, digits = 2)) |> 
    parse(text = _)
  
  # 凡例用の図を作成
  dummy_graph <- ggplot() + 
    geom_point(
      data    = param_df, 
      mapping = aes(x = mu, y = dens_max, color = factor(n)), 
      size = 3
    ) + # サンプル
    geom_line(
      data = res_sample_df, 
      mapping = aes(x = x, y = dens, color = factor(n)), 
      linewidth = 1
    ) + # サンプルによる分布
    scale_color_hue(labels = sample_lbl, name = "parameter") + # パラメータのラベル
    labs(title ="dummy")
  
  #### グラフの出力 -----
  
  # 凡例を取得
  legend <- cowplot::get_legend(dummy_graph)
  
  # 凡例用の図を作成
  legend_graph <- cowplot::ggdraw(legend) + 
    theme(plot.background = element_rect(fill = "white", color = NA)) # (透過背景の対策用)
  
  # グラフを並べて描画
  tmp_graph <- cowplot::plot_grid(
    generator_graph, sample_graph, 
    nrow = 2, ncol = 1, 
    align = "hv" # (グラフ位置のズレ対策用)
  )
  comb_graph <- cowplot::plot_grid(
    tmp_graph, legend_graph, 
    nrow = 1, ncol = 2, 
    rel_widths = c(1, 0.1) # (凡例の位置の調整用)
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = comb_graph, 
    width = 12, height = 9, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gaussian/generate_gaussian/gauss.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


