
# ベータ分布 -------------------------------------------------------------------

# 二項分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# ハイパラとサンプル分布の形状の関係 -------------------------------------------------------------------

### パラメータの設定 -----

# フレーム数を指定
frame_num <- 100

# ハイパーパラメータを指定
alpha_vals <- seq(from = 0, to = 10, length.out = frame_num+1)[-1]
beta_vals  <- seq(from = 2, to = 1, length.out = frame_num)

# 試行回数を指定
M <- 10

# サンプルサイズを指定
N <- 10


# Φ軸の値を作成
phi_vec <- seq(from = 0, to = 1, length.out = 1001)

# x軸の値を作成
x_vec <- 0:M


### サンプルの作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"


# 確率密度軸の範囲を設定
u <- 0.05
dens_max <- ((alpha_vals - 1) / (alpha_vals + beta_vals - 2)) |> # 最頻値
  dbeta(x = _, shape1 = alpha_vals, shape2 = beta_vals) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ## パラメータの生成
  
  # ハイパーパラメータを取得
  alpha <- alpha_vals[i]
  beta  <- beta_vals[i]
  
  # サンプル(パラメータ)の期待値を計算
  E_phi <- alpha / (alpha + beta)
  
  # パラメータを生成
  pctl_n <- seq(from = 0, to = 1, length.out = N) # 等間隔の累積確率に設定
  phi_n  <- qbeta(p = pctl_n, shape1 = alpha, shape2 = beta) # 分布の形状に応じて設定
  
  # パラメータを格納
  param_df <- tibble::tibble(
    n   = 1:N,  # サンプル番号
    phi = phi_n # 確率変数
  )
  
  ## サンプルの生成分布の作図
  
  # ベータ分布を計算
  beta_df <- tibble::tibble(
    phi  = phi_vec, # 確率変数
    dens = dbeta(x = phi, shape1 = alpha, shape2 = beta) # 確率密度
  )
  
  # ラベル用の文字列を作成
  beta_param_lbl <- paste0(
    "list(", 
    "alpha == ", round(alpha, digits = 2), ", ", 
    "beta == ", round(beta, digits = 2), ")"
  ) |> 
    parse(text = _)
  sample_lbl <- paste0("phi == ", round(phi_n, digits = 2)) |> 
    parse(text = _)
  
  # ベータ分布を作図
  beta_graph <- ggplot() + 
    geom_line(
      data    = beta_df, 
      mapping = aes(x = phi, y = dens), 
      color = "#00A968", linewidth = 1
    ) + # パラメータの生成分布
    geom_vline(
      xintercept = E_phi, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = phi, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = param_df, 
      mapping = aes(x = phi, y = 0, color = factor(n)), 
      size = 3
    ) + # パラメータのサンプル
    scale_color_hue(labels = sample_lbl) + # サンプルのラベル
    guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, 1), 
      y = c(0, dens_max)
    ) + # 描画範囲
    labs(
      title = "Beta distribution", 
      subtitle = beta_param_lbl, 
      color = "sample", 
      x = expression(phi), 
      y = expression(p(phi ~"|"~ alpha, beta))
    )
  
  ## サンプルによる分布の作図
  
  # パラメータの期待値によるベルヌーイ分布を計算
  E_binom_df <- tibble::tibble(
    x    = x_vec, # 確率変数
    prob = dbinom(x = x, size = M, prob = E_phi) # 確率
  )
  
  # サンプルごとに二項分布を計算
  res_binom_df <- tidyr::expand_grid(
    n = 1:N,  # サンプル番号
    x = x_vec # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      phi  = phi_n[n], # パラメータ
      prob = dbinom(x = x, size = M, prob = phi) # 確率
    )
  
  # ラベル用の文字列を作成
  binom_param_lbl <- paste0(
    "list(", 
    "M == ", M, ", ", 
    "E(phi) == ", round(E_phi, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルごとに二項分布を作図
  binom_graph <- ggplot() + 
    geom_vline(
      xintercept = M*E_phi, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # パラメータの期待値の位置
    geom_bar(
      data = E_binom_df, 
      mapping = aes(x = x, y = prob), 
      stat = "identity", position = "identity", 
      fill = NA, color = "red", linetype = "dashed"
    ) + # 期待値による分布
    geom_vline(
      data    = param_df, 
      mapping = aes(xintercept = M*phi, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルごとの期待値の位置
    geom_line(
      data    = res_binom_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      linewidth = 1
    ) + # サンプルによる分布
    geom_point(
      data    = res_binom_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      size = 3, shape = "circle open", show.legend = FALSE
    ) + # サンプルによる分布
    scale_x_continuous(
      breaks = x_vec, minor_breaks = FALSE, 
      sec.axis = sec_axis(
        transform = ~ ./M, name = expression(frac(x, M))
      )
    ) + # x軸目盛
    scale_color_hue(labels = sample_lbl) + # サンプルのラベル
    #guides(color = "none") + # 凡例の体裁
    coord_cartesian(
      xlim = c(0, M), 
      ylim = c(0, 1)
    ) + # 描画範囲
    labs(
      title = "Binomial distribution", 
      subtitle = binom_param_lbl, 
      color = "parameter", 
      x = expression(x), 
      y = expression(p(x ~"|"~ M, phi))
    )
  
  # グラフの出力
  
  # グラフを並べて描画
  wrap_graph <- patchwork::wrap_plots(
    beta_graph, binom_graph, 
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
  magick::image_write_video(path = "figure/beta/generate_binomial/binom.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


