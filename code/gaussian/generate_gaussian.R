
# 1次元ガウス分布 --------------------------------------------------------------

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
frame_num <- 101

# 生成分布のパラメータを指定
mu_vals    <- seq(from = -5, to = 5, length.out = frame_num)
sigma_vals <- seq(from = 1, to = 1, length.out = frame_num)
#mu_vals    <- seq(from = 0, to = 0, length.out = frame_num)
#sigma_vals <- seq(from = 0, to = 10, length.out = frame_num+1)[-1] # (0を除去)

# サンプル分布のパラメータを指定
s <- 1


### 変数の設定 -----

# m軸の範囲を設定
u <- 5
sgm_num <- 1 # 倍率を指定
m_min <- (mu_vals - sgm_num*sigma_vals) |> # 基準値を指定
  min() |> 
  (\(.) {floor(. /u)*u})() # u単位で切り下げ
m_max <- (mu_vals + sgm_num*sigma_vals) |> # 基準値を指定
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
m_min; m_max

# m軸の値を作成
m_vec <- seq(from = m_min, to = m_max, length.out = 1001)
head(m_vec)


# x軸の範囲を設定
x_min <- m_min
x_max <- m_max
x_min; x_max

# x軸の値を作成
x_vec <- m_vec
head(x_vec)


### 対応関係の作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"

# サンプルサイズを指定
N <- 10


# 確率密度軸の範囲を設定
u <- 0.05
gen_dens_max <- 1
gen_dens_max <- dnorm(
  x    = mu_vals, # 最頻値
  mean = mu_vals, 
  sd   = sigma_vals
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
smp_dens_max <- dnorm(
  x    = mu_vals, # 最頻値
  mean = mu_vals, 
  sd   = s
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
gen_dens_max; smp_dens_max

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ##### パラメータの生成 -----
  
  # 生成分布のパラメータを取得
  mu    <- mu_vals[i]
  sigma <- sigma_vals[i]
  
  # 期待値を計算
  E_m <- mu
  E_x <- E_m
  
  # サンプル分布のパラメータを生成
  pctl_min <- pnorm(q = m_min, mean = mu, sd = sigma)             # 描画範囲の最小値を取得
  pctl_max <- pnorm(q = m_max, mean = mu, sd = sigma)             # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔に累積確率を設定
  m_n      <- qnorm(p = pctl_n, mean = mu, sd = sigma)            # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  m_n[m_n < m_min] <- m_min
  m_n[m_n > m_max] <- m_max
  
  # サンプル分布のパラメータを格納
  smp_data_df <- tibble::tibble(
    n    = 1:N, # サンプル番号
    m    = m_n  # サンプル値
  )
  
  ##### 生成分布の作図 -----
  
  # 生成分布の確率密度を計算
  gen_dens_df <- tidyr::tibble(
    m    = m_vec, # 確率変数
    dens = dnorm(x = m, mean = mu, sd = sigma) # 確率密度
  )
  
  # 生成分布のラベルを作成
  gen_param_lbl <- paste0(
    "list(", 
    "mu == ", round(mu, digits = 2), ", ", 
    "sigma == ", round(sigma, digits = 2), 
    ")"
  ) |> 
    parse(text = _)
  gen_stats_lbl <- paste0(
    "list(", 
      "paste(", 
        "E(m) == mu, {} == ", round(E_m, digits = 2), 
      "), ", 
      "paste(", 
        "V(m) == sigma^2, {} == ", round(sigma^2, digits = 2), 
      ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(m))
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "m[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "m[", 1:N, "] == ", round(m_n, digits = 2)
  ) |> 
    parse(text = _)
  
  # 生成分布を作図
  gen_graph <- ggplot() + 
    geom_vline(
      xintercept = E_m, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = m, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_line(
      data    = gen_dens_df, 
      mapping = aes(x = m, y = dens, linetype = "generator"), 
      linewidth = 1
    ) + # 生成分布
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = m, y = 0, color = factor(n)), 
      size = 4
    ) + # サンプル
    scale_x_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_m, m_n), 
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
      xlim = c(m_min, m_max), 
      ylim = c(0, gen_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gaussian distribution", 
      subtitle = gen_stats_lbl, 
      color = "sample", 
      x = expression(m), 
      y = expression(p(m ~"|"~ mu, sigma^2))
    )
  
  ##### サンプル分布の作図 -----
  
  # 期待値による分布の確率密度を計算
  E_dens_df <- tidyr::tibble(
    x    = x_vec, # 確率変数
    dens = dnorm(x = x, mean = E_m, sd = s) # 確率密度
  )
  
  # サンプル分布の確率密度を計算
  smp_dens_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      m    = m_n[n], # 平均パラメータ
      s    = s,      # 標準偏差パラメータ
      dens = dnorm(x = x, mean = m, sd = s) # 確率密度
    )
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(m))
  E_param_lbl <- paste0(
    "list(", 
    "E(m) == ", round(E_x, digits = 2), ", ", 
    "s == ", s, 
    ")"
  ) |> 
    parse(text = _)
  E_stats_lbl <- paste0(
    "list(", 
      "paste(", 
        "E(x) == mu, {} == ", round(E_x, digits = 2), 
        "), ", 
      "paste(", 
        "V(x) == s^2, {} == ", round(s^2, digits = 2), 
      ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "m[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "list(", 
    "m[", 1:N, "] == ", round(m_n, digits = 2), ", ", 
    "s == ", s, 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプル分布を作図
  smp_graph <- ggplot() + 
    geom_vline(
      xintercept = E_m, 
      color = "red", linewidth = 1, linetype ="dashed"
    ) + # 期待値との対応
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = m, color = factor(n)), 
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
    ) + # サンプルによる分布
    scale_x_continuous(
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_m, m_n), 
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
    ) + 
    coord_cartesian(
      xlim = c(x_min, x_max), 
      ylim = c(0, smp_dens_max)
    ) + # (軸の対応用)
    labs(
      title = "Gaussian distribution", 
      subtitle = E_stats_lbl, 
      x = expression(x), 
      y = expression(p(x ~"|"~ m, s^2))
    )
  
  ##### 対応関係の作図 -----
  
  # グラフを並べて描画
  comb_graph <- cowplot::plot_grid(
    gen_graph, smp_graph, 
    nrow = 2, ncol = 1, 
    align = "hv" # (グラフ位置のズレ対策用)
  )
  
  ##### グラフの出力 -----
  
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
  magick::image_write_video(path = "figure/gaussian/generate_gaussian/gauss_to_gauss.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出


