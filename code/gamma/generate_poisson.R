
# ガンマ分布 -------------------------------------------------------------------

# ポアソン分布の生成


# パッケージの読込 -------------------------------------------------------------

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
a_vals <- seq(from = 2, to = 2, length.out = frame_num)
b_vals <- seq(from = 2, to = 2, length.out = frame_num)

# 変化させるパラメータ用
b_vals <- seq(from = 0, to = 20, length.out = frame_num+1)[-1] # (0を除去)


### 変数の設定 -----

# λ軸の範囲を設定
u <- 1
k <- 1
lambda_min <- 0 # 整数を指定:(x軸との対応用)
lambda_max <- (a_vals / b_vals) |> # 期待値
  max() |> 
  (\(.) {. * k})() |> # 定数倍
  (\(.) {ceiling(. /u)*u})() |> # u単位で切り上げ
  as.integer() # (x軸との対応用)
cat("λ size:", lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec <- seq(from = lambda_min, to = lambda_max, length.out = 1001)
lambda_vec[lambda_vec == 0] <- 1e-10 # 値を調整:(infの回避用)
head(lambda_vec)


# x軸の範囲を設定
x_min <- ceiling(lambda_min) # (固定)
x_max <- floor(lambda_max)   # (固定)
cat("x size:", x_min, x_max)

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, by = 1)
head(x_vec)


### 対応関係の作図 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"

# サンプルサイズを指定
N <- 6


# p(λ)軸の範囲を設定
u <- 0.05
gen_dens_max <- 5
gen_dens_max <- dgamma(
  x     = (a_vals-1) / b_vals, # 最頻値
  shape = a_vals, 
  rate  = b_vals
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("p(λ) size:", gen_dens_max)

# p(x)軸の範囲を設定
u <- 0.05
smp_prob_max <- dpois(
  x      = floor(lambda_min), # 最頻値
  lambda = lambda_min # 最大値
) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("p(x) size:", gen_prob_max)

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ##### パラメータの生成 -----
  
  # 生成分布のパラメータを取得
  a <- a_vals[i] # 形状パラメータ
  b <- b_vals[i] # 尺度パラメータ
  
  # 統計量を計算
  E_lambda <- a / b
  #s_lambda <- sqrt(a / b^2)
  s_lambda <- sqrt(a) / b
  E_x      <- E_lambda
  s_x      <- sqrt(E_lambda)
  
  # サンプル分布のパラメータを生成
  pctl_min <- pgamma(q = lambda_min, shape = a, rate = b)         # 描画範囲の最小値を取得
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)         # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔に累積確率を設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)             # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  lambda_n[lambda_n == 0]         <- 1e-10
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # サンプル分布のパラメータを格納
  smp_data_df <- tibble::tibble(
    n      = 1:N,     # サンプル番号
    lambda = lambda_n # サンプル値
  )
  
  ##### 生成分布の作図 -----
  
  # 生成分布の確率密度を計算
  gen_dens_df <- tidyr::tibble(
    lambda = lambda_vec, # 確率変数
    a      = a, # 形状パラメータ
    b      = b, # 尺度パラメータ
    dens   = dgamma(x = lambda, shape = a, rate = b) # 確率密度
  )
  
  # 生成分布のラベルを作成
  gen_param_lbl <- sprintf(
    fmt = "list(a == '%s', b == '%s')", 
    formatC(a, digits = 2, format = "f"), 
    formatC(b, digits = 2, format = "f")
  ) |> 
    parse(text = _)
  gen_stats_lbl <- sprintf(
    fmt = paste0(
      "list(", 
      "paste(E(lambda) == frac(a, b), phantom() == '%s'), ", 
      "paste(s(lambda) == sqrt(frac(a, b^2)), phantom() == '%s')", 
      ")"
    ), 
    formatC(E_lambda, digits = 2, format = "f"), 
    formatC(s_lambda, digits = 2, format = "f")
  ) |> 
    parse(text = _)
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(lambda))
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "lambda[", 1:N, "]"
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
      size = 4, show.legend = FALSE
    ) + # サンプル
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~ ., 
        breaks    = c(E_lambda, lambda_n), 
        labels    = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_linetype_manual(
      breaks = "generator", 
      values = "solid", 
      labels = gen_param_lbl, 
      name   = "generator"
    ) + # (凡例の表示用)
    guides(
      linetype = guide_legend(override.aes = list(linewidth = 0.5))
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min-0.5, lambda_max+0.5), 
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
  
  # 期待値によ分布の確率を計算
  E_prob_df <- tidyr::tibble(
    x      = x_vec, # 確率変数
    lambda = E_lambda, # 期待値
    prob   = dpois(x = x, lambda = lambda) # 確率
  )
  
  # サンプル分布の確率を計算
  smp_prob_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      lambda = lambda_n[n], # 期待値パラメータ
      prob   = dpois(x = x, lambda = lambda) # 確率
    )
  
  # 期待値のラベルを作成
  E_data_lbl  <- expression(E(lambda))
  E_param_lbl <- paste0(
    "E(lambda) == '", formatC(E_lambda, digits = 2, format = "f"), "'"
  ) |> 
    parse(text = _)
  E_stats_lbl <- sprintf(
    fmt = paste0(
      "list(", 
      "paste(E(x) == frac(a, b), phantom() == '%s'), ", 
      "paste(s(x) == sqrt(frac(a, b)), phantom() == '%s')", 
      ")"
    ), 
    formatC(E_x, digits = 2, format = "f"), 
    formatC(s_x, digits = 2, format = "f")
  ) |> 
    parse(text = _)
  
  # サンプルのラベルを作成
  smp_data_lbl <- paste0(
    "lambda[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "lambda[", 1:N, "] == '", formatC(lambda_n, digits = 2, format = "f"), "'"
  ) |> 
    parse(text = _)
  
  # サンプル分布を作図
  smp_graph <- ggplot() + 
    geom_vline(
      xintercept = E_lambda, 
      color = "red", linewidth = 1, linetype ="dashed"
    ) + # 期待値との対応
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
    geom_bar(
      data    = E_prob_df, 
      mapping = aes(x = x, y = prob, color = "expected"), 
      stat = "identity", position = "identity", 
      fill = NA, linewidth = 1, linetype = "dotdash"
    ) + # 期待値による分布
    geom_line(
      data    = E_prob_df, 
      mapping = aes(x = x, y = prob, color = "expected"), 
      linewidth = 1, linetype = "dotdash"
    ) + # 期待値による分布
    geom_line(
      data    = smp_prob_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      linewidth = 1
    ) + # サンプル分布
    geom_point(
      data    = smp_prob_df, 
      mapping = aes(x = x, y = prob, color = factor(n)), 
      size = 3, shape = "circle open"
    ) + # サンプル分布
    scale_x_continuous(
      breaks = x_vec, # x軸目盛
      sec.axis = sec_axis(
        trans  = ~ ., 
        breaks = c(E_lambda, lambda_n), 
        labels = c(E_data_lbl, smp_data_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_color_manual(
      breaks = c("expected", 1:N), 
      values = c("red", scales::hue_pal()(n = N)), 
      labels = c(E_param_lbl, smp_param_lbl), 
      name   = "sample" 
    ) + # (凡例の表示用)
    guides(
      color = guide_legend(override.aes = list(linewidth = 0.5, size = 2))
    ) + 
    coord_cartesian(
      xlim = c(x_min-0.5, x_max+0.5), 
      ylim = c(0, smp_prob_max)
    ) + # (軸の対応用)
    labs(
      title = "Poisson istribution", 
      subtitle = E_stats_lbl, 
      x = expression(x), 
      y = expression(p(x ~"|"~ lambda))
    )
  
  ##### 対応関係の作図 -----
  
  # 凡例を取得
  gen_legend <- cowplot::get_legend(gen_graph)
  smp_legend <- cowplot::get_legend(smp_graph)
  
  # 凡例幅を固定
  legend_size <- 5
  gen_legend$widths <- unit(rep(legend_size, times = length(gen_legend$widths)), units = "cm")
  smp_legend$widths <- unit(rep(legend_size, times = length(smp_legend$widths)), units = "cm")
  
  # 凡例を並べて描画:(余白サイズの調整用)
  legend_graph <- cowplot::plot_grid(
    gen_legend, smp_legend, 
    nrow = 2, ncol = 1
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
  tmp_comb_graph <- cowplot::plot_grid(
    tmp_gen_graph, tmp_smp_graph, 
    nrow = 2, ncol = 1, 
    align = "hv" # (グラフ位置のズレ対策用)
  )
  comb_graph <- cowplot::plot_grid(
    tmp_comb_graph, legend_graph, 
    nrow = 1, ncol = 2, 
    rel_widths = c(1, 0.2) # (凡例位置の調整用)
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


### アニメーションの変換 -----

# ファイル名の末尾を指定
add_str <- ""

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(
    path = paste0("figure/gamma/generate_poisson/gam_to_pois", add_str, ".mp4"), 
    framerate = 30
  ) -> tmp_path # mp4ファイルを書出


