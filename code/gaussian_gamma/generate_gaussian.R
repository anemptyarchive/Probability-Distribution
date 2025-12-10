
# ガウス-ガンマ分布 ------------------------------------------------------------

# 1次元ガウス分布の生成


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
m_vals    <- seq(from = 0, to = 0, length.out = frame_num)
beta_vals <- seq(from = 1, to = 1, length.out = frame_num)
a_vals    <- seq(from = 2, to = 2, length.out = frame_num)
b_vals    <- seq(from = 2, to = 2, length.out = frame_num)

# 変化させるパラメータ用
b_vals <- seq(from = 0, to = 10, length.out = frame_num+1)[-1]


### 変数の設定 -----

# λ軸の範囲を設定
u <- 1
k <- 0.25
lambda_min <- 0
lambda_max <- (a_vals / b_vals) |> # 期待値
  max() |> 
  (\(.) {. * k})() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("λ size:", lambda_min, lambda_max)

# λ軸の値を作成
lambda_vec <- seq(from = lambda_min, to = lambda_max, length.out = 101)
lambda_vec[lambda_vec == 0] <- 1e-10 # 値を調整:(infの回避用)
head(lambda_vec)


# m軸の範囲を設定
u <- 1
k <- 1
mu_size_vals <- (1/sqrt(beta_vals * a_vals / b_vals)) |> # 期待値による標準偏差
  (\(.) {. * k})()
mu_min <- (m_vals - mu_size_vals) |> # 偏差
  min() |> 
  (\(.) {floor(. /u)*u})() # u単位で切り下げ
mu_max <- (m_vals + mu_size_vals) |> # 偏差
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("μ size:", mu_min, mu_max)

# m軸の値を作成
mu_vec <- seq(from = mu_min, to = mu_max, length.out = 101)
head(mu_vec)


# x軸の範囲を設定
x_min <- mu_min # (固定)
x_max <- mu_max # (固定)
cat("x size:", x_min, x_max)

# x軸の値を作成
x_vec <- seq(from = x_min, to = x_max, length.out = 1001)
head(x_vec)


### 補助線の設定 -----

# 変換曲線の座標を計算
adapt_line_df <- tibble::tibble(
  lambda   = lambda_vec,     # 生成分布の確率変数
  sigma    = sqrt(1/lambda), # 標準偏差
  dens_max = dnorm(x = 0, mean = 0, sd = sigma) # 最頻値における確率密度
)


### 対応関係の作図 -----

#### 精度パラメータとの関係 -----

# 一時書き出し先を指定
dir_path <- "figure/tmp_folder"

# サンプルサイズを指定
N <- 6

# p(μ, λ)軸の範囲を設定
u <- 0.1
gen_dens_max <- 2.5
N_dens_vals <- dnorm(
  x    = 0, # 最頻値
  mean = 0, 
  sd   = 1/sqrt(beta_vals * (a_vals-1) / b_vals) # 最頻値による標準偏差
)
Gam_dens_vals <- dgamma(
  x     = (a_vals-1) / b_vals, # 最頻値
  shape = a_vals, 
  rate  = b_vals
)
gen_dens_max <- (N_dens_vals * Gam_dens_vals) |> # 最頻値における確率密度
  (\(.) {.[!is.infinite(.)]})() |> # Infを除去
  max(na.rm = TRUE) |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("p(μ, λ):", gen_dens_max)

# 等高線を設定
level_num     <- 11
gen_dens_vals <- seq(from = 0, to = gen_dens_max, length.out = level_num)
head(gen_dens_vals)

# p(x)軸の範囲を設定
u <- 0.05
smp_dens_max <- 1.25
smp_dens_max <- dnorm(
  x    = 0, # 最頻値
  mean = 0, 
  sd   = 1/sqrt(lambda_max) # 最大値による標準偏差
) |> 
  max() |> 
  (\(.) {ceiling(. /u)*u})() # u単位で切り上げ
cat("p(x):", smp_dens_max)

i <- 30

# ハイパラごとに作図
for(i in 1:frame_num) {
  
  ##### パラメータの生成 -----
  
  # 生成分布のパラメータを取得
  m    <- m_vals[i]    # 平均パラメータ
  beta <- beta_vals[i] # 係数パラメータ
  a    <- a_vals[i]    # 形状パラメータ
  b    <- b_vals[i]    # 尺度パラメータ
  
  # 期待値を計算
  E_mu     <- m
  E_lambda <- a / b
  E_x      <- E_mu
  
  # サンプル分布のパラメータを生成
  pctl_min <- pgamma(q = lambda_min, shape = a, rate = b)         # 描画範囲の最小値を取得
  pctl_max <- pgamma(q = lambda_max, shape = a, rate = b)         # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N) # 等間隔に累積確率を設定
  lambda_n <- qgamma(p = pctl_n, shape = a, rate = b)             # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  lambda_n[lambda_n == 0]         <- 1e-10
  lambda_n[lambda_n > lambda_max] <- lambda_max
  
  # サンプル分布のパラメータを生成
  pctl_min <- pnorm(q = mu_min, mean = m, sd = 1/sqrt(beta*E_lambda))  # 描画範囲の最小値を取得
  pctl_max <- pnorm(q = mu_max, mean = m, sd = 1/sqrt(beta*E_lambda))  # 描画範囲の最大値を取得
  pctl_n   <- seq(from = pctl_min, to = pctl_max, length.out = N)      # 等間隔に累積確率を設定
  mu_n     <- qnorm(p = pctl_n, mean = m, sd = 1/sqrt(beta*E_lambda))  # 分布の形状に応じて設定
  
  # 値を調整:(infの回避用)
  mu_n[mu_n < mu_min] <- mu_min
  mu_n[mu_n > mu_max] <- mu_max
  
  # サンプル分布のパラメータを格納
  E_data_df <- tibble::tibble(
    mu       = E_mu,           # 生成分布の期待値
    lambda   = E_lambda,       # 生成分布の期待値
    sigma    = 1/sqrt(lambda), # 標準偏差
    dens_max = dnorm(x = mu, mean = mu, sd = sigma) # 最頻値における確率密度
  )
  smp_data_df <- tibble::tibble(
    n        = 1:N,      # サンプル番号
    mu       = mu_n,     # サンプル値
    lambda   = lambda_n, # サンプル値
    sigma    = 1/sqrt(lambda), # 標準偏差
    dens_max = dnorm(x = 0, mean = 0, sd = sigma) # 最頻値における確率密度
  )
  
  ##### 生成分布の作図 -----
  
  # 生成分布の確率密度を計算
  gen_dens_df <- tidyr::expand_grid(
    mu     = mu_vec,    # 確率変数
    lambda = lambda_vec # 確率変数
  ) |> # 格子点を作成
    dplyr::mutate(
      N_dens   = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), 
      Gam_dens = dgamma(x = lambda, shape = a, rate = b), 
      dens     = N_dens * Gam_dens # 確率密度
    )
  
  # 生成分布のラベルを作成
  gen_param_lbl <- sprintf(
    fmt = "list(m == '%s', beta == '%s', a == '%s', b == '%s')", 
    formatC(m,    digits = 2, format = "f"),
    formatC(beta, digits = 2, format = "f"),
    formatC(a,    digits = 2, format = "f"),
    formatC(b,    digits = 2, format = "f")
  ) |> parse(text = _)
  gen_stats_lbl <- paste0(
    "list(", 
      "paste(", 
        "E(mu) == m, {} == '", formatC(E_mu, digits = 2, format = "f"), "'", 
      "), ", 
        "s(mu) == sqrt(frac(1, beta * lambda)), ", 
      "paste(", 
        "E(lambda) == frac(a, b), {} == '", formatC(E_lambda, digits = 2, format = "f"), "'", 
      "), ", 
      "paste(", 
        "s(lambda) == sqrt(frac(a, b^2)), {} == '", formatC(sqrt(a/b^2), digits = 2, format = "f"), "'", 
      ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # 期待値のラベルを作成
  E_data_mu_lbl     <- expression(E(mu))
  E_data_lambda_lbl <- expression(E(lambda))
  
  # サンプルのラベルを作成
  smp_data_mu_lbl <- paste0(
    "mu[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_data_lambda_lbl <- paste0(
    "lambda[", 1:N, "]"
  ) |> 
    parse(text = _)
  
  # 生成分布を作図
  gen_graph <- ggplot() + 
    geom_contour_filled(
      data    = gen_dens_df, 
      mapping = aes(x = mu, y = lambda, z = dens, fill = after_stat(level), linetype = "generator"), 
      breaks = gen_dens_vals, 
      alpha = 0.5
    ) + # 生成分布
    geom_vline(
      xintercept = E_mu, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_hline(
      yintercept = E_lambda, 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値の位置
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = mu, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_hline(
      data    = smp_data_df, 
      mapping = aes(yintercept = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルの位置
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = mu, y = lambda, color = factor(n)), 
      size = 4, show.legend = FALSE
    ) + # サンプル
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~ ., 
        breaks    = c(E_mu, mu_n), 
        labels    = c(E_data_mu_lbl, smp_data_mu_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~ ., 
        breaks    = c(E_lambda, lambda_n), 
        labels    = c(E_data_lambda_lbl, smp_data_lambda_lbl), 
      ) # サンプルのラベル
    ) + 
    scale_fill_viridis_d(drop = FALSE) + # (目盛の固定用)
    scale_linetype_manual(
      breaks = "generator", 
      values = "solid", 
      labels = gen_param_lbl, 
      name   = "generator" 
    ) + # (凡例の表示用)
    guides(
      fill     = guide_legend(order = 2), 
      linetype = guide_legend(order = 1)
    ) + 
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(mu_min, mu_max), 
      ylim = c(lambda_min, lambda_max)
    ) + # (軸の対応用)
    labs(
      title = "Gaussian-Gamma distribution", 
      subtitle = gen_stats_lbl, 
      fill = expression(p(mu, lambda ~"|"~ m, beta, a, b)), 
      x = expression(mu), 
      y = expression(lambda)
    )
  
  ##### サンプル分布の作図 -----
  
  # 期待値による分布の確率密度を計算
  E_dens_df <- tidyr::tibble(
    x      = x_vec, # 確率変数
    mu     = E_mu, 
    lambda = E_lambda, 
    sigma  = 1/sqrt(lambda), 
    dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
  )
  
  # サンプル分布の確率密度を計算
  smp_dens_df <- tidyr::expand_grid(
    n = 1:N,   # サンプル番号
    x = x_vec, # 確率変数
  ) |> # サンプルごとに変数を複製
    dplyr::mutate(
      mu     = mu_n[n],        # 平均パラメータ
      lambda = lambda_n[n],    # 精度パラメータ, 
      sigma  = 1/sqrt(lambda), # 標準偏差パラメータ
      dens   = dnorm(x = x, mean = mu, sd = sigma) # 確率密度
    )
  
  # 期待値のラベルを作成
  E_data_mu_lbl     <- expression(E(mu))
  E_data_lambda_lbl <- expression(
    p(x == mu ~'|'~ mu, E(lambda)^{-1})
  )
  E_param_lbl <- sprintf(
    fmt = "list(E(mu) == '%s', E(lambda) == '%s')", 
    formatC(E_mu,     digits = 2, format = "f"), 
    formatC(E_lambda, digits = 2, format = "f")
  ) |> 
    parse(text = _)
  E_stats_lbl <- paste0(
    "list(", 
      "paste(", 
        "E(x) == mu, {} == '", formatC(E_x, digits = 2, format = "f"), "'", 
      "), ", 
      "paste(", 
        "V(x) == sqrt(frac(b, a)), {} == '", formatC(1/sqrt(E_lambda), digits = 2, format = "f"), "'", 
      ")", 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプルのラベルを作成
  smp_data_mu_lbl <- paste0(
    "mu[", 1:N, "]"
  ) |> 
    parse(text = _)
  smp_data_lambda_lbl <- paste0(
    "p(x == mu ~'|'~ mu, lambda[", 1:N, "]^{-1})"
  ) |> 
    parse(text = _)
  smp_param_lbl <- paste0(
    "list(", 
    "mu[", 1:N, "] == '",     formatC(mu_n, digits = 2, format = "f"), "', ", 
    "lambda[", 1:N, "] == '", formatC(lambda_n, digits = 2, format = "f"), "'", 
    ")"
  ) |> 
    parse(text = _)
  
  # サンプル分布を作図
  smp_graph <- ggplot() + 
    geom_vline(
      data    = E_data_df, 
      mapping = aes(xintercept = mu), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値との対応
    geom_hline(
      data    = E_data_df, 
      mapping = aes(yintercept = dens_max), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 期待値との対応
    geom_vline(
      data    = smp_data_df, 
      mapping = aes(xintercept = mu, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプルとの対応
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
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~ ., 
        breaks    = c(E_mu, mu_n), 
        labels    = c(E_data_mu_lbl, smp_data_mu_lbl), 
      ) # パラメータのラベル
    ) + 
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~ ., 
        breaks    = c(E_data_df[["dens_max"]], smp_data_df[["dens_max"]]), 
        labels    = c(E_data_lambda_lbl, smp_data_lambda_lbl), 
      ) # パラメータのラベル
    ) + 
    scale_color_manual(
      breaks = c("expected", 1:N), 
      values = c("red", scales::hue_pal()(n = N)), 
      labels = c(E_param_lbl, smp_param_lbl), 
      name   = "sample" 
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
      color = "sample", 
      x = expression(x), 
      y = expression(p(x ~"|"~ mu[n], lambda[n]^{-1}))
    )
  
  ##### 軸変換の作図：(λ -> λ) -----
  
  # 恒等関数を作図
  identity_graph <- ggplot() + 
    geom_line(
      data    = adapt_line_df, 
      mapping = aes(x = lambda, y = lambda), 
      linewidth = 1
    ) + # 恒等関数
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = Inf, y = lambda, xend = lambda, yend = lambda), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 生成分布との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = lambda, xend = lambda, yend = -Inf), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # 変換曲線との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = Inf, y = lambda, xend = lambda, yend = lambda, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 生成分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = lambda, xend = lambda, yend = -Inf, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 変換曲線との対応
    geom_point(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = lambda, color = factor(n)), 
      size = 4, show.legend = FALSE
    ) + # 変換点
    theme(
      axis.title    = element_text(size = 12), 
      plot.title    = element_text(size = 14), 
      plot.subtitle = element_text(size = 12)
    ) + # (グラフ位置のズレ対策用)
    coord_cartesian(
      xlim = c(lambda_min, lambda_max), 
      ylim = c(lambda_min, lambda_max)
    ) + # (軸の対応用)
    labs(
      x = expression(lambda), 
      y = expression(lambda)
    )
  
  ##### 軸変換の作図：(λ -> p(x)) -----
  
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
    ) + # 恒等関数との対応
    geom_segment(
      data    = E_data_df, 
      mapping = aes(x = lambda, y = dens_max, xend = Inf, yend = dens_max), 
      color = "red", linewidth = 1, linetype = "dashed"
    ) + # サンプル分布との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = Inf, xend = lambda, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # 恒等関数との対応
    geom_segment(
      data    = smp_data_df, 
      mapping = aes(x = lambda, y = dens_max, xend = Inf, yend = dens_max, color = factor(n)), 
      linewidth = 1, linetype = "dotted", show.legend = FALSE
    ) + # サンプル分布との対応
    geom_point(
      data    = smp_data_df,
      mapping = aes(x = lambda, y = dens_max, color = factor(n)),
      size = 4, show.legend = FALSE
    ) + # 変換点
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
    identity_graph, tmp_gen_graph, 
    adapt_graph,    tmp_smp_graph, 
    nrow = 2, ncol = 2, 
    rel_widths = c(3, 4), # サイズを指定
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
    width = 18, height = 12, units = "in", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}


### アニメーションの変換 -----

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # pngファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gifファイルを作成
  magick::image_write_video(path = "figure/gaussian_gamma/generate_gaussian/NG_to_gauss.mp4", framerate = 30) -> tmp_path # mp4ファイルを書出



