
# 1次元ガウス分布 ----------------------------------------------------------------

### 確率密度関数・累積分布関数・分位点関数の関係の可視化


# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージを読込
library(ggplot2)


# 分布の設定 -------------------------------------------------------------------

# 正規分布のパラメータを指定
mu    <- 0
sigma <- 1

# 確率変数の範囲を指定
x_min <- -2
x_max <- 2

# 正規分布を計算
norm_df <- tibble::tibble(
  x = seq(from = x_min, to = x_max, length.out = 1001), # 確率変数(クォンタイル)
  d = dnorm(x = x, mean = mu, sd = sigma), # 確率変数 → 確率密度
  p = pnorm(q = x, mean = mu, sd = sigma), # クォンタイル → パーセンタイル
  q = qnorm(p = p, mean = mu, sd = sigma)  # パーセンタイル → クォンタイル
)


# 分位数を指定
n <- 20

# 分位点を計算
quan_df <- tibble::tibble(
  i = 1:n, # 分位番号
  p = (i - 0.5) / n, # 分位番号 → パーセンタイル:(両端は0.5)
  #p = i / (n + 1), # 分位番号 → パーセンタイル:(両端も1)
  q = qnorm(p = p, mean = mu, sd = sigma), # パーセンタイル → クォンタイル
  d = dnorm(x = q, mean = mu, sd = sigma)  # クォンタイル → 確率密度
) |> 
  dplyr::select(i, d, p, q)


# 確率密度関数と累積分布関数の関係 ---------------------------------------------

# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num <- 101

# フレームごとの確率変数を作成
q_vals <- seq(from = x_min, to = x_max, length.out = frame_num)

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 確率変数を取得
  q_val <- q_vals[i]
  
  # 関数を計算
  p_val <- pnorm(q = q_val, mean = mu, sd = sigma)
  d_val <- dnorm(x = q_val, mean = mu, sd = sigma)
  
  # 接線の傾きを計算
  b_val <- p_val - d_val * q_val
  
  # ラベル用の文字列を作成
  dist_label <- paste0(
    "N(", 
    "x ~'|'~ list(mu == ", round(mu, digits = 2), ", sigma == ", round(sigma, digits = 2), ")", 
    ")", 
    " == ", round(d_val, digits = 5)
  )
  
  # 累積分布関数を作図
  prob_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # q・p軸線
    geom_hline(
      data    = quan_df, 
      mapping = aes(yintercept = p), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = p)
    ) + # 分位点
    geom_line(
      data    = norm_df, 
      mapping = aes(x = x, y = p), 
      linewidth = 1
    ) + # 累積確率
    geom_segment(
      mapping = aes(x = q_val, y = p_val, xend = q_val, yend = -Inf), 
      linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_segment(
      mapping = aes(x = q_val, y = p_val, xend = q_val+1, yend = p_val)
    ) + # 長さ1の線分
    geom_abline(
      slope = d_val, intercept = b_val
    ) + # 接線
    geom_segment(
      mapping = aes(x = q_val+1, y = p_val, xend = q_val+1, yend = p_val+d_val), 
      color = "red", linewidth = 1
    ) + # 確率密度
    geom_point(
      mapping = aes(x = q_val, y = p_val), 
      size = 5
    ) + # 変数
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["p"]], 
        labels    = round(quan_df[["p"]], digits = 3)
      )
    ) + # p軸
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 1)) + 
    labs(
      title = "Normal distribution", 
      subtitle = parse(text = dist_label), 
      x = "variable, quantile", 
      y = "probability, percentile"
    )
  
  # 確率密度関数を作図
  dens_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # x・d軸線
    geom_vline(
      data    = quan_df, 
      mapping = aes(xintercept = q), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_hline(
      data    = quan_df, 
      mapping = aes(yintercept = d), 
      linetype = "dotted"
    ) + # 分位点のd軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = d)
    ) + # 分位点
    geom_line(
      data    = norm_df, 
      mapping = aes(x = x, y = d), 
      linewidth = 1
    ) + # 確率密度
    geom_vline(
      xintercept = q_val, 
      linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_hline(
      yintercept = d_val, 
      color = "red", linewidth = 1, linetype = "dotted"
    ) + # 変数のd軸補助線
    geom_segment(
      mapping = aes(x = q_val, y = 0, xend = q_val, yend = d_val), 
      color = "red", linewidth = 1
    ) + # 確率密度
    geom_point(
      mapping = aes(x = q_val, y = d_val), 
      size = 5
    ) + # 変数
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["q"]], 
        labels    = round(quan_df[["q"]], digits = 3)
      )
    ) + # q軸
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["d"]], 
        labels    = round(quan_df[["d"]], digits = 3)
      )
    ) + # d軸
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 1)) + 
    labs(
      x = "variable, quantile", 
      y = "density"
    )
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    prob_graph, dens_graph, 
    nrow = 2
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_graph, 
    width = 1200, height = 1200, units = "px", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "figure/gaussian/prob_to_dens.mp4", framerate = 10) -> tmp_path # mp4ファイルを書出


# 累積分布関数と確率密度関数の関係 ---------------------------------------------

# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num <- 101

# フレームごとの確率変数を作成
q_vals <- seq(from = x_min, to = x_max, length.out = frame_num)

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 確率変数を取得
  q_val <- q_vals[i]
  
  # 関数を計算
  p_val <- pnorm(q = q_val, mean = mu, sd = sigma)
  d_val <- dnorm(x = q_val, mean = mu, sd = sigma)
  
  # 左側確率を計算
  prob_df <- tibble::tibble(
    x = seq(from = x_min, to = q_val, length.out = 1001), # 累積確率(パーセンタイル) → 確率変数(クォンタイル)
    d = dnorm(x = x, mean = mu, sd = sigma) # 確率変数 → 確率密度
  )
  
  # ラベル用の文字列を作成
  dist_label <- paste0(
    "list(", 
      "p(x) == N(", 
        "x ~'|'~ list(mu == ", round(mu, digits = 2), ", sigma == ", round(sigma, digits = 2), ")", 
      "), ", 
      "p(X <= x) == ", round(p_val, digits = 5), 
    ")"
  )
  
  # 累積分布関数を作図
  prob_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # q・p軸線
    geom_vline(
      data    = quan_df, 
      mapping = aes(xintercept = q), 
      linetype = "dotted"
    ) + # 分位点のq軸目盛線
    geom_hline(
      data    = quan_df, 
      mapping = aes(yintercept = p), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = p)
    ) + # 分位点
    geom_line(
      data    = norm_df, 
      mapping = aes(x = x, y = p), 
      linewidth = 1
    ) + # 累積確率
    geom_vline(
      xintercept = q_val, 
      linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_hline(
      yintercept = p_val, 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_segment(
      mapping = aes(x = q_val, y = 0, xend = q_val, yend = p_val), 
      color = "blue", linewidth = 1
    ) + # 確率
    geom_point(
      mapping = aes(x = q_val, y = p_val), 
      size = 5
    ) + # 変数
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["q"]],
        labels    = round(quan_df[["q"]], digits = 3))
    ) + # q軸
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["p"]],
        labels    = round(quan_df[["p"]], digits = 3))
    ) + # p軸
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
    coord_cartesian(xlim = c(x_min, x_max),  ylim = c(0, 1)) + 
    labs(
      title = "Normal distribution", 
      subtitle = parse(text = dist_label), 
      x = "variable, quantile", 
      y = "probability, percentile"
    )
  
  # 確率密度関数を作図
  dens_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # x・d軸線
    geom_vline(
      data    = quan_df, 
      mapping = aes(xintercept = q), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = d)
    ) + # 分位点
    geom_ribbon(
      data    = prob_df, 
      mapping = aes(x = x, ymin = 0, ymax = d), 
      fill = "blue", alpha = 0.5
    ) + # 累積確率
    geom_line(
      data    = norm_df, 
      mapping = aes(x = x, y = d), 
      linewidth = 1
    ) + # 確率密度
    geom_segment(
      mapping = aes(x = q_val, y = d_val, xend = q_val, yend = -Inf), 
      linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_segment(
      mapping = aes(x = q_val, y = d_val, xend = q_val, yend = Inf), 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_point(
      mapping = aes(x = q_val, y = d_val), 
      size = 5
    ) + # 変数
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["q"]], 
        labels    = round(quan_df[["p"]], digits = 3), 
        name      = "cumulative probability")
    ) + # p軸
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 1)) + 
    labs(
      x = "variable, quantile", 
      y = "density"
    )
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    prob_graph, dens_graph, 
    nrow = 2
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_graph, 
    width = 1200, height = 1200, units = "px", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "figure/gaussian/dens_to_prob.mp4", framerate = 10) -> tmp_path # mp4ファイルを書出


# 累積分布関数と分位点関数の関係 -----------------------------------------------

# 一時保存フォルダを指定
dir_path <- "figure/tmp"

# フレーム数を指定
frame_num <- 101

# フレームごとの確率変数を作成
q_vals <- seq(from = x_min, to = x_max, length.out = frame_num)

# グラフサイズを設定
digit_num <- 1
max_dens <- dnorm(x = mu, mean = mu, sd = sigma) |> 
  (\(val) {round(val * 10^digit_num, digits = 0) * 0.1^digit_num})() # 桁数を指定して丸め込み

# 変数ごとに作図
for(i in 1:frame_num) {
  
  # 確率変数を取得
  q_val <- q_vals[i]
  
  # 関数を計算
  p_val <- pnorm(q = q_val, mean = mu, sd = sigma)
  
  # ラベル用の文字列を作成
  dist_label <- paste0(
    "list(", 
      "p(x) == N(", 
        "x ~'|'~ list(mu == ", round(mu, digits = 2), ", sigma == ", round(sigma, digits = 2), ")", 
      "), ", 
      "paste(p == {}, F(q) == p(X <= q))", 
    ")"
  )
  
  # 累積分布関数を作図
  prob_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # q・p軸線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = p)
    ) + # 分位点
    geom_line(
      data    = norm_df, 
      mapping = aes(x = x, y = p), 
      linewidth = 1
    ) + # 累積確率
    geom_vline(
      xintercept = q_val, 
      color ="green4", linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_hline(
      yintercept = p_val, 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_segment(
      mapping = aes(x = 0, y = p_val, xend = q_val, yend = p_val), 
      color = "green4", linewidth = 1
    ) + # 確率変数
    geom_segment(
      mapping = aes(x = q_val, y = 0, xend = q_val, yend = p_val), 
      color = "blue", linewidth = 1
    ) + # 確率
    geom_point(
      mapping = aes(x = q_val, y = p_val), 
      size = 5
    ) + # 変数
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, 1)) + 
    labs(
      title = "Normal distribution", 
      subtitle = parse(text = dist_label), 
      x = "variable, quantile", 
      y = "probability, percentile"
    )
  
  # 分位点関数を作図
  quan_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # p・q軸線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = p, y = q)
    ) + # 分位点
    geom_line(
      data    = norm_df, 
      mapping = aes(x = p, y = q), 
      linewidth = 1
    ) + # 確率変数
    geom_vline(
      xintercept = p_val, 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_hline(
      yintercept = q_val, 
      color = "green4", linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_segment(
      mapping = aes(x = 0, y = q_val, xend = p_val, yend = q_val),
      color = "blue", linewidth = 1
    ) + # 確率
    geom_segment(
      mapping = aes(x = p_val, y = 0, xend = p_val, yend = q_val),
      color = "green4", linewidth = 1
    ) + # 確率変数
    geom_point(
      mapping = aes(x = p_val, y = q_val), 
      size = 5
    ) + # 変数
    coord_cartesian(xlim = c(0, 1), ylim = c(x_min, x_max)) + 
    labs(
      subtitle = expression(q == F^{-1}*(p)), 
      x = "probability, percentile", 
      y = "variable, quantile"
    )
  
  # クォンタイルの軸変換を作図
  axis_q_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # q軸線
    geom_vline(
      data    = quan_df,
      mapping = aes(xintercept = q),
      linetype = "dotted"
    ) + # 分位点のq軸目盛線
    geom_hline(
      data    = quan_df,
      mapping = aes(yintercept = q),
      linetype = "dotted"
    ) + # 分位点のq軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = q, y = q)
    ) + # 分位点
    geom_abline(
      slope = 1, intercept = 0, 
      linewidth = 1
    ) + # 恒等関数
    geom_vline(
      xintercept = q_val, 
      color = "green4", linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_hline(
      yintercept = q_val, 
      color = "green4", linewidth = 1, linetype = "dotted"
    ) + # 変数のq軸補助線
    geom_point(
      mapping = aes(x = q_val, y = q_val), 
      size = 5
    ) + # 変数
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["q"]], 
        labels    = round(quan_df[["q"]], digits = 3))
    ) + # q軸
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["q"]], 
        labels    = round(quan_df[["q"]], digits = 3))
    ) + # q軸
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(x_min, x_max)) + 
    labs(
      subtitle = parse(text = paste0("q == ", round(q_val, digits = 5))), 
      x = "quantile", 
      y = "quantile"
    )
  
  # パーセンタイルの軸変換を作図
  axis_p_graph <- ggplot() + 
    geom_segment(
      mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), xend = c(Inf, 0), yend = c(0, Inf)), 
      arrow = arrow(length = unit(10, units = "pt"), ends = "last")
    ) + # p軸線
    geom_vline(
      data    = quan_df, 
      mapping = aes(xintercept = p), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_hline(
      data    = quan_df, 
      mapping = aes(yintercept = p), 
      linetype = "dotted"
    ) + # 分位点のp軸目盛線
    geom_point(
      data    = quan_df, 
      mapping = aes(x = p, y = p)
    ) + # 分位点
    geom_abline(
      slope = 1, intercept = 0, 
      linewidth = 1
    ) + # 恒等関数
    geom_vline(
      xintercept = p_val, 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_hline(
      yintercept = p_val, 
      color = "blue", linewidth = 1, linetype = "dotted"
    ) + # 変数のp軸補助線
    geom_point(
      mapping = aes(x = p_val, y = p_val), 
      size = 5
    ) + # 変数
    scale_x_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["p"]], 
        labels    = round(quan_df[["p"]], digits = 3))
    ) + # p軸
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~., 
        breaks    = quan_df[["p"]], 
        labels    = round(quan_df[["p"]], digits = 3))
    ) + # p軸
    theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) + 
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
    labs(
      subtitle = parse(text = paste0("p == ", round(p_val, digits = 5))), 
      x = "percentile", 
      y = "percentile"
    )
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    prob_graph, axis_p_graph, 
    axis_q_graph, quan_graph, 
    nrow = 2, ncol = 2, 
    widths = c(2, 1), heights = c(1, 2)
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(
    filename = file_path, plot = wrap_graph, 
    width = 1250, height = 1250, units = "px", dpi = 100
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# 動画を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_video(path = "figure/gaussian/prob_to_quan.mp4", framerate = 10) -> tmp_path # mp4ファイルを書出


