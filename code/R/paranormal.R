
# パラノーマル分布 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(ggstar)

# パッケージ名の省略用
library(ggplot2)


# ゴーストの浮遊 -----------------------------------------------------------------

### ・ガワの作図 -----

# フレーム数を指定
frame_num <- 100

# 身体の伸び縮み(ガワのフワフワ)用の標準偏差を指定
sgm_vals <- seq(from = 0, to = pi, length.out = frame_num+1)[1:frame_num] |> 
  cos() |> 
  abs() |> 
  (\(vec) {2 + vec * 0.25})() # 値を指定
sgm_vals <- c(
  seq(from = 2, to = 2.2, length.out = floor(0.5*frame_num+1))[1:floor(0.5*frame_num)], 
  seq(from = 2.2, to = 2, length.out = ceiling(0.5*frame_num+1))[1:ceiling(0.5*frame_num)]
)

# 浮遊用のラジアンを作成
t_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 胴の縦サイズを指定
dens_lower <- 0.02

# 縦・横の移動量を指定
turn_dist_x <- 1.5
turn_dist_y <- 0.05

# 身体の座標を作成
body_df <- tidyr::expand_grid(
  i   = 1:frame_num, # フレーム番号
  var = seq(from = -5, to = 5, length.out = 1001)
) |> # フレームごとに複製
  dplyr::mutate(
    # 身体を作成
    sgm  = sgm_vals[i], 
    dens = dnorm(x = var, mean = 0, sd = sgm)
  ) |> 
  dplyr::filter(
    # 胴を作成
    dens >= dens_lower
  ) |> 
  dplyr::mutate(
    # 浮遊の軌道を作成
    t = t_vals[i], 
    x = var  + turn_dist_x * cos(t), 
    y = dens + turn_dist_y * sin(t)
  ) |> 
  dplyr::mutate(
    # 裾の形を指定
    j        = dplyr::row_number(x), 
    y_min    = min(y), 
    tmp_y    = (y - y_min) * 0.2, # 0以上の値
    w        = tmp_y / max(tmp_y), # 0から1の値
    cos1_x   = 0.01 * cos(4*x), 
    cos2_x   = 0.01 * cos(3.5*x + 0.5*pi), 
    cos3_x   = 0.001 * sqrt(i) * cos(x + 0.5*pi), 
    cos_x    = 0.5 * (cos1_x + cos2_x + cos3_x), 
    y_bottom = y_min - tmp_y + w*cos_x, 
    .by = i
  )

# 目の位置を指定
eye_dist_x <- 0.4
eye_dist_y <- 0.05

# 目の座標を作成
eye_df <- body_df |> 
  dplyr::select(i, x_med = x, y_max = y) |> 
  dplyr::filter(y_max == max(y_max), .by = i) |> # 頭上を抽出
  dplyr::reframe(
    sgn_x = c(-1, 1), .by = dplyr::everything()
  ) |> # 左右用に複製
  dplyr::mutate(
    # 目の位置を調整
    x = x_med + sgn_x*eye_dist_x, 
    y = y_max - eye_dist_y
  )

# ゴーストのアニメーションを作図
anim <- ggplot() + 
  geom_ribbon(data = body_df, 
              mapping = aes(x = x, ymin = y_bottom, ymax = y), 
              fill = "white", color = "gray", alpha = 0.8) + # 身体
  ggstar::geom_star(data = eye_df,
                    mapping = aes(x = x, y = y),
                    starshape = "ellipse", fill = "black", size = 8) + # 目
  gganimate::transition_manual(frames = i) + # フレーム切替
  guides(x = "none", y = "none") + 
  labs(title = "paranormal distribution", 
       #subtitle = "frame: {current_frame}", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 100, width = 800, height = 800)


### ・ナカの作図 -----

# 胴の横サイズを指定
var_lower <- 3.6

# 中身(ナカ)の身体の座標を作成
inner_body_df <- tidyr::expand_grid(
  i   = 1:frame_num, # フレーム番号
  var = seq(from = -5, to = 5, length.out = 1001)
) |> # フレームごとに複製
  dplyr::mutate(
    # 身体を作成
    dens = LaplacesDemon::dst(x = var, mu = 0, sigma = 2, nu = 2.5)
  ) |> 
  dplyr::filter(
    # 縦方向の胴の位置を作成
    dens >= dens_lower
  ) |> 
  dplyr::mutate(
    # 縦方向の浮遊の軌道を作成
    t = t_vals[i], 
    y = dens + turn_dist_y * sin(t), 
    # 裾を作成
    y_min    = min(y), 
    tmp_y    = (y - y_min) * 0.2, # 0以上の値
    y_bottom = y_min - tmp_y, 
    .by = i
  ) |> 
  dplyr::filter(
    # 横方向の胴の位置を作成
    var >= -var_lower, var <= var_lower
  ) |> 
  dplyr::mutate(
    # 横方向の浮遊の軌道を作成
    x = var  + turn_dist_x * cos(t)
  )

# 中身(ナカ)の頭上の2点の座標を作成
inner_top_df <- inner_body_df |> 
  dplyr::select(i, x_med = x, y_max = y) |> 
  dplyr::filter(y_max == max(y_max), .by = i) |> # 頭上を抽出
  dplyr::reframe(
    sgn_x = c(-1, 1), .by = dplyr::everything()
  ) |> # 左右用に複製
    dplyr::mutate(
      # 耳の角度を指定
      deg = -sgn_x * 40, 
      .by = i
    )

# 耳・目・口の位置を指定
ear_dist_x   <- 0.9
ear_dist_y   <- 0.015
eye_dist_x   <- 0.6
eye_dist_y   <- 0.04
mouth_dist_y <- 0.05

# ゴーストのアニメーションを作図
anim <- ggplot() + 
  geom_ribbon(data = inner_body_df, 
              mapping = aes(x =  x, ymin = y_bottom, ymax = y), 
              fill = "gray76") + # ナカの身体
  geom_text(data = inner_top_df, 
            mapping = aes(x = x_med+sgn_x*ear_dist_x, y = y_max-ear_dist_y, angle = deg), 
            label = "▲", color = "gray67", size = 15) + # ナカの耳
  geom_point(data = inner_top_df,
             mapping = aes(x = x_med+sgn_x*eye_dist_x, y = y_max-eye_dist_y),
             shape = "circle", color = "gray56", size = 8) + # ナカの目
  geom_text(data = inner_top_df, 
            mapping = aes(x = x_med, y = y_max-mouth_dist_y), 
            label = "x", color = "gray56", size = 8) + # ナカの口
  geom_ribbon(data = body_df, 
              mapping = aes(x = x, ymin = y_bottom, ymax = y), 
              fill = "white", color = "gray", alpha = 0.8) + # ガワの身体
  ggstar::geom_star(data = eye_df,
                    mapping = aes(x = x, y = y),
                    starshape = "ellipse", fill = "black", size = 8) + # ガワの目
  gganimate::transition_manual(frames = i) + # フレーム切替
  guides(x = "none", y = "none") + 
  labs(title = "paranormal distribution", 
       #subtitle = "frame: {current_frame}", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 100, width = 800, height = 800)


# メンダコの浮遊 --------------------------------------------------------------------

# フレーム数を指定
frame_num <- 300

# 身体の伸び縮み(腕の開閉)用の標準偏差を指定
sgm_vals <- c(
  seq(from = 2, to = 2.5, length.out = 0.25*frame_num+1)[1:(0.25*frame_num)], 
  seq(from = 2.5, to = 2, length.out = 0.25*frame_num+1)[1:(0.25*frame_num)], 
  seq(from = 2, to = 2.5, length.out = 0.25*frame_num+1)[1:(0.25*frame_num)], 
  seq(from = 2.5, to = 2, length.out = 0.25*frame_num+1)[1:(0.25*frame_num)]
)

# 浮遊用のラジアンを作成
rad_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 身体の座標を作成
body_df <- tidyr::expand_grid(
  i   = 1:frame_num, # フレーム番号
  var = seq(from = -5, to = 5, length.out = 1001)
) |> # フレームごとに複製
  dplyr::mutate(
    # 身体を作成
    sgm = sgm_vals[i], 
    dens = dnorm(x = var, mean = 0, sd = sgm)
  ) |> 
  dplyr::filter(
    # 胴のサイズを指定
    dens >= 0.05
  ) |> 
  dplyr::mutate(
    # 浮遊の軌道を指定
    t = rad_vals[i], 
    x = var  + cos(t) * 5, 
    y = dens + sin(2*t) * 0.05, 
    # 裾用のラジアンを作成
    j = dplyr::row_number(var), 
    u = seq(from = 0, to = 2*pi, length.out = length(j))[j], 
    # 裾の形を指定
    y_min    = min(y), 
    tmp_y    = (y - y_min) * 0.2, # 0以上の値
    w        = tmp_y / max(tmp_y), # 0から1の値
    cos_u    = 0.01 * cos(4 * u), 
    y_bottom = y_min - tmp_y + w*cos_u, 
    .by = i
  )

# 頭上の2点の座標を作成
top_df <- body_df |> 
  dplyr::select(i, x_med = x, y_max = y) |> 
  dplyr::filter(y_max == max(y_max), .by = i) |> # 頭上を抽出
  dplyr::reframe(
    sgn_x = c(-1, 1), .by = dplyr::everything()
  ) |> # 左右用に複製
  dplyr::mutate(
    t   = rad_vals[i], 
    deg = sgn_x * (asin(sin(6*t + 0.5*pi)) * 180/pi * 7/9 + 35), # 角度
    .by = i
  )

# ヒレ・目の位置を指定
fin_dist_x <- 0.8
fin_dist_y <- 0.01
eye_dist_x <- 0.8
eye_dist_y <- 0.05

# メンダコのアニメーションを作図
anim <- ggplot() + 
  ggstar::geom_star(data = top_df, 
                    mapping = aes(x = x_med+sgn_x*fin_dist_x, y = y_max-fin_dist_y, angle = deg), 
                    starshape = "thin triangle", fill = "tomato", color = "red", size = 20) + # ヒレ
  geom_ribbon(data = body_df, 
              mapping = aes(x = x, ymin = y_bottom, ymax = y), 
              fill = "tomato", color = "red") + # 身体
  geom_point(data = top_df, 
             mapping = aes(x = x_med+sgn_x*eye_dist_x, y = y_max-eye_dist_y), 
             shape = "circle filled", fill = "yellow", alpha = 0.9, size = 12) + # 目
  ggstar::geom_star(data = top_df, 
                    mapping = aes(x = x_med+sgn_x*eye_dist_x, y = y_max-eye_dist_y),
                    starshape = "rectangle", fill = "black", size = 6) + # 瞳
  gganimate::transition_manual(frames = i) + # フレーム切替
  guides(x = "none", y = "none") + 
  labs(title = "paranormal distribution", 
       #subtitle = "frame: {current_frame}", 
       x = "", y = "")

# gif画像を作成
gganimate::animate(plot = anim, nframes = frame_num, fps = 12, width = 1000, height = 500)


