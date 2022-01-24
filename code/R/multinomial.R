
# 多項分布 ------------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)
library(gganimate)
library(barplot3d)
library(rgl)


# 確率の計算 -------------------------------------------------------------------

# パラメータを指定
phi_v <- c(0.3, 0.5, 0.2)

# 確率変数の値を指定
x_v <- c(2, 3, 1)

# データ数を計算
M <- sum(x_v)


# 定義式により確率を計算
C <- gamma(M + 1) / prod(gamma(x_v + 1))
prob <- C * prod(phi_v^x_v)
prob

# 対数をとった定義式により確率を計算
log_C <- lgamma(M + 1) - sum(lgamma(x_v + 1))
log_prob <- log_C + sum(x_v * log(phi_v))
prob <- exp(log_prob)
prob; log_prob

# 多項分布の関数により確率を計算
prob <- dmultinom(x = x_v, size = M, prob = phi_v)
prob

# 多項分布の対数をとった関数により確率を計算
log_prob <- dmultinom(x = x_v, size = M, prob = phi_v, log = TRUE)
prob <- exp(log_prob)
prob; log_prob


# 統計量の計算 ------------------------------------------------------------------

# パラメータを指定
phi_v <- c(0.3, 0.5, 0.2)

# 試行回数を指定
M <- 10

# クラス番号を指定
v <- 1

# クラスvの平均を計算
E_x <- M * phi_v[v]
E_x

# クラスvの分散を計算
V_x <- M * phi_v[v] * (1 - phi_v[v])
V_x


# グラフの作成 ------------------------------------------------------------------

### ・分布の計算 -----

# パラメータを指定
phi_v <- c(0.3, 0.5, 0.2)

# 試行回数を指定
M <- 10

# 作図用のxの値を作成
x_vals <- 0:M

# 作図用のxの点を作成
x_points <- tidyr::tibble(
  x_1 = rep(x = x_vals, times = M + 1), # 確率変数の成分1
  x_2 = rep(x = x_vals, each = M + 1) # 確率変数の成分2
) %>% 
  dplyr::mutate(
    x_3 = dplyr::if_else(condition = x_1 + x_2 < M, true = M - (x_1 + x_2), false = 0)
  ) %>% # 確率変数の成分3
  as.matrix()

# 確率変数の組み合わせごとに確率を計算
prob_df <- tidyr::tibble()
for(i in 1:nrow(x_points)) {
  # 確率変数の値を取得
  x_v <- x_points[i, ]
  
  # 多項分布の情報を格納
  tmp_df <- tidyr::tibble(
    x_1 = x_v[1], # 確率変数の成分1
    x_2 = x_v[2], # 確率変数の成分2
    x_3 = x_v[3] # 確率変数の成分3
  ) %>% 
    dplyr::mutate(
      probability = dplyr::if_else(
        sum(x_v) == M, true = dmultinom(x = x_v, size = sum(x_v), prob = phi_v), false = 0
      ) # (size = Mにすべきだけど何故かエラーになる)
    ) # 確率
  
  # 結果を結合
  prob_df <- rbind(prob_df, tmp_df)
}


### ・ggplot2による作図 -----

# カテゴリ分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x_1, y = x_2, fill = probability)) + # データ
  geom_tile() + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Multinomial Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), "), M=", M), 
       x = expression(x[1]), y = expression(x[2])) # ラベル


# 補助線用の統計量を計算
E_x1 <- M * phi_v[1]
V_x1 <- M * phi_v[1] * (1 - phi_v[1])
s_x1 <- sqrt(V_x1)
E_x2 <- M * phi_v[2]
V_x2 <- M * phi_v[2] * (1 - phi_v[2])
s_x2 <- sqrt(V_x2)

# カテゴリ分布のグラフを作成
ggplot(data = prob_df, mapping = aes(x = x_1, y = x_2, fill = probability)) + # データ
  geom_tile() + # ヒートマップ
  geom_vline(xintercept = E_x1, color = "orange", size = 1, linetype = "dashed") + # 平均
  geom_vline(xintercept = E_x1 - s_x1, color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
  geom_vline(xintercept = E_x1 + s_x1, color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  geom_hline(yintercept = E_x2, color = "orange", size = 1, linetype = "dashed") + # 平均
  geom_hline(yintercept = E_x2 - s_x2, color = "orange", size = 1, linetype = "dotted") + # 平均 - 標準偏差
  geom_hline(yintercept = E_x2 + s_x2, color = "orange", size = 1, linetype = "dotted") + # 平均 + 標準偏差
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Multinomial Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), "), M=", M), 
       x = expression(x[1]), y = expression(x[2])) # ラベル


### ・barplot3dによる作図 -----

# インデックスによる色付けを作成
rainbow_vec <- rep(rainbow(M + 1), each = M + 1)

# カテゴリ分布のグラフを作成
barplot3d::barplot3d(
  rows = M + 1, cols = M + 1, z = prob_df[["probability"]], 
  topcolors = rainbow_vec, sidecolors = rainbow_vec, alpha = 0.1, 
  xlabels = as.character(0:M), ylabels = as.character(0:M), 
  xsub = "x1", ysub = "x2", zsub = "probability", 
  scalexy = 0.01, theta = 30, phi = 30
)


# 確率による色付けを作成
p <- prob_df[["probability"]]
heat_idx <- round((1 - p / max(p)) * 100) + 1
heat_vec <- heat.colors(101)[heat_idx]

# カテゴリ分布のグラフを作成
barplot3d::barplot3d(
  rows = M + 1, cols = M + 1, z = prob_df[["probability"]], 
  topcolors = heat_vec, sidecolors = heat_vec, alpha = 0.1, 
  xlabels = as.character(0:M), ylabels = as.character(0:M), 
  xsub = "x1", ysub = "x2", zsub = "probability", 
  scalexy = 0.01, theta = 30, phi = 30
)


# ウィンドウサイズを指定:(左, 上, 右, 下)
rgl::par3d(windowRect=c(0, 100, 600, 700))

# ウィンドウのスクリーンショットを保存
rgl::rgl.snapshot("figure/Multinomial_3dbar.png")

# ウィンドウを閉じる
rgl::rgl.close()


# パラメータと分布の形状の関係 ----------------------------------------------------------

### ・パラメータphiの影響 -----

# 作図用のphiの値を作成
phi_vals <- seq(from = 0, to = 1, by = 0.01)

# 試行回数を指定
M <- 10

# 作図用のxの値を作成
x_vals <- 0:M

# 作図用のxの点を作成
x_points <- tidyr::tibble(
  x_1 = rep(x = x_vals, times = M + 1), # 確率変数の成分1
  x_2 = rep(x = x_vals, each = M + 1) # 確率変数の成分2
) %>% 
  dplyr::mutate(
    x_3 = dplyr::if_else(condition = x_1 + x_2 < M, true = M - (x_1 + x_2), false = 0)
  ) %>% # 確率変数の成分3
  as.matrix()

# phiの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(phi in phi_vals) {
  # phi_1以外の割り当てを指定
  phi_v <- c(phi, (1 - phi) * 0.6, (1 - phi) * 0.4)
  
  # 確率変数の組み合わせごとに確率を計算
  tmp_prob_df <- tidyr::tibble()
  for(i in 1:nrow(x_points)) {
    # 確率変数の値を取得
    x_v <- x_points[i, ]
    
    # 多項分布の情報を格納
    tmp_df <- tidyr::tibble(
      x_1 = x_v[1], # 確率変数の成分1
      x_2 = x_v[2], # 確率変数の成分2
      x_3 = x_v[3] # 確率変数の成分3
    ) %>% 
      dplyr::mutate(
        probability = dplyr::if_else(
          sum(x_v) == M, true = dmultinom(x = x_v, size = sum(x_v), prob = phi_v), false = 0
        ) # (size = Mにすべきだけど何故かエラーになる)
      ) # 確率
    
    # 結果を結合
    tmp_prob_df <- rbind(tmp_prob_df, tmp_df)
  }
  
  # フレーム切替用のラベルを付与
  tmp_prob_df <- tmp_prob_df %>% 
    dplyr::mutate(
      parameter = paste0("phi=(", paste0(round(phi_v, 2), collapse = ", "), "), M=", M) %>% 
        as.factor()
    )
  
  # 結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
  
  # 途中経過を表示
  print(paste0("phi=", phi, " (", round(which(phi_vals == phi) / length(phi_vals) * 100, 2), "%)"))
}

# グラデーションの最大値を指定
p_max <- 0.15

# アニメーション用の多項分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x_1, y = x_2, fill = probability)) + # データ
  geom_tile() + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), limits = c(0, p_max)) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Multinomial Distribution", 
       subtitle = "{current_frame}", 
       x = expression(x[1]), y = expression(x[2])) # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = length(phi_vals), fps = 100)


### ・試行回数Mの影響 -----

# パラメータを指定
phi_v <- c(0.3, 0.5, 0.2)

# 試行回数の最大値を指定
M_max <- 25

# Mの値ごとに分布を計算
anime_prob_df <- tidyr::tibble()
for(M in 1:M_max) {
  # 数値型に変換
  M <- as.numeric(M)
  
  # 作図用のxの値を作成
  x_vals <- 0:M
  
  # 作図用のxの点を作成
  x_points <- tidyr::tibble(
    x_1 = rep(x = x_vals, times = M + 1), # 確率変数の成分1
    x_2 = rep(x = x_vals, each = M + 1) # 確率変数の成分2
  ) %>% 
    dplyr::mutate(
      x_3 = dplyr::if_else(condition = x_1 + x_2 < M, true = M - (x_1 + x_2), false = 0)
    ) %>% # 確率変数の成分3
    as.matrix()
  
  # 確率変数の組み合わせごとに確率を計算
  tmp_prob_df <- tidyr::tibble()
  for(i in 1:nrow(x_points)) {
    # 確率変数の値を取得
    x_v <- x_points[i, ]
    
    # 多項分布の情報を格納
    tmp_df <- tidyr::tibble(
      x_1 = x_v[1], # 確率変数の成分1
      x_2 = x_v[2], # 確率変数の成分2
      x_3 = x_v[3] # 確率変数の成分3
    ) %>% 
      dplyr::mutate(
        probability = dplyr::if_else(
          sum(x_v) == M, true = dmultinom(x = x_v, size = sum(x_v), prob = phi_v), false = 0
        ) # (xize = Mにすべきだけど何故かエラーになる)
      ) # 確率
    
    # 結果を結合
    tmp_prob_df <- rbind(tmp_prob_df, tmp_df)
  }
  
  # フレーム切替用のラベルを付与
  tmp_prob_df <- tmp_prob_df %>% 
    dplyr::mutate(
      parameter = paste0("phi=(", paste0(round(phi_v, 2), collapse = ", "), "), M=", M) %>% 
        as.factor()
    )
  
  # 結果を結合
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
  
  # 途中経過を表示
  print(paste0("M=", M, " (", round(M / M_max * 100, 2), "%)"))
}

# グラデーションの最大値を指定
p_max <- 0.15

# アニメーション用の多項分布を作図
anime_prob_graph <- ggplot(data = anime_prob_df, mapping = aes(x = x_1, y = x_2, fill = probability)) + # データ
  geom_tile() + # ヒートマップ
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), limits = c(0, p_max)) + # グラデーション
  coord_fixed(ratio = 1) + # アスペクト比
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Multinomial Distribution", 
       subtitle = "{current_frame}", 
       x = expression(x[1]), y = expression(x[2])) # ラベル

# gif画像を作成
gganimate::animate(anime_prob_graph, nframes = M_max, fps = 100)


# 乱数の生成 -------------------------------------------------------------------

### ・乱数の集計 -----

# パラメータを指定
phi_v <- c(0.3, 0.5, 0.2)

# クラス数を取得
V <- length(phi_v)

# 試行回数を指定
M <- 10

# データ数を指定
N <- 10000

# 多項分布に従う乱数を生成
x_nv <- rmultinom(n = N, size = M, prob = phi_v) %>% 
  t()

# 作図用のxの値を作成
x_vals <- 0:M

# 作図用のxの点を作成
x_df <- tidyr::tibble(
  x_1 = rep(x = x_vals, times = M + 1), # 確率変数の成分1
  x_2 = rep(x = x_vals, each = M + 1) # 確率変数の成分2
) %>% 
  dplyr::mutate(
    x_3 = dplyr::if_else(condition = x_1 + x_2 < M, true = M - (x_1 + x_2), false = 0)
  ) # 確率変数の成分3

# 乱数を集計して格納
freq_df <- tidyr::tibble(
  x_1 = x_nv[, 1], # 確率変数の成分1
  x_2 = x_nv[, 2] # 確率変数の成分2
) %>% # 乱数を格納
  dplyr::count(x_1, x_2, name = "frequency") %>% # 乱数を集計
  dplyr::right_join(x_df, by = c("x_1", "x_2")) %>% # 確率変数の全ての組み合わせを結合
  dplyr::select(x_1, x_2, x_3, frequency) %>% # 頻度を測定
  dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) %>% # サンプルにない場合のNAを0に置換
  dplyr::mutate(proportion = frequency / N) %>%  # 構成比を計算
  dplyr::arrange(x_2, x_1) # 昇順に並び替え:(barplot3d用)


### ・ggplot2による作図 -----

# サンプルのヒストグラムを作成
ggplot(data = freq_df, mapping = aes(x = x_1, y = x_2, fill = frequency)) + # データ
  geom_tile() + # ヒストグラム
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Multinomial Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), "), M=", M, ", N=", N), 
       x = expression(x[1]), y = expression(x[2])) # ラベル


# マトリクスに変換
x_points <- as.matrix(x_df)

# 確率変数の組み合わせごとに確率を計算
prob_df <- tidyr::tibble()
for(i in 1:nrow(x_points)) {
  # 確率変数の値を取得
  x_v <- x_points[i, ]
  
  # 多項分布の情報を格納
  tmp_df <- tidyr::tibble(
    x_1 = x_v[1], # 確率変数の成分1
    x_2 = x_v[2], # 確率変数の成分2
    x_3 = x_v[3] # 確率変数の成分3
  ) %>% 
    dplyr::mutate(
      probability = dplyr::if_else(
        sum(x_v) == M, true = dmultinom(x = x_v, size = sum(x_v), prob = phi_v), false = 0
      ) # (size = Mにすべきだけど何故かエラーになる)
    ) # 確率
  
  # 結果を結合
  prob_df <- rbind(prob_df, tmp_df)
}

# 構成比と確率の最大値を取得
p_max <- max(freq_df[["proportion"]], prob_df[["probability"]])

# サンプルの構成比を作図
ggplot() + 
  geom_tile(data = freq_df, mapping = aes(x = x_1, y = x_2, fill = proportion), 
            alpha = 0.9) + # 構成比
  geom_tile(data = prob_df, mapping = aes(x = x_1, y = x_2, color = probability), 
            alpha = 0, size = 1, linetype = "dashed") + # 真の分布
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), limits = c(0, p_max)) + # タイルのグラデーション
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red"), limits = c(0, p_max)) + # 枠線のグラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  guides(color = FALSE) + # 凡例
  labs(title = "Multinomial Distribution", 
       subtitle = paste0("phi=(", paste0(phi_v, collapse = ", "), "), M=", M, ", N=", N), 
       x = expression(x[1]), y = expression(x[2])) # ラベル


### ・barplot3dによる作図 -----

# サンプルの構成比による色付けを作成
p <- freq_df[["proportion"]]
heat_idx <- round((1 - p / max(p)) * 100) + 1
heat_prop <- heat.colors(101)[heat_idx]

# 分布の確率による色付けを作成
p <- prob_df[["probability"]]
heat_idx <- round((1 - p / max(p)) * 100) + 1
heat_prob <- heat.colors(101)[heat_idx]

# サンプルのヒストグラムを作成
barplot3d::barplot3d(
  rows = M + 1, cols = M + 1, z = freq_df[["frequency"]], 
  topcolors = heat_prop, sidecolors = heat_prop, alpha = 0.1, 
  xlabels = as.character(0:M), ylabels = as.character(0:M), 
  xsub = "x1", ysub = "x2", zsub = "frequency", 
  scalexy = 100, theta = 30, phi = 30
)

# サンプルの構成比を作図
barplot3d::barplot3d(
  rows = M + 1, cols = M + 1, z = freq_df[["proportion"]], 
  topcolors = heat_prop, sidecolors = heat_prop, linecolors = heat_prob, alpha = 0.1, 
  xlabels = as.character(0:M), ylabels = as.character(0:M), 
  xsub = "x1", ysub = "x2", zsub = "proportion", 
  scalexy = 0.01, theta = 30, phi = 30
)


# ウィンドウサイズを指定:(左, 上, 右, 下)
rgl::par3d(windowRect=c(0, 100, 600, 700))

# ウィンドウのスクリーンショットを保存
rgl::rgl.snapshot("figure/Multinomial_3dbar.png")

# ウィンドウを閉じる
rgl::rgl.close()


### ・アニメーションによる可視化 -----

# データ数を指定
N <- 300

# グラデーションの最大値を指定
p_max <- 0.1

# 乱数を1つずつ生成
x_nv <- matrix(NA, nrow = N, ncol = V)
anime_freq_df <- tidyr::tibble()
anime_data_df <- tidyr::tibble()
anime_prob_df <- tidyr::tibble()
for(n in 1:N) {
  # 多項分布に従う乱数を生成
  x_nv[n, ] <- rmultinom(n = 1, size = M, prob = phi_v)
  
  # n個の乱数を集計して格納
  tmp_freq_df <- tidyr::tibble(
    x_1 = x_nv[, 1], # 確率変数の成分1
    x_2 = x_nv[, 2] # 確率変数の成分2
  ) %>% # 乱数を格納
    dplyr::count(x_1, x_2, name = "frequency") %>% # 乱数を集計
    dplyr::right_join(prob_df, by = c("x_1", "x_2")) %>% # 確率変数の全ての組み合わせを結合
    dplyr::select(x_1, x_2, frequency) %>% # 頻度を測定
    dplyr::mutate(frequency = tidyr::replace_na(frequency, 0)) %>% # サンプルにない場合のNAを0に置換
    dplyr::mutate(proportion = frequency / n) # 構成比を計算
  
  # ラベル用のテキストを作成
  label_text <- paste0("phi=(", paste0(phi_v, collapse = ", "), "), M=", M, ", N=", n)
  
  # フレーム切替用のラベルを付与
  tmp_freq_df <- tmp_freq_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # n番目の乱数を格納
  tmp_data_df <- tidyr::tibble(
    x_1 = x_nv[n, 1], # サンプルの成分1
    x_2 = x_nv[n, 2], # サンプルの成分2
    parameter = as.factor(label_text) # フレーム切替用のラベル
  )
  
  # n回目のラベルを付与
  tmp_prob_df <- prob_df %>% 
    dplyr::mutate(parameter = as.factor(label_text))
  
  # 結果を結合
  anime_freq_df <- rbind(anime_freq_df, tmp_freq_df)
  anime_data_df <- rbind(anime_data_df, tmp_data_df)
  anime_prob_df <- rbind(anime_prob_df, tmp_prob_df)
}


# アニメーション用のサンプルのヒストグラムを作成
anime_freq_graph <- ggplot() + # データ
  geom_tile(data = anime_freq_df, mapping = aes(x = x_1, y = x_2, fill = frequency)) + # ヒストグラム
  geom_point(data = anime_data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", size = 5) + 
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) + # グラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Multinomial Distribution", 
       subtitle = "{current_frame}", 
       x = expression(x[1]), y = expression(x[2])) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 100)


# アニメーション用のサンプルの構成比を作成
anime_prop_graph <- ggplot() + # データ
  geom_tile(data = anime_freq_df, mapping = aes(x = x_1, y = x_2, fill = proportion), 
            alpha = 0.9) + # 構成比
  geom_tile(data = anime_prob_df, mapping = aes(x = x_1, y = x_2, color = probability), 
            alpha = 0, size = 0.7, linetype = "dashed") + # 真の分布
  geom_point(data = anime_data_df, mapping = aes(x = x_1, y = x_2), 
             color = "orange", size = 5) + # 真の分布
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red"), limits = c(0, p_max)) + # タイルのグラデーション
  scale_color_gradientn(colors = c("blue", "green", "yellow", "red"), limits =c(0, p_max)) + # 枠線のグラデーション
  scale_x_continuous(breaks = 0:M, labels = 0:M) + # x軸目盛
  scale_y_continuous(breaks = 0:M, labels = 0:M) + # y軸目盛
  coord_fixed(ratio = 1) + # アスペクト比
  guides(color = FALSE) + # 凡例
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Multinomial Distribution", 
       subtitle = "{current_frame}", 
       x = expression(x[1]), y = expression(x[2])) # ラベル

# gif画像を作成
gganimate::animate(anime_prop_graph, nframes = N, fps = 100)


