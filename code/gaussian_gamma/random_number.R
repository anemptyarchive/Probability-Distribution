
# 乱数の生成 -------------------------------------------------------------------

### ・サンプリング -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6

# データ数(サンプルサイズ)を指定
N <- 10000


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを格納
data_df <- tidyr::tibble(
  mu = mu_n, # μのサンプル
  lambda = lambda_n # λのサンプル
)


### ・乱数の可視化 -----

# μの値を作成
mu_vals <- seq(
  from = m - 1/sqrt(beta*a/b) * 4, 
  to = m + 1/sqrt(beta*a/b) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = a/b * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


# サンプルの散布図を作成
ggplot() + 
  geom_point(data = data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ", N==", N, ")")), 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# サンプルのヒストグラムを作成:度数
ggplot() + 
  geom_bin_2d(data = data_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
              alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ", N==", N, ")")), 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# サンプルのヒストグラムを作成:密度
ggplot() + 
  geom_density_2d_filled(data = data_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                         alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = parse(text = paste0("list(m==", m, ", beta==", beta, ", a==", a, ", b==", b, ", N==", N, ")")), 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル


# 乱数と分布の関係：アニメーションによる可視化 --------------------------------------------------

### ・パラメータの設定 -----

# ガウス分布の平均パラメータを指定
m <- 0

# ガウス分布の精度パラメータの係数を指定
beta <- 2

# ガンマ分布のパラメータを指定
a <- 5
b <- 6


# μの値を作成
mu_vals <- seq(
  from = m - 1/sqrt(beta*a/b) * 4, 
  to = m + 1/sqrt(beta*a/b) * 4, 
  length.out = 200
)

# λの値を作成
lambda_vals <- seq(from = 0, to = a/b * 3, length.out = 200)

# ガウス-ガンマ分布を計算
dens_df <- tidyr::expand_grid(
  mu = mu_vals, # 確率変数μ
  lambda = lambda_vals # 確率変数λ
) |> # 確率変数(μとλの格子点)を作成
  dplyr::mutate(
    N_dens = dnorm(x = mu, mean = m, sd = 1/sqrt(beta*lambda)), # μの確率密度
    Gam_dens = dgamma(x = lambda, shape = a, rate = b), # λの確率密度
    density = N_dens * Gam_dens # 確率密度
  )


### ・1データずつ可視化 -----

# データ数(フレーム数)を指定
N <- 100


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを格納
anime_data_df <- tibble::tibble(
  n = 1:N, # データ番号
  mu = mu_n, # μのサンプル
  lambda = lambda_n, # λのサンプル
  parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N) |> 
    factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N)) # フレーム切替用ラベル
)

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:N, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame) |> # 各試行までのサンプルを抽出
  dplyr::mutate(
    mu = mu_n[n], # μのサンプル
    lambda = lambda_n[n], # λのサンプル
    parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", frame) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:N)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5, size = 3) + # n個のサンプル
  geom_point(data = anime_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin_2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
              breaks = list(x = mu_breaks, y = lambda_breaks), 
              alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = anime_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # 
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N, fps = 10, width = 800, height = 600)


# (データが少ないと密度を計算できないため)最初のフレームを除去
n_min <- 10
tmp_data_df <- anime_data_df|> 
  dplyr::filter(n > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )
tmp_freq_df <- anime_freq_df |> 
  dplyr::filter(frame > n_min) |> # 始めのデータを削除
  dplyr::mutate(
    parameter = parameter |> 
      as.character() |> 
      (\(.){factor(., levels = unique(.))})() # レベルを再設定
  )

# 密度の等高線のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density_2d_filled(data = tmp_freq_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                         alpha = 0.8) + # n個のサンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  geom_point(data = tmp_data_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", size = 6) + # n番目のサンプル
  gganimate::transition_manual(parameter) + # 
  scale_color_viridis_c(option = "D") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = N-n_min, fps = 10, width = 800, height = 600)


### ・複数データずつ可視化 -----

# データ数を指定
N <- 1000

# フレーム数を指定
frame_num <- 100

# 1フレーム当たりのデータ数を計算
n_per_frame <- N %/% frame_num


# ガンマ分布に従う乱数を生成
lambda_n <- rgamma(n = N, shape = a, rate = b)

# ガウス分布に従う乱数を生成
mu_n <- rnorm(n = N, mean = m, sd = 1/sqrt(beta*lambda_n))

# サンプルを複製して格納
anime_freq_df <- tidyr::expand_grid(
  frame = 1:frame_num, # フレーム番号
  n = 1:N # データ番号
) |> # 全ての組み合わせを作成
  dplyr::filter(n <= frame*n_per_frame) |> # 各フレームで利用するサンプルを抽出
  dplyr::mutate(
    mu = mu_n[n], # μのサンプル
    lambda = lambda_n[n], # λのサンプル
    parameter = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", frame*n_per_frame) |> 
      factor(levels = paste0("m=", m, ", beta=", beta, ", a=", a, ", b=", b, ", N=", 1:frame_num*n_per_frame)) # フレーム切替用ラベル
  )


# 散布図のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_point(data = anime_freq_df, mapping = aes(x = mu, y = lambda), 
             color = "orange", alpha = 0.5, size = 3) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # フレーム
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# メッシュ用の値を設定
mu_breaks     <- seq(from = min(mu_vals), to = max(mu_vals), length.out = 30)
lambda_breaks <- seq(from = min(lambda_vals), to = max(lambda_vals), length.out = 30)

# 度数のヒートマップのアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_bin2d(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..count..), 
             breaks = list(x = mu_breaks, y = lambda_breaks), 
             alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # 
  scale_fill_distiller(palette = "Spectral") + # 塗りつぶしの色
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "frequency", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)


# 密度の等高線のアニメーションを作図
anime_freq_graph <- ggplot() + 
  geom_density2d_filled(data = anime_freq_df, mapping = aes(x = mu, y = lambda, fill = ..level..), 
                        alpha = 0.8) + # サンプル
  geom_contour(data = dens_df, mapping = aes(x = mu, y = lambda, z = density, color = ..level..)) + # 元の分布
  gganimate::transition_manual(parameter) + # 
  scale_color_viridis_c(option = "D") + # 等高線の色
  coord_cartesian(xlim = c(min(mu_vals), max(mu_vals)), 
                  ylim = c(min(lambda_vals), max(lambda_vals))) + # 軸の表示範囲
  labs(title = "Gaussian-Gamma Distribution", 
       subtitle = "{current_frame}", 
       fill = "density", color = "density", 
       x = expression(mu), y = expression(lambda)) # ラベル

# gif画像を作成
gganimate::animate(anime_freq_graph, nframes = frame_num+10, end_pause = 10, fps = 10, width = 800, height = 600)

