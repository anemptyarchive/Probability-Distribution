
# 確率分布に利用する関数 -------------------------------------------------------------

# 利用するパッケージ
library(tidyverse)


# ガンマ関数の計算 -------------------------------------------------------------------

# 変数の値を指定
x <- 4

# ガンマ関数の計算
y <- gamma(x)
y

# 対数ガンマ関数による計算
log_y <- lgamma(x)
y <- exp(log_y)
y


# ガンマ関数を計算:(発散)
gamma(171); gamma(172)

# 対数ガンマ関数による計算
lgamma(172)


# ガンマ関数を計算:(計算できない)
gamma(0); gamma(-3)


# ガンマ関数の作図 -------------------------------------------------------------------

### ・非負の値の場合 -----

# xの値を指定:(x > 0)
x_vals <- seq(from = 0.01, to = 6, by = 0.01)

# ガンマ関数の計算
gamma_df <- tidyr::tibble(
  x = x_vals, 
  y = gamma(x_vals)
)

# ガンマ関数を作図
ggplot(data = gamma_df, mapping = aes(x = x, y = y)) + # データ
  geom_line(color = "orange") + # 折れ線グラフ
  labs(title = "Gamma Function", 
       y = expression(Gamma(x))) # ラベル


### ・負の値を含むの場合 -----

# xの値を指定:(0と負の整数は計算できない)
x_vals <- seq(from = -5, to = 5, by = 0.0005)

# ガンマ関数の計算
gamma_df <- tidyr::tibble(
  x = x_vals, 
  y = gamma(x_vals)
)

# ガンマ関数を作図
ggplot(data = gamma_df, mapping = aes(x = x, y = y)) + # データ
  geom_line(color = "orange") + # 折れ線グラフ
  scale_x_continuous(breaks = seq(from = floor(min(x_vals)), to = floor(max(x_vals)))) + # x軸目盛
  ylim(c(-10, 10)) + # y軸の表示範囲
  labs(title = "Gamma Function", 
       y = expression(Gamma(x))) # ラベル


# ディガンマ関数の計算 -------------------------------------------------------------------

# 変数の値を指定
x <- 4

# ディガンマ関数の計算
y <- digamma(x)
y


# ディガンマ関数を計算:(計算できない)
digamma(0); digamma(-3)


# ディガンマ関数の作図 -----------------------------------------------------------------

### ・非負の値の場合 -----

# xの値を指定:(x > 0)
x_vals <- seq(from = 0.01, to = 5, by = 0.01)

# ディガンマ関数の計算
digamma_df <- tidyr::tibble(
  x = x_vals, 
  y = digamma(x_vals)
)

# ディガンマ関数を作図
ggplot(data = digamma_df, mapping = aes(x = x, y = y)) + # データ
  geom_line(color = "orange") + # 折れ線グラフ
  labs(title = "Digamma Function", 
       y = expression(psi(x))) # ラベル


### ・負の値を含む場合 -----

# xの値を指定:(0と負の整数は計算できない)
x_vals <- seq(from = -5, to = 5, by = 0.002)

# ディガンマ関数の計算
digamma_df <- tidyr::tibble(
  x = x_vals, 
  y = digamma(x_vals)
)

# ディガンマ関数を作図
ggplot(data = digamma_df, mapping = aes(x = x, y = y)) + # データ
  geom_line(color = "orange") + # 折れ線グラフ
  scale_x_continuous(breaks = seq(from = floor(min(x_vals)), to = floor(max(x_vals)))) + # x軸目盛
  ylim(c(-10, 10)) + # y軸の表示範囲
  labs(title = "Digamma Function", 
       y = expression(psi(x))) # ラベル


# ベータ関数の計算 ----------------------------------------------------------------

# 変数の値を指定
a <- 2.1
b <- 3.2

# ガンマ関数によりベータ関数の計算
z <- gamma(a) * gamma(b) / gamma(a + b)
z

# ガンマ関数によりベータ関数の計算
log_z <- lgamma(a) + lgamma(b) - lgamma(a + b)
z <- exp(log_z)
z

# ベータ関数の計算
z <- beta(a, b)
z

# 対数ベータ関数の計算
log_z <- lbeta(a, b)
z <- exp(log_z)
z

# ベータ関数の計算:(計算できない)
beta(2.5, 0); beta(-3.5, 2.5)


# ベータ関数の作図 -------------------------------------------------------------------

### ・非負の値の場合 -----

# x軸とy軸の値を指定:(a > 0, b > 0)
a_vals <- seq(from = 0.01, to = 3, by = 0.01)
b_vals <- seq(from = 0.01, to = 3, by = 0.01)

# 格子状の点を作成
points_df  <- expand.grid(alpha = a_vals, beta = b_vals)
a_grid <- points_df[["alpha"]]
b_grid <- points_df[["beta"]]

# ベータ関数の計算
z_grid <- beta(a_grid, b_grid)

# 値を格納
beta_df <- tidyr::tibble(
  alpha = a_grid, 
  beta = b_grid, 
  z = z_grid
)

# ベータ関数を作図
ggplot(data = beta_df, mapping = aes(x = alpha, y = beta, z = z, color = ..level..)) + 
  geom_contour() + # 等高線図
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Beta Function", 
       x = expression(alpha), y = expression(beta), color = expression(Beta(alpha, beta))) # ラベル

# ベータ関数を作図
ggplot(data = beta_df, mapping = aes(x = alpha, y = beta, z = z, fill = ..level..)) + 
  geom_contour_filled() + # 塗りつぶし等高線図
  scale_fill_brewer(palette = "Spectral", direction = -1) + # 塗りつぶしの色
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Beta Function", 
       x = expression(alpha), y = expression(beta), fill = expression(Beta(alpha, beta))) # ラベル


### ・負の値を含む場合 -----

# x軸とy軸の値を指定:(0と負の整数は計算できない)
a_vals <- seq(from = -2, to = 2, by = 0.01)
b_vals <- seq(from = -2, to = 2, by = 0.01)

# 格子状の点を作成
points_df  <- expand.grid(alpha = a_vals, beta = b_vals)
a_grid <- points_df[["alpha"]]
b_grid <- points_df[["beta"]]

# ベータ関数の計算
z_grid <- gamma(a_grid) * gamma(b_grid) / gamma(a_grid + b_grid)

# 値を格納
beta_df <- tidyr::tibble(
  alpha = a_grid, 
  beta = b_grid, 
  z = z_grid
)


# 閾値を指定
threshold <- 10

# ベータ関数を作図
ggplot(data = beta_df, mapping = aes(x = alpha, y = beta, z = z, color = ..level..)) + 
  #geom_contour() + # 等高線図:(デフォルト)
  geom_contour(breaks = seq(from = -threshold, to = threshold, length.out = 10)) + # 等高線図:(線の位置を指定)
  scale_color_distiller(palette = "Spectral") + # 等高線の色
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Beta Function", 
       x = expression(alpha), y = expression(beta), color = expression(Beta(alpha, beta))) # ラベル

# ベータ関数を作図
ggplot(data = beta_df, mapping = aes(x = alpha, y = beta, z = z, fill = ..level..)) + 
  #geom_contour_filled() + # 塗りつぶし等高線図:(デフォルト)
  geom_contour_filled(breaks = seq(from = -threshold, to = threshold, length.out = 10)) + # 塗りつぶし等高線図:(線の位置を指定)
  scale_fill_brewer(palette = "Spectral", direction = -1) + # 塗りつぶしの色
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Beta Function", 
       x = expression(alpha), y = expression(beta), fill = expression(Beta(alpha, beta))) # ラベル


