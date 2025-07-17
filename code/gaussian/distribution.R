
# 1次元ガウス分布 --------------------------------------------------------------

# 分布の作図


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# パッケージ名の省略用
library(ggplot2)


# 確率密度の計算 -----------------------------------------------------------------

### ・パラメータの設定 -----

# 平均パラメータを指定
mu <- 2

# 標準偏差パラメータを指定
sigma <- 0.5

# 精度パラメータを指定
lambda <- 4

# 確率変数の値を指定
x <- 1.5


### ・標準偏差を使用 -----

# 定義式により確率密度を計算
C    <- 1 / sqrt(2 * pi) / sigma
dens <- C * exp(-0.5 * (x - mu)^2 / sigma^2)
dens

# 対数をとった定義式により確率密度を計算
log_C    <- -0.5 * log(2 * pi) - log(sigma)
log_dens <- log_C - 0.5 * (x - mu)^2 / sigma^2
dens     <- exp(log_dens)
dens; log_dens

# ガウス分布の関数により確率密度を計算
dens <- dnorm(x = x, mean = mu, sd = sigma)
dens

# ガウス分布の対数をとった関数により確率密度を計算
log_dens <- dnorm(x = x, mean = mu, sd = sigma, log = TRUE)
dens     <- exp(log_dens)
dens; log_dens

# 標準正規分布の関数により確率密度を計算
y    <- (x - mu) / sigma # 標準化
dens <- dnorm(x = y, mean = 0, sd = 1) / sigma
dens


### ・精度を使用 -----

# 定義式により確率密度を計算
C    <- sqrt(lambda / 2 / pi)
dens <- C * exp(-0.5 * lambda * (x - mu)^2)
dens

# 対数をとった定義式により確率密度を計算
log_C    <- -0.5 * (log(2 * pi) - log(lambda))
log_dens <- log_C - 0.5 * lambda * (x - mu)^2
dens     <- exp(log_dens)
dens; log_dens

# ガウス分布の関数により確率密度を計算
dens <- dnorm(x = x, mean = mu, sd = 1/sqrt(lambda))
dens

# ガウス分布の対数をとった関数により確率密度を計算
log_dens <- dnorm(x = x, mean = mu, sd = 1/sqrt(lambda), log = TRUE)
dens     <- exp(log_dens)
dens; log_dens

# 標準正規分布の関数により確率密度を計算
y    <- (x - mu) * sqrt(lambda) # 標準化
dens <- dnorm(x = y, mean = 0, sd = 1) * sqrt(lambda)
dens


# 統計量の計算 ------------------------------------------------------------------

# 平均を指定
mu <- 2

# 標準偏差を指定
sigma <- 0.5


# 標準偏差から分散を計算
sigma2 <- sigma^2
sigma2

# 分散から精度を計算
lambda <- 1 / sigma2
lambda

# 精度から標準偏差を計算
sigma <- 1 / sqrt(lambda)
sigma


# グラフの作成 ------------------------------------------------------------------

# 平均パラメータを指定
mu <- 0

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- seq(from = mu - sigma*4, to = mu + sigma*4, length.out = 251)

# ガウス分布を計算
dens_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = sigma) # 確率密度
)


# ガウス分布のグラフを作成
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 折れ線グラフ
  labs(
    title = "Gaussian Distribution", 
    subtitle = paste0("mu=", mu, ", sigma=", sigma), # (文字列表記用)
    #subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), # (数式表記用)
    x = "x", y = "density"
  ) # ラベル


# 統計量を重ねたガウス分布のグラフを作成:線のみ
ggplot(data = dens_df, mapping = aes(x = x, y = density)) + # データ
  geom_line(color = "#00A968", size = 1) + # 分布
  geom_vline(xintercept = mu, color = "blue", size = 1, linetype = "dashed") + # 期待値
  geom_vline(xintercept = mu-sigma, color = "orange", size = 1, linetype = "dotted") + # 期待値 - 標準偏差
  geom_vline(xintercept = mu+sigma, color = "orange", size = 1, linetype = "dotted") + # 期待値 + 標準偏差
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), 
    x = "x", y = "density"
  ) # ラベル


# 統計量を格納
stat_df <- tibble::tibble(
  statistic = c(mu, mu-sigma, mu+sigma), # 統計量
  type = c("mean", "sd", "sd") # 色分け用ラベル
)

# 凡例用の設定を作成:(数式表示用)
color_vec    <- c(mean = "blue", sd = "orange")
linetype_vec <- c(mean = "dashed", sd = "dotted")
label_vec    <- c(mean = expression(E(x)), sd = expression(E(x) %+-% sqrt(V(x))))

# 統計量を重ねたポアソン分布のグラフを作成:凡例付き
ggplot() + 
  geom_line(data = dens_df, mapping = aes(x = x, y = density), 
            color = "#00A968", size = 1) + # 分布
  geom_vline(data = stat_df, mapping = aes(xintercept = statistic, color = type, linetype = type), 
             size = 1) + # 期待値
  scale_color_manual(values = color_vec, labels = label_vec, name = "statistic") + # 線の色:(色指定と数式表示用)
  scale_linetype_manual(values = linetype_vec, labels = label_vec, name = "statistic") + # 線の種類:(線指定と数式表示用)
  guides(color = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gaussian Distribution", 
       subtitle = parse(text = paste0("list(mu==", mu, ", sigma==", sigma, ")")), 
       x = "x", y = "density") # ラベル


