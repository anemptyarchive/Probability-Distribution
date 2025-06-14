
# ガンマ分布 -------------------------------------------------------------------

# 1次元ガウス分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(patchwork)

# パッケージ名の省略用
library(ggplot2)


# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# 超パラメータを指定
a <- 5
b <- 2

# 分布の数(サンプルサイズ)を指定
N <- 10


# ポアソン分布・ガウス分布のパラメータを生成
lambda_n <- rgamma(n = N, shape = a, rate = b) |> # ガンマ分布の乱数を生成
  sort() # 昇順に並べ替え

# パラメータを格納
param_df <- tibble::tibble(
  lambda = lambda_n, 
  parameter = round(lambda_n, 3) |> 
    factor(), # 色分け用ラベル
  param_rev = round(lambda_n, 3) |> 
    factor(levels = round(rev(lambda_n), 3)) # 色分け用ラベル:逆順
)


# lambdaの値を作成
lambda_vals <- seq(from = 0, to = a/b * 4, length.out = 501)

# ガンマ分布を計算
gamma_df <- tidyr::tibble(
  lambda = lambda_vals, # 確率変数
  density = dgamma(x = lambda_vals, shape = a, rate = b) # 確率密度
)

# ガンマ分布の期待値を計算
E_lambda <- a / b


# 生成分布とパラメータのサンプルを作図:通常
gamma_graph <- ggplot() + 
  geom_vline(xintercept = E_lambda, 
             color = "red", size = 1, linetype = "dashed") + # パラメータの期待値
  geom_line(data = gamma_df, mapping = aes(x = lambda, y = density), 
            color = "#00A968", size = 1) + # パラメータの生成分布
  geom_point(data = param_df, mapping = aes(x = lambda, y = 0, color = parameter), 
             size = 6, alpha = 0.8, show.legend = FALSE) + # パラメータのサンプル
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) + # 凡例の体裁
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       color = expression(lambda), 
       x = expression(lambda), y = "density") # ラベル
gamma_graph

# 生成分布とパラメータのサンプルを作図:軸の入替
gamma_graph_flip <- ggplot() + 
  geom_vline(xintercept = E_lambda, 
             color = "red", size = 1, linetype = "dashed") + # パラメータの期待値
  geom_line(data = gamma_df, mapping = aes(x = lambda, y = density), 
            color = "#00A968", size = 1) + # パラメータの生成分布
  geom_point(data = param_df, mapping = aes(x = lambda, y = 0, color = param_rev), 
             alpha = 0.8, size = 6, show.legend = FALSE) + # パラメータのサンプル
  coord_flip() + # 軸の入れ替え
  labs(title = "Gamma Distribution", 
       subtitle = paste0("a=", a, ", b=", b), 
       color = expression(lambda), 
       x = expression(lambda), y = "density") # ラベル
gamma_graph_flip


### ・分布の作図：1次元ガウス分布 -----

# 平均パラメータを指定
mu = 0

# 標準偏差の期待値を計算
E_sigma <- sqrt(1 / E_lambda)


# xの値を作成
x_vals <- seq(from = mu-E_sigma * 4, to = mu+E_sigma * 4, length.out = 500)

# パラメータのサンプルごとにガウス分布を計算
res_gaussian_df <- tidyr::expand_grid(
  x = x_vals, 
  lambda = lambda_n
) |> # 全ての組み合わせを作成
  dplyr::arrange(lambda, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sqrt(1/lambda)), # 確率密度
    param_rev = round(lambda, 3) |> 
      factor(levels = round(rev(lambda_n), 3)) # 色分け用ラベル:逆順
  )

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = mu, sd = E_sigma) # 確率密度
)


# サンプルと期待値によるガウス分布を作図
gaussian_graph <- ggplot() + 
  geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density,), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_line(data = res_gaussian_df, mapping = aes(x = x, y = density, color = param_rev), 
            alpha = 0.8, size = 1) + # サンプルによる分布
  theme(legend.text.align = 0) + # 図の体裁:凡例
  labs(title = "Gaussian Distribution", 
       subtitle = parse(text = paste0("list(E(lambda)==", round(E_lambda, 3), ", mu==", mu, ")")), 
       color = expression(lambda), 
       x = "x", y  = "density") # ラベル

# グラフを並べて描画
gamma_graph_flip + gaussian_graph + 
  patchwork::plot_layout(widths = c(1, 2), guides = "collect")


