
# 1次元ガウス分布 --------------------------------------------------------------

# 1次元ガウス分布の生成


# パッケージの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)

# パッケージ名の省略用
library(ggplot2)
library(patchwork)



# 分布の生成 -------------------------------------------------------------------

### ・パラメータの生成 -----

# パラメータを指定
mu_gen    <- 5
sigma_gen <- 2.5

# 分布の数(サンプルサイズ)を指定
N <- 10


# ガウス分布の平均パラメータを生成
mu_n <- rnorm(n = N, mean = mu_gen, sd = sigma_gen) |> # ガウス分布の乱数を生成
  sort() # 昇順に並べ替え

# パラメータを格納
param_df <- tibble::tibble(
  mu = mu_n, 
  parameter = round(mu_n, 3) |> 
    factor() # 色分け用ラベル
)


# muの値を作成
mu_vals <- seq(from = mu_gen - sigma_gen*3, to = mu_gen + sigma_gen*3, length.out = 251)

# 生成分布を計算
gaussian_gen_df <- tibble::tibble(
  mu = mu_vals, # 確率変数
  density = dnorm(x = mu_vals, mean = mu_gen, sd = sigma_gen) # 確率密度
)

# 平均パラメータの期待値を計算
E_mu <- mu_gen


# 生成分布とパラメータのサンプルを作図
gaussian_gen_graph <- ggplot() + # データ
  geom_vline(xintercept = E_mu, 
             color = "red", size = 1, linetype = "dashed") + # パラメータの期待値
  geom_line(data = gaussian_gen_df, mapping = aes(x = mu, y = density), 
            color = "#00A968", size = 1) + # パラメータの生成分布
  geom_point(data = param_df, mapping = aes(x = mu, y = 0, color = parameter), 
             alpha = 0.8, size = 6, show.legend = FALSE) + # パラメータのサンプル
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) + # 凡例の体裁
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(
      text = paste0("list(mu[paste(g,e,n)]==", mu_gen, ", sigma[paste(g,e,n)]==", sigma_gen, ")"), 
    ), 
    color = expression(mu), 
    x = expression(mu), y = "density"
  ) # ラベル
gaussian_gen_graph


### ・分布の作図：1次元ガウス分布 -----

# 標準偏差パラメータを指定
sigma <- 1


# xの値を作成
x_vals <- mu_vals

# パラメータのサンプルごとにガウス分布を計算
res_gaussian_df <- tidyr::expand_grid(
  x = x_vals, 
  mu = mu_n
) |> # 全ての組み合わせを作成
  dplyr::arrange(mu, x) |> # パラメータごとに並べ替え
  dplyr::mutate(
    density = dnorm(x = x, mean = mu, sd = sigma), # 確率密度
    parameter = round(mu, 3) |> 
      factor() # 色分け用ラベル
  )

# パラメータの期待値によるガウス分布を計算
E_gaussian_df <- tidyr::tibble(
  x = x_vals, # 確率変数
  density = dnorm(x = x_vals, mean = E_mu, sd = sigma) # 確率密度
)


# サンプルと期待値によるガウス分布を作図
gaussian_graph <- ggplot() + 
  geom_line(data = E_gaussian_df, mapping = aes(x = x, y = density), 
            color = "red", size = 1, linetype = "dashed") + # 期待値による分布
  geom_line(data = res_gaussian_df, mapping = aes(x = x, y = density, color = parameter), 
            alpha = 0.8, size = 1) + # サンプルによる分布
  labs(
    title = "Gaussian Distribution", 
    subtitle = parse(text = paste0("list(E(mu)==", E_mu, ", sigma==", sigma, ")"), ), 
    color = expression(mu), 
    x = "x", y = "density"
  ) # ラベル
gaussian_graph

# グラフを並べて描画
gaussian_gen_graph / gaussian_graph + 
  patchwork::plot_layout(guides = "collect")


