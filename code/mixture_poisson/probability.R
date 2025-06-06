
# 混合ポアソン分布 --------------------------------------------------------------

# 分布の作図


# ライブラリの読込 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(MCMCpack)

# パッケージ名の省略用
library(ggplot2)


### パラメータの設定 -----

# K個のパラメータを指定
lambda_k <- c(10, 25, 40)

# 混合比率を指定
pi_k <- c(0.35, 0.25, 0.4)

# クラスタ数を取得
K <- length(lambda_k)


# x軸の最大値を指定
x_max <- max(lambda_k) * 2

# x軸の値を作成
x_vec <- seq(from = 0, to = x_max, by = 1)


### 分布の計算 -----

# クラスタごとの重み付け確率を計算
cluster_prob_df <- tidyr::expand_grid(
  k = 1:K,  # クラスタ番号
  x = x_vec # 確率変数の値
) |> # クラスタごとに変数を複製
  dplyr::mutate(
    lambda = lambda_k[k], # パラメータ
    pi     = pi_k[k],     # 混合比率
    prob = pi * dpois(x = x, lambda = lambda) # 重み付け確率
  )

# 確率を計算
prob_df <- cluster_prob_df |> 
  dplyr::reframe(
    prob = prob, # 混合確率
    .by = x
  )


### 分布の作図 -----

# ラベル用の文字列を作成
param_lbl <- paste0(
  "list(", 
  "K == ", K, ", ", 
  "lambda == (list(", paste0(lambda_k, collapse = ", "), ")), ", 
  "pi == (list(", paste0(round(pi_k, digits = 2), collapse = ", "), "))", 
  ")"
)

# 分布を作図
ggplot() + 
  geom_bar(
    data = prob_df, 
    mapping = aes(x = x, y = prob), 
    stat = "identity"
  ) + # 分布
  labs(
    title = "mixture Poisson distribution", 
    subtitle = parse(text = param_lbl), 
    x = expression(x), 
    y = "probability"
  )

# クラスタのごとの分布を作図
ggplot() + 
  geom_bar(
    data = cluster_prob_df, 
    mapping = aes(x = x, y = prob, fill = factor(k)), 
    stat = "identity", position = "stack", 
    alpha = 0.5
  ) + # 混合分布
  geom_line(
    data = cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k)), 
    linewidth = 1
  ) + # クラスタごとの重み付け分布
  geom_point(
    data = cluster_prob_df, 
    mapping = aes(x = x, y = prob, color = factor(k)), 
    size = 2.5
  ) + # クラスタごとの重み付け分布
  geom_vline(
    mapping = aes(xintercept = lambda_k, color = factor(1:K)), 
    linetype = "dashed"
  ) + # クラスタごとの期待値
  geom_text(
    mapping = aes(x = lambda_k, y = -Inf, label = paste0("lambda[", 1:K, "]")), 
    hjust = 0.5, vjust = 0, parse = TRUE
  ) + # クラスタごとの期待値ラベル
  geom_segment(
    mapping = aes(
      x = lambda_k-sqrt(lambda_k), y = -Inf, 
      xend = lambda_k+sqrt(lambda_k), yend = -Inf, 
      color = factor(1:K)
    )
  ) + # クラスタごとの標準偏差
  geom_text(
    mapping = aes(x = lambda_k-sqrt(lambda_k), y = -Inf, label = "|", color = factor(1:K)), 
    size = 5
  ) + # クラスタごとの標準偏差の始点
  geom_text(
    mapping = aes(x = lambda_k+sqrt(lambda_k), y = -Inf, label = "|", color = factor(1:K)), 
    size = 5
  ) + # クラスタごとの標準偏差の終点
  scale_fill_hue(label = parse(text = paste0("k == ", 1:K))) + # クラスタ番号
  guides(
    fill = guide_legend(override.aes = list(alpha = 1)), 
    color = FALSE
  ) + # 凡例の体裁
  labs(
    title = "mixture Poisson distribution", 
    subtitle = parse(text = param_lbl), 
    fill = "cluster", 
    x = expression(x), 
    y = "probability"
  )


