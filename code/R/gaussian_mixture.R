
# 1次元混合ガウス分布 -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# 混合分布の定義 -----------------------------------------------------------------

### ・パラメータの設定 -----

# クラスタ数を指定
K <- 3

# 混合比率を指定
pi_k <- c(0.45, 0.25, 0.3)

# K個の平均パラメータを指定
mu_k <- c(-4, 0, 2.6)

# K個の標準偏差パラメータを指定
sigma_k <- c(1, 1.2, 0.8)


# 確率変数の値を作成
x_vals <- seq(
  from = min(mu_k - sigma_k*3), 
  to = max(mu_k + sigma_k*3), 
  length.out = 401
)


### ・分布の計算 -----

# クラスタごとにガウス分布を計算
dens_cluster_df <- tidyr::expand_grid(
  k = 1:K, # クラスタ番号
  x = x_vals # x軸の値
) |> # クラスタごとに確率変数の値を複製
  dplyr::group_by(k) |> # クラスタごとの計算用にグループ化
  dplyr::mutate(
    dens_k = dnorm(x = x_vals, mean = mu_k[unique(k)], sd = sigma_k[unique(k)]), # クラスタごとの確率密度
    dens_k_weighted = pi_k[unique(k)] * dens_k, # 重み付け確率密度
    k = factor(k, levels = 1:K), # 色分け用に因子化
    param_label = paste0(
      "list(", 
      "k==", unique(k), 
      ", pi==", pi_k[unique(k)], 
      ", mu==", mu_k[unique(k)], 
      ", sigma==", sigma_k[unique(k)], 
      ")"
    ) # パラメータラベル
  ) |> 
  dplyr::ungroup() # グループ化を解除
dens_cluster_df

# 混合ガウス分布を計算
dens_mixture_df <- dens_cluster_df |> 
  dplyr::group_by(x) |> # 点ごとの計算用にグループ化
  dplyr::summarise(density = sum(dens_k_weighted), .groups = "drop") # K個の分布の加重平均を計算
dens_mixture_df


### ・分布の作図 -----

# パラメータラベルを作成
param_label_cluster <- dens_cluster_df[["param_label"]] |> 
  unique() |> 
  parse(text = _)
param_label_mixture <- paste0(
  "list(", 
  "K==", K, 
  ", pi==(list(", paste0(pi_k, collapse = ", "), "))", 
  ", mu==(list(", paste0(mu_k, collapse = ", "), "))", 
  ", sigma==(list(", paste0(sigma_k, collapse = ", "), "))", 
  ")"
)

# 混合ガウス分布を作図
ggplot() + 
  geom_line(data = dens_mixture_df, 
            mapping = aes(x = x, y = density, linetype = "mixture"), 
            color = "hotpink", size = 1) + # 混合分布の確率密度
  geom_line(data = dens_cluster_df, 
            mapping = aes(x = x, y = dens_k_weighted, color = k, linetype = "weighted"), 
            size = 1) + # クラスタごとの割り引き確率密度
  geom_line(data = dens_cluster_df, 
            mapping = aes(x = x, y = dens_k, color = k, linetype = "cluster"), 
            size = 1) + # クラスタごとの確率密度
  scale_color_hue(breaks = 1:K, labels = param_label_cluster, name = "cluster") + # 色の凡例ラベル
  scale_linetype_manual(breaks = c("cluster", "weighted", "mixture"), 
                        values = c("dotdash", "dashed", "solid"), 
                        labels = c("cluster k distribution", "weighted distribution", "mixture distribution"), 
                        name = "density") + # 線の種類
  theme(legend.text.align = 0) + # 図の体裁
  guides(linetype = guide_legend(override.aes = list(size = 0.5))) + # 凡例の体裁
  labs(title = "Gaussian Mixture Distribution", 
       subtitle = parse(text = param_label_mixture), 
       x = "x", y = "density")


# 確率密度の計算 -----------------------------------------------------------------









